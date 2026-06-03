# ЧАСТИНА 3: Частково-лінійна модель (PLM) 

library(tidyverse)
library(np)
library(ggplot2)
library(broom)


# 1. Завантаження даних

df <- read_csv("data/processed/nonparametric_model_data.csv", show_col_types = FALSE) %>%
  mutate(
    speed_limit_factor = factor(speed_limit, levels = c(20, 30, 40, 50, 60, 70)),
    rural_factor = factor(rural_factor, levels = c("Urban", "Rural")),
    light_conditions = as.factor(light_conditions),
    weather_conditions = as.factor(weather_conditions),
    road_surface_conditions = as.factor(road_surface_conditions),
    road_type = as.factor(road_type),
    first_road_class = as.factor(first_road_class),
    day_type = as.factor(day_type),
    police_force = as.factor(police_force),
    hour = as.numeric(hour),
    speed_30 = as.integer(speed_limit == 30),
    speed_40 = as.integer(speed_limit == 40),
    speed_50 = as.integer(speed_limit == 50),
    speed_60 = as.integer(speed_limit == 60),
    speed_70 = as.integer(speed_limit == 70)
  )

# 2. Завантаження базової LPM моделі
baseline_model <- readRDS("nonparametric/outputs/models/model_baseline_lpm.rds")

lpm_tidy_robust <- broom::tidy(baseline_model) %>%
  filter(str_detect(term, "speed_limit_factor")) %>%
  mutate(model = "Базова LPM (Робастні SE)")

# 3. PLM: speed_limit (parametric) | hour (nonparametric)
plm_formula <- severe ~ speed_30 + speed_40 + speed_50 + speed_60 + speed_70 +
  rural_factor + light_conditions + weather_conditions +
  road_surface_conditions + road_type + first_road_class +
  day_type + police_force | hour

cat("=== Крок 1: Крос-валідація bandwidth на підвибірці ===\n")
set.seed(2026)
subset_size <- 8000                   
subset_indices <- sort(sample(1:nrow(df), size = subset_size))

bw_plm <- npplregbw(
  formula = plm_formula,
  data = df,
  subset = subset_indices,
  bwmethod = "cv.ls",
  ckertype = "gaussian",
  tol = 1e-4,
  ftol = 1e-4
)

cat("Bandwidth для hour (непараметрична частина):", 
    round(as.numeric(bw_plm$bw[[1]]$bw), 4), "\n")

print(summary(bw_plm))

cat("\n=== Крок 2: Оцінка PLM на повному датасеті ===\n")
pl_model <- npplreg(bws = bw_plm, data = df)
saveRDS(pl_model, "nonparametric/outputs/models/model_plm_person3.rds")
cat("PLM модель оцінено!\n")

# КРОК 3: Bootstrap SE
cat("\n=== Крок 3: Bootstrap для SE ===\n")
set.seed(2026)
n_boot <- 150

boot_coefs <- matrix(NA, nrow = n_boot, ncol = length(pl_model$xcoef))
colnames(boot_coefs) <- names(pl_model$xcoef)

pb <- txtProgressBar(min = 0, max = n_boot, style = 3)

for(i in 1:n_boot) {
  boot_idx <- sample(1:nrow(df), replace = TRUE)
  boot_model <- tryCatch(
    npplreg(bws = bw_plm, data = df[boot_idx, ]),
    error = function(e) NULL
  )
  if (!is.null(boot_model)) {
    boot_coefs[i, ] <- as.numeric(boot_model$xcoef)
  }
  setTxtProgressBar(pb, i)
}
close(pb)

plm_se_boot <- apply(boot_coefs, 2, sd, na.rm = TRUE)

# Підготовка результатів — xcoeferr 
plm_results_clean <- tibble(
  term      = names(pl_model$xcoef),
  estimate  = as.numeric(pl_model$xcoef),
  std.error = as.numeric(pl_model$xcoeferr) 
) %>%
  filter(str_detect(term, "^speed_")) %>%
  mutate(
    statistic = estimate / std.error,
    p.value   = 2 * pnorm(-abs(statistic)),
    term = case_when(
      term == "speed_30" ~ "speed_limit_factor30",
      term == "speed_40" ~ "speed_limit_factor40",
      term == "speed_50" ~ "speed_limit_factor50",
      term == "speed_60" ~ "speed_limit_factor60",
      term == "speed_70" ~ "speed_limit_factor70",
      TRUE ~ term
    ),
    model = "Частково лінійна PLM"
  )

print(plm_results_clean)

# get_mode + модальні значення 
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

mode_values <- list(
  speed_limit_factor      = get_mode(df$speed_limit_factor),  
  rural_factor            = get_mode(df$rural_factor),
  light_conditions        = get_mode(df$light_conditions),
  weather_conditions      = get_mode(df$weather_conditions),
  road_surface_conditions = get_mode(df$road_surface_conditions),
  road_type               = get_mode(df$road_type),
  first_road_class        = get_mode(df$first_road_class),
  day_type                = get_mode(df$day_type),
  police_force            = get_mode(df$police_force),
  hour                    = as.numeric(get_mode(df$hour))
)

# Таблиця типових значень 
cat("\n=== Типові (модальні) значення контрольних змінних ===\n")
mode_table <- tibble(
  `Змінна` = c("Швидкісний режим", "Тип місцевості", "Освітлення",
               "Погода", "Стан дороги", "Тип дороги",
               "Клас дороги", "Тип дня", "Поліція", "Година"),
  `Мода`   = c(
    paste0(mode_values$speed_limit_factor, " mph"),
    as.character(mode_values$rural_factor),
    as.character(mode_values$light_conditions),
    as.character(mode_values$weather_conditions),
    as.character(mode_values$road_surface_conditions),
    as.character(mode_values$road_type),
    as.character(mode_values$first_road_class),
    as.character(mode_values$day_type),
    as.character(mode_values$police_force),
    paste0(mode_values$hour, " год")
  )
)
print(as.data.frame(mode_table), row.names = FALSE)

# ГРАФІК 1: g(hour)
ggplot(hour_plot_df, aes(x = hour, y = prob)) +
  geom_line(color = "#4A148C", linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "lavender", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title    = "Нелінійний вплив часу доби на ймовірність тяжкої ДТП",
    subtitle = paste0(
      "Частково лінійна модель • Контролі зафіксовані на модальних значеннях\n",
      "Швидкість: ", mode_values$speed_limit_factor, " mph  •  ",
      "Місцевість: ", mode_values$rural_factor, "  •  ",
      "Освітлення: ", mode_values$light_conditions
    ),
    x = "Година доби",
    y = "P(severe = 1 | X)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9.5, color = "grey30")
  )

ggsave("nonparametric/outputs/figures/plm_hour_effect.png",
       width = 10, height = 7, dpi = 200)

# ГРАФІК 2: Швидкість
ggplot(speed_plot_df, aes(x = speed, y = prob)) +
  geom_point(size = 4.5, color = "darkblue") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                width = 0.25, linewidth = 1.1, color = "#00C9FF") +
  labs(
    title    = "Прогнозована ймовірність тяжкого ДТП залежно від ліміту швидкості",
    subtitle = paste0(
      "Частково лінійна модель (PLM)  •  Година зафіксована на моді (",
      mode_values$hour, ":00)\n",
      "Інші контролі: ", mode_values$rural_factor, ", ",
      mode_values$light_conditions, ", ", mode_values$weather_conditions
    ),
    x = "Швидкісний режим (mph)",
    y = "P(severe = 1 | X)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9.5, color = "grey30")
  )

ggsave("nonparametric/outputs/figures/plm_speed_marginal_effects.png",
       width = 10, height = 7.5, dpi = 200)

# ГРАФІК 3: Caterpillar
ggplot(caterpillar_data,
       aes(x = clean_term, y = estimate, color = model)) +
  geom_point(position = position_dodge(0.5), size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(0.5), width = 0.35, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  coord_flip() +
  scale_color_manual(values = c(
    "Базова LPM (Робастні SE)" = "#88D8B0",
    "Частково лінійна PLM"     = "#FFB085"
  )) +
  labs(
    title    = "Ефекти швидкісних режимів на P(severe) відносно базової категорії (20 mph)",
    subtitle = "Базова LPM проти частково лінійної моделі (PLM)",
    x        = NULL,
    y        = expression(hat(beta) ~ "(95% CI)"),
    color    = "Специфікація моделі"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("nonparametric/outputs/figures/model_comparison_caterpillar.png",
       width = 11, height = 7.5, dpi = 200)

# Збереження таблиць
write_csv(plm_results_clean %>% select(-model),
          "nonparametric/outputs/tables/plm_speed_limit_coefficients.csv")

final_table_df <- left_join(
  lpm_tidy_robust   %>% select(term, estimate, std.error, p.value),
  plm_results_clean %>% select(term, estimate, std.error, p.value),
  by     = "term",
  suffix = c(" (LPM)", " (PLM)")
) %>%
  mutate(term = str_replace(term, "speed_limit_factor", "Швидкість: ") %>%
           paste0(" mph")) %>%
  rename(`Пояснююча змінна` = term)

write_csv(final_table_df,
          "nonparametric/outputs/tables/plm_vs_baseline_speed_limit.csv")

cat("ЗВЕДЕНА ТАБЛИЦЯ ПОРІВНЯННЯ ЕФЕКТІВ ШВИДКІСНОГО РЕЖИМУ (LPM vs PLM)\n")
print(as.data.frame(final_table_df), row.names = FALSE)
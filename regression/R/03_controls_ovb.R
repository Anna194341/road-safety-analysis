library(tidyverse)
library(sandwich)    
library(lmtest)     
library(car)        
library(knitr)
library(broom)

df <- read_csv("data/processed/model_data_imputed.csv")

base_model <- readRDS("regression/outputs/models/base_model.rds")

glimpse(df)
table(df$severe)

# 1. Підготовка змінних 
df <- df %>%
  mutate(
    light_conditions        = factor(light_conditions),
    weather_conditions      = factor(weather_conditions),
    road_surface_conditions = factor(road_surface_conditions),
    road_type               = factor(road_type),
    day_type                = factor(day_type),
    speed_limit             = factor(speed_limit) 
  )

# Задаємо базові рівні для інтерпретації
df$light_conditions    <- relevel(df$light_conditions,    ref = "Daylight")
df$weather_conditions  <- relevel(df$weather_conditions,  ref = "Fine no high winds")
df$road_type           <- relevel(df$road_type,           ref = "Single carriageway")


# 2. Базові коефіцієнти для OVB-порівняння 
base_key <- read_csv("regression/outputs/tables/base_model_key_coefficients.csv")

# 3. Модель з контролями 
controls_model <- lm(
  severe ~ is_night
  + rural
  + speed_limit
  + light_conditions
  + weather_conditions
  + road_surface_conditions
  + road_type
  + day_type,
  data = df
)

summary(controls_model)

# 4. Таблиця ключових коефіцієнтів 
key_terms_to_extract <- base_key$term 

controls_key <- tidy(controls_model) %>%
  filter(term %in% key_terms_to_extract) %>%
  rename(
    std_error_raw = std.error,
    p_value_raw = p.value
  ) %>%
  mutate(
    model = "controls_model",
    val_vp = round(estimate * 100, 2),
    direction = if_else(val_vp >= 0, "зростання", "зниження"),
    abs_vp = abs(val_vp),
    
    interpretation_note = case_when(
      term == "is_night" ~ paste0("За наявності контрольних змінних, нічний час призводить до ", direction, " ймовірності тяжкого ДТП на ", abs_vp, " в.п."),
      term == "rural" ~ paste0("За наявності контрольних змінних, сільська місцевість призводить до ", direction, " ймовірності тяжкого ДТП на ", abs_vp, " в.п."),
      str_detect(term, "speed_limit") ~ paste0("За наявності контрольних змінних, ліміт швидкості ", str_replace(term, "speed_limit", ""), " миль/год призводить до ", direction, " ймовірності тяжкого ДТП на ", abs_vp, " в.п. порівняно з базовим лімітом."),
      TRUE ~ paste0("Ефект чинника ", term, " призводить до ", direction, " ймовірності на ", abs_vp, " в.п.")
    )
  ) %>%
  select(model, term, estimate, std_error_raw, p_value_raw, interpretation_note)

write_csv(controls_key, "regression/outputs/tables/controls_model_key_coefficients.csv")
# 5. OVB-порівняння 
comparison <- bind_rows(
  base_key, 
  controls_key
) %>%
  select(model, term, estimate) %>%
  pivot_wider(names_from = model, values_from = estimate) %>%
  mutate(
    delta_estimate = controls_model - base_model,
    pct_change     = (delta_estimate / abs(base_model)) * 100,
    
    ovb_conclusion = case_when(
      pct_change < -10 ~ paste0("Вплив чинника суттєво зменшився (на ", round(abs(pct_change), 1), "%). Початкова модель мала позитивне зміщення OVB."),
      pct_change > 10  ~ paste0("Вплив чинника суттєво збільшився (на ", round(pct_change, 1), "%). Початкова модель мала негативне зміщення OVB."),
      TRUE ~ "Коефіцієнт стабільний, додавання контролів майже не змінило оцінку."
    )
  )

print(comparison)
write_csv(comparison, "regression/outputs/tables/base_vs_controls_comparison.csv")

# 6. Joint tests 
vcov_controls <- vcovHC(controls_model, type = "HC1")

# 6a. light_conditions
light_terms <- grep("^light_conditions", names(coef(controls_model)), value = TRUE)
jt_light    <- linearHypothesis(controls_model, light_terms, vcov. = vcov_controls)

# 6b. weather_conditions
weather_terms <- grep("^weather_conditions", names(coef(controls_model)), value = TRUE)
jt_weather    <- linearHypothesis(controls_model, weather_terms, vcov. = vcov_controls)

# 6c. road_surface_conditions
surface_terms <- grep("^road_surface_conditions", names(coef(controls_model)), value = TRUE)
jt_surface    <- linearHypothesis(controls_model, surface_terms, vcov. = vcov_controls)

# 6d. road_type
roadtype_terms <- grep("^road_type", names(coef(controls_model)), value = TRUE)
jt_road        <- linearHypothesis(controls_model, roadtype_terms, vcov. = vcov_controls)

# 6e. Зводимо в одну таблицю
extract_jt <- function(jt_obj, group_name) {
  tibble(
    group       = group_name,
    F_statistic = jt_obj$F[2],
    df_num      = jt_obj$Df[2],
    p_value     = jt_obj$`Pr(>F)`[2]
  )
}

joint_tests_controls <- bind_rows(
  extract_jt(jt_light,   "light_conditions"),
  extract_jt(jt_weather, "weather_conditions"),
  extract_jt(jt_surface, "road_surface_conditions"),
  extract_jt(jt_road,    "road_type")
) %>%
  mutate(
    significant_5pct = p_value < 0.05,
    
    group_ukr = case_when(
      group == "light_conditions" ~ "Умови освітлення",
      group == "weather_conditions" ~ "Погодні умови",
      group == "road_surface_conditions" ~ "Стан поверхні дороги",
      group == "road_type" ~ "Тип дорожнього полотна",
      TRUE ~ group
    )
  ) %>%
  select(group, group_ukr, F_statistic, df_num, p_value, significant_5pct)

jt_pretty <- joint_tests_controls %>%
  select(group_ukr, F_statistic, df_num, p_value, significant_5pct) %>%
  mutate(
    F_statistic = round(F_statistic, 2),
    p_value = sprintf("%.2e", p_value),
    significant_5pct = if_else(significant_5pct == TRUE, "Значуща ", "Незначуща ")
  )

cat("\n=== Таблиця 3.2 — Результати спільних тестів (Joint Tests) ===\n")
kable(jt_pretty, 
      format = "markdown", 
      align = "lrrcr",
      col.names = c("Група чинників", "F-статистика", "df", "p-value", "Статус"))
write_csv(joint_tests_controls, "regression/outputs/tables/joint_tests_controls.csv")

cat("\n Всі файли збережено!\n")


# Plot 1: OVB-порівняння коефіцієнтів
comparison_long <- bind_rows(
  base_key,
  controls_key
) %>%
  mutate(
    model = factor(model,
                   levels = c("base_model", "controls_model"),
                   labels = c("Базова модель", "Модель з контролями"))
  ) %>%
  filter(term %in% c("is_night", "rural", "speed_limit70")) %>%
  mutate(
    term = factor(term,
                  levels = c("is_night", "rural", "speed_limit70"),
                  labels = c("Ніч", "Сільська місцевість", "Обмеження 70 миль/год"))
  )

ggplot(comparison_long, aes(x = term, y = estimate, fill = model)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std_error_raw,
        ymax = estimate + 1.96 * std_error_raw),
    position = position_dodge(width = 0.6), width = 0.2
  ) +
  geom_text(
    aes(label = round(estimate, 3), 
        y = estimate + 1.96 * std_error_raw), 
    position = position_dodge(width = 0.6),
    vjust = -0.5, 
    size = 4,
    color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("#4E79A7", "#E15759")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
  labs(
    x = NULL, 
    y = "LPM коефіцієнт ",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom", 
    panel.grid.minor = element_blank() 
  )

# Plot 2: Joint tests 
ggplot(joint_tests_controls,
       aes(x = reorder(group, p_value), y = -log10(p_value), fill = significant_5pct)) +
  geom_col(width = 0.5, alpha = 0.9) + 
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  annotate("text", x = 0.6, y = -log10(0.05) + 0.2, label = "Поріг 5%", 
           color = "#C0392B", size = 4, fontface = "italic", hjust = 0) +
  geom_text(aes(label = round(-log10(p_value), 1), y = -log10(p_value)),
            hjust = -0.2, size = 4, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("TRUE" = "#59A14F", "FALSE" = "#F28E2B")) +
  scale_x_discrete(labels = c(
    "light_conditions"        = "Освітлення",
    "weather_conditions"      = "Погода",
    "road_surface_conditions" = "Стан покриття",
    "road_type"               = "Тип дороги"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    x = NULL, 
    y = "Рівень значущості (-log10 p-value)" 
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 12)) 
  )

# Plot 3: Coefficient plot controls model 
coef_df <- as.data.frame(summary(controls_model)$coefficients) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, se = `Std. Error`, p_value = `Pr(>|t|)`) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    significant = p_value < 0.05,
    term_clean = case_when(
      str_detect(term, "light_conditions") ~ str_replace(term, "light_conditions", "Освітлення: "),
      str_detect(term, "weather_conditions") ~ str_replace(term, "weather_conditions", "Погода: "),
      str_detect(term, "road_surface_conditions") ~ str_replace(term, "road_surface_conditions", "Покриття: "),
      str_detect(term, "road_type") ~ str_replace(term, "road_type", "Тип дороги: "),
      str_detect(term, "speed_limit") ~ str_replace(term, "speed_limit", "Швидкість: "),
      term == "day_typeWeekend" ~ "Вихідний день",
      term == "is_night" ~ "Ніч",
      term == "rural" ~ "Сільська місцевість",
      TRUE ~ term
    )
  )

ggplot(coef_df, aes(x = reorder(term_clean, estimate),
                    y = estimate, color = significant)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se,
                    ymax = estimate + 1.96 * se), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "#E15759", "FALSE" = "#BAB0AC"),
                     labels = c("TRUE" = "Значущий вплив (p < 0.05)", "FALSE" = "Незначущий вплив (p ≥ 0.05)")) +
  coord_flip() +
  labs(
    x = NULL, 
    y = "Коефіцієнт LPM",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom", 
    panel.grid.minor = element_blank() 
  )
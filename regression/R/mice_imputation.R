# ============================================================
# MICE-імпутація та фінальні датасети
# ============================================================

library(tidyverse)
library(mice)

plots_dir <- "plots/regression/missing"
models_dir <- "regression/outputs/models"

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

mice_model_path <- file.path(models_dir, "imp_final_5x5.rds")


df_raw <- read_csv("data/processed/collisions_2024_cleaned.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# Вибір змінних
# ------------------------------------------------------------

selected_vars <- c(
  "collision_severity",
  "time",
  "day_of_week",
  "urban_or_rural_area",
  "speed_limit",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "road_type",
  "number_of_vehicles"
)

df_selected <- df_raw %>%
  select(all_of(selected_vars))

# ------------------------------------------------------------
# Спільні базові змінні для регресії
# ------------------------------------------------------------

df_model <- df_selected %>%
  mutate(
    hour = as.integer(str_extract(as.character(time), "^\\d{1,2}")),

    severe = case_when(
      collision_severity %in% c("Fatal", "Serious") ~ 1,
      collision_severity == "Slight" ~ 0,
      TRUE ~ NA_real_
    ),

    is_night = case_when(
      hour %in% 0:4 ~ 1,
      !is.na(hour) ~ 0,
      TRUE ~ NA_real_
    ),

    rural = case_when(
      urban_or_rural_area == "Rural" ~ 1,
      urban_or_rural_area == "Urban" ~ 0,
      TRUE ~ NA_real_
    ),

    day_type = case_when(
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend",
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday",
      TRUE ~ NA_character_
    )
  )

model_vars <- c(
  "severe",
  "hour",
  "is_night",
  "rural",
  "day_type",
  "speed_limit",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "road_type"
)

df_model_vars <- df_model %>%
  select(all_of(model_vars))


make_missing_summary <- function(data) {
  data %>%
    summarise(
      across(
        everything(),
        list(
          n_missing = ~ sum(is.na(.x)),
          pct_missing = ~ round(mean(is.na(.x)) * 100, 3),
          n_non_missing = ~ sum(!is.na(.x))
        )
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.+)_(n_missing|pct_missing|n_non_missing)"
    ) %>%
    arrange(desc(n_missing))
}


df_model_after_small_drop <- df_model_vars %>%
  drop_na(light_conditions, rural, speed_limit)

# ------------------------------------------------------------
# Датасет для MICE
# ------------------------------------------------------------

mice_predictor_vars <- c(
  "severe",
  "hour",
  "rural",
  "day_type",
  "speed_limit",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "road_type",
  "number_of_vehicles"
)

df_mice <- df_selected %>%
  mutate(
    hour = as.integer(str_extract(as.character(time), "^\\d{1,2}")),

    severe = case_when(
      collision_severity %in% c("Fatal", "Serious") ~ 1,
      collision_severity == "Slight" ~ 0,
      TRUE ~ NA_real_
    ),

    rural = case_when(
      urban_or_rural_area == "Rural" ~ 1,
      urban_or_rural_area == "Urban" ~ 0,
      TRUE ~ NA_real_
    ),

    day_type = case_when(
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend",
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday",
      TRUE ~ NA_character_
    )
  ) %>%
  select(all_of(mice_predictor_vars)) %>%
  drop_na(light_conditions, rural, speed_limit) %>%
  mutate(
    severe = as.numeric(severe),
    hour = as.numeric(hour),
    rural = as.numeric(rural),
    number_of_vehicles = as.numeric(number_of_vehicles),

    day_type = factor(day_type),
    speed_limit = factor(speed_limit),
    light_conditions = factor(light_conditions),
    weather_conditions = factor(weather_conditions),
    road_surface_conditions = factor(road_surface_conditions),
    road_type = factor(road_type)
  )

# ------------------------------------------------------------
# MICE
# ------------------------------------------------------------

ini <- mice(df_mice, maxit = 0)

meth <- ini$method
pred <- ini$predictorMatrix

target_vars <- c(
  "weather_conditions",
  "road_surface_conditions",
  "road_type"
)

meth[] <- ""
meth[target_vars] <- "polyreg"

pred[,] <- 0
predictor_vars <- setdiff(names(df_mice), target_vars)
pred[target_vars, predictor_vars] <- 1
diag(pred) <- 0


set.seed(123)

mice_time <- system.time({
  imp_final <- mice(
    df_mice,
    method = meth,
    predictorMatrix = pred,
    m = 5,
    maxit = 5,
    printFlag = TRUE
  )
})

print(mice_time)

saveRDS(
  imp_final,
  mice_model_path
)

# ------------------------------------------------------------
# Формування фінального імпутованого датасету
# ------------------------------------------------------------

completed_final <- complete(imp_final, 1)

if (nrow(completed_final) != nrow(df_model_after_small_drop)) {
  stop("Кількість рядків у completed_final не збігається з df_model_after_small_drop.")
}

model_data_imputed <- df_model_after_small_drop %>%
  mutate(
    weather_conditions = completed_final$weather_conditions,
    road_surface_conditions = completed_final$road_surface_conditions,
    road_type = completed_final$road_type
  )

missing_summary_final <- make_missing_summary(model_data_imputed)
print(missing_summary_final)

# ------------------------------------------------------------
# Complete-case датасет для контрольного порівняння
# ------------------------------------------------------------

model_data_complete_case <- df_model_vars %>%
  drop_na()

comparison_imputed_vs_complete_case <- tibble(
  dataset = c("MICE imputed", "Complete case"),
  n_rows = c(
    nrow(model_data_imputed),
    nrow(model_data_complete_case)
  ),
  severe_rate_pct = c(
    mean(model_data_imputed$severe) * 100,
    mean(model_data_complete_case$severe) * 100
  )
) %>%
  mutate(
    severe_rate_pct = round(severe_rate_pct, 2),
    rows_lost_vs_imputed = nrow(model_data_imputed) - n_rows,
    rows_lost_pct = round(rows_lost_vs_imputed / nrow(model_data_imputed) * 100, 3)
  )

print(comparison_imputed_vs_complete_case)


png(
  filename = file.path(plots_dir, "mice_trace_diagnostics_5x5.png"),
  width = 1200,
  height = 800
)

plot(imp_final)

dev.off()

# ------------------------------------------------------------
# Порівняння розподілів до та після MICE
# ------------------------------------------------------------

distribution_before_after <- map_dfr(
  target_vars,
  function(var_name) {
    before <- df_model_after_small_drop %>%
      filter(!is.na(.data[[var_name]])) %>%
      count(value = as.character(.data[[var_name]])) %>%
      mutate(
        pct = n / sum(n) * 100,
        source = "Observed before MICE",
        variable = var_name
      )

    after <- model_data_imputed %>%
      count(value = as.character(.data[[var_name]])) %>%
      mutate(
        pct = n / sum(n) * 100,
        source = "Completed after MICE",
        variable = var_name
      )

    bind_rows(before, after)
  }
)

p_distribution_before_after <- ggplot(
  distribution_before_after,
  aes(x = value, y = pct, fill = source)
) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ variable, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Розподіли змінних до та після MICE залишаються подібними",
    subtitle = "Порівняння спостережених значень до імпутації та повного датасету після імпутації",
    x = NULL,
    y = "Частка, %",
    fill = NULL
  ) +
  theme_minimal(base_size = 12)

print(p_distribution_before_after)

ggsave(
  filename = file.path(plots_dir, "mice_distribution_before_after.png"),
  plot = p_distribution_before_after,
  width = 11,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# Розподіл лише імпутованих значень
# ------------------------------------------------------------

imputed_only_distribution <- map_dfr(
  target_vars,
  function(var_name) {
    tibble(
      was_missing = is.na(df_model_after_small_drop[[var_name]]),
      imputed_value = as.character(model_data_imputed[[var_name]])
    ) %>%
      filter(was_missing) %>%
      count(value = imputed_value) %>%
      mutate(
        pct = n / sum(n) * 100,
        variable = var_name
      )
  }
)

p_imputed_only_distribution <- ggplot(
  imputed_only_distribution,
  aes(x = value, y = pct)
) +
  geom_col() +
  facet_wrap(~ variable, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Розподіл значень, вставлених методом MICE",
    subtitle = "Показано лише ті рядки, де значення було пропущене до імпутації",
    x = NULL,
    y = "Частка серед імпутованих значень, %"
  ) +
  theme_minimal(base_size = 12)

print(p_imputed_only_distribution)

ggsave(
  filename = file.path(plots_dir, "mice_imputed_only_distribution.png"),
  plot = p_imputed_only_distribution,
  width = 11,
  height = 8,
  dpi = 300
)

write_csv(
  model_data_imputed,
  "data/processed/model_data_imputed.csv"
)

write_csv(
  model_data_complete_case,
  "data/processed/model_data_complete_case.csv"
)

cat("\nГотово. MICE-об'єкт, графіки та фінальні датасети збережено.\n")

# ------------------------------------------------------------
# Порівняння MICE vs complete-case за ключовими групами
# ------------------------------------------------------------

compare_group_rates <- function(data, dataset_name, group_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      severe_rate_pct = round(mean(severe) * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(
      dataset = dataset_name,
      group_variable = group_var,
      group_value = as.character(.data[[group_var]])
    ) %>%
    select(dataset, group_variable, group_value, n, severe_rate_pct)
}

robustness_group_comparison <- bind_rows(
  compare_group_rates(model_data_imputed, "MICE imputed", "is_night"),
  compare_group_rates(model_data_complete_case, "Complete case", "is_night"),
  compare_group_rates(model_data_imputed, "MICE imputed", "rural"),
  compare_group_rates(model_data_complete_case, "Complete case", "rural")
)

print(robustness_group_comparison)

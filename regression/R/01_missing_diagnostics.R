# ============================================================
# діагностика пропусків
# ============================================================

library(tidyverse)


plots_dir <- "plots/regression/missing"
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
-

df_raw <- read_csv("data/processed/collisions_2024_cleaned.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# 2. Вибір змінних
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
# 3. Створення базових змінних для регресії
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

# ------------------------------------------------------------
# Таблиці діагностики пропусків
# ------------------------------------------------------------

missing_model <- make_missing_summary(df_model_vars)
missing_only <- missing_model %>%
  filter(n_missing > 0)

print(missing_model)
print(missing_only)

row_loss_summary <- tibble(
  total_rows = nrow(df_model_vars),
  complete_case_rows = df_model_vars %>% drop_na() %>% nrow(),
  removed_rows = total_rows - complete_case_rows,
  removed_pct = round(removed_rows / total_rows * 100, 3)
)

print(row_loss_summary)


missing_vs_severe <- df_model_vars %>%
  mutate(
    weather_missing = is.na(weather_conditions),
    road_type_missing = is.na(road_type),
    road_surface_missing = is.na(road_surface_conditions),
    light_missing = is.na(light_conditions),
    rural_missing = is.na(rural),
    speed_limit_missing = is.na(speed_limit)
  ) %>%
  pivot_longer(
    cols = ends_with("_missing"),
    names_to = "missing_indicator",
    values_to = "is_missing"
  ) %>%
  group_by(missing_indicator, is_missing) %>%
  summarise(
    n = n(),
    severe_rate = round(mean(severe, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(missing_indicator, desc(is_missing))

print(missing_vs_severe)


missing_plot_data <- missing_only %>%
  mutate(
    variable = fct_reorder(variable, pct_missing),
    label = paste0(n_missing, " (", pct_missing, "%)")
  )

p_missing_share <- ggplot(
  missing_plot_data,
  aes(x = pct_missing, y = variable)
) +
  geom_col() +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(missing_plot_data$pct_missing) * 1.25)
  ) +
  labs(
    title = "Найбільше пропусків є у погоді, типі дороги та стані покриття",
    subtitle = "Частка пропущених значень серед змінних регресійної моделі",
    x = "Частка пропусків",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

print(p_missing_share)

ggsave(
  filename = file.path(plots_dir, "missing_share_by_variable.png"),
  plot = p_missing_share,
  width = 8,
  height = 5,
  dpi = 300
)


main_missing_indicators <- c(
  "weather_missing",
  "road_type_missing",
  "road_surface_missing"
)

missing_vs_severe_plot_data <- missing_vs_severe %>%
  filter(missing_indicator %in% main_missing_indicators) %>%
  mutate(
    variable = recode(
      missing_indicator,
      "weather_missing" = "weather_conditions",
      "road_type_missing" = "road_type",
      "road_surface_missing" = "road_surface_conditions"
    ),
    status = if_else(is_missing, "Missing", "Observed"),
    label = paste0(severe_rate, "%\n(n=", n, ")")
  )

p_missing_vs_severe <- ggplot(
  missing_vs_severe_plot_data,
  aes(x = variable, y = severe_rate, fill = status)
) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    vjust = -0.2,
    size = 3.2
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(missing_vs_severe_plot_data$severe_rate) * 1.25)
  ) +
  labs(
    title = "Рядки з пропусками мають іншу частку тяжких ДТП",
    subtitle = "Це аргумент проти простого видалення всіх рядків із пропусками",
    x = NULL,
    y = "Частка Fatal або Serious ДТП",
    fill = "Статус значення"
  ) +
  theme_minimal(base_size = 12)

print(p_missing_vs_severe)

ggsave(
  filename = file.path(plots_dir, "missing_vs_severe_rate.png"),
  plot = p_missing_vs_severe,
  width = 9,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 9. Видалення поодиноких пропусків
# ------------------------------------------------------------

df_model_after_small_drop <- df_model_vars %>%
  drop_na(light_conditions, rural, speed_limit)

missing_after_small_drop <- make_missing_summary(df_model_after_small_drop)

print(missing_after_small_drop)

cat("\nПідсумок після видалення дрібних пропусків:\n")
cat("Було рядків:", nrow(df_model_vars), "\n")
cat("Стало рядків:", nrow(df_model_after_small_drop), "\n")
cat("Видалено рядків:", nrow(df_model_vars) - nrow(df_model_after_small_drop), "\n")
cat(
  "Видалено, %:",
  round((nrow(df_model_vars) - nrow(df_model_after_small_drop)) / nrow(df_model_vars) * 100, 4),
  "%\n"
)

# ------------------------------------------------------------
# Діагностика змінних для MICE
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

df_mice_check <- df_selected %>%
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
  drop_na(light_conditions, rural, speed_limit)

mice_missing_summary <- make_missing_summary(df_mice_check)
print(mice_missing_summary)

mice_missing_plot_data <- mice_missing_summary %>%
  mutate(
    variable = fct_reorder(variable, n_missing),
    label = paste0(n_missing, " (", pct_missing, "%)")
  )

p_mice_predictors_missing <- ggplot(
  mice_missing_plot_data,
  aes(x = n_missing, y = variable)
) +
  geom_col() +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  scale_x_continuous(
    limits = c(0, max(mice_missing_plot_data$n_missing) * 1.2)
  ) +
  labs(
    title = "Пропуски в предикторах для MICE",
    subtitle = "Для імпутації використано компактний набір змістовних предикторів",
    x = "Кількість пропусків",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

print(p_mice_predictors_missing)

ggsave(
  filename = file.path(plots_dir, "mice_predictors_missingness.png"),
  plot = p_mice_predictors_missing,
  width = 9,
  height = 6,
  dpi = 300
)

cat("\nДіагностику пропусків завершено. Графіки збережено в plots/regression/missing.\n")

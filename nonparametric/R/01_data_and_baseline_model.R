library(tidyverse)

# ============================================================
# 1. Зчитування даних
# ============================================================

cleaned_data <- read_csv("data/processed/collisions_2024_cleaned.csv",
                         show_col_types = FALSE)

imputed_data <- read_csv("data/processed/model_data_imputed.csv",
                         show_col_types = FALSE)

# ============================================================
# 2. Видаляємо поодинокі пропуски в очищеному датасеті
# ============================================================

cleaned_filtered <- cleaned_data %>%
  filter(
    !is.na(light_conditions),
    !is.na(speed_limit),
    !is.na(urban_or_rural_area)
  )

# Перевірка, що можна об'єднувати по порядку
stopifnot(nrow(cleaned_filtered) == nrow(imputed_data))

# ============================================================
# 3. Формуємо модельний датасет
# ============================================================

model_data <- bind_cols(
  imputed_data %>%
    select(
      severe,
      hour,
      is_night,
      rural,
      day_type,
      speed_limit,
      light_conditions,
      weather_conditions,
      road_surface_conditions,
      road_type
    ),
  cleaned_filtered %>%
    select(
      urban_or_rural_area,
      police_force,
      longitude,
      latitude,
      first_road_class
    )
)

# ============================================================
# 4. Типи змінних
# ============================================================

speed_levels <- sort(unique(as.numeric(as.character(model_data$speed_limit))))

model_data <- model_data %>%
  mutate(
    severe = as.integer(severe),
    rural = as.integer(rural),
    hour = as.numeric(hour),
    hour_factor = factor(hour),
    
    speed_limit = as.numeric(as.character(speed_limit)),
    speed_limit_factor = factor(speed_limit, levels = speed_levels),
    
    rural_factor = factor(
      rural,
      levels = c(0, 1),
      labels = c("Urban", "Rural")
    ),
    
    day_type = factor(day_type),
    road_type = factor(road_type),
    first_road_class = factor(first_road_class),
    light_conditions = factor(light_conditions),
    weather_conditions = factor(weather_conditions),
    road_surface_conditions = factor(road_surface_conditions),
    police_force = factor(police_force),
    
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# ============================================================
# 5. Перевірка пропусків у фінальному датасеті
# ============================================================

missing_summary <- model_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) %>%
  mutate(
    total_n = nrow(model_data),
    missing_pct = round(100 * missing_n / total_n, 3)
  ) %>%
  arrange(desc(missing_pct))

print(missing_summary, n = Inf)

write_csv(model_data, "data/processed/nonparametric_model_data.csv")
write_csv(missing_summary, "nonparametric/outputs/tables/final_missing_summary.csv")

# ============================================================
# 6. Базова LPM-модель
# ============================================================

model_baseline_lpm <- lm(
  severe ~ speed_limit_factor +
    rural_factor +
    road_type +
    first_road_class +
    light_conditions +
    weather_conditions +
    road_surface_conditions +
    day_type +
    hour_factor +
    police_force,
  data = model_data
)

saveRDS(
  model_baseline_lpm,
  "nonparametric/outputs/models/model_baseline_lpm.rds"
)

# ============================================================
# 7. Robust SE HC1 без додаткових пакетів
# ============================================================

robust_vcov_hc1 <- function(model) {
  X_full <- model.matrix(model)
  u <- residuals(model)
  beta <- coef(model)
  
  keep <- !is.na(beta)
  X <- X_full[, keep, drop = FALSE]
  
  n <- nrow(X)
  df_resid <- df.residual(model)
  
  XtX_inv <- solve(crossprod(X))
  meat <- crossprod(X, X * as.numeric(u^2))
  
  (n / df_resid) * XtX_inv %*% meat %*% XtX_inv
}

robust_table <- function(model) {
  V <- robust_vcov_hc1(model)
  
  beta <- coef(model)
  keep <- !is.na(beta)
  beta <- beta[keep]
  
  se <- sqrt(diag(V))
  t_value <- beta / se
  p_value <- 2 * pt(abs(t_value), df = df.residual(model), lower.tail = FALSE)
  
  tibble(
    term = names(beta),
    estimate = as.numeric(beta),
    std_error_robust = as.numeric(se),
    t_value = as.numeric(t_value),
    p_value = as.numeric(p_value)
  )
}

baseline_results <- robust_table(model_baseline_lpm)

write_csv(
  baseline_results,
  "nonparametric/outputs/tables/baseline_lpm_robust_se.csv"
)

speed_limit_results <- baseline_results %>%
  filter(str_detect(term, "^speed_limit_factor"))

write_csv(
  speed_limit_results,
  "nonparametric/outputs/tables/baseline_lpm_speed_limit_robust_se.csv"
)

print(speed_limit_results, n = Inf)

# ============================================================
# 8. Прогнозовані ймовірності для speed_limit
# ============================================================

mode_value <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

newdata_speed <- tibble(
  speed_limit_factor = levels(model_data$speed_limit_factor),
  rural_factor = mode_value(model_data$rural_factor),
  road_type = mode_value(model_data$road_type),
  first_road_class = mode_value(model_data$first_road_class),
  light_conditions = mode_value(model_data$light_conditions),
  weather_conditions = mode_value(model_data$weather_conditions),
  road_surface_conditions = mode_value(model_data$road_surface_conditions),
  day_type = mode_value(model_data$day_type),
  hour_factor = mode_value(model_data$hour_factor),
  police_force = mode_value(model_data$police_force)
) %>%
  mutate(
    speed_limit_factor = factor(speed_limit_factor, levels = levels(model_data$speed_limit_factor)),
    rural_factor = factor(rural_factor, levels = levels(model_data$rural_factor)),
    road_type = factor(road_type, levels = levels(model_data$road_type)),
    first_road_class = factor(first_road_class, levels = levels(model_data$first_road_class)),
    light_conditions = factor(light_conditions, levels = levels(model_data$light_conditions)),
    weather_conditions = factor(weather_conditions, levels = levels(model_data$weather_conditions)),
    road_surface_conditions = factor(road_surface_conditions, levels = levels(model_data$road_surface_conditions)),
    day_type = factor(day_type, levels = levels(model_data$day_type)),
    hour_factor = factor(hour_factor, levels = levels(model_data$hour_factor)),
    police_force = factor(police_force, levels = levels(model_data$police_force))
  )

predict_lpm_robust <- function(model, newdata) {
  V <- robust_vcov_hc1(model)
  
  beta <- coef(model)
  keep <- !is.na(beta)
  beta <- beta[keep]
  
  X_full <- model.matrix(delete.response(terms(model)), newdata)
  X <- X_full[, names(beta), drop = FALSE]
  
  fit <- as.numeric(X %*% beta)
  se <- sqrt(rowSums((X %*% V) * X))
  
  newdata %>%
    mutate(
      pred_prob = fit,
      se_robust = se,
      conf_low = pred_prob - 1.96 * se_robust,
      conf_high = pred_prob + 1.96 * se_robust
    )
}

speed_predictions <- predict_lpm_robust(model_baseline_lpm, newdata_speed)

write_csv(
  speed_predictions,
  "nonparametric/outputs/tables/baseline_lpm_speed_predictions.csv"
)

print(speed_predictions)
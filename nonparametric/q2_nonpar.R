# ============================================================
# Лабораторна робота №4
# Людина 2 — Kernel regression:
# Nadaraya-Watson + Local linear regression
#
# Основне питання:
# Чи виглядає зв'язок між speed_limit і severe
# лінійним / монотонним, якщо не нав'язувати
# жорстку функціональну форму?
#
# Компактна модель:
# severe ~ speed_limit + hour + factor(urban_or_rural_area)
# ============================================================

# -----------------------------
# 0. Налаштування
# -----------------------------

SAVE_OUTPUTS <- TRUE

set.seed(123)

packages <- c(
  "readr",
  "dplyr",
  "ggplot2",
  "tidyr",
  "stringr",
  "tibble",
  "gt",
  "htmltools"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tibble)
library(gt)
library(htmltools)

# -----------------------------
# 1. Папки для збереження
# -----------------------------

if (SAVE_OUTPUTS) {
  dir.create("nonparametric/outputs/models", recursive = TRUE, showWarnings = FALSE)
  dir.create("nonparametric/outputs/tables", recursive = TRUE, showWarnings = FALSE)
  dir.create("nonparametric/outputs/figures", recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# 2. Допоміжні функції
# -----------------------------

has_col <- function(data, col) {
  col %in% names(data)
}

first_existing_col <- function(data, possible_names) {
  found <- possible_names[possible_names %in% names(data)]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

to_binary_numeric <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  
  x_chr <- tolower(as.character(x))
  
  case_when(
    x_chr %in% c("1", "yes", "true", "t", "так") ~ 1,
    x_chr %in% c("0", "no", "false", "f", "ні") ~ 0,
    TRUE ~ NA_real_
  )
}

clip_prob <- function(x) {
  x <- as.numeric(x)
  pmin(pmax(x, 0.0001), 0.9999)
}

show_gt <- function(data, title, subtitle = NULL) {
  gt_tbl <- data %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = subtitle
    ) %>%
    opt_table_font(
      font = list(
        google_font("Arial"),
        default_fonts()
      )
    ) %>%
    tab_options(
      table.font.size = 15,
      heading.title.font.size = 20,
      heading.subtitle.font.size = 13,
      data_row.padding = px(5),
      table.border.top.width = px(2),
      table.border.bottom.width = px(2)
    )
  
  print(gt_tbl)
  invisible(gt_tbl)
}

even_rows_by_area <- function(data, n_points = 10) {
  data %>%
    group_by(area_label) %>%
    group_modify(~ {
      if (nrow(.x) == 0) return(.x)
      
      idx <- unique(
        round(
          seq(
            from = 1,
            to = nrow(.x),
            length.out = min(n_points, nrow(.x))
          )
        )
      )
      
      .x[idx, , drop = FALSE]
    }) %>%
    ungroup()
}

# -----------------------------
# 3. Завантаження даних
# -----------------------------

possible_paths <- c(
  "data/processed/nonparametric_model_data.csv",
  "data/processed/model_data_complete_case.csv",
  "data/processed/model_data_imputed.csv",
  "data/processed/collisions_2024_cleaned.csv",
  "data/processed/collisions_2024_decoded.csv",
  "data/dft-road-casualty-statistics-collision-2024 (1).csv"
)

data_path <- possible_paths[file.exists(possible_paths)][1]

if (is.na(data_path)) {
  stop("Не знайдено файл даних. Перевір, чи файл лежить у data/processed.")
}

cat("Використовується файл:", data_path, "\n")

raw <- read_csv(data_path, show_col_types = FALSE)

cat("\nНазви змінних у файлі:\n")
print(names(raw))

# -----------------------------
# 4. Підготовка змінних
# -----------------------------

df <- raw

# 4.1 Залежна змінна severe

if (has_col(df, "severe")) {
  df <- df %>%
    mutate(severe = to_binary_numeric(severe))
}

if (!has_col(df, "severe")) {
  severity_col <- first_existing_col(
    df,
    c(
      "collision_severity",
      "accident_severity",
      "severity",
      "casualty_severity",
      "collision_severity_label",
      "accident_severity_label"
    )
  )
  
  if (!is.na(severity_col)) {
    if (is.numeric(df[[severity_col]])) {
      df <- df %>%
        mutate(severe = if_else(.data[[severity_col]] %in% c(1, 2), 1, 0))
    } else {
      df <- df %>%
        mutate(
          severe = if_else(
            str_to_lower(as.character(.data[[severity_col]])) %in%
              c("fatal", "serious", "фатальна", "тяжка", "важка"),
            1, 0
          )
        )
    }
  }
}

if (!has_col(df, "severe")) {
  stop("Не знайдено змінну severe або змінну тяжкості ДТП.")
}

# 4.2 Час доби hour

if (!has_col(df, "hour")) {
  if (has_col(df, "hour_decimal")) {
    df <- df %>%
      mutate(hour = as.numeric(hour_decimal))
  } else if (has_col(df, "time")) {
    df <- df %>%
      mutate(
        time_chr = as.character(time),
        hour = as.numeric(str_sub(time_chr, 1, 2))
      )
  } else {
    stop("Не знайдено змінну hour, hour_decimal або time.")
  }
} else {
  df <- df %>%
    mutate(hour = as.numeric(hour))
}

# 4.3 Ліміт швидкості speed_limit

speed_col <- first_existing_col(
  df,
  c("speed_limit", "Speed_limit", "speed", "speed_limit_numeric")
)

if (is.na(speed_col)) {
  stop("Не знайдено змінну speed_limit.")
}

df <- df %>%
  mutate(
    speed_limit = as.numeric(.data[[speed_col]]),
    speed_limit_ord = ordered(speed_limit)
  )

# 4.4 Urban / Rural

if (has_col(df, "urban_or_rural_area")) {
  
  if (is.numeric(df$urban_or_rural_area)) {
    df <- df %>%
      mutate(
        urban_or_rural_area = as.numeric(urban_or_rural_area),
        area_label = case_when(
          urban_or_rural_area == 1 ~ "Urban",
          urban_or_rural_area == 2 ~ "Rural",
          TRUE ~ NA_character_
        )
      )
  } else {
    df <- df %>%
      mutate(
        area_chr = str_to_lower(as.character(urban_or_rural_area)),
        area_label = case_when(
          str_detect(area_chr, "urban") ~ "Urban",
          str_detect(area_chr, "rural") ~ "Rural",
          TRUE ~ NA_character_
        ),
        urban_or_rural_area = case_when(
          area_label == "Urban" ~ 1,
          area_label == "Rural" ~ 2,
          TRUE ~ NA_real_
        )
      )
  }
  
} else if (has_col(df, "rural")) {
  
  df <- df %>%
    mutate(
      rural = to_binary_numeric(rural),
      urban_or_rural_area = if_else(rural == 1, 2, 1),
      area_label = if_else(rural == 1, "Rural", "Urban")
    )
  
} else {
  stop("Не знайдено urban_or_rural_area або rural.")
}

# 4.5 Фінальне очищення

df <- df %>%
  mutate(
    severe = as.numeric(severe),
    hour = as.numeric(hour),
    speed_limit = as.numeric(speed_limit),
    urban_or_rural_area = as.numeric(urban_or_rural_area),
    area_label = factor(area_label, levels = c("Rural", "Urban"))
  ) %>%
  filter(
    !is.na(severe),
    severe %in% c(0, 1),
    !is.na(hour),
    !is.na(speed_limit),
    !is.na(area_label),
    hour >= 0,
    hour < 24,
    speed_limit > 0
  ) %>%
  droplevels()

cat("\nКількість спостережень після очищення:", nrow(df), "\n")
cat("Частка тяжких ДТП:", round(mean(df$severe), 4), "\n")

# -----------------------------
# 5. Таблиця: структура частини
# -----------------------------

model_structure <- tibble(
  `Елемент` = c(
    "Назва частини",
    "Файл",
    "Змістовна модель",
    "Головний регресор",
    "Контроль 1",
    "Контроль 2",
    "Мета"
  ),
  `Значення` = c(
    "Kernel regression: Nadaraya-Watson і local linear regression",
    "nonparametric/R/02_kernel_regression.R",
    "severe ~ speed_limit + hour + factor(urban_or_rural_area)",
    "speed_limit",
    "hour",
    "urban_or_rural_area",
    "Перевірити форму зв'язку між лімітом швидкості та ймовірністю тяжких наслідків ДТП"
  )
)

show_gt(
  model_structure,
  title = "Структура частини kernel regression",
  subtitle = "Компактна модель для перевірки нелінійності speed_limit"
)

# -----------------------------
# 6. Таблиця: мотивація змінних
# -----------------------------

variable_motivation <- tibble(
  `Змінна` = c("severe", "speed_limit", "hour", "urban_or_rural_area"),
  `Роль` = c("залежна змінна", "головний регресор", "часовий контроль", "контекст місцевості"),
  `Чому включаємо` = c(
    "показує, чи мало ДТП тяжкі або фатальні наслідки",
    "основна змінна інтересу; перевіряється форма зв'язку з тяжкістю ДТП",
    "ризик тяжких наслідків може змінюватися протягом доби",
    "міські та сільські ДТП відрізняються за умовами руху, швидкістю і тяжкістю"
  )
)

show_gt(
  variable_motivation,
  title = "Мотивація змінних",
  subtitle = "Чому модель є компактною, але змістовною"
)

# -----------------------------
# 7. Таблиця: загальна характеристика вибірки
# -----------------------------

summary_table <- df %>%
  summarise(
    `Кількість спостережень` = n(),
    `Частка тяжких ДТП` = mean(severe, na.rm = TRUE),
    `Середній speed_limit` = mean(speed_limit, na.rm = TRUE),
    `Медіанний speed_limit` = median(speed_limit, na.rm = TRUE),
    `Середній hour` = mean(hour, na.rm = TRUE),
    `Частка Rural` = mean(area_label == "Rural", na.rm = TRUE),
    `Частка Urban` = mean(area_label == "Urban", na.rm = TRUE)
  ) %>%
  mutate(
    `Частка тяжких ДТП` = round(`Частка тяжких ДТП`, 3),
    `Середній speed_limit` = round(`Середній speed_limit`, 2),
    `Медіанний speed_limit` = round(`Медіанний speed_limit`, 2),
    `Середній hour` = round(`Середній hour`, 2),
    `Частка Rural` = round(`Частка Rural`, 3),
    `Частка Urban` = round(`Частка Urban`, 3)
  )

show_gt(
  summary_table,
  title = "Загальна характеристика вибірки",
  subtitle = "Очищені дані для kernel regression"
)

# -----------------------------
# 8. Таблиця: частка тяжких ДТП за speed_limit
# -----------------------------

speed_summary_table <- df %>%
  group_by(speed_limit) %>%
  summarise(
    `Кількість ДТП` = n(),
    `Тяжкі ДТП` = sum(severe, na.rm = TRUE),
    `Частка тяжких ДТП` = mean(severe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(speed_limit) %>%
  mutate(
    `Частка тяжких ДТП` = round(`Частка тяжких ДТП`, 3)
  ) %>%
  rename(`Ліміт швидкості` = speed_limit)

show_gt(
  speed_summary_table,
  title = "Частка тяжких ДТП за лімітом швидкості",
  subtitle = "Емпірична мотивація для непараметричного підходу"
)

# -----------------------------
# 9. Таблиця: Urban vs Rural
# -----------------------------

area_summary_table <- df %>%
  group_by(area_label) %>%
  summarise(
    `Кількість ДТП` = n(),
    `Середній speed_limit` = mean(speed_limit, na.rm = TRUE),
    `Медіанний speed_limit` = median(speed_limit, na.rm = TRUE),
    `Середній hour` = mean(hour, na.rm = TRUE),
    `Частка тяжких ДТП` = mean(severe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Середній speed_limit` = round(`Середній speed_limit`, 2),
    `Медіанний speed_limit` = round(`Медіанний speed_limit`, 2),
    `Середній hour` = round(`Середній hour`, 2),
    `Частка тяжких ДТП` = round(`Частка тяжких ДТП`, 3)
  ) %>%
  rename(`Тип місцевості` = area_label)

show_gt(
  area_summary_table,
  title = "Порівняння Urban та Rural",
  subtitle = "Базове зіставлення структури ДТП"
)

# -----------------------------
# 10. Дані для kernel regression
# -----------------------------

n_kernel <- min(6000, nrow(df))

kernel_data <- df %>%
  select(
    severe,
    speed_limit,
    speed_limit_ord,
    hour,
    urban_or_rural_area,
    area_label
  ) %>%
  sample_n(n_kernel) %>%
  arrange(area_label, speed_limit, hour)

kernel_data_summary <- kernel_data %>%
  summarise(
    `Кількість у підвибірці` = n(),
    `Частка тяжких ДТП` = round(mean(severe), 3),
    `Мінімальний speed_limit` = min(speed_limit),
    `Максимальний speed_limit` = max(speed_limit),
    `Медіанний hour` = median(hour)
  )

show_gt(
  kernel_data_summary,
  title = "Підвибірка для kernel regression",
  subtitle = "Використовується для пришвидшення обчислень і стабільної візуалізації"
)

# -----------------------------
# 11. Bandwidth
# -----------------------------

h_speed <- 1.06 * sd(kernel_data$speed_limit, na.rm = TRUE) * nrow(kernel_data)^(-1 / 5)
h_hour <- 1.06 * sd(kernel_data$hour, na.rm = TRUE) * nrow(kernel_data)^(-1 / 5)

if (is.na(h_speed) || h_speed <= 0) h_speed <- 5
if (is.na(h_hour) || h_hour <= 0) h_hour <- 2

h_speed <- max(h_speed, 5)
h_hour <- max(h_hour, 2)

bandwidth_table <- tibble(
  `Bandwidth` = c("h_speed", "h_hour"),
  `Змінна` = c("speed_limit", "hour"),
  `Значення` = c(h_speed, h_hour),
  `Пояснення` = c(
    "ширина локального вікна для головного регресора speed_limit",
    "ширина локального вікна для контрольної змінної hour"
  )
) %>%
  mutate(`Значення` = round(`Значення`, 3))

show_gt(
  bandwidth_table,
  title = "Обрані bandwidth для kernel regression",
  subtitle = "Чим більше bandwidth, тим гладша оцінена крива"
)

if (SAVE_OUTPUTS) {
  write_csv(
    bandwidth_table,
    "nonparametric/outputs/tables/kernel_bandwidths.csv"
  )
}

# -----------------------------
# 12. Kernel-функції
# -----------------------------

kernel_weights <- function(speed, hour, area, speed0, hour0, area0, h_speed, h_hour) {
  w_speed <- dnorm((speed - speed0) / h_speed)
  w_hour <- dnorm((hour - hour0) / h_hour)
  w_area <- ifelse(area == area0, 1, 0)
  
  w_speed * w_hour * w_area
}

predict_nadaraya_watson <- function(data, speed_grid, hour0, area0, h_speed, h_hour) {
  preds <- numeric(length(speed_grid))
  
  for (i in seq_along(speed_grid)) {
    speed0 <- speed_grid[i]
    
    w <- kernel_weights(
      speed = data$speed_limit,
      hour = data$hour,
      area = data$area_label,
      speed0 = speed0,
      hour0 = hour0,
      area0 = area0,
      h_speed = h_speed,
      h_hour = h_hour
    )
    
    if (sum(w, na.rm = TRUE) == 0) {
      preds[i] <- NA_real_
    } else {
      preds[i] <- sum(w * data$severe, na.rm = TRUE) / sum(w, na.rm = TRUE)
    }
  }
  
  clip_prob(preds)
}

predict_local_linear <- function(data, speed_grid, hour0, area0, h_speed, h_hour) {
  preds <- numeric(length(speed_grid))
  
  for (i in seq_along(speed_grid)) {
    speed0 <- speed_grid[i]
    
    w <- kernel_weights(
      speed = data$speed_limit,
      hour = data$hour,
      area = data$area_label,
      speed0 = speed0,
      hour0 = hour0,
      area0 = area0,
      h_speed = h_speed,
      h_hour = h_hour
    )
    
    fit <- tryCatch(
      lm(severe ~ I(speed_limit - speed0), data = data, weights = w),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      preds[i] <- NA_real_
    } else {
      preds[i] <- coef(fit)[1]
    }
  }
  
  clip_prob(preds)
}

# -----------------------------
# 13. Прогноз за speed_limit
# -----------------------------

speed_grid <- seq(
  min(df$speed_limit, na.rm = TRUE),
  max(df$speed_limit, na.rm = TRUE),
  length.out = 120
)

hour_ref <- median(df$hour, na.rm = TRUE)
area_levels <- levels(df$area_label)

prediction_list <- list()

for (a in area_levels) {
  
  pred_nw <- predict_nadaraya_watson(
    data = kernel_data,
    speed_grid = speed_grid,
    hour0 = hour_ref,
    area0 = a,
    h_speed = h_speed,
    h_hour = h_hour
  )
  
  pred_ll <- predict_local_linear(
    data = kernel_data,
    speed_grid = speed_grid,
    hour0 = hour_ref,
    area0 = a,
    h_speed = h_speed,
    h_hour = h_hour
  )
  
  prediction_list[[a]] <- tibble(
    speed_limit = speed_grid,
    hour_fixed = hour_ref,
    area_label = a,
    Nadaraya_Watson = pred_nw,
    Local_linear = pred_ll
  )
}

kernel_predictions_wide <- bind_rows(prediction_list)

kernel_predictions_preview <- kernel_predictions_wide %>%
  even_rows_by_area(n_points = 10) %>%
  mutate(
    speed_limit = round(speed_limit, 1),
    hour_fixed = round(hour_fixed, 1),
    Nadaraya_Watson = round(Nadaraya_Watson, 4),
    Local_linear = round(Local_linear, 4)
  ) %>%
  rename(
    `Ліміт швидкості` = speed_limit,
    `Hour зафіксовано` = hour_fixed,
    `Тип місцевості` = area_label,
    `Nadaraya-Watson` = Nadaraya_Watson,
    `Local linear` = Local_linear
  )

show_gt(
  kernel_predictions_preview,
  title = "Фрагмент прогнозів kernel regression",
  subtitle = "Оцінена ймовірність тяжких наслідків за speed_limit"
)

if (SAVE_OUTPUTS) {
  write_csv(
    kernel_predictions_wide,
    "nonparametric/outputs/tables/kernel_predictions_speed_limit.csv"
  )
}

# -----------------------------
# 14. Bootstrap confidence bands
# -----------------------------

B <- 50

boot_results <- list()

for (a in area_levels) {
  
  boot_mat <- matrix(NA_real_, nrow = length(speed_grid), ncol = B)
  
  for (b in seq_len(B)) {
    
    boot_index <- sample(seq_len(nrow(kernel_data)), replace = TRUE)
    boot_data <- kernel_data[boot_index, ]
    
    boot_mat[, b] <- predict_local_linear(
      data = boot_data,
      speed_grid = speed_grid,
      hour0 = hour_ref,
      area0 = a,
      h_speed = h_speed,
      h_hour = h_hour
    )
  }
  
  boot_results[[a]] <- tibble(
    speed_limit = speed_grid,
    hour_fixed = hour_ref,
    area_label = a,
    local_linear_fit = kernel_predictions_wide %>%
      filter(area_label == a) %>%
      pull(Local_linear),
    lower = apply(boot_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
    upper = apply(boot_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
}

local_linear_ci <- bind_rows(boot_results)

ci_preview <- local_linear_ci %>%
  even_rows_by_area(n_points = 8) %>%
  mutate(
    speed_limit = round(speed_limit, 1),
    hour_fixed = round(hour_fixed, 1),
    local_linear_fit = round(local_linear_fit, 4),
    lower = round(lower, 4),
    upper = round(upper, 4)
  ) %>%
  rename(
    `Ліміт швидкості` = speed_limit,
    `Hour зафіксовано` = hour_fixed,
    `Тип місцевості` = area_label,
    `Local linear fit` = local_linear_fit,
    `Нижня межа 95% CI` = lower,
    `Верхня межа 95% CI` = upper
  )

show_gt(
  ci_preview,
  title = "Bootstrap confidence bands",
  subtitle = "95% довірчі смуги для local linear regression"
)

# -----------------------------
# 15. Графік 1:
# фактична частка severe за speed_limit
# -----------------------------

speed_area_summary <- df %>%
  group_by(speed_limit) %>%
  summarise(
    n = n(),
    severe_cases = sum(severe),
    severe_share = mean(severe),
    .groups = "drop"
  )

p_empirical <- ggplot(
  speed_area_summary,
  aes(x = speed_limit, y = severe_share)
) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Фактична частка тяжких ДТП за лімітом швидкості",
    x = "Ліміт швидкості",
    y = "Частка тяжких ДТП"
  ) +
  theme_minimal(base_size = 13)

print(p_empirical)

if (SAVE_OUTPUTS) {
  ggsave(
    "nonparametric/outputs/figures/empirical_speed_limit_severe_share.png",
    p_empirical,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# -----------------------------
# 16. Графік 2:
# основний kernel-графік за speed_limit
# -----------------------------

kernel_predictions_long_plot <- kernel_predictions_wide %>%
  pivot_longer(
    cols = c(Nadaraya_Watson, Local_linear),
    names_to = "method",
    values_to = "predicted_probability"
  ) %>%
  mutate(
    method = recode(
      method,
      Nadaraya_Watson = "Nadaraya-Watson",
      Local_linear = "Local linear"
    )
  )

p_kernel <- ggplot() +
  geom_ribbon(
    data = local_linear_ci,
    aes(
      x = speed_limit,
      ymin = lower,
      ymax = upper,
      group = area_label
    ),
    alpha = 0.16
  ) +
  geom_line(
    data = kernel_predictions_long_plot,
    aes(
      x = speed_limit,
      y = predicted_probability,
      linetype = method
    ),
    linewidth = 1
  ) +
  facet_wrap(~ area_label) +
  labs(
    title = "Kernel regression: Nadaraya-Watson та local linear",
    subtitle = "Залежність між speed_limit і P(severe), окремо для Urban та Rural; hour зафіксовано на медіані",
    x = "Ліміт швидкості",
    y = "Оцінена ймовірність тяжких наслідків ДТП",
    linetype = "Метод"
  ) +
  theme_minimal(base_size = 13)

print(p_kernel)

if (SAVE_OUTPUTS) {
  ggsave(
    "nonparametric/outputs/figures/kernel_nw_vs_local_linear.png",
    p_kernel,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# -----------------------------
# 17. Додатковий графік 3:
# kernel regression за hour
# -----------------------------

hour_grid <- seq(0, 23.99, length.out = 200)

h_hour_only <- 1.06 * sd(kernel_data$hour, na.rm = TRUE) * nrow(kernel_data)^(-1 / 5)

if (is.na(h_hour_only) || h_hour_only <= 0) {
  h_hour_only <- 1
}

h_hour_only <- max(h_hour_only, 0.6)

predict_nw_hour <- function(data, hour_grid, h_hour) {
  preds <- numeric(length(hour_grid))
  
  for (i in seq_along(hour_grid)) {
    hour0 <- hour_grid[i]
    w <- dnorm((data$hour - hour0) / h_hour)
    
    if (sum(w, na.rm = TRUE) == 0) {
      preds[i] <- NA_real_
    } else {
      preds[i] <- sum(w * data$severe, na.rm = TRUE) / sum(w, na.rm = TRUE)
    }
  }
  
  clip_prob(preds)
}

predict_ll_hour <- function(data, hour_grid, h_hour) {
  preds <- numeric(length(hour_grid))
  
  for (i in seq_along(hour_grid)) {
    hour0 <- hour_grid[i]
    w <- dnorm((data$hour - hour0) / h_hour)
    
    fit <- tryCatch(
      lm(severe ~ I(hour - hour0), data = data, weights = w),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      preds[i] <- NA_real_
    } else {
      preds[i] <- coef(fit)[1]
    }
  }
  
  clip_prob(preds)
}

nw_hour_pred <- predict_nw_hour(kernel_data, hour_grid, h_hour_only)
ll_hour_pred <- predict_ll_hour(kernel_data, hour_grid, h_hour_only)

B_hour <- 50

boot_hour_mat <- matrix(NA_real_, nrow = length(hour_grid), ncol = B_hour)

for (b in seq_len(B_hour)) {
  boot_index <- sample(seq_len(nrow(kernel_data)), replace = TRUE)
  boot_data <- kernel_data[boot_index, ]
  
  boot_hour_mat[, b] <- predict_ll_hour(
    data = boot_data,
    hour_grid = hour_grid,
    h_hour = h_hour_only
  )
}

hour_ci <- tibble(
  hour = hour_grid,
  fit = ll_hour_pred,
  lower = apply(boot_hour_mat, 1, quantile, probs = 0.025, na.rm = TRUE),
  upper = apply(boot_hour_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
)

hour_kernel_long <- tibble(
  hour = hour_grid,
  `Local linear` = ll_hour_pred,
  `Nadaraya-Watson` = nw_hour_pred
) %>%
  pivot_longer(
    cols = c(`Local linear`, `Nadaraya-Watson`),
    names_to = "method",
    values_to = "predicted_probability"
  )

p_hour_kernel <- ggplot() +
  geom_ribbon(
    data = hour_ci,
    aes(x = hour, ymin = lower, ymax = upper),
    alpha = 0.16
  ) +
  geom_line(
    data = hour_kernel_long,
    aes(x = hour, y = predicted_probability, linetype = method),
    linewidth = 1
  ) +
  labs(
    title = "Непараметрична оцінка ймовірності тяжких наслідків ДТП протягом доби",
    subtitle = "Nadaraya-Watson та local linear kernel regression",
    x = "Час доби, години",
    y = "Оцінена ймовірність тяжких наслідків",
    linetype = "Метод"
  ) +
  theme_minimal(base_size = 13)

print(p_hour_kernel)

if (SAVE_OUTPUTS) {
  ggsave(
    "nonparametric/outputs/figures/kernel_hour_nw_vs_local_linear.png",
    p_hour_kernel,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# -----------------------------
# 18. Інтерпретаційна таблиця
# -----------------------------

interpretation_table <- tibble(
  `Питання` = c(
    "Що перевіряє ця частина?",
    "Чому використано компактну модель?",
    "Що порівнюється?",
    "Що означає вигнута форма кривої?",
    "Навіщо Urban / Rural?",
    "Що головне винести у презентацію?"
  ),
  `Відповідь` = c(
    "Форму зв'язку між speed_limit і ймовірністю severe без жорсткого припущення про лінійність.",
    "Kernel regression із багатьма змінними стає повільною, нестабільною і складною для інтерпретації.",
    "Nadaraya-Watson як local constant підхід і local linear regression як локально лінійний підхід.",
    "Зв'язок між speed_limit і severe не є простою прямою залежністю.",
    "Міські та сільські ДТП мають різну структуру швидкостей і ризиків, тому їх доцільно показувати окремо.",
    "Основний графік speed_limit проти predicted P(severe) з двома kernel-методами та довірчою смугою."
  )
)

show_gt(
  interpretation_table,
  title = "Інтерпретація для звіту та презентації",
  subtitle = "Як пояснювати отримані результати"
)

# -----------------------------
# 19. Збереження моделей
# -----------------------------

model_kernel_nw <- list(
  model_name = "Nadaraya-Watson / local constant kernel regression",
  formula = "severe ~ speed_limit + hour + factor(urban_or_rural_area)",
  n_used = nrow(kernel_data),
  hour_fixed = hour_ref,
  bandwidths = bandwidth_table,
  predictions = kernel_predictions_wide %>%
    select(speed_limit, hour_fixed, area_label, Nadaraya_Watson),
  note = "Головний регресор speed_limit; hour і urban/rural враховані як контрольний контекст."
)

model_kernel_local_linear <- list(
  model_name = "Local linear kernel regression",
  formula = "severe ~ speed_limit + hour + factor(urban_or_rural_area)",
  n_used = nrow(kernel_data),
  hour_fixed = hour_ref,
  bandwidths = bandwidth_table,
  predictions = kernel_predictions_wide %>%
    select(speed_limit, hour_fixed, area_label, Local_linear),
  confidence_bands = local_linear_ci,
  note = "Local linear порівнюється з Nadaraya-Watson; bootstrap CI побудовано для local linear."
)

if (SAVE_OUTPUTS) {
  saveRDS(
    model_kernel_nw,
    "nonparametric/outputs/models/model_kernel_nw.rds"
  )
  
  saveRDS(
    model_kernel_local_linear,
    "nonparametric/outputs/models/model_kernel_local_linear.rds"
  )
}

# -----------------------------
# 20. Фінальне повідомлення
# -----------------------------

cat("\nГотово: частина Людини 2 виконана за планом.\n")
cat("Модель: severe ~ speed_limit + hour + factor(urban_or_rural_area)\n")
cat("Побудовано Nadaraya-Watson і local linear regression.\n")
cat("Головний графік: speed_limit -> predicted P(severe), окремо для Urban і Rural.\n")
cat("Додатковий графік: hour -> predicted P(severe).\n")
cat("Таблиці зроблено у презентаційному форматі через gt.\n")

if (SAVE_OUTPUTS) {
  cat("\nЗбережено файли:\n")
  cat("nonparametric/outputs/models/model_kernel_nw.rds\n")
  cat("nonparametric/outputs/models/model_kernel_local_linear.rds\n")
  cat("nonparametric/outputs/tables/kernel_bandwidths.csv\n")
  cat("nonparametric/outputs/tables/kernel_predictions_speed_limit.csv\n")
  cat("nonparametric/outputs/figures/empirical_speed_limit_severe_share.png\n")
  cat("nonparametric/outputs/figures/kernel_nw_vs_local_linear.png\n")
  cat("nonparametric/outputs/figures/kernel_hour_nw_vs_local_linear.png\n")
} else {
  cat("\nSAVE_OUTPUTS = FALSE, тому файли не зберігалися.\n")
}
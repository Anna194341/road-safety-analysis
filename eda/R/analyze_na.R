library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Завантаження даних
df <- read_csv("data/processed/collisions_2024_cleaned.csv", show_col_types = FALSE)

df <- df %>%
  mutate(missing_junction = is.na(junction_detail))


# -----------------------------------
# 1. Частка пропущених значень у змінних датасету
# -----------------------------------
na_summary <- data.frame(
  variable = names(df),
  na_percent = sapply(df, function(x) mean(is.na(x)))
) %>%
  filter(na_percent > 0) %>%
  arrange(na_percent)

p_na <- ggplot(na_summary, aes(x = na_percent, y = reorder(variable, na_percent))) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = percent(na_percent, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Частка пропущених значень у змінних датасету",
    x = "Частка пропущених значень",
    y = "Змінна"
  ) +
  theme_minimal()

ggsave("plots/eda/missing/na_overview.png", p_na, width = 12, height = 6)


# -----------------------------------
# 2. Залежність частки пропущених значень змінної junction_detail від часу доби
# -----------------------------------
df_time <- df %>%
  mutate(hour = as.integer(substr(as.character(time), 1, 2))) %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(
    na_rate = mean(missing_junction),
    .groups = "drop"
  )

p_time <- ggplot(df_time, aes(x = hour, y = na_rate)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.2)) +
  labs(
    title = "Залежність частки пропущених значень змінної junction_detail від часу доби",
    x = "Година доби",
    y = "Частка пропущених значень у junction_detail"
  ) +
  theme_minimal()

ggsave("plots/eda/missing/junction_by_hour_line.png", p_time, width = 10, height = 5)


# -----------------------------------
# 3. Залежність частки пропущених значень змінної junction_detail
#    від типу дороги та місцевості
# -----------------------------------
df_heat <- df %>%
  filter(!is.na(road_type), !is.na(urban_or_rural_area)) %>%
  group_by(road_type, urban_or_rural_area) %>%
  summarise(
    na_rate = mean(missing_junction),
    .groups = "drop"
  )

p_heat <- ggplot(df_heat, aes(x = urban_or_rural_area, y = road_type, fill = na_rate)) +
  geom_tile() +
  geom_text(aes(label = percent(na_rate, accuracy = 0.1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red", labels = percent_format()) +
  labs(
    title = "Залежність частки пропущених значень змінної junction_detail від типу дороги та місцевості",
    x = "Тип місцевості (urban_or_rural_area)",
    y = "Тип дороги (road_type)",
    fill = "Частка NA"
  ) +
  theme_minimal()

ggsave("plots/eda/missing/junction_heatmap.png", p_heat, width = 9, height = 6)


# -----------------------------------
# 4. Розподіл значень змінної junction_control
# -----------------------------------
df_control <- df %>%
  mutate(
    junction_control_cat = ifelse(is.na(junction_control), "NA", as.character(junction_control))
  ) %>%
  count(junction_control_cat) %>%
  mutate(share = n / sum(n)) %>%
  arrange(share)

p_control <- ggplot(
  df_control,
  aes(
    x = share,
    y = reorder(junction_control_cat, share),
    fill = junction_control_cat == "NA"
  )
) +
  geom_col() +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Розподіл значень змінної junction_control",
    x = "Частка спостережень",
    y = "Категорії junction_control"
  ) +
  theme_minimal()

ggsave("plots/eda/missing/junction_control_distribution.png", p_control, width = 11, height = 5)


# -----------------------------------
# 5. Розподіл значень змінної special_conditions_at_site
# -----------------------------------
df_special <- df %>%
  mutate(
    special_cat = ifelse(is.na(special_conditions_at_site), "NA", as.character(special_conditions_at_site))
  ) %>%
  count(special_cat) %>%
  mutate(share = n / sum(n)) %>%
  slice_max(n, n = 20) %>%
  arrange(share)

p_special <- ggplot(
  df_special,
  aes(
    x = share,
    y = reorder(special_cat, share),
    fill = special_cat == "NA"
  )
) +
  geom_col() +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Розподіл значень змінної special_conditions_at_site",
    x = "Частка спостережень",
    y = "Категорії special_conditions_at_site"
  ) +
  theme_minimal()

ggsave("plots/eda/missing/special_conditions_distribution.png", p_special, width = 12, height = 6)


# -----------------------------------
# 6. Залежність ймовірності тяжкого ДТП від погодних умов
# -----------------------------------
df_weather_prob <- df %>%
  mutate(
    weather_cat = ifelse(is.na(weather_conditions), "NA", as.character(weather_conditions))
  ) %>%
  group_by(weather_cat) %>%
  summarise(
    mean_prob = mean(collision_adjusted_severity_serious, na.rm = TRUE),
    .groups = "drop"
  )

p_weather_prob <- ggplot(
  df_weather_prob,
  aes(
    x = mean_prob,
    y = reorder(weather_cat, mean_prob),
    fill = weather_cat == "NA"
  )
) +
  geom_col() +
  geom_text(
    aes(label = round(mean_prob, 3)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Залежність ймовірності тяжкого ДТП від погодних умов",
    x = "Середня ймовірність тяжкого ДТП",
    y = "Погодні умови (weather_conditions)"
  ) +
  theme_minimal()

ggsave(
  "plots/eda/missing/weather_mean_probability.png",
  p_weather_prob,
  width = 9,
  height = 6
)


# -----------------------------------
# 7. Залежність ймовірності тяжкого ДТП від стану дорожнього покриття
# -----------------------------------
df_surface_prob <- df %>%
  mutate(
    surface_cat = ifelse(is.na(road_surface_conditions), "NA", as.character(road_surface_conditions))
  ) %>%
  group_by(surface_cat) %>%
  summarise(
    mean_prob = mean(collision_adjusted_severity_serious, na.rm = TRUE),
    .groups = "drop"
  )

p_surface_prob <- ggplot(
  df_surface_prob,
  aes(
    x = mean_prob,
    y = reorder(surface_cat, mean_prob),
    fill = surface_cat == "NA"
  )
) +
  geom_col() +
  geom_text(
    aes(label = round(mean_prob, 3)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Залежність ймовірності тяжкого ДТП від стану дорожнього покриття",
    x = "Середня ймовірність тяжкого ДТП",
    y = "Стан дорожнього покриття (road_surface_conditions)"
  ) +
  theme_minimal()

ggsave(
  "plots/eda/missing/road_surface_mean_probability.png",
  p_surface_prob,
  width = 9,
  height = 6
)
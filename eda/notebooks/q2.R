library(tidyverse)
library(lubridate)
library(gt)
library(scales)

collisions <- read_csv("data/processed/collisions_2024_decoded.csv")

selected_vars <- collisions %>%
  select(
    date,
    time,
    weather_conditions,
    light_conditions,
    road_surface_conditions,
    collision_severity,
    speed_limit
  )

missing_percent <- tibble(
  variable = names(selected_vars),
  missing_percent = round(colMeans(is.na(selected_vars)) * 100, 2)
)

missing_percent %>%
  gt() %>%
  tab_header(
    title = "Відсоток пропущених значень (%)"
  )

problem_values <- c("Unknown", "Other", "Unclassified")

undefined_summary <- selected_vars %>%
  summarise(
    weather_conditions = sum(weather_conditions %in% problem_values, na.rm = TRUE),
    light_conditions = sum(light_conditions %in% problem_values, na.rm = TRUE),
    road_surface_conditions = sum(road_surface_conditions %in% problem_values, na.rm = TRUE),
    collision_severity = sum(collision_severity %in% problem_values, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "undefined_count"
  )

undefined_summary %>%
  gt() %>%
  tab_header(
    title = "Кількість невизначених значень"
  )

collisions <- collisions %>%
  mutate(
    date = dmy(date),
    time = hms(time),
    hour = hour(time),
    month = month(date)
  ) %>%
  filter(
    !is.na(date),
    !is.na(hour),
    !is.na(month)
  )

collisions <- collisions %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Зима",
      month %in% c(3, 4, 5) ~ "Весна",
      month %in% c(6, 7, 8) ~ "Літо",
      month %in% c(9, 10, 11) ~ "Осінь",
      TRUE ~ NA_character_
    ),
    season = factor(
      season,
      levels = c("Осінь", "Весна", "Літо", "Зима")
    )
  )

collisions <- collisions %>%
  mutate(
    collision_severity = case_when(
      collision_severity == "Slight" ~ "Легка",
      collision_severity == "Serious" ~ "Серйозна",
      collision_severity == "Fatal" ~ "Фатальна",
      TRUE ~ collision_severity
    ),
    collision_severity = factor(
      collision_severity,
      levels = c("Легка", "Серйозна", "Фатальна")
    )
  )

#Кількість ДТП за порами року
season_summary <- collisions %>%
  count(season) %>%
  mutate(
    percent = round(n / sum(n) * 100, 2)
  )

season_summary

season_summary %>%
  gt() %>%
  tab_header(
    title = "Кількість ДТП за сезонами у відсотковому співвідношенні"
  ) %>%
  cols_label(
    season = "Сезон",
    n = "Кількість ДТП",
    percent = "Відсоток, %"
  )

ggplot(season_summary, aes(x = season, y = n, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  labs(
    title = "Кількість ДТП за порами року",
    x = "Пора року",
    y = "Кількість ДТП"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#к-ть дтп по місяцях
ggplot(collisions, aes(x = factor(month))) +
  geom_bar(
    width = 0.7,
    fill = "#B084CC",
    color = "white"
  ) +
  labs(
    title = "Кількість ДТП за місяцями",
    x = "Місяць",
    y = "Кількість ДТП"
  ) 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

#к-ть дтп за сезонами та год
ggplot(collisions, aes(x = hour, fill = season)) +
  geom_histogram(
    binwidth = 1,
    position = "dodge",
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  scale_x_continuous(
    breaks = 0:23
  ) +
  labs(
    title = "Кількість ДТП по годинах та сезонах",
    x = "Година доби",
    y = "Кількість ДТП",
    fill = "Пора року"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#погода за сезонами
ggplot(collisions, aes(x = weather_conditions, fill = season)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  labs(
    title = "Погодні умови за сезонами",
    x = "Погодні умови",
    y = "Кількість ДТП",
    fill = "Пора року"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#освітлення за сезонами
ggplot(collisions, aes(x = light_conditions, fill = season)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  labs(
    title = "Умови освітлення за сезонами",
    x = "Умови освітлення",
    y = "Кількість ДТП",
    fill = "Пора року"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#тяжкість дтп за сезонами
ggplot(collisions, aes(x = collision_severity, fill = season)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  labs(
    title = "Тяжкість ДТП за сезонами",
    x = "Тяжкість ДТП",
    y = "Кількість ДТП",
    fill = "Пора року"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#ліміт швидкості за сезонами
speed_data <- collisions %>%
  filter(!is.na(speed_limit)) %>%
  count(speed_limit, season)

ggplot(speed_data, aes(x = speed_limit, y = n, color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Осінь" = "#B07AA1",
      "Весна" = "#59A14F",
      "Літо" = "#F28E2B",
      "Зима" = "#4E79A7"
    )
  ) +
  labs(
    title = "Кількість ДТП за лімітом швидкості та сезоном",
    x = "Ліміт швидкості",
    y = "Кількість ДТП",
    color = "Пора року"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
#частка дтп за станом дороги
road_surface_data <- collisions %>%
  mutate(
    road_surface_ua = case_when(
      road_surface_conditions == "Dry" ~ "Суха дорога",
      road_surface_conditions == "Wet or damp" ~ "Мокра або волога дорога",
      road_surface_conditions == "Snow" ~ "Сніг",
      road_surface_conditions == "Frost or ice" ~ "Ожеледиця або іній",
      road_surface_conditions == "Flood over 3cm. deep" ~ "Затоплення понад 3 см",
      road_surface_conditions == "unknown (self reported)" ~ "Невідомо",
      road_surface_conditions == "Data missing or out of range" ~ "Дані відсутні або некоректні",
      TRUE ~ "Інше"
    ),
    road_surface_ua = factor(
      road_surface_ua,
      levels = c(
        "Суха дорога",
        "Мокра або волога дорога",
        "Сніг",
        "Ожеледиця або іній",
        "Затоплення понад 3 см",
        "Невідомо",
        "Дані відсутні або некоректні",
        "Інше"
      )
    )
  )

ggplot(road_surface_data, aes(x = season, fill = road_surface_ua)) +
  geom_bar(
    position = "fill",
    width = 0.72,
    color = "white",
    linewidth = 0.3
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Суха дорога" = "#C9B26B",                  # теплий пісочний
      "Мокра або волога дорога" = "#7B8FA1",      # приглушений синьо-сірий
      "Сніг" = "#BFD7EA",                         # світло-блакитний
      "Ожеледиця або іній" = "#9CC5C9",           # м'який бірюзовий
      "Затоплення понад 3 см" = "#5C9EAD",        # глибший синьо-бірюзовий
      "Невідомо" = "#B0B7C3",                     # нейтральний сіро-блакитний
      "Дані відсутні або некоректні" = "#D9A5A5", # приглушений рожево-сірий
      "Інше" = "#D8CFC4"                          # світлий бежево-сірий
    )
  ) +
  labs(
    title = "Частка ДТП за станом дорожнього покриття в кожному сезоні",
    x = "Пора року",
    y = "Частка ДТП",
    fill = "Стан дорожнього покриття"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )
#тепловий графік зща год і сезоном
heatmap_data <- collisions %>%
  count(hour, season)

ggplot(heatmap_data, aes(x = hour, y = season, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#D6EAF8",
    high = "#08306B"
  ) +
  scale_x_continuous(
    breaks = 0:23
  ) +
  labs(
    title = "Тепловий графік ДТП за годиною та порою року",
    x = "Година доби",
    y = "Пора року",
    fill = "Кількість ДТП"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

chisq_result <- chisq.test(table(collisions$season))

print(chisq_result)


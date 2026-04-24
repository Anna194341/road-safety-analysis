packages <- c("dplyr", "tidyr", "ggplot2", "scales", "gt", "broom")

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages, library, character.only = TRUE))


#Завантаження даних

df <- read.csv("data/processed/collisions_2024_decoded.csv")


#ф-я для табл

show_gt <- function(data, title_text) {
  data %>%
    gt() %>%
    tab_header(
      title = title_text
    ) %>%
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    )
}


#перевірка даних

vars_for_study <- df %>%
  select(
    collision_severity,
    weather_conditions,
    light_conditions
  )

missing_table <- data.frame(
  Змінна = names(vars_for_study),
  Кількість_пропущених = sapply(vars_for_study, function(x) sum(is.na(x)))
)

show_gt(
  missing_table,
  "Кількість пропущених значень у змінних дослідження"
)


#підготовка даних

df2 <- df %>%
  mutate(
    тяжкі_наслідки = case_when(
      collision_severity %in% c("Fatal", "Serious") ~ 1,
      collision_severity == "Slight" ~ 0,
      TRUE ~ NA_real_
    ),
    
    категорія_тяжкості = case_when(
      collision_severity %in% c("Fatal", "Serious") ~ "Серйозні або смертельні",
      collision_severity == "Slight" ~ "Легкі",
      TRUE ~ NA_character_
    ),
    
    дощ = case_when(
      weather_conditions %in% c(
        "Raining no high winds",
        "Raining + high winds"
      ) ~ "Дощ",
      
      weather_conditions %in% c(
        "Fine no high winds",
        "Fine + high winds",
        "Fog or mist",
        "Other",
        "Snowing no high winds",
        "Snowing + high winds"
      ) ~ "Без дощу",
      
      TRUE ~ NA_character_
    ),
    
    освітлення = case_when(
      light_conditions == "Darkness - no lighting" ~ "Ніч без освітлення",
      
      light_conditions %in% c(
        "Daylight",
        "Darkness - lights lit",
        "Darkness - lights unlit",
        "Darkness - lighting unknown"
      ) ~ "Інші умови освітлення",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(тяжкі_наслідки),
    !is.na(категорія_тяжкості),
    !is.na(дощ),
    !is.na(освітлення),
    weather_conditions != "Unknown",
    light_conditions != "Data missing or out of range"
  ) %>%
  mutate(
    дощ = factor(
      дощ,
      levels = c("Без дощу", "Дощ")
    ),
    
    освітлення = factor(
      освітлення,
      levels = c("Інші умови освітлення", "Ніч без освітлення")
    ),
    
    категорія_тяжкості = factor(
      категорія_тяжкості,
      levels = c("Легкі", "Серйозні або смертельні")
    )
  )


#розподіл за дощем---

rain_distribution <- df2 %>%
  count(дощ) %>%
  mutate(
    Відсоток = round(n / sum(n) * 100, 2)
  ) %>%
  rename(
    Погодні_умови = дощ,
    Кількість_ДТП = n
  )

show_gt(
  rain_distribution,
  "Розподіл ДТП за погодними умовами"
)


#розподіл за освітленням

light_distribution <- df2 %>%
  count(освітлення) %>%
  mutate(
    Відсоток = round(n / sum(n) * 100, 2)
  ) %>%
  rename(
    Умови_освітлення = освітлення,
    Кількість_ДТП = n
  )

show_gt(
  light_distribution,
  "Розподіл ДТП за умовами освітлення"
)


#дощ і тяжкість дтп 

tab_rain <- df2 %>%
  group_by(дощ, категорія_тяжкості) %>%
  summarise(
    Кількість_ДТП = n(),
    .groups = "drop"
  ) %>%
  group_by(дощ) %>%
  mutate(
    Частка_у_групі = round(Кількість_ДТП / sum(Кількість_ДТП) * 100, 2)
  ) %>%
  ungroup()

show_gt(
  tab_rain,
  "Дощ і тяжкість наслідків ДТП"
)


#освітлення і тяжкість дтп 

tab_light <- df2 %>%
  group_by(освітлення, категорія_тяжкості) %>%
  summarise(
    Кількість_ДТП = n(),
    .groups = "drop"
  ) %>%
  group_by(освітлення) %>%
  mutate(
    Частка_у_групі = round(Кількість_ДТП / sum(Кількість_ДТП) * 100, 2)
  ) %>%
  ungroup()

show_gt(
  tab_light,
  "Освітлення і тяжкість наслідків ДТП"
)


#комбінація умов табл

combo_table <- df2 %>%
  group_by(дощ, освітлення) %>%
  summarise(
    Кількість_ДТП = n(),
    Кількість_тяжких_ДТП = sum(тяжкі_наслідки),
    Частка_тяжких_ДТП_відсотків = round(mean(тяжкі_наслідки) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Частка_тяжких_ДТП_відсотків))

show_gt(
  combo_table,
  "Комбінований вплив дощу та освітлення на тяжкість ДТП"
)


#тепловий графік
heatmap_data <- df2 %>%
  group_by(дощ, освітлення) %>%
  summarise(
    кількість_ДТП = n(),
    кількість_тяжких_ДТП = sum(тяжкі_наслідки),
    частка_тяжких_ДТП = mean(тяжкі_наслідки),
    .groups = "drop"
  )

plot_combo_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = дощ,
    y = освітлення,
    fill = частка_тяжких_ДТП
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1.8,
    width = 0.95,
    height = 0.95
  ) +
  geom_text(
    aes(
      label = paste0(
        percent(частка_тяжких_ДТП, accuracy = 0.1),
        "\n",
        "n = ", кількість_ДТП
      )
    ),
    color = "white",
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#BFD7EA",
    high = "#3C096C",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Комбінований вплив дощу та освітлення на тяжкість ДТП",
    subtitle = "Колір показує частку серйозних або смертельних ДТП; n — кількість ДТП у групі",
    x = "Погодні умови",
    y = "Умови освітлення",
    fill = "Частка тяжких ДТП"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11
    ),
    axis.title = element_text(
      face = "bold",
      size = 13
    ),
    axis.text = element_text(
      size = 12,
      face = "bold"
    ),
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid = element_blank()
  )

print(plot_combo_heatmap)


# 13. ДОДАТКОВИЙ ГРАФІК: КІЛЬКІСТЬ ДТП ЗА ОСВІТЛЕННЯМ -------

light_count_data <- df2 %>%
  count(освітлення)

plot_count_light <- ggplot(
  light_count_data,
  aes(
    x = освітлення,
    y = n,
    fill = освітлення
  )
) +
  geom_col(
    width = 0.65,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Інші умови освітлення" = "#8FA6B2",
      "Ніч без освітлення" = "#5E548E"
    )
  ) +
  labs(
    title = "Кількість ДТП за умовами освітлення",
    x = "Умови освітлення",
    y = "Кількість ДТП"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 10,
      hjust = 1
    ),
    legend.position = "none"
  )

print(plot_count_light)


#тяжкі дтп за дощем 
plot_rain <- ggplot(
  df2,
  aes(
    x = дощ,
    fill = категорія_тяжкості
  )
) +
  geom_bar(
    position = "fill",
    width = 0.7,
    color = "white",
    linewidth = 0.4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Легкі" = "#BFD7EA",
      "Серйозні або смертельні" = "#5E548E"
    )
  ) +
  labs(
    title = "Частка тяжких ДТП залежно від дощу",
    x = "Погодні умови",
    y = "Частка ДТП",
    fill = "Тяжкість наслідків"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(plot_rain)


#тяжкість дтп за освітленням
plot_light <- ggplot(
  df2,
  aes(
    x = освітлення,
    fill = категорія_тяжкості
  )
) +
  geom_bar(
    position = "fill",
    width = 0.7,
    color = "white",
    linewidth = 0.4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Легкі" = "#BFD7EA",
      "Серйозні або смертельні" = "#5E548E"
    )
  ) +
  labs(
    title = "Частка тяжких ДТП залежно від умов освітлення",
    x = "Умови освітлення",
    y = "Частка ДТП",
    fill = "Тяжкість наслідків"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 10,
      hjust = 1
    ),
    legend.title = element_text(face = "bold")
  )

print(plot_light)


#точковий графік
summary_points <- df2 %>%
  group_by(дощ, освітлення) %>%
  summarise(
    кількість_ДТП = n(),
    частка_тяжких = mean(тяжкі_наслідки),
    .groups = "drop"
  )

plot_points <- ggplot(
  summary_points,
  aes(
    x = дощ,
    y = частка_тяжких,
    color = освітлення,
    group = освітлення
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4.5) +
  geom_text(
    aes(label = percent(частка_тяжких, accuracy = 0.1)),
    vjust = -1,
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "Інші умови освітлення" = "#8FA6B2",
      "Ніч без освітлення" = "#5E548E"
    )
  ) +
  labs(
    title = "Ймовірність тяжких ДТП за погодою та освітленням",
    x = "Погодні умови",
    y = "Частка тяжких ДТП",
    color = "Умови освітлення"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(plot_points)



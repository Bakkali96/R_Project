
library(readxl)
library(tidyverse)
library(scales)
library(ggplot2)

data <- read_excel("current account balance.xlsx")

year_cols <- names(data)[grepl("^\\d{4}$", names(data))]


data_long <- data |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "current"
  ) |>
  mutate(
    year = as.numeric(year),
    balance = as.numeric(current),
    `Country Name` = str_trim(str_to_title(`Country Name`))
  ) |>
  drop_na(current)

selected_countries <- c("Indonesia","Malaysia", "Philippines","Thailand")

filtered_data <- data_long |>
  filter(
    `Country Name` %in% selected_countries,
    year >= 1985,
    year <= 2000
  )

arrow_data <- filtered_data |>
  filter(`Country Name` == "Thailand", year %in% c(1987, 1996))


ggplot(filtered_data, aes(x = year, y = balance, fill = `Country Name`)) +
  geom_area(position = "stack", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "yellow", linewidth = 1) +
  geom_vline(xintercept = 1997.5, color = "red", linetype = "dashed", linewidth = 1.2) +
  annotate("segment",
           x = arrow_data$year[1],   # 1985
           xend = arrow_data$year[2], 
           y = arrow_data$balance[1], 
           yend = arrow_data$balance[2],
           color = "black",
           arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  
  labs(
    title = "Current Account Balance (1985–2000)",
    subtitle = "Selected Southeast Asian Countries with 1997 Crisis Marker",
    x = "Year",
    y = "BoP",
    fill = "Country"
  ) +
  scale_x_continuous(breaks = seq(1985, 2000, by = 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  ) + scale_y_continuous(labels=label_number(scale = 1e-9, suffix = "B"))

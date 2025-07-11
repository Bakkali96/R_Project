
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(tidyverse)


exchange_data <- read_excel("Official Exchange rates.xlsx")

year_cols <- names(exchange_data)[grepl("^\\d{4}$", names(exchange_data))]

exchange_long <- exchange_data %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "exchange_rate"
  ) %>%
  mutate(
    year = as.numeric(year),
    exchange_rate = as.numeric(exchange_rate),
    `Country Name` = str_trim(str_to_title(`Country Name`))
  ) %>%
  drop_na(exchange_rate)

selected_countries <- c("Indonesia", "Japan", "Malaysia", "Philippines", "Thailand")

exchange_filtered <- exchange_long %>%
  filter(
    `Country Name` %in% selected_countries,
    year >= 1981,
    year <= 2003
  )

ggplot(exchange_filtered, aes(x = year, y = exchange_rate)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = 1998, color = "red", linetype = "dashed", linewidth = 1)+geom_vline(xintercept = 1997, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~`Country Name`, ncol = 2, scales="free_y") +
  scale_x_continuous(breaks = seq(1981, 2003, by = 3)) +
  labs(
    title = "Official Exchange Rate (1975-2005)",
    subtitle = "With 1997 Asian Crisis Indicator",
    x = "Year",
    y = "Exchange Rate (Local Currency per USD)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8), panel.spacing = unit(1, "lines"))


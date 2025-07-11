
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
installed.packages("ggplot2")
install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

credit_data <- read_excel("Credit to private sector.xlsx")



year_cols <- names(credit_data)[grepl("^\\d{4}$", names(credit_data))]


credit_long <- credit_data |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "credit"
  ) |>
  mutate(
    year = as.numeric(year),
    credit = as.numeric(credit),
    `Country Name` = str_trim(str_to_title(`Country Name`))  # clean name
  ) |>
  filter(year >= 1985, year <= 2023) |>
  drop_na(credit)


credit_world <- credit_long |> filter(`Country Name` == "World")
credit_countries <- credit_long |> filter(`Country Name` != "World")

ggplot() +
  
  geom_col(
    data = credit_countries,
    aes(x = factor(year), y = credit, fill = `Country Name`),
    position = "dodge"
  ) +
  
  geom_line(
    data = credit_world,
    aes(x = factor(year), y = credit, group = 1),
    color = "red",
    linewidth = 1.2
  ) +
  scale_x_discrete(breaks = as.character(seq(1985, 2023, by = 2))) +
  labs(
    title = "Credit to Private Sector (1985–2023)",
    x = "Year",
    y = "Credit (% of GDP)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(credit_world)



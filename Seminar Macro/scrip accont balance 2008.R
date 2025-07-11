install.packages("tidyr")

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)
# Load the Excel file
data <- read_excel("current account balance.xlsx")

# Detect year columns
year_cols <- names(data)[grepl("^\\d{4}$", names(data))]

# Reshape to long format
data_long <- data |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "balance"
  ) |>
  mutate(
    year = as.numeric(year),
    balance = as.numeric(balance),
    `Country Name` = str_trim(str_to_title(`Country Name`))
  ) |>
  filter(
    `Country Name` %in% c("United States", "United Kingdom", "Spain", "Greece"),
    year >= 1991,
    year <= 2024
  ) |>
  drop_na(balance)

# Create diverging bar plot
ggplot(data_long, aes(x = factor(year), y = balance, fill = balance > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~`Country Name`, ncol = 2, scales= "free_y") +
  geom_vline(xintercept = which(levels(factor(data_long$year)) == "2008"), 
             color = "red", linetype = "dashed", linewidth = 1.2) +
  scale_fill_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C")) +
  labs(
    title = "Current Account Balance (1991–2024)",
    subtitle = "Highlighting Divergence Around the 2008 Crisis",
    x = "Year",
    y = "Balance (% of GDP)"
  ) +
  scale_x_discrete(breaks = seq(1991, 2024, by = 3)) + scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +

  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

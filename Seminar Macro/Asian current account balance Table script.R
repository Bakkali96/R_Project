###   Summary table

install.packages("readxl")
install.packages("tidyverse")

library(readxl)
library(tidyverse)

data <- read_excel("current account balance.xlsx")


year_cols <- names(data)[grepl("^\\d{4}$", names(data))]


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
  drop_na(balance)



countries <- c("Thailand", "Indonesia", "Malaysia", "Philippines")

filtered_data <- data_long |>
  filter(
    `Country Name` %in% countries,
    year >= 1987,
    year <= 2000
  )


summary_table <- filtered_data |>
  group_split(`Country Name`) |>
  map_dfr(function(df) {
    country <- unique(df$`Country Name`)
    df_8796 <- df |> filter(year <= 1996)
    
    # Trend
    slope <- coef(lm(balance ~ year, data = df_8796))[2]
    trend <- ifelse(slope < 0, "⬇️ decreasing", "⬆️ increasing")
    
    # Peak deficit year
    peak_year <- df$year[which.min(df$balance)]
    
    # Reversal test
    value_1996 <- df$balance[df$year == 1996]
    value_2000 <- df$balance[df$year == 2000]
    reversed <- ifelse(value_2000 > value_1996, "✅ Yes", "❌ No")
    
    # Crisis signal
    crisis_signal <- if (value_1996 < 0 && value_2000 > value_1996) "✅ Yes" else "🟥 No"
    
    tibble(
      Country = country,
      `Trend (1987–1996)` = trend,
      `Peak Deficit Year` = peak_year,
      `Reversal After Crisis` = reversed,
      `Crisis Signal` = crisis_signal
    )
  })

###########
###   Table plot

install.packages("ggpubr")

library(ggpubr)
library(ggplot2)
ggtexttable(summary_table, rows = NULL, theme = ttheme("mOrange"))


# Create a text column with all info in one line per country
summary_table_plot <- summary_table |>
  mutate(row_text = paste0(
    "Trend: ", `Trend (1987–1996)`, "\n",
    "Peak Year: ", `Peak Deficit Year`, "\n",
    "Reversal: ", `Reversal After Crisis`, "\n",
    "Crisis Signal: ", `Crisis Signal`
  ))




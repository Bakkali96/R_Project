
install.packages("zoo")
install.packages("ggplot2")

library(readxl)
library(dplyr)    
library(ggplot2) 
library(zoo)
library(scales) 

price_data <- read_excel("Residential price index.xlsx")

# Clean and prepare the data
price_data <- price_data |>
  rename(
    period = 1,
    index = 2
  ) |>
  mutate(
    period = as.Date(period),          # Convert to Date
    index = as.numeric(index)          # Ensure numeric index
  ) |>
  arrange(period) |>
  mutate(
    ma_10yr= zoo::rollmean(index, k = 40, fill = NA, align = "center")  # 5-year = 20 quarters
  )

# Create the plot with shaded 2008–2009 crisis area
ggplot(price_data, aes(x = period)) +
  # Raw index points
  geom_point(aes(y = index), color = "darkblue", size = 1.2, alpha = 0.6) +
  
  # 5-year moving average line
  geom_line(aes(y = ma_10r), color = "red", size = 1.1, linetype = "longdash") +
  
  # Shaded area for 2008–2009 crisis
  annotate(
    "rect",
    xmin = as.Date("2008-01-01"),
    xmax = as.Date("2009-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.15,
    fill = "firebrick"
  ) +
  
  # Text label on the shaded area
  annotate(
    "text",
    x = as.Date("2008-06-30"),
    y = max(price_data$index, na.rm = TRUE),
    label = "Global Financial Crisis",
    color = "firebrick",
    size = 4,
    fontface = "bold"
  ) +
  
  # Axis and title formatting
  scale_x_date(
    breaks = date_breaks("10 years"),
    labels = date_format("%Y"),
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Residential Price Index with 10-Year Moving Average",
    x = "Year",
    y = "Price Index",
    caption = "Source: Residential price index.xlsx"
  ) +
  theme_minimal(base_size = 13)


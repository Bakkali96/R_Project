library(readxl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)

credit_data <- read_excel("credit non financial sector.xlsx") |>
  rename(
    period = 1,
    credit = 2
  ) |>
  mutate(
    period = as.Date(period),
    credit = as.numeric(credit)
  ) |>
  arrange(period) |>
  filter(!is.na(period), !is.na(credit))

#########
p <- ggplot(credit_data, aes(x = period, y = credit)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 1.8) +
  labs(
    title = "Credit to Non-Financial Sector",
    subtitle = "Animated with 2008 Crisis Marker",
    x = "Period",
    y = "Quarter-over-Quarter Change (%)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2008-09-30"), color = "red", linetype = "dashed", linewidth = 1)


animated_plot <- p + transition_reveal(period) + labs(
  title = 'Credit Non Financial Sector',
  subtitle = 'Animated with 2008 Crisis Marker')


animate(animated_plot, fps = 10, width=800, heigh= 500, renderer= gifski_renderer())

anim_save(animated_plot, filename= "credit non financial sector.gif", nframes= 200, fps = 10, width= 800, height= 600, renderer= gifski_renderer())


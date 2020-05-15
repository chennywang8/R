library(ggplot2)
library(scales)

raw <- read.csv("model3_seat.csv")
raw <- raw[raw$prop_value>60, ]
raw <- raw[!(raw$prop_tag=="020123L"&raw$prop_value<100), ]
raw$date_time <- as.Date(raw$date_time)

ggplot(raw, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "Shipping Position")


ggplot(raw, aes(x = prop_value, color = prop_tag, fill = prop_tag)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

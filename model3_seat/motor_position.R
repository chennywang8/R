library(ggplot2)
library(scales)
library(dplyr)

fph <- "model3_seat_RH.csv"
raw <- read.csv(fph) %>%
       mutate(tag = substring(prop_tag, 1, 5)) %>%
       filter((tag=="02003"&between(prop_value, 650, 800)) |
              (tag=="02012"&between(prop_value, 100, 200)) |
              (tag=="02006"&between(prop_value, 60, 100)) |
              (tag=="02009"&between(prop_value, 700, 785)))
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

ff <- read.csv(fph) %>%
      filter(prop_value==0) %>%
      group_by(prop_tag) %>%
      summarise(n = n())

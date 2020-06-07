library(ggplot2)
library(scales)
library(dplyr)

fph <- "model3_seat_LH_66.csv"
raw <- read.csv(fph) %>%
       mutate(tag = substring(prop_tag, 1, 5),
              prop_value = as.double(prop_value),
              date_time = as.Date(date_time)) %>%
       filter((tag=="02003"&between(prop_value, 650, 800)) |
              (tag=="02012"&between(prop_value, 100, 200)) |
              (tag=="02006"&between(prop_value, 60, 100)) |
              (tag=="02009"&between(prop_value, 770, 785)))
fff <- read.csv(fph) %>% filter(prop_status==0)

ggplot(raw, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "Shipping Position")


ggplot(raw, aes(x = prop_value, color = prop_tag, fill = prop_tag)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

f_zero <- fff %>% filter(prop_value==0) %>% group_by(prop_tag) %>% summarise(n = n())

f_fail <- fff %>% group_by(prop_tag) %>% summarise(n = n())

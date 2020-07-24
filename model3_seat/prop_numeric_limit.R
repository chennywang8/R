library(ggplot2)
library(scales)
library(dplyr)

file_path <- "Test Data 3.0.17 7-24-2020.csv"
raw <- read.csv(file_path) %>%
        mutate(tag = substring(PROP_TAG, 1, 5),
               DATE_TIME = as.POSIXct(DATE_TIME))
names(raw) <- tolower(names(raw))
shp <- raw %>% select(-c("uut_name", "uut_part_number", "comp_operator", "step_name")) %>%
      filter((tag=="02003"&between(prop_value, 670, 800)) |
               (tag=="02012"&between(prop_value, 130, 200)) |
               (tag=="02006"&between(prop_value, 67, 100)) |
               (tag=="02009"&between(prop_value, 750, 785)) )

recline <- shp %>% filter(tag=="02003"&!between(prop_value, 713, 719))

# ======== PLOT ========
ggplot(shp, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag, ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "Shipping Position")


ggplot(shp, aes(x = prop_value, color = prop_tag, fill = prop_tag)) +
  facet_wrap(. ~ prop_tag, ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()


# ======= LASER ========
lt1 <- filter(raw, tag=="02105"&step_status!="Passed") %>%
  left_join(filter(raw, tag=="02005"), by = "date_time") %>%
  left_join(filter(raw, tag=="02006"), by = "date_time") %>%
  select(date_time, prop_tag.x, prop_value.x, prop_tag.y, prop_value.y, prop_tag, prop_value) %>%
  arrange(date_time)
names(lt1) <- c("date_time", "laser_tag", "laser_reading",
                "second_run_tag", "second_run_encoder",
                "third_run_tag", "third_run_encoder")

lt2 <- lt1 %>% filter(third_run_encoder!=0)

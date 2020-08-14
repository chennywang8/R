library(ggplot2)
library(scales)
library(dplyr)

funcReadRaw <- function(file_path) {
  raw <- read.csv(file_path) %>%
          mutate(tag = substring(PROP_TAG, 1, 5),
                 DATE_TIME = as.POSIXct(DATE_TIME))
  names(raw) <- tolower(names(raw))
  raw
}

funcFilterShp <- function(raw) {
  raw %>% select(-c("uut_name", "uut_part_number", "comp_operator", "step_name")) %>%
      filter((tag=="02003"&between(prop_value, 700, 800)) |
               (tag=="02012"&between(prop_value, 130, 200)) |
               (tag=="02006"&between(prop_value, 67, 100)) |
               (tag=="02009"&between(prop_value, 765, 785)) )
}


# ======== MAIN ========
freg<- "*.csv"
raw <- list.files('./data_with_version/', pattern = freg,
                  full.names = TRUE, recursive = TRUE) %>%
  lapply(., funcReadRaw) %>%
  do.call(rbind, .) %>%
  unique()
shp <- funcFilterShp(raw)
recline <- shp %>% filter(tag=="02003"&!between(prop_value, 713, 719))
cdr <- raw %>% filter(tag %in% c('02133', '02134', '02131', '02132'))

# ======== PLOT ========
ggplot(shp, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag, ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = sw_version)) +
  labs(x="Test Date", y = "Shipping Position")


ggplot(shp, aes(x = prop_value, color = sw_version, fill = sw_version)) +
  facet_wrap(. ~ prop_tag, ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

ggplot(cdr, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = sw_version)) +
  labs(x="Test Date", y = "Current Draw [A]")


ggplot(cdr, aes(x = prop_value, color = sw_version, fill = sw_version)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

# ======= LASER ========
lt1 <- filter(raw, tag=="02105"&step_status!="Passed") %>%
  left_join(filter(raw, tag=="02005"), by = "date_time") %>%
  left_join(filter(raw, tag=="02006"), by = "date_time") %>%
  select(date_time, prop_tag.x, prop_value.x, prop_tag.y, prop_value.y,
         prop_tag, prop_value, sw_version, station_id) %>%
  arrange(date_time)
names(lt1) <- c("date_time", "laser_tag", "laser_reading",
                "second_run_tag", "second_run_encoder",
                "third_run_tag", "third_run_encoder",
                "software_version", "station_id")

lt2 <- lt1 %>% filter(third_run_encoder!=0)

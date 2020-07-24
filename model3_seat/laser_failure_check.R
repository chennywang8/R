library(ggplot2)
library(scales)
library(dplyr)


# ======= FUNCTION READ RAW DATA ========
funcReadCsv <- function(fph) {
  read.csv(fph) %>%
    mutate(tag = substring(prop_tag, 1, 5),
           date_time = as.POSIXct(date_time))
}


# ======== FETCH ALL DATA ==============
freg<- "*.csv"
fff <- list.files('./7-15-2020/', pattern = freg,
                  full.names = TRUE, recursive = TRUE) %>%
        lapply(., funcReadCsv) %>%
        do.call(rbind, .) %>%
        unique()


lt1 <- filter(fff, tag=="02105"&prop_status==0) %>%
  left_join(filter(fff, tag=="02005"), by = "date_time") %>%
  left_join(filter(fff, tag=="02006"), by = "date_time") %>%
  select(date_time, prop_tag.x, prop_value.x, prop_tag.y, prop_value.y, prop_tag, prop_value) %>%
  arrange(date_time)
names(lt1) <- c("date_time", "laser_tag", "laser_reading",
                "second_run_tag", "second_run_encoder",
                "third_run_tag", "third_run_encoder")

lt2 <- lt1 %>% filter(third_run_encoder!=0)

#write.csv(lt2, file = "laser_position.csv")

gt1 <- filter(fff, tag=="02105"&prop_value<500) %>%
  inner_join(filter(fff, tag=="02005"), by = "date_time") %>%
  inner_join(filter(fff, tag=="02006"&prop_value<200), by = "date_time")
gt2 <- select(gt1, date_time, prop_tag.x, prop_value.x)
names(gt2) <- c("date_time", "prop_tag", "prop_value")
gt3 <- select(gt1, date_time, prop_tag.y, prop_value.y)
names(gt3) <- c("date_time", "prop_tag", "prop_value")
gt4 <- select(gt1, date_time, prop_tag, prop_value)
gtt <- rbind(gt4, gt3, gt2)

ggplot(gtt, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "TBD")

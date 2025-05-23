library(ggplot2)
library(scales)
library(dplyr)


# ======= FUNCTION READ RAW DATA ========
funcReadCsv <- function(fph) {
  read.csv(fph) %>%
    mutate(tag = substring(prop_tag, 1, 5),
           prop_value = as.double(prop_value),
           date_time = as.POSIXct(date_time))
}

# ======== FETCH ALL DATA ==============
freg<- "*.csv"
fff <- list.files('./7-15-2020/', pattern = freg,
                  full.names = TRUE, recursive = TRUE) %>%
       lapply(., funcReadCsv) %>%
       do.call(rbind, .) %>%
       unique()
raw <- fff %>%
       filter((tag=="02003"&between(prop_value, 670, 800)) |
               (tag=="02012"&between(prop_value, 130, 200)) |
               (tag=="02006"&between(prop_value, 60, 100)) |
               (tag=="02009"&between(prop_value, 750, 785)) )
cdr <- fff %>% filter(tag %in% c('02133', '02134', '02131', '02132'))

ffff<- fff %>% group_by(prop_tag) %>% summarise(n = n())
fail<- fff %>% filter(prop_status==0)


f_zero <- fail %>% filter(prop_value==0) %>%
          group_by(prop_tag) %>% summarise(n = n())

f_fail <- fail %>% group_by(prop_tag) %>% summarise(n = n())

# ======== PLOT ========
ggplot(raw, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "Shipping Position")


ggplot(raw, aes(x = prop_value, color = prop_tag, fill = prop_tag)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

ggplot(cdr, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "Current Draw [A]")


ggplot(cdr, aes(x = prop_value, color = prop_tag, fill = prop_tag)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_bar()

# ========== LASER POSITION ================
laser <- fff %>% filter((tag=="02105"&prop_value<500) |
                        (tag=="02005"&between(prop_value, 1000, 2000)) |
                        (tag=="02006"&prop_value<400))

ggplot(laser, aes(x = date_time, y = prop_value)) +
  facet_wrap(. ~ prop_tag , ncol = 2, scales = "free") +
  theme_linedraw(base_size = 20) +
  geom_point(aes(color = prop_tag)) +
  labs(x="Test Date", y = "TBD")

lll <- laser %>% group_by(prop_tag) %>% summarise(n = n())
lt1 <- filter(laser, tag=="02105"&prop_status==0) %>%
       left_join(filter(laser, tag=="02005"), by = "date_time") %>%
       left_join(filter(laser, tag=="02006"), by = "date_time") %>%
       select(date_time, prop_tag.x, prop_value.x, prop_tag.y, prop_value.y, prop_tag, prop_value)

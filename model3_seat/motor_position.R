library(ggplot2)
library(scales)
library(dplyr)


# ======= FUNCTION READ RAW DATA ========
funcReadCsv <- function(fph) {
    read.csv(fph) %>%
    mutate(tag = substring(prop_tag, 1, 5),
           prop_value = as.double(prop_value),
           date_time = as.Date(date_time)) %>%
    filter((tag=="02003"&between(prop_value, 650, 800)) |
             (tag=="02012"&between(prop_value, 100, 200)) |
             (tag=="02006"&between(prop_value, 60, 100)) |
             (tag=="02009"&between(prop_value, 765, 785)))
}

# ======== FETCH ALL DATA ==============
raw <- list.files('.', pattern = ".csv",
                  full.names = TRUE, recursive = TRUE) %>%
       lapply(., funcReadCsv) %>%
       do.call(rbind, .) %>%
       unique()
fff <- list.files('.', pattern = ".csv",
                  full.names = TRUE, recursive = TRUE) %>%
       lapply(., read.csv) %>%
       do.call(rbind, .) %>%
       filter(prop_status==0) %>%
       unique()

f_zero <- fff %>% filter(prop_value==0) %>%
          group_by(prop_tag) %>% summarise(n = n())

f_fail <- fff %>% group_by(prop_tag) %>% summarise(n = n())

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


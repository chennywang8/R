library(jsonlite)
library(DBI)
library(RMariaDB)
library(dplyr)
library(tidyr)

db <- fromJSON("db_query_info.json")
finger_filter <- paste(lapply(db$finger, function(item) {paste0("task_name like '", item, "'")}), collapse = " or ")

conn <- dbConnect(RMariaDB::MariaDB(), 
                  dbname = db$rv$dbname, 
                  host = db$rv$host, 
                  user = db$rv$user, 
                  password = db$rv$password, 
                  port = 3306)
sn_list <- dbGetQuery(conn,
    
)


sn_list %>% group_by() %>%
  count()
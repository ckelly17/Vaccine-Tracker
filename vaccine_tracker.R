library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)

setwd("/Users/conorkelly/Documents/Vaccine-Tracker")

## data source from CDC
return <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")

## import existing data
old_data <- read_csv("vaccine_db.csv") %>%
  mutate(date = as.character(date))

## add new data and write back
return <- fromJSON("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations.json")
new_data <- return[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

# save a copy
date <- as.character(max(ymd(new_data$date)))
write_csv(new_data, paste0("daily_backup/", date, ".csv"))

# bind to old
vaccines <- bind_rows(old_data, new_data) %>%
  distinct(date, location, .keep_all = TRUE) 

## write to main repo
write_csv(vaccines, "vaccine_db.csv")

## clean for viz
vaccines <- vaccines %>%
  rename(state_abb = location,
         state = long_name,
         pop = census2019) %>%
  
  # set up to get new values
  mutate(date = ymd(date)) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(n = row_number()) %>%
  
  # calculate daily change
  mutate(new_dist = ifelse(n >= 2, doses_distributed - lag(doses_distributed, 1), 0),
         new_admin = ifelse(n >= 2, doses_administered - lag(doses_administered, 1), 0)) %>%
  
  ungroup() %>%
  
  # max_date_ind
  mutate(max_date_ind = ifelse(date == max(date), "Yes", "No"))


## categorize states and non-states
territories <- c("American Samoa",
                 "Federated States of Micronesia",
                 "Guam",
                 "Marshall Islands",
                 "Northern Mariana Islands",
                 "Republic of Palau",
                 "Virgin Islands",
                 "Puerto Rico")

fed_programs <- c("Long Term Care",
                  "Dept of Defense",
                  "Bureau of Prisons",
                  "Indian Health Svc",
                  "Veterans Health")

vaccines <- vaccines %>%
  mutate(category = "state",
         category = ifelse(state %in% territories, "territory", category),
         category = ifelse(state %in% fed_programs, "federal program", category),
         category = ifelse(state %in% "United States", "United States", category))

# export  
write_csv(vaccines, "vaccine_viz.csv")

# what are the categories?
vaccines %>% 
  group_by(date) %>% 
  filter(category %in% "United States") %>%
  summarise(total = sum(doses_administered))

vaccines %>% 
  group_by(date) %>% 
  filter(category %in% "state") %>%
  summarise(total = sum(doses_administered))

vaccines %>% 
  group_by(date) %>% 
  filter(!category %in% "United States") %>%
  summarise(total = sum(doses_administered))



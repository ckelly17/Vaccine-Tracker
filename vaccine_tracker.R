library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)

setwd("/Users/conorkelly/Documents/Vaccine-Tracker")
#setwd("C:/Users/ckelly/Documents/Covid-Personal - Copy/Vaccine Tracker/Vaccine-Tracker")

# google credentials
gs4_auth(email = "conor.richard.kelly@gmail.com")

## data source from CDC
return <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")

## import existing data
old_data <- read_csv("https://raw.githubusercontent.com/ckelly17/Vaccine-Tracker/main/vaccine_db.csv") %>%
  mutate(date = as.character(date),
         skipped = "No") %>%
  filter(!is.na(date))

## add new data and write back
new_data <- return[[2]] %>%
  clean_names() %>%
  mutate(date = as.character(mdy(date))) %>%
  distinct(date, location, .keep_all = TRUE) %>%
  mutate(skipped = "No")

# for skipped days
yesterday <- new_data %>%
  mutate(date = as.character(as.Date(date) - 1),
         skipped = "Yes")

# day before
day_before <- new_data %>%
  mutate(date = as.character(as.Date(date) - 2),
         skipped = "Yes")

# save a copy
date <- as.character(max(ymd(new_data$date)))
write_csv(new_data, paste0("daily_backup/", date, ".csv"))

# bind to old
vaccines <- bind_rows(old_data, new_data, yesterday, day_before) %>%
  distinct(date, location, .keep_all = TRUE) %>%
  group_by(location, date) %>%
  filter(doses_administered == min(doses_administered, na.rm = TRUE)) %>%
  ungroup()

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
  mutate(doses_distributed = ifelse(skipped %in% "Yes", lag(doses_distributed), doses_distributed),
         doses_administered = ifelse(skipped %in% "Yes", lag(doses_administered), doses_administered),
         administered_dose1 = ifelse(skipped %in% "Yes", lag(administered_dose1), administered_dose1),
         administered_dose2 = ifelse(skipped %in% "Yes", lag(administered_dose2), administered_dose2),
         
         # hot fix for 2021-01-11
         doses_distributed = ifelse(date  == "2021-01-09", lag(doses_distributed), doses_distributed),
         doses_distributed = ifelse(date  == "2021-01-10", lag(doses_distributed), doses_distributed),
         doses_administered = ifelse(date  == "2021-01-09", lag(doses_administered), doses_administered),
         doses_administered = ifelse(date  == "2021-01-10", lag(doses_administered), doses_administered),
        
         # all doses
         new_dist = ifelse(n >= 2, doses_distributed - lag(doses_distributed, 1), 0),
         new_admin = ifelse(n >= 2, doses_administered - lag(doses_administered, 1), 0),
         
         # dose 1 and 2
         new_dose1 = ifelse(n >= 2, administered_dose1 - lag(administered_dose1, 1), 0),
         new_dose2 = ifelse(n >= 2, administered_dose2 - lag(administered_dose2, 1), 0)) %>%
  
  ungroup() %>%
  
  # max_date_ind
  mutate(max_date_ind = ifelse(date == max(date), "Yes", "No"))

## add US abbr
vaccines <- vaccines %>%
  group_by(state) %>%
  fill(state_abb, .direction = "up")


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

## add new cases from CTP
ctp <- fromJSON("https://api.covidtracking.com/v1/states/daily.json") %>%
  select(state_abb = state, date, new_reported_cases = positiveIncrease) %>%
  mutate(date = ymd(date))

nat_ctp <- ctp %>%
  group_by(date) %>%
  summarize(new_reported_cases =sum(new_reported_cases, na.rm = TRUE)) %>%
  mutate(state_abb = "US")

ctp <- bind_rows(ctp, nat_ctp)

vaccines <- left_join(vaccines, ctp, by = c("state_abb", "date"))

## add estimate for new infections from covid-19 projections
youyang <- read_csv("https://raw.githubusercontent.com/youyanggu/covid19_projections/master/infection_estimates/latest_all_estimates_states.csv") %>%
  select(state_abb = state, date, new_infected_est = new_infected_mean) %>%
  mutate(date = ymd(date) + 15,
         new_infected_est = as.integer(new_infected_est))

nat_youyang <- youyang %>%
  group_by(date) %>%
  summarize(new_infected_est =sum(new_infected_est, na.rm = TRUE)) %>%
  mutate(state_abb = "US")

youyang <- bind_rows(youyang, nat_youyang)

vaccines <- left_join(vaccines, youyang, by = c("state_abb", "date"))

# export  
write_csv(vaccines, "vaccine_viz.csv")
sheet_write(vaccines, ss = "https://docs.google.com/spreadsheets/d/1ezajFR0idY0ifWumhn0J8G0UzCl_qF__5D7mwgR4PD8/edit#gid=0", sheet = "vaccines")

# by date
vaccines %>% 
  group_by(date) %>% 
  filter(category %in% "United States") %>%
  summarise(total = sum(doses_administered))




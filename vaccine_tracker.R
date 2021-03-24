library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)

setwd("/Users/conorkelly/Documents/Vaccine-Tracker")

## import existing data cached from previous day
old_data <- read_csv("https://raw.githubusercontent.com/ckelly17/Vaccine-Tracker/main/vaccine_db.csv") %>%
  mutate(date = as.character(date),
         skipped = "No",
         skip_n = 0) %>%
  filter(!is.na(date))

us_only <- old_data %>% filter(long_name %in% "United States") %>% 
  select(series_complete_18plus, everything())

## get new data
return <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")

history <- read_csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations_timeseries_daily.csv") %>%
  clean_names() %>%
  filter(date >= "2021-02-12") %>%
  mutate(date = as.character(date))

new_data <- return[[2]] %>%
  clean_names() %>%
  mutate(date = as.character(ymd(date))) %>%
  distinct(date, location, .keep_all = TRUE) %>%
  mutate(skipped = "No",
         skip_n = 0) # to flag if CDC did not upload for some days

date_cutoff <- ymd(max(new_data$date, na.rm = TRUE))

# for skipped days
yesterday <- new_data %>%
  mutate(date = as.character(as.Date(date) - 1),
         skipped = "Yes",
         skip_n = 1)

# day before
day_before <- new_data %>%
  mutate(date = as.character(as.Date(date) - 2),
         skipped = "Yes",
         skip_n = 2)

# n-3
three_days_ago <- new_data %>%
  mutate(date = as.character(as.Date(date) - 3),
         skipped = "Yes",
         skip_n = 3)

# save a copy
date <- as.character(max(ymd(new_data$date)))
write_csv(new_data, paste0("daily_backup/", date, ".csv"))

## add skipped values to old
last <- old_data %>%
  group_by(long_name) %>%
  mutate(date = ymd(date)) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup()

# n + 1
n1 <- last %>%
  mutate(date = date + 1)

# n +2
n2 <- last %>%
  mutate(date = date + 2)

# n + 3
n3 <- last %>%
  mutate(date = date + 3)

temp <- old_data %>%
  mutate(date = ymd(date)) %>%
  bind_rows(n1, n2, n3) %>%
  filter(date < date_cutoff) %>%
  arrange(desc(date)) %>%
  mutate(date = as.character(date))
  
# bind to old
vaccines_raw <- bind_rows(new_data, temp) %>%
  distinct(date, location, .keep_all = TRUE) %>%
  arrange(date, location) %>%
  group_by(location, date) %>%
  filter(doses_administered == min(doses_administered, na.rm = TRUE)) %>% # to get rid of duplicates for skipped days
  ungroup() %>%
  arrange(desc(date))

## replace series complete as 0 so it stays numeric
# vaccines_raw <- vaccines_raw %>%
#   mutate(series_complete_18plus = ifelse(is.na(series_complete_18plus), 0, series_complete_18plus)) %>%
#   mutate(series_complete_yes = ifelse(is.na(series_complete_yes), 0, series_complete_yes)) %>%
#   mutate(administered_dose1_recip = ifelse(is.na(administered_dose1_recip), 0, administered_dose1_recip)) %>%
#   mutate(series_complete_65plus = ifelse(is.na(series_complete_65plus), 0, series_complete_65plus)) %>%
#   mutate(series_complete_65plus_pop_pct = ifelse(is.na(series_complete_65plus_pop_pct), 0, series_complete_65plus_pop_pct)) %>%
#   mutate(series_complete_18plus_pop_pct = ifelse(is.na(series_complete_18plus_pop_pct), 0, series_complete_18plus_pop_pct))

## write to main repo
write_csv(vaccines_raw, "vaccine_db.csv")

us_only <- vaccines_raw %>% filter(long_name %in% "United States") %>% 
  select(series_complete_18plus, everything())

## clean for viz
vaccines <- vaccines_raw %>%
  rename(state_abb = location,
         state = long_name,
         pop = census2019) %>%
  
  # set up to get new values
  mutate(date = ymd(date)) %>%
  group_by(state) %>%
  mutate(n = row_number()) %>%
  arrange(state, date)

## fill in miss values for dose 2 on 1/14
vaccines <- vaccines %>%
  mutate(administered_dose2 = ifelse(date == "2021-01-14", lag(administered_dose2), administered_dose2))

## fix March 13 total administered
vaccines <- vaccines %>%
  mutate(doses_administered = ifelse(state %in% "United States" & date == "2021-03-13", 
                                     104048005, doses_administered))

# get new values
vaccines <- vaccines %>%
  
  # calculate daily change
  mutate(
         
         # hot fix for 2021-01-11
         doses_distributed = ifelse(date  == "2021-01-09", lag(doses_distributed), doses_distributed),
         doses_distributed = ifelse(date  == "2021-01-10", lag(doses_distributed), doses_distributed),
         doses_administered = ifelse(date  == "2021-01-09", lag(doses_administered), doses_administered),
         doses_administered = ifelse(date  == "2021-01-10", lag(doses_administered), doses_administered),
        
         # all doses
         new_dist = ifelse(n >= 2, doses_distributed - lag(doses_distributed, 1), 0),
         new_admin = ifelse(n >= 2, doses_administered - lag(doses_administered, 1), 0),
         
         new_dist_all = ifelse(n >= 2, doses_distributed - lag(doses_distributed, 1), doses_distributed),
         new_admin_all = ifelse(n >= 2, doses_administered - lag(doses_administered, 1), doses_administered),
         
         # dose 1 and 2
         administered_dose1 = ifelse(is.na(administered_dose1), administered_dose1_recip, administered_dose1),
         administered_dose2 = ifelse(is.na(administered_dose2), administered_dose2_recip, administered_dose2),
         
         new_dose1 = ifelse(n >= 2, administered_dose1 - lag(administered_dose1, 1), 0),
         new_dose2 = ifelse(n >= 2, administered_dose2 - lag(administered_dose2, 1), 0)) %>%
  
  ungroup() %>%
  
  # max_date_ind
  mutate(max_date_ind = ifelse(date == max(date), "Yes", "No"))

## adding in dose 1/2 etc. on 1/12
vaccines <- vaccines %>%
 mutate(unknown_dose = ifelse(date < "2021-01-12", doses_administered, 0))

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

sum(!is.na(vaccines$census2019_18plus_pop))
vaccines %>% 
  filter(!is.na(census2019_18plus_pop)) %>% 
  group_by(state) %>% 
  summarize(pop18 = max(census2019_18plus_pop, na.rm = TRUE))

# fix pop over 18
pop18 <- read_csv("https://raw.githubusercontent.com/ckelly17/Vaccine-Tracker/main/daily_backup/2021-03-07.csv") %>%
  group_by(location) %>%
  summarize(pop18 = max(census2019_18plus_pop, na.rm = TRUE)) %>%
  rename(state_abb = location)

vaccines <- left_join(vaccines, pop18, by = "state_abb")

# hot fix for March 8
march8 <- read_csv("https://raw.githubusercontent.com/ckelly17/Vaccine-Tracker/main/daily_backup/2021-03-08.csv") %>%
  mutate(date = ymd(date),
         series_complete_18plus = as.numeric(series_complete_18plus)) %>%
  rename(state_abb = location) %>%
  select(state_abb, date, series_complete_18plus) %>%
  rename(series_complete_march8 = series_complete_18plus)

# hot fix for March 9
march9 <- read_csv("https://raw.githubusercontent.com/ckelly17/Vaccine-Tracker/main/daily_backup/2021-03-09.csv") %>%
  mutate(date = ymd(date),
         series_complete_18plus = as.numeric(series_complete_18plus)) %>%
  rename(state_abb = location) %>%
  select(state_abb, date, series_complete_18plus) %>%
  rename(series_complete_march9 = series_complete_18plus)

check8 <- inner_join(vaccines, march8, by = c("state_abb", "date"))
check9 <- inner_join(vaccines, march9, by = c("state_abb", "date"))

vaccines <- left_join(vaccines, march8, by = c("state_abb", "date"))
vaccines <- left_join(vaccines, march9, by = c("state_abb", "date"))


march8_check <- vaccines %>%
  filter(date == "2021-03-08") %>%
  filter(state_abb %in% "US") %>%
  select(series_complete_18plus, administered_dose2, administered_dose2_recip, everything())

march9_check <- vaccines %>%
  filter(date == "2021-03-09") %>%
  filter(state_abb %in% "US") %>%
  select(series_complete_18plus, administered_dose2, administered_dose2_recip, everything())

## fill population over 18
vaccines <- vaccines %>%
  group_by(state) %>%
  mutate(census2019_18plus_pop = max(census2019_18plus_pop, na.rm = TRUE)) %>%
  ungroup()

## state rank 
vaccines <- vaccines %>%
  group_by(category, date) %>%
  mutate(doses_rank = rank(desc(doses_administered / pop18)),
         complete_18_rank = rank(desc(series_complete_18plus_pop_pct)),
         complete_65_rank = rank(desc(series_complete_65plus_pop_pct)),
         dose1_18_rank = rank(desc(administered_dose1_recip_18plus_pop_pct)),
         dose1_65_rank = rank(desc(administered_dose1_recip_65plus_pop_pct))) %>%
  
  # fix one missing day for 65+
  mutate(administered_dose1_recip_65plus_pop_pct = ifelse(is.na(administered_dose1_recip_65plus_pop_pct),
                                                          lag(administered_dose1_recip_65plus_pop_pct, n = 1),
                                                          administered_dose1_recip_65plus_pop_pct)) %>%
  ungroup()

## check US
us <- vaccines %>%
  filter(state_abb %in% "US")

# export  
write_csv(vaccines, "vaccine_viz.csv")

##### AGES

json <- fromJSON("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations_demographics.json")
vax_demo <- json[[2]] %>%
  clean_names()

ages <- vax_demo %>%
  select(administered_dose1, administered_dose2, date, demographic_category,
         series_complete_yes) %>%
  filter(demographic_category %in% c("Ages_<18yrs",
                                     "Ages_18-29_yrs",
                                     "Ages_30-39_yrs",
                                     "Ages_40-49_yrs",
                                     "Ages_50-64_yrs",
                                     "Ages_65-74_yrs",
                                     "Ages_75+_yrs")) %>%
  
  rename(age_group = demographic_category) %>%
  
  mutate(age_group_pop = ifelse(age_group %in% "Ages_<18yrs", 81625416, NA),
         age_group_pop = ifelse(age_group %in% "Ages_18-29_yrs", 45141956, age_group_pop),
         age_group_pop = ifelse(age_group %in% "Ages_30-39_yrs", 44168826, age_group_pop),
         age_group_pop = ifelse(age_group %in% "Ages_40-49_yrs", 40319374, age_group_pop),
         age_group_pop = ifelse(age_group %in% "Ages_50-64_yrs", 62925688, age_group_pop),
         age_group_pop = ifelse(age_group %in% "Ages_65-74_yrs", 31483433, age_group_pop),
         age_group_pop = ifelse(age_group %in% "Ages_75+_yrs", 22574830, age_group_pop),
 
         pct_vax1 = administered_dose1 / age_group_pop,
         pct_vax2 = series_complete_yes / age_group_pop)

pct_known <- vax_demo %>%
  filter(demographic_category %in% "Age_known")
pct_known <- max(pct_known$administered_dose1_pct_us)

ages$pct_known <- pct_known

write_csv(ages, "ages_viz.csv")

# by date
vaccines %>% 
  group_by(date) %>% 
  filter(category %in% "United States") %>%
  summarise(total = sum(doses_administered)) %>%
  tail()

nrow(vaccines %>% distinct(state, date)) / nrow(vaccines)


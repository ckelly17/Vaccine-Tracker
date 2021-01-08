library(jsonlite)
library(tidyverse)
library(janitor)

#setwd("/Users/conorkelly/Documents/Vaccine-Tracker")
setwd("C:/Users/ckelly/Documents/Covid-Personal - Copy/Vaccine Tracker/Vaccine-Tracker")

## get archived data from January 5 and 6
j5_ret <- fromJSON(("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/43bbee9e99c534a9236b5fd9fdaf2266cc6a6913/data/cdc_vaccinations.json"))
j6_ret <- fromJSON("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/f46805fff4b765c6f8f82037b8294ffccc2b9e8f/data/cdc_vaccinations.json")

j5 <- j5_ret[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

j6 <- j6_ret[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

## get OWID data at US level
owid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/United%20States.csv") %>%
  select(long_name = location, date, doses_administered = total_vaccinations) %>%
  filter(date < "2021-01-05") %>%
  mutate(date = as.character(date)) %>%
  
  # fill in missing values
  add_row(date = "2020-12-22", long_name = "United States", doses_administered = 614117) %>%
  add_row(date = "2020-12-24", long_name = "United States", doses_administered = 1008025) %>%
  add_row(date = "2020-12-25", long_name = "United States", doses_administered = 1008025) %>%
  add_row(date = "2020-12-27", long_name = "United States", doses_administered = 1944585) %>%
  add_row(date = "2020-12-29", long_name = "United States", doses_administered = 2127143) %>%
  add_row(date = "2020-12-31", long_name = "United States", doses_administered = 2794588) %>%
  add_row(date = "2021-01-01", long_name = "United States", doses_administered = 2794588) %>%
  add_row(date = "2021-01-03", long_name = "United States", doses_administered = 4225756) %>%
  
  arrange(date)

## initial setup of local file - from Jan 7
v_init <- return[[2]] %>%
  clean_names()

v_init <- bind_rows(v_init, j5, j6, owid) %>%
  distinct(date, location, .keep_all = TRUE)

write_csv(v_init, "vaccine_db.csv")




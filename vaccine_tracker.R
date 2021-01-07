library(jsonlite)
library(tidyverse)
library(janitor)

setwd("C:/Users/ckelly/Documents/Covid-Personal - Copy/Vaccine Tracker/Vaccine-Tracker")

## data source from CDC
return <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")

## import existing data
vaccines <- read_csv("vaccine_db.csv") %>%
  mutate(date = as.character(date))

## add new data and write back
return <- fromJSON("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations.json")
new_data <- return[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

vaccines <- bind_rows(vaccines, new_data, j5) %>%
  distinct(date, location, .keep_all = TRUE) 

write_csv(vaccines, "vaccine_db.csv")

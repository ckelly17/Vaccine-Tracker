library(jsonlite)
library(tidyverse)
library(janitor)

setwd("C:/Users/ckelly/Documents/Covid-Personal - Copy/Vaccine Tracker/Vaccine-Tracker")

## get archived data from January 5
j5_ret <- fromJSON(("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/43bbee9e99c534a9236b5fd9fdaf2266cc6a6913/data/cdc_vaccinations.json"))

j5 <- j5_ret[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

## initial setup of local file - from Jan 7
new_data <- return[[2]]
vaccines <- return[[2]] %>%
  clean_names()

vaccines <- bind_rows(vaccines, j5) %>%
  distinct(date, location, .keep_all = TRUE)

write_csv(vaccines, "vaccine_db.csv")
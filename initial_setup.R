library(jsonlite)
library(tidyverse)
library(janitor)

setwd("/Users/conorkelly/Documents/Vaccine-Tracker")

## get archived data from January 5
j5_ret <- fromJSON(("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/43bbee9e99c534a9236b5fd9fdaf2266cc6a6913/data/cdc_vaccinations.json"))
j6_ret <- fromJSON("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/f46805fff4b765c6f8f82037b8294ffccc2b9e8f/data/cdc_vaccinations.json")

j5 <- j5_ret[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

j6 <- j6_ret[[2]] %>%
  clean_names() %>%
  distinct(date, location, .keep_all = TRUE)

## initial setup of local file - from Jan 7
v_init <- return[[2]] %>%
  clean_names()

v_init <- bind_rows(vaccines, j5, j6) %>%
  distinct(date, location, .keep_all = TRUE)

write_csv(v_init, "vaccine_db.csv")




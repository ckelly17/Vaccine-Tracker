library(lubridate)
library(tidyverse)
library(janitor)

url <- "https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations_timeseries.csv"

hist_raw <- read_csv(url,col_types = cols(Administered_Janssen = "d")) %>%
  clean_names()

hist <- hist_raw %>%
  select(location,
         date,
         
         # manufacturer
         administered_janssen) %>%
  
  mutate(date = as.character(date))

us_ctp <- hist %>%
  filter(location %in% "US") # good


# filter so that there's only one record for state and date
hist <- hist %>%
  distinct(location, date, .keep_all = TRUE)

nrow(hist %>% distinct(location, date)) / nrow(hist)


new_cols <- names(hist)
l <- length(new_cols)
new_cols <- new_cols[3:l]  

# drop from vaccines_raw
vaccines_raw <- vaccines_raw %>%
  select(-all_of(new_cols))

# merge in new data
vaccines_raw <- vaccines_raw %>%
  stata_join(hist, keys = c("location", "date"))

## check US
us_raw <- vaccines_raw %>%
  filter(long_name %in% "United States")
         
    


  


library(pdftools)
library(tidyverse)

## read raw data from cvs
cvs_raw <- pdf_text("https://cvshealth.com/sites/default/files/cvs-health-covid-19-vaccination-data.pdf") %>%
  read_lines()

data_date <- as.character(cvs_raw[1])

## clean up
cvs <- cvs_raw[15:67] %>%
  str_squish() %>%
  str_split("Activated") # delineates the state name from the other columns

# result is a list with nested elements for each state

## get data frames for each state -----------------

vax <- tibble() # empty dataframe to append results to

  # loop through the rows of the PDF
  # convert each row to a matrix and transpose it
  # append to dataframe
  # end result has state name and two bulk columns of data separated by spaces
  for (i in 1:53) {
    x <- as.matrix(cvs[[i]]) %>%
      t()
    y <- as_tibble(x)
    vax <- bind_rows(vax, y)
  }

## get column of state names
states_col <- as_tibble(vax$V1)

## isolate total vaccinations
data <- vax %>% select(V2) 
text <- as_tibble(str_split_fixed(data$V2, " ", 8)) # split column into eight with space delimeters
total_vax <- text %>% select(V5) # total vaccinations is the fifth column

## get final dataframe
cvs_vaccines <- bind_cols(states_col, total_vax)
  
## clean up final dataframe
cvs_vaccines <- cvs_vaccines %>%
  
  rename(vaccines_administered = V5,
         entity = value) %>%
  mutate(state = entity,
         entity = str_trim(entity),
         entity = str_replace_all(entity, " ", ""),
         
         vaccines_administered = as.numeric(vaccines_administered),
         
         entity = ifelse(entity %in% "NewYork", "New York", entity),
         entity = ifelse(entity %in% "NorthCarolina", "North Carolina", entity),
         entity = ifelse(entity %in% "SouthCarolina", "South Carolina", entity),
         entity = ifelse(entity %in% "DistrictofColumbia", "District of Columbia", entity),
         entity = ifelse(entity %in% "NewJersey", "New Jersey", entity),
         entity = ifelse(entity %in% "PuertoRico", "Puerto Rico", entity),
         
         date = data_date,
         
         timestamp = timestamp())

write_csv(cvs_vaccines, "daily_backup_cvs/cvs 2020-01-15.csv")


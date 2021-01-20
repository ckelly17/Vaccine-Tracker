library(pdftools)
library(tidyverse)
library(lubridate)

## read raw data from cvs
cvs_raw <- pdf_text("https://cvshealth.com/sites/default/files/cvs-health-covid-19-vaccination-data.pdf")
cvs1 <- str_replace_all(cvs_raw, "1/25", "") %>%
  read_lines()

data_date <- as.character(cvs1[1])

## clean up
cvs <- cvs1[15:67] %>%
  str_squish() %>%
  str_split("2020") # delineates the state name from the other columns

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
    vax <- bind_rows(vax, y) %>%
      select(V1, V2)
  }

## get column of state names
states_col <- as_tibble(vax$V1)

## isolate total vaccinations
data <- vax %>% select(V2) 
text1 <- as_tibble(str_split_fixed(data$V2, "Complete", 7)) # split column into four with space delimeters
text2 <- as_tibble(str_split_fixed(text1$V2, "Activated", n = 10))
text3 <- str_split_fixed(text2$V1, " ", n = 10)
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
         
         date = str_replace(data_date, "Data as of ", ""),
         date = ymd(paste0("2020/", date)),
         
         timestamp = timestamp())

date <- as.character(max(cvs_vaccines$date, na.rm = TRUE))

write_csv(cvs_vaccines, paste0("daily_backup_cvs/cvs", date, ".csv"))


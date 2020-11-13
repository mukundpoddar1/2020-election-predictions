#########################
# Title: COVID-19 Data
# Purpose: Download JHU data from https://github.com/CSSEGISandData/COVID-19 and keep only US data
# Author: Nellie Ponarul
# Last Updated: 11/1/2020
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)

setwd("Documents/2020-election-predictions/")


# FUNCTIONS ----------------------------------------------------------------

# Read in function: reads in csv with all file names as character and cleans field names
# Takes in file path and returns data frame
read_in <- function(path) {
  # Read in csv and rename fields
  tmp <- read_csv(path, col_types = cols(.default = "c"))
  names(tmp) <- str_replace_all(str_replace_all(tolower(names(tmp)), " ", "_"), "\\/", "_")
  # recast name cleanup for long and lat
  if("lat" %in% names(tmp)) {
    tmp <- tmp %>% 
      rename(
        latitude = lat
      )
  }
  
  if("long_" %in% names(tmp)) {
    tmp <- tmp %>% 
      rename(
        longitude = long_
      )
  }
  return(tmp)
}


# Function that downloads latest archive of JHU data, filters for the US data, and aggregates the US data into one dataframe
refresh_jhu <- function() {
  if(!file.exists("data/jhu")) dir.create("data/jhu/")
  if(!file.exists("data/jhu_reports")) dir.create("data/jhu_reports/")
  # Download latest zip file from https://github.com/CSSEGISandData/COVID-19
  download.file("https://github.com/CSSEGISandData/COVID-19/archive/master.zip", "data/jhu/jhu.zip")
  # Unzip latest download
  unzip("data/jhu/jhu.zip", exdir = "data/jhu/files/")
  
  # get list of daily report names
  all_files <- dir("data/jhu/files/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", full.names = TRUE)
  # keep only csvs
  all_files <- all_files[str_detect(all_files, "\\.csv$")]
  
  # # empty the current folder
  unlink("data/jhu_reports")
  # copy files into current folder
  file.copy(all_files, "data/jhu_reports")
  # Remove the latest download
  unlink("data/jhu/", recursive = TRUE)
  
  # load and zip up files into one R object and output
  # Create a list of read-in files
  read_files <- list.files("data/jhu_reports/", full.names = TRUE) %>% 
    set_names(nm = paste0("file_",basename(.) %>% tools::file_path_sans_ext() %>% str_replace_all("-","_"))) %>% map(read_in)
  
  # Remove downlaoded files after aggregation
  unlink("data/jhu_reports/", recursive = TRUE)
  
  ## Limit to only United States
  read_files <- map(read_files, function(x) return(x %>% filter(country_region == "US")))
  
  #Aggregate
  output <- read_files %>% map_df(bind_rows)
  
  #Save out
  saveRDS(output, "data/COVID/all_us_jhu.rds")
}



# CREATE JHU DATA ---------------------------------------------------------------

# Run this line only to update, (takes several minutes to run)
# refresh_jhu()


# LOAD DATA ---------------------------------------------------------------


jhu <- readRDS("data/COVID/all_us_jhu.rds")

#QC - 
count(jhu, country_region) # confirmed all US

count(jhu, province_state) #has state and some more regional info (not county)


# CLEAN FIELDS -------------------------------------------------------------

# A. Dates: Dates are in different formats, so we'll need to standardize and convert to date variable
# all date patterns:
dates <- c(
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{2}:[0-9]{2}",
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{2}:[0-9]{2}",
  "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}",
  "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
)

date_format <- c(
  "%m/%d/%y %H:%M",
  "%m/%d/%Y %H:%M",
  "%Y-%m-%dT%H:%M:%S",
  "%m/%d/%y %H:%M:%S",
  "%Y-%m-%d %H:%M:%S"
)

#Update date (keep as character to avoid numerical conversion)
jhu <- jhu %>% 
  mutate(
    last_update = ifelse(str_detect(last_update, dates[1]), as.character(as.Date(last_update, format = date_format[1])),
                         ifelse(str_detect(last_update, dates[2]), as.character(as.Date(last_update, format = date_format[2])),
                                ifelse(str_detect(last_update, dates[3]), as.character(as.Date(last_update, format = date_format[3])),
                                       ifelse(str_detect(last_update, dates[3]), as.character(as.Date(last_update,format = date_format[4])), last_update))))
  )

# QC: ensure all dates have been successfully reformatted and not converted to NAs before making date variable
stopifnot(jhu %>% filter(!str_detect(last_update, "[0-9]{4}-[0-9]{2}-[0-9]{2}") | is.na(last_update)) %>% nrow() ==0)

# Convert standardized date to date field
jhu$last_update <- as.Date(jhu$last_update)

#QC:
range(jhu$last_update)
# "2020-01-22" "2020-11-01", as expected
count(jhu, lubridate::year(last_update), lubridate::month(last_update)) # makes sense  


# B. Counts

# i. Confirmed
# check for non-numeric values
jhu %>% filter(is.na(confirmed) | str_detect(confirmed, "[^0-9]")) %>% nrow() # none
# Convert
jhu$confirmed <- as.numeric(jhu$confirmed)

# ii. Deaths
# check for non-numeric values
jhu %>% filter(is.na(deaths) | str_detect(deaths, "[^0-9]")) %>% nrow() # 30

jhu %>% filter(is.na(deaths) | str_detect(deaths, "[^0-9]")) %>% count(deaths) # 30 missing, this is fine for conversion

#Convert
jhu$deaths <- as.numeric(jhu$deaths)


# iii. Recovered
# check for non-numeric values
jhu %>% filter(is.na(recovered) | str_detect(recovered, "[^0-9]")) %>% nrow() # 30

jhu %>% filter(is.na(recovered) | str_detect(recovered, "[^0-9]")) %>% count(recovered) # 30 missing, this is fine for conversion

# Convert
jhu$recovered <- as.numeric(jhu$recovered)



# DEDUPE ------------------------------------------------------------------
nrow(jhu) - nrow(jhu %>% distinct()) # 1,057 duplicate rows
# remove duplicate rows in datset
jhu <- jhu %>% distinct()


# SUBSET PROVINCE ---------------------------------------------------------

jhu %>% count(province_state) # we have states and some counties and some cities, and Diamond princess

# A. Identify states
jhu %>% filter(province_state %in% state.name) %>% distinct(province_state) %>% nrow() # confirmed all 50 states

# B. Identify counties
jhu %>% filter(str_detect(province_state, "County")) %>% distinct(province_state) %>% nrow() # 103 counties (there are about 3,000 counties in the US so we don't have a complete dataset here)

# C. Identify city data
jhu %>% filter(str_detect(province_state, "(?!County), [A-Z]{2}$|Chicago")) %>% distinct(province_state) %>% nrow() # 129 cities

# D. Identify Cruise ship
jhu %>% filter(str_detect(province_state, "Diamond Princess|Grand Princess")) %>% distinct(province_state) %>% nrow() # 7 locations

# E. Identify "Recovered" data
jhu %>% filter(province_state == "Recovered") %>% nrow() # 222 reports

# QC: is this the complete set? 
stopifnot(50 + 103+ 129 + 5 + 1 == unique(jhu$province_state)) # no 

# What's left: US territories and DC. Since DC votes and has electors, will include in as a "state"
jhu %>% filter(!province_state %in% state.name & !str_detect(province_state, "County") & !str_detect(province_state, "(?!County), [A-Z]{2}$|Chicago") & !str_detect(province_state, "Diamond Princess|Grand Princess") & province_state != "Recovered" ) %>% distinct(province_state)
# A tibble: 11 x 1
# province_state              
# <chr>                       
#   1 Washington, D.C.            
# 2 District of Columbia        
# 3 Puerto Rico                 
# 4 Virgin Islands, U.S.        
# 5 Guam                        
# 6 Virgin Islands              
# 7 United States Virgin Islands
# 8 US                          
# 9 Wuhan Evacuee               
# 10 American Samoa              
# 11 Northern Mariana Islands 

# Standardize DC
jhu$province_state[jhu$province_state == "Washington, D.C."] <- "District of Columbia"

# QC:
jhu %>% filter(province_state == "District of Columbia") %>% .$last_update %>% range() # "2020-03-08" "2020-11-01", good


jhu %>% filter(is.na(admin2)) %>% .$last_update %>% range()

jhu %>% filter(is.na(admin2)) %>% View()


## Save out a data frame with only states + D.C. 
output <- jhu %>% 
  filter(province_state %in% c(state.name, "District of Columbia")) 

# Check county data
output %>% filter(is.na(admin2)) %>% .$last_update %>% range() # "2020-01-22" "2020-03-21"

# data from 3/22 on for simplicity 
output <- output %>% 
  filter(last_update >= as.Date("2020-03-22"))

# create state level and county level
output_state <- output %>% 
  group_by(province_state, last_update) %>% 
  summarize(
    confirmed = sum(confirmed, na.rm = T),
    deaths = sum(deaths, na.rm = T)
  ) %>% 
  ungroup()

output_county <- output %>% 
  select(province_state, last_update, admin2, confirmed, deaths)

# Check for duplicates
stopifnot(nrow(output_state) == nrow(output_state %>% distinct()))
stopifnot(nrow(output_county) == nrow(output_county %>% distinct()))
output_county %>% group_by_all() %>% summarize(cnt =  n()) %>% filter(cnt > 1) # confirmed in raw data that these are duplicates so we can remove

output_county <- output_county %>% distinct()
stopifnot(nrow(output_state) == nrow(output_state %>% distinct()))
stopifnot(nrow(output_county) == nrow(output_county %>% distinct()))


# Clean up column names
names(output_state) <- c("state", "report_date", "confirmed_cases", "deaths")
names(output_county) <- c("state", "report_date", "county", "confirmed_cases", "deaths")

# OUTPUT ------------------------------------------------------------------

saveRDS(output_state, "data/COVID/state_us_jhu_clean.rds")
saveRDS(output_county, "data/COVID/county_us_jhu_clean.rds")
  


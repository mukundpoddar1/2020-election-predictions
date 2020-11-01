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
  saveRDS(output, "data/all_us_jhu.rds")
}



# CREATE JHU DATA ---------------------------------------------------------------

refresh_jhu()


jhu <- readRDS("data/all_us_jhu.rds")

#QC - 
count(jhu, country_region) # confirmed all US

count(jhu, province_state) #has state and some more regional info (not country)


# CLEAN DATES -------------------------------------------------------------

# all date patterns:
dates <- c(
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{2}:[0-9]{2}",
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{2}:[0-9]{2}",
  "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",
  "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}",
  "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
)

#Update date 
jhu <- jhu %>% 
  mutate(
    last_update = ifelse(str_detect(last_update, dates[1]), as.Date(last_update, format = dates[1]),
                         ifelse(str_detect(last_update, dates[2]), as.Date(last_update, format = dates[2]),
                                ifelse(str_detect(last_update, dates[3]), as.Date(last_update, format = dates[3]),
                                       ifelse(str_detect(last_update, dates[3]), as.Date(last_update,format = dates[4]), last_update))))
  )


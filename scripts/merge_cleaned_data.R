#########################
# Title: BST 260 Project
# Purpose: Merge all cleaned files
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD ALL DATA IN CLEAN DATA FOLDER --------------------------------------

# 1. List all file names in the clean data folder
data_files <- list.files("../data/Clean Data/", recursive = T, full.names = T)

# Keep only csvs
data_files <- data_files[str_detect(data_files, "\\.csv")]

# Determine 2016 and 2020 data files
files_2016 <- c("../data/Clean Data//base_county_state_fips_lkp.csv",
                "../data/Clean Data//campaign_raised_2016.csv", 
                "../data/Clean Data//census_clean_2016.csv", 
                "../data/Clean Data//consumer_spending_2015.csv", 
                "../data/Clean Data//county_gdp_2016.csv", 
                "../data/Clean Data//unemployment_2016.csv" , 
                "../data/Clean Data//saul_cleaned/clean_polls_2016.csv", 
                "../data/Clean Data//saul_cleaned/clean_election_results_2016.csv")

files_2020 <- c("../data/Clean Data//base_county_state_fips_lkp.csv", 
                "../data/Clean Data//campaign_raised_2020.csv" ,
                "../data/Clean Data//census_clean_2019.csv", 
                "../data/Clean Data//county_gdp_2020.csv" ,
                "../data/Clean Data//unemployment_2020.csv" ,
                "../data/Clean Data//saul_cleaned/clean_polls_2020.csv", 
                "../data/Clean Data//election_returns_2020.csv" )


read_2016 <- files_2016 %>% 
  setNames(nm = basename(.)) %>% 
  map(read_csv)

read_2020 <- files_2020 %>% 
  setNames(nm = basename(.)) %>% 
  map(read_csv)

# Re-key state polling files 
chk <- full_join(read_2016[[7]] %>% mutate(state_poll = 1),
read_2016[[1]] %>% mutate(base = 1),
by = c("state"="stname"))
count(chk, state_poll, base)
 # drops out Maine/Nebraska congressional polls

chk <- full_join(read_2020[[6]] %>% mutate(state_poll = 1),
                 read_2020[[1]] %>% mutate(base = 1),
                 by = c("state"="stname"))
count(chk, state_poll, base)

# drops out Maine/Nebraska congressional polls

read_2016[[7]] <- read_2016[[1]] %>% select(fips, stname) %>% inner_join(read_2016[[7]], by = c("stname" = "state")) %>% select(-stname)
read_2020[[6]] <- read_2020[[1]] %>% select(fips, stname) %>% inner_join(read_2020[[6]], by = c("stname" = "state")) %>% select(-stname)

# read_files <- data_files %>% 
#   setNames(nm = basename(.)) %>% 
#   map(read_csv)

# Check names of files
# read_files %>% map(names)

#lowercase all names
read_2016 <- read_2016 %>% 
  map(function(x) setNames(x, tolower(names(x))))

read_2020 <- read_2020 %>% 
  map(function(x) setNames(x, tolower(names(x))))

# read_files <- read_files %>% 
#   map(function(x) setNames(x, tolower(names(x))))

# Keep only files that have FIPS codes
test_key_vars <- function(df) {
  if("fips" %in% tolower(names(df))) {
    return(TRUE)
  }
  return(FALSE)
}

# Convert fips that are numeric
convert_fips <- function(df){
  tmp <- df
  if("fips" %in% names(df)) {
    tmp <- tmp %>% 
      mutate(fips = paste0("0", fips),
             fips = str_sub(fips, -5)
      )
  }
  return(tmp)
}

read_files_fips_2016 <- read_2016 %>% 
  map(function(x) {if(test_key_vars(x) == TRUE) return(x)}) %>% compact()

read_files_fips_2020 <- read_2020 %>% 
  map(function(x) {if(test_key_vars(x) == TRUE) return(x)}) %>% compact()

read_files_fips_2016 <- read_files_fips_2016 %>% map(convert_fips)
read_files_fips_2020 <- read_files_fips_2020 %>% map(convert_fips)

merged_final_2016 <- read_files_fips_2016 %>% 
  reduce(full_join, by = "fips")

merged_final_2020 <- read_files_fips_2020 %>% 
  reduce(full_join, by = "fips")

# read_files_fips <- read_files %>% 
#   map(function(x) {if(test_key_vars(x) == TRUE) return(x)}) %>% compact() # compact() removes NULL list values
# 
# read_files_fips <- read_files_fips %>% 
#   map(convert_fips)
# 
# final <- read_files_fips %>% 
#   reduce(full_join, by = c("fips"))

# OUTPUT ------------------------------------------------------------------

write_csv(merged_final_2016, "../data/merged_final_2016.csv")
write_csv(merged_final_2020, "../data/merged_final_2020.csv")

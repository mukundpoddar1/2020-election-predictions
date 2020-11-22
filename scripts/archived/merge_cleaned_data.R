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

# 2. Read in all of these csvs to one list
read_in_files <- map(data_files, read_csv)

# Look at the names 
read_in_files %>% map(names)

# This function checks if state and county vars are in the data frame and they are the key variables, return TRUE
test_key_vars <- function(df) {
  if("state"%in% names(df) & "county" %in% names(df)) {
    if(sum(str_detect(df$state, "[0-9]{2}")) > 0 & sum(str_detect(df$county, "[0-9]{3}"))) {
      return(TRUE)
    }
    return(FALSE)
  }
  return(FALSE)
}

#3. Which datasets need to be modified? Print out data files that need to be re-keyed
read_in_files %>% 
  map(function(x) {if(test_key_vars(x) == FALSE) print(x)})

# 4. Keep the ready keyed datasets and remove the rest.
read_ready_merge <- read_in_files %>% 
  map(function(x) {if(test_key_vars(x) == TRUE) return(x)}) %>% compact() # compact() removes NULL list values

# 5. Full join all files together. recduce completes this.
final <- read_ready_merge %>% 
  reduce(full_join, by = c("state", "county"))


# OUTPUT ------------------------------------------------------------------
# Output the file
write_csv(final, "../data/Clean Data/merged_final_dataset.csv")

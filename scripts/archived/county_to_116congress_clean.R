#########################
# Title: 260 Project 
# Purpose: Download and process county to 116th congressional district crosswalk 
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# DOWNLOAD DATA -----------------------------------------------------------


cw <- read.table("https://www2.census.gov/geo/relfiles/cdsld18/natl/natl_cocd_delim.txt", skip = 2, colClasses = "character")


cw <- cw %>% 
  separate(V1, into = c("state", "county","congress_district"), sep = ",")


# OUTPUT ------------------------------------------------------------------

write_csv(cw, "../../data/county_to_116th_congress.csv")

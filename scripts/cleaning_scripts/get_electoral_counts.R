#########################
# Title: BST 260 Project
# Purpose: Recreate electoral college counts
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(ggplot2)
library(rvest)




# LOAD BASE FILE ----------------------------------------------------------
base <- read_csv("../../data/Clean Data/base_county_state_fips_lkp.csv")

# SCRAPE ELECTORAL COLLEGE ------------------------------------------------

url <- "https://www.britannica.com/topic/United-States-Electoral-College-Votes-by-State-1787124"

h <- read_html(url)

elect_tbl <- h %>% html_nodes("table") %>% .[[1]] %>% html_table()


# CLEAN -------------------------------------------------------------------
elect_tbl <- elect_tbl[, c(1,2)] %>% bind_rows(elect_tbl[, c(3,4)])
names(elect_tbl) <- c("state", "elect_votes")

# remove missing value 
elect_tbl <- elect_tbl %>% filter(!is.na(elect_votes))

# Which states are missing?
state.name[!state.name %in% elect_tbl$state]
# "Mississippi" "Wyoming"  

# Manually add these in:
elect_tbl <- elect_tbl %>% 
  bind_rows(
    data.frame(
      state = c("Mississippi", "Wyoming"),
      elect_votes = c(6, 3),
      stringsAsFactors = F
    )
  ) %>% arrange(state)

# QC: 
sum(elect_tbl$elect_votes) #538


# Add on fips codes
elect_tbl <- base %>% 
  distinct(state, stname) %>% 
  left_join(
    elect_tbl,
    by = c("stname"= "state")
  )

names(elect_tbl) <- c("fips", "state", "elect_votes")


# Remove the congressional votes from Maine and Nebraska
elect_tbl[elect_tbl$fips %in% c("23", "31"), ]$elect_votes <- 2


# Set up Maine and Nebraska  ----------------------------------------------
maine_neb <- elect_tbl %>% 
  filter(state %in% c("Maine", "Nebraska")) %>% 
  select(fips, state) %>% 
  left_join(
    base,
    by = c("fips"="state")
  )

names(maine_neb) <- c("state_fips", "state", "county", "stname", "ctyname", "fips")

# Manually extracted from ballotpedia.com
# Maine: 
# 1st congressional - Cumberland, Knox, Lincoln, Sagadahoc, York, Kennebec
# 2nd congressional - Androscoggin, Aroostook, Franklin, Hancock, Oxford, Penobscot, Piscataquis, Somerset, Waldo, Washington
maine_neb$congress_district[maine_neb$state_fips == "23"] <- "CD-2"
maine_neb$congress_district[maine_neb$fips %in% c("23005", "23013", "23015", "23023", "23011", "23031")] <- "CD-1"

# Nebraska:
# 1st congressional: Burt, Butler, Cass, Colfax, Cuming, Dodge, Lancaster, Madison, Otoe, Platte, Polk, Saunders, Seward, Stanton, Thurston, Washington, Dixon, Merrick, and Sarpy 
# 2nd congressional: Douglas
# 3rd congressional: Adams, Antelope, Arthur, Banner, Blaine, Boone, Box Butte, Boyd, Brown, Buffalo, Cedar, chase, Cherry, Cheyenne, Clay, Custer, Dakota, Dawes, Dawson, Deuel, Dundy, Fillmore, Franklin, Frontier, Furnas, Gage, Garden, Garfield, Gosper, Grant, Greeley, Hall, Hamilton, Harlan, Hayes, Hitchcock, Holt, Hooker, Howard, Jefferson, Johnson, Kearney, Keith, Keya Paha, Kimball, Knox, Lincoln, Logan, Loup, McPherson, Morrill, Nance, Nemaha, Nuckolls, Pawnee, Perkins, Phelps, Pierce, Red Willow, Richardson, Rock, Saline, Scotts Bluff, Sheridan, Sherman, Sioux, Thayer, Thomas, Valley, Wayne, Webster, Wheeler, York
maine_neb$congress_district[maine_neb$state_fips == "31"] <- "CD-3"
maine_neb$congress_district[maine_neb$fips %in% c("31021", "31023", "31025", "31037", "31039", "31053", "31109", "31119", "31131", "31141", "31143", "31155", "31159", "31167", "31173", "31177", "31051", "31121", "31153")] <- "CD-1"
maine_neb$congress_district[maine_neb$fips == "31055"] <- "CD-2"


## 2020 Actual results - 1 for dem win, 0 for rep win



# OUTPUT ------------------------------------------------------------------


write_csv(elect_tbl %>% distinct(), "../data/electoral_college.csv")
write_csv(maine_neb %>% distinct(), "../data/electoral_votes_main_nebraska.csv")


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
base <- read_csv("../data/Clean Data/base_county_state_fips_lkp.csv")

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



# OUTPUT ------------------------------------------------------------------


write_csv(elect_tbl, "../data/electoral_college.csv")

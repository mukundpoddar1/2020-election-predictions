#########################
# Title: BST 260 Project
# Purpose: Clean up pasted in 2020 elections data from # https://apps.npr.org/elections20-interactive/
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

# Load in 2016 election returns to create consistency metrics
elections_2016 <- read_csv("../../data/Clean Data/saul_cleaned/clean_election_results_2016.csv")

# FUNCTION ----------------------------------------------------------------


# Functionalize the above steps:
reformat_state <- function(df){
  # pull in the reduced table
  tmp <- df[6,1] %>% as.character()
  # Split up the rows
  tmp <- str_split(tmp, pattern = "(?<=[0-9,])(?=[A-Z])") %>% unlist()
  # put the split rows into a dataframe
  rfmt <- data.frame(x = tmp)
  
  # Get the columns
  # 1. county
  rfmt$COUNTY <- str_extract(rfmt$x, "^[A-z ]+")
  # 2.  % of votes in
  rfmt$pct_in <- str_extract(rfmt$x, "(?<=[a-z])[0-9]+\\% in")
  # 3. % for each candidate (one field, to be split after)
  rfmt$pct_trump_biden_other <- str_extract(rfmt$x, "(?<=in)[0-9\\.\\%]+")

  # split up % to candidates
  rfmt <- rfmt %>%
  mutate(
    pct_trump_biden_other = str_extract(pct_trump_biden_other, ".+(?=\\%$)")
  ) %>% 
  separate(pct_trump_biden_other, into = c("BIDEN", "TRUMP", "OTHER"), sep = "\\%", remove = T)
  
  rfmt <-  rfmt %>% 
  mutate_at(vars(BIDEN:OTHER), .funs = function(x) return(as.numeric(x)/100.0))
  return(rfmt)
}



# ON WORKBOOK -------------------------------------------------------------

# Load data
# 1. First get sheet names
sheet_names <- openxlsx::getSheetNames("../../data/Source Data/election_returns_2020_NPR.xlsx")

# 2. read in all sheets
dfs <- sheet_names %>% 
  setNames(nm = .) %>% 
  map(function(x) {readxl::read_excel(path = "../../data/Source Data/election_returns_2020_NPR.xlsx", sheet = x)})

# 3. reformat states as needed (first filter to list that needs to be reformatted)
dfs2 <- dfs %>% map(function(x) if(length(names(x)) == 1) return(reformat_state(x)) else return(x))

# 4. Make the state name a column
dfs2 <- map2(.x = names(dfs2), .y = dfs2, function(.x, .y) {return(.y %>% mutate(state = .x))})

#export results of % won and population for comparison with covid trends
write_csv(dfs2 %>% map_df(bind_rows),"../../data/Clean Data/election_results_2020_population_percent.csv")
          
# 5. row bind and keep state, COUNTY, BIDEN, TRUMP, OTHER
dfs2 <- dfs2 %>% map_df(bind_rows) %>% select(state, COUNTY, BIDEN, TRUMP, OTHER)

names(dfs2) <- tolower(names(dfs2))

# Need to flip Wyoming
wyoming <- dfs2 %>% filter(state == "Wyoming")
names(wyoming) <- c("state", "county", "trump", "biden", "other")

dfs2 <- dfs2 %>% 
  filter(state != "Wyoming") %>% 
  bind_rows(wyoming) %>% 
  arrange(state)

# Add on keys
keys <- read_csv("../../data/Clean Data/base_county_state_fips_lkp.csv")

dfs3 <- dfs2 %>% left_join(
  keys,
  by = c("state" = "stname", "county" = "ctyname"),
  # suffix = c("_remove", "")
)
# Fix any miss matches
count(dfs3, is.na(state.y))

count(dfs3, is.na(county.y))

# both 2

dfs3 %>% filter(is.na(state.y)) # New Mexico, Do - this is Dona Ana county
# state: 35, county: 013, and DC
dfs3$state.y[is.na(dfs3$state.y) & dfs3$state == "New Mexico"] <- "35"
dfs3$county.y[is.na(dfs3$county.y) & dfs3$state == "New Mexico"] <- "013"

dfs3$state.y[is.na(dfs3$state.y) & dfs3$state == "Washington DC"] <- "11"
dfs3$county.y[is.na(dfs3$county.y) & dfs3$state == "Washington DC"] <- "001"

dfs3 <- dfs3 %>% 
  select(state.y, county.y, biden, trump, other) %>% 
  rename(
    state = state.y,
    county = county.y
  )

# Manually append Alaska
# Alaska state level FIPS: 02001
manual <- data.frame(
  state = c("02"),
  county = c("000"),
  biden = c(0.43),
  trump = c(0.531),
  other = c(0.039),
  stringsAsFactors = F
)

dfs3 <- dfs3 %>% 
  bind_rows(manual)

# Concatenate state and fips into fips column
dfs3 <- dfs3 %>% 
  unite(state, county, col = "fips", sep = "")

# Missing 15005 (Hawaii, Kalawao county), smallest county! doesn't seem to have any returns

# Rename fields to match 2016 data
dfs3 <- dfs3 %>% 
  select(-other)

names(dfs3) <- c("Fips", "Democrats.2020", "Republicans.2020")

# add on 2016 data to create consistentcy
dfs3 <- dfs3 %>% 
  left_join(elections_2016 %>% 
              select(Fips, Republicans.2016, Democrats.2016) %>% 
              mutate(
                Fips = str_sub(paste0("0",Fips), -5)
              ), by = c( "Fips")
            )

# Create consistency metric
dfs3 <- dfs3 %>% 
  mutate(
    consistency_dem = Democrats.2020/Democrats.2016,
    consistency_rep = Republicans.2020/Republicans.2016
  ) %>% 
  select(Fips, consistency_dem, consistency_rep, Democrats.2020, Republicans.2020)




# OUTPUT  -----------------------------------------------------------------

write_csv(dfs3 %>% arrange(Fips), "../../data/Clean Data/election_returns_2020.csv")

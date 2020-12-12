#########################
# Title: BST 260 project
# Purpose: Check if we can get electoral college counts from county-level results
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

#election results 2020
elections_2020 <- read_csv("../../data/merged_final_2020.csv")  

# electoral college
elect_tbl <- read_csv("../../data/electoral_college.csv")


# CALCULATE VOTES ---------------------------------------------------------

# 1. If dems get over 50%, indicate 1 
elections_2020$dems_over_50pct <- ifelse(elections_2020$democrats.2020 >= 0.5, 1, 0)

# 2. Group by state and indicator from #1, count up total populations
grp_elections <- elections_2020 %>% 
  mutate(
    state_fips = str_sub(fips, 1,2)
  ) %>% 
  group_by(state_fips, dems_over_50pct) %>% 
  mutate(
    pop_sum = sum(popestimate2019)
  ) %>% 
  ungroup() %>% 
  group_by(state_fips) %>% 
  mutate(
    total_state_pop = sum(popestimate2019)
  ) %>% ungroup() %>% 
  distinct(state_fips,dems_over_50pct, pop_sum, total_state_pop) 

# Make percentages
grp_elections$elect_pct <- grp_elections$pop_sum/grp_elections$total_state_pop

# Get the max percentage by state:
chk <- grp_elections %>% 
  group_by(state_fips) %>% 
  summarize(max_pct = max(elect_pct)) %>% inner_join(grp_elections %>% distinct(state_fips, dems_over_50pct, elect_pct), by = c("max_pct" = "elect_pct", "state_fips")) %>% inner_join(elect_tbl, by = c("state_fips" = "fips"))

chk %>% group_by(dems_over_50pct) %>% summarize(total_votes = sum(elect_votes))  

# A tibble: 2 x 2
# dems_over_50pct total_votes
# <dbl>       <dbl>
# 1               0         210
# 2               1         328
# This approach doesn't work, we should get 306 for democrats

# Multiple % by population, roll-up to state, whichever total is greater wins the electoral votes

elections_2020 %>% 
  mutate(
    pop_dem = democrats.2020*popestimate2019,
    pop_rep = republicans.2020*popestimate2019
  ) %>% 
  select(fips, pop_dem, pop_rep, popestimate2019) %>% 
  mutate(
    state_fips = str_sub(fips, 1,2)
  ) %>% 
  # Remove Maine and Nebraska
  filter(
    !state_fips %in% c("23", "31")
  ) %>% 
  group_by(state_fips) %>% 
  summarize(
    pct_dem = sum(pop_dem, na.rm = T)/sum(popestimate2019),
    pct_rep = sum(pop_rep, na.rm = T)/sum(popestimate2019)
  ) %>% 
  mutate(
    dem_ind = ifelse(pct_dem >= pct_rep, 1, 0)
  ) %>% 
  inner_join(
    elect_tbl,
    by = c("state_fips" = "fips")
  ) %>% 
  group_by(dem_ind) %>% 
  summarize(total_votes = sum(elect_votes))


# A tibble: 2 x 2
# dem_ind total_votes
# <dbl>       <dbl>
# 1       0         227
# 2       1         302

# maine 23, nebraska 31 - we'll need to recode these




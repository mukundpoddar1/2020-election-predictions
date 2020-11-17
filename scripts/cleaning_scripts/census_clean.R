#########################
# Title: COVID-19 Data
# Purpose: Clean county-level and state-level census data
# Author: Nellie Ponarul
# Last Updated: 11/16/2020
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

# Annual Resident Population Estimates, Estimated Components of Resident Population Change, and Rates of the Components of Resident Population Change for States and Counties: April 1, 2010 to July 1, 2019
# Documentation here:
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.pdf

pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")


## Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019
# Documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf

demo <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv")


# Interpret net migration rates: (from https://www.census.gov/programs-surveys/popest/about/glossary.html)
# The difference between the number of migrants entering and those leaving a country in a year, per 1,000 midyear population. May also be expressed in percent. A positive figure is known as a net immigration rate and a negative figure as a net emigration rate.
# SUBSET ------------------------------------------------------------------

### 11/16 - REMOVING ALL STATE LEVEL DATA SETS

### POPULATION
# For 2012, 2016 and 2019 (These are 7/1 estimates)
# - Net migration (domestic and international)
# - Population estimates
# Field names from pop: 
pop_cols <- c("REGION", "DIVISION", "STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE2012", "POPESTIMATE2016", "POPESTIMATE2019", "NETMIG2012", "NETMIG2016", "NETMIG2019")

# # A. State level population data
# pop_state_sub <- pop %>% 
#   filter(SUMLEV == "040") %>% 
#   .[, pop_cols[!pop_cols %in% c("CTYNAME", "COUNTY")]]

# B. County level population data
pop_county_sub <- pop %>% 
  filter(SUMLEV == "050") %>% 
  .[, pop_cols]


### DEMOGRAPHICS 
# Pull race/gender info 
# for race only including white only, black only, and hispanic, and asian/combination (we can include others if we want)
demo_cols <- c("YEAR", "STATE", "COUNTY", "STNAME", "CTYNAME", 
               "TOT_POP", "TOT_MALE", "TOT_FEMALE", "WA_MALE", "WA_FEMALE",
               "BA_MALE", "BA_FEMALE", "H_MALE", "H_FEMALE", "AAC_MALE", "AAC_FEMALE")

## RACE/GENDER

# 5 = 7/1/2012 population estimate
# 9 = 7/1/2016 population estimate
# 12 = 7/1/2019 population estimate

# # A. State level demographic data
# demo_state_sub <- demo %>% 
#   filter(YEAR %in% c(5,9,12) & AGEGRP == 0) %>% 
#   .[, demo_cols] %>% 
#   group_by(YEAR, STATE, STNAME) %>% 
#   summarize_at(vars(TOT_POP:AAC_FEMALE), sum) %>% ungroup()

# B. County level demographic data
demo_county_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP == 0 & SUMLEV == "050") %>% 
  .[, demo_cols]

# Make dataset wide 
# 1. Create a list of three datasets (1 for each year) and rename all columns with that year
demo_county <- map2(.x = list(5, 9, 12), .y = list(2012, 2016, 2019), .f = ~demo_county_sub %>% filter(YEAR == .x) %>% select(-YEAR) %>% setNames(paste0(names(.), "_", .y)))
# 2. Reset first four column names
demo_county <- map(.x = demo_county, .f = ~setNames(.x, nm = str_replace_all(names(.x), "(?<=STATE)_[0-9]{4}|(?<=COUNTY)_[0-9]{4}|(?<=STNAME)_[0-9]{4}|(?<=CTYNAME)_[0-9]{4}", "")))
# 3. Reduce list of data frames to one data frame by full join
demo_county_final <- demo_county %>% reduce(full_join, by = c("STATE", "STNAME", "COUNTY", "CTYNAME"))

### AGEGRP BREAKDOWN  (all at county level)

# # A. State level
# age_state_sub <- demo %>% 
#   filter(YEAR %in% c(5,9,12) & AGEGRP != 0 ) %>% 
#   group_by(STATE, STNAME,YEAR, AGEGRP) %>% 
#   summarize(
#     TOT_POP = sum(TOT_POP),
#     TOT_MALE = sum(TOT_MALE),
#     TOT_FEMALE = sum(TOT_FEMALE)
#   ) %>% ungroup()

# B. County level
age_county_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP != 0 ) %>% 
  group_by(STATE, STNAME, COUNTY, CTYNAME,YEAR, AGEGRP) %>% 
  summarize(
    TOT_POP = sum(TOT_POP),
    TOT_MALE = sum(TOT_MALE),
    TOT_FEMALE = sum(TOT_FEMALE)
  ) %>% ungroup()

# Roll up to 20 year breakdown (as it comes these are in 5 year breakdowns)
# age groups start at AGEGRP 1, 5, 9, 13, 17
age_county_sub <- age_county_sub %>% 
  arrange(YEAR, STATE, COUNTY, AGEGRP) %>% 
  group_by(YEAR, STATE, COUNTY) %>% 
  mutate(
    # Determine new age categories (1-5)
    age_rollup = ifelse(AGEGRP %in% c(1,5,9,13,17), 1, 0), 
    age_rollup = cumsum(age_rollup)
  ) %>% 
  ungroup()


# Recode AGEGRP and age_rollup group

age_county_sub <- age_county_sub %>% 
  mutate(
    age_rollup = case_when(
      age_rollup == 1 ~ "AGE 0 to 19 years",
      age_rollup == 2 ~ "AGE 20 to 39 years",
      age_rollup == 3 ~ "AGE 40 to 59 years",
      age_rollup == 4 ~ "AGE 60 to 79 years",
      age_rollup == 5 ~ "AGE 80 years or older",
      TRUE ~ as.character(age_rollup)
    )
  )


age_county_sub <- age_county_sub %>% 
  mutate(
    AGEGRP = case_when(
      AGEGRP == 1 ~ "AGE 0 to 4 years",
      AGEGRP == 2 ~ "AGE 5 to 9 years",
      AGEGRP == 3 ~ "AGE 10 to 14 years",
      AGEGRP == 4 ~ "AGE 15 to 19 years",
      AGEGRP == 5 ~ "AGE 20 to 24 years",
      AGEGRP == 6 ~ "AGE 25 to 29 years",
      AGEGRP == 7 ~ "AGE 30 to 34 years",
      AGEGRP == 8 ~ "AGE 35 to 39 years",
      AGEGRP == 9 ~ "AGE 40 to 44 years",
      AGEGRP == 10 ~ "AGE 45 to 49 years", 
      AGEGRP == 11 ~ "AGE 50 to 54 years",
      AGEGRP == 12 ~ "AGE 55 to 59 years",
      AGEGRP == 13 ~ "AGE 60 to 64 years",
      AGEGRP == 14 ~ "AGE 65 to 69 years",
      AGEGRP == 15 ~ "AGE 70 to 74 years",
      AGEGRP == 16 ~ "AGE 75 to 79 years",
      AGEGRP == 17 ~ "AGE 80 to 84 years",
      AGEGRP == 18 ~ "AGE 85 years or older",
      TRUE ~ as.character(AGEGRP)
    )
  )

## QC:
# Check that age rollup makes sense
age_county_sub %>% 
  group_by(age_rollup, AGEGRP) %>% 
  summarize(cnt = n()) 

# After checking , reset AGEGRP, aggregate numbers
age_county_sub <- age_county_sub %>% 
  select(-AGEGRP) %>% 
  rename(AGEGRP = age_rollup) %>% 
  group_by(YEAR, STATE, STNAME, COUNTY, CTYNAME, AGEGRP) %>% 
  summarize(TOT_POP = sum(TOT_POP, na.rm = T),
            TOT_MALE = sum(TOT_MALE, na.rm = T),
            TOT_FEMALE = sum(TOT_FEMALE, na.rm = T)) %>% 
  ungroup()


# Make dataset wide 

# 1. Create a list of three datasets (1 for each year) and rename all columns with that year
age_county <- map2(.x = list(5, 9, 12), .y = list(2012, 2016, 2019), .f = ~age_county_sub %>% filter(YEAR == .x) %>% select(-YEAR) %>% setNames(paste0(names(.), "_", .y)))

# 2. Get the age categories to have the years as well
age_county <- map2(.x = age_county, .y = list(2012,2016,2019),
                  .f = ~.x %>% 
                    gather(cat ,value, paste0(c("TOT_POP_", "TOT_MALE_", "TOT_FEMALE_"), .y)) %>% 
                    unite(cat2, paste0("AGEGRP_", .y), cat, remove = T) %>%
                    spread(cat2, value) %>%
                    setNames(str_replace_all(names(.), " ", "_"))
                  )


# 3. Reset first four column names

age_county <- map(.x = age_county, .f = ~setNames(.x, nm = str_replace_all(names(.x), "(?<=STATE)_[0-9]{4}|(?<=COUNTY)_[0-9]{4}|(?<=STNAME)_[0-9]{4}|(?<=CTYNAME)_[0-9]{4}", "")))


# 4. Reduce list of data frames to one data frame by full join

age_county_final <- age_county %>% reduce(full_join, by = c("STATE", "STNAME", "COUNTY", "CTYNAME"))


outputs <- list(
  pop_county_sub, 
  # pop_state_sub,  
  demo_county_sub, 
  # demo_state_sub, 
  age_county_sub, 
  # age_state_sub, 
  demo_county_final, 
  age_county_final
)

# Lower case all names
outputs <- map(outputs,
    function(x) setNames(x, tolower(names(x))))



# OUTPUT ------------------------------------------------------------------

# Ouput a base file of just state and county FIPS information
base <- outputs[[1]] %>% distinct(state, stname, county, ctyname)
write_csv(base, "../../data/Clean Data/base_county_state_fips_lkp.csv")

# remove the ctyname and stname fields 
outputs <- map(outputs,
               function(x) select(x, -stname, -ctyname))

## New outputs are only at county level
write_csv(outputs[[1]], "../../data/Clean Data/demographics/county_populations.csv")
write_csv(outputs[[4]], "../../data/Clean Data/demographics/county_race_gender.csv")
write_csv(outputs[[5]], "../../data/Clean Data/demographics/county_age.csv")


# # Population
# write_csv(outputs[[1]], "../../data/demographics/county_populations.csv")
# write_csv(outputs[[2]], "../../data/demographics/state_populations.csv")
# 
# # Race/gender
# write_csv(outputs[[3]], "../../data/demographics/county_race_gender.csv")
# write_csv(outputs[[4]], "../../data/demographics/state_race_gender.csv")
# 
# # Age
# write_csv(outputs[[5]], "../../data/demographics/county_age.csv")
# write_csv(outputs[[6]], "../../data/demographics/state_age.csv")



#########################
# Title: COVID-19 Data
# Purpose: Clean county-level and state-level census data
# Author: Nellie Ponarul
# Last Updated: 11/12/2020
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


### POPULATION
# For 2012, 2016 and 2019 (These are 7/1 estimates)
# - Net migration (domestic and international)
# - Population estimates
# Field names from pop: 
pop_cols <- c("REGION", "DIVISION", "STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE2012", "POPESTIMATE2016", "POPESTIMATE2019", "NETMIG2012", "NETMIG2016", "NETMIG2019")

# A. State level population data
pop_state_sub <- pop %>% 
  filter(SUMLEV == "040") %>% 
  .[, pop_cols[!pop_cols %in% c("CTYNAME")]]

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

# A. State level demographic data
demo_state_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP == 0) %>% 
  .[, demo_cols] %>% 
  group_by(YEAR, STATE, STNAME) %>% 
  summarize_at(vars(TOT_POP:AAC_FEMALE), sum) %>% ungroup()

# B. County level demographic data
demo_county_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP == 0 & SUMLEV == "050") %>% 
  .[, demo_cols]


### AGEGRP BREAKDOWN  (all at county level)

# A. State level
age_state_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP != 0 ) %>% 
  group_by(STATE, STNAME,YEAR, AGEGRP) %>% 
  summarize(
    TOT_POP = sum(TOT_POP),
    TOT_MALE = sum(TOT_MALE),
    TOT_FEMALE = sum(TOT_FEMALE)
  ) %>% ungroup()

# B. County level
age_county_sub <- demo %>% 
  filter(YEAR %in% c(5,9,12) & AGEGRP != 0 ) %>% 
  group_by(COUNTY, CTYNAME,YEAR, AGEGRP) %>% 
  summarize(
    TOT_POP = sum(TOT_POP),
    TOT_MALE = sum(TOT_MALE),
    TOT_FEMALE = sum(TOT_FEMALE)
  ) %>% ungroup()


# Recode AGEGRP group

age_state_sub <- age_state_sub %>% 
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




# OUTPUT ------------------------------------------------------------------


# Population
saveRDS(pop_county_sub, "~/Documents/2020-election-predictions/data/demographics/county_populations.rds")
saveRDS(pop_state_sub, "~/Documents/2020-election-predictions/data/demographics/state_populations.rds")

# Race/gender
saveRDS(demo_county_sub, "~/Documents/2020-election-predictions/data/demographics/county_race_gender.rds")
saveRDS(demo_state_sub, "~/Documents/2020-election-predictions/data/demographics/state_race_gender.rds")

# Age
saveRDS(age_county_sub, "~/Documents/2020-election-predictions/data/demographics/county_age.rds")
saveRDS(age_state_sub, "~/Documents/2020-election-predictions/data/demographics/state_age.rds")



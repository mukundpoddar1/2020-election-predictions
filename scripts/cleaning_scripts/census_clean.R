#########################
# Title: COVID-19 Data
# Purpose: Clean county-level and state-level census data
# Author: Nellie Ponarul
# Last Updated: 11/20/2020
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

state_codes <- read_csv("../../data/Source Data/state_codes.csv")

# Interpret net migration rates: (from https://www.census.gov/programs-surveys/popest/about/glossary.html)
# The difference between the number of migrants entering and those leaving a country in a year, per 1,000 midyear population. May also be expressed in percent. A positive figure is known as a net immigration rate and a negative figure as a net emigration rate.
# SUBSET ------------------------------------------------------------------

# Base file
# Ouput a base file of just state and county FIPS information
base <- pop %>% distinct(STATE, STNAME, COUNTY, CTYNAME)
## EDITS 11/20:
# Concatenate fips into one field
base$fips <- paste0(base$STATE, base$COUNTY)

names(base) <- tolower(names(base))

# Drop Alaska
base <- base %>% 
  mutate(
    fips = ifelse(stname == "Alaska", "02001", fips)
  )
base <- left_join(base, state_codes, by=c("stname"="state"))

# write_csv(base, "../../data/Clean Data/base_county_state_fips_lkp.csv")


# Field names from pop: 
pop_cols <- c("REGION", "DIVISION", "STATE", "COUNTY", "STNAME", "CTYNAME",  "POPESTIMATE2016", "POPESTIMATE2019", "NETMIG2016", "NETMIG2019")

pop_county_sub <- pop %>% 
  filter(SUMLEV == "050") %>% 
  .[, pop_cols]

# 2016 pop dataset
pop2016 <- pop_county_sub[, c("STATE", "COUNTY", "POPESTIMATE2016","NETMIG2016")]
# 
pop2019 <- pop_county_sub[, c("STATE", "COUNTY", "POPESTIMATE2019","NETMIG2019")]



### DEMOGRAPHICS 
# Pull race/gender info 
# for race only including white only, black only, and hispanic, and asian/combination (we can include others if we want)
demo_cols <- c("STATE", "COUNTY",  "TOT_MALE", "TOT_FEMALE", "WA_MALE", "WA_FEMALE",
               "BA_MALE", "BA_FEMALE", "H_MALE", "H_FEMALE", "AAC_MALE", "AAC_FEMALE")

# B. County level demographic data

# 2016
demo_county_sub_2016 <- demo %>% 
  filter(YEAR %in% c(9) & AGEGRP == 0 & SUMLEV == "050") %>% 
  .[, demo_cols]

# 2019
demo_county_sub_2019 <- demo %>% 
  filter(YEAR %in% c(12) & AGEGRP == 0 & SUMLEV == "050") %>% 
  .[, demo_cols]


### AGEGRP BREAKDOWN  (all at county level)

# A. County level
age_county_sub <- demo %>% 
  filter(YEAR %in% c(9,12) & AGEGRP != 0 ) %>% 
  group_by(STATE, COUNTY, YEAR, AGEGRP) %>% 
  summarize(
    # TOT_POP = sum(TOT_POP), # removing total population
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
  group_by(YEAR, STATE, COUNTY, AGEGRP) %>% 
  summarize(
    # TOT_POP_AGEGRP = sum(TOT_POP, na.rm = T),
            TOT_MALE_AGEGRP = sum(TOT_MALE, na.rm = T),
            TOT_FEMALE_AGEGRP = sum(TOT_FEMALE, na.rm = T)) %>% 
  ungroup()

# Flip wide
age_county_sub <- age_county_sub %>% 
  gather(key, value, c("TOT_MALE_AGEGRP", "TOT_FEMALE_AGEGRP")) %>% 
  unite(AGEGRP, key, col = grp, sep = "_") %>% 
  mutate(
    grp = tolower(str_replace_all(grp, " ", "_"))
  ) %>% 
  spread(grp, value)


# Make separate sets
age_county_2016 <- age_county_sub %>% 
  filter(YEAR == 9) %>% select(-YEAR)

age_county_2019 <- age_county_sub %>% 
  filter(YEAR == 12) %>% select(-YEAR)


## Merge datasets 
# 2016
list_2016 <- list(pop2016, demo_county_sub_2016, age_county_2016)
list_2016 <- list_2016 %>% map(function(x) setNames(x, tolower(names(x))))


final_dem_2016 <- list_2016 %>% 
  reduce(full_join, by = c("state", "county"))
# 2019
list_2019 <- list(pop2019, demo_county_sub_2019, age_county_2019)
list_2019 <- list_2019 %>% map(function(x) setNames(x, tolower(names(x))))


final_dem_2019 <- list_2019 %>% 
  reduce(full_join, by = c("state", "county"))

# Normalize variables
# divide all variables that have female in them by tot_female and same for male
final_dem_2016 <- final_dem_2016 %>% 
  mutate_at(names(final_dem_2016)[str_detect(names(final_dem_2016), "_female") & names(final_dem_2016) != "tot_female"], ~./tot_female)

# divide all variables that have male in them by tot_male 
final_dem_2016 <- final_dem_2016 %>% 
  mutate_at(names(final_dem_2016)[str_detect(names(final_dem_2016), "_male") & names(final_dem_2016) != "tot_male"], ~./tot_male)

# Normalize female and male against population estimate
final_dem_2016 <- final_dem_2016 %>% 
  mutate_at(c("tot_female", "tot_male"), ~./popestimate2016)


# Normalize variables
# divide all variables that have female in them by tot_female and same for male
final_dem_2019 <- final_dem_2019 %>% 
  mutate_at(names(final_dem_2019)[str_detect(names(final_dem_2019), "_female") & names(final_dem_2019) != "tot_female"], ~./tot_female)

# divide all variables that have male in them by tot_male 
final_dem_2019 <- final_dem_2019 %>% 
  mutate_at(names(final_dem_2019)[str_detect(names(final_dem_2019), "_male") & names(final_dem_2019) != "tot_male"], ~./tot_male)

# Normalize female and male against population estimate
final_dem_2019 <- final_dem_2019 %>% 
  mutate_at(c("tot_female", "tot_male"), ~./popestimate2019)



# Merge fips codes
final_dem_2016 <- final_dem_2016 %>% 
  unite(state, county, col = "fips", sep = "")

final_dem_2019 <- final_dem_2019 %>% 
  unite(state, county, col = "fips", sep = "")


# OUTPUT ------------------------------------------------------------------

write_csv(final_dem_2016, "../../data/Clean Data/census_clean_2016.csv")
write_csv(final_dem_2019, "../../data/Clean Data/census_clean_2019.csv")



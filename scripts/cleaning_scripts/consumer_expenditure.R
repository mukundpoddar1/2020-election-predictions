#########################
# The source data was downloaded from https://apps.bea.gov/regional/histdata/releases/1017pce/index.cfm
# original site: https://apps.bea.gov/regional/histdata/ 
# This script changes the data from long to wide format for consumer spending per capita on specific types of goods
# county population data from census.gov (United States link): https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres.xlsx
# Author: Ben Shea
#########################

#original data PCPCE_all.csv

library(tidyverse)
library(readxl)

######DATA SOURCES
#read in table for consumer spending from 2015
consumer_consumption <- read_csv("../../data/Source Data/PCPCE_all.csv")

#read in state name to abbreviation crosswalk
state_abbrev <- read_xlsx("../../data/Source Data/state name to abbrev crosswalk.xlsx")

#read in county/state crosswalk
base_county_state_fips <- read_csv("../../data/Clean Data/base_county_state_fips_lkp.csv")

#read in county population size table and clean the data
county_pop <- read_xlsx("../../data/Source Data/co-est2019-annres.xlsx",skip = 3)
county_pop$...1 <- str_remove(county_pop$...1,".")
county_pop <- county_pop %>% separate(...1, c("County","State"), sep = ", ") %>% slice(2:3143) %>% select(County, State, "2016")
######

######DATA WRANGLING
#get percent of state population for each county
county_pop <- county_pop %>%
  group_by(State) %>%
  mutate(countT= sum(`2016`)) %>%
  group_by(County, add=TRUE) %>%
  mutate(percent=`2016`/countT) %>% select(everything(),-c(`2016`,countT))

consumer_consumption_cleaned <- consumer_consumption %>% select(GeoName, Description, `2015`) %>%
  filter(GeoName %in% state_abbrev$`US State`) %>% spread(Description, `2015`) %>% left_join(state_abbrev,c("GeoName"="US State")) %>%
  select("Abbreviation",everything(),-GeoName)
names(consumer_consumption_cleaned)[1] <- "State"
######

######MERGING

#merges state consumption onto counties
consumer_consumption_county <- base_county_state_fips %>% left_join(consumer_consumption_cleaned, by=c("abbreviation"="State")) %>%
  left_join(county_pop, by=c("ctyname"="County","stname"="State"))

#multiply state consumption by percent of population for each county
list <- names(consumer_consumption_county) %in% c("state","county","stname","ctyname","abbreviation")
consumer_consumption_county[!list] <- consumer_consumption_county[!list] %>% mutate_each(funs(.*percent)) 

#remove percent column 
consumer_consumption_county <- select(consumer_consumption_county, everything(),-percent)
######OUTPUT
head(consumer_consumption_county)
write_csv(consumer_consumption_county,"../../data/Clean Data/consumer_spending_2015.csv")

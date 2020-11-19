#########################
# The source data was downloaded from https://apps.bea.gov/regional/histdata/releases/1017pce/index.cfm
# original site: https://apps.bea.gov/regional/histdata/ 
# This script changes the data from long to wide format for consumer spending per capita on specific types of goods
# Author: Ben Shea
#########################

#original data PCPCE_all.csv

library(tidyverse)

#read in table for consumer spending from 2015
consumer_consumption <- read_csv("../../data/Source Data/PCPCE_all.csv")

#read in state name to abbreviation crosswalk
state_abbrev <- read_xlsx("../../data/Source Data/state name to abbrev crosswalk.xlsx")

consumer_consumption <- read_csv("PCPCE_all.csv")
state_abbrev <- read_excel("state name to abbrev crosswalk.xlsx")

head(consumer_consumption)

consumer_consumption_cleaned <- consumer_consumption %>% select(GeoName, Description, `2015`) %>%
  filter(GeoName %in% state_abbrev$`US State`) %>% spread(Description, `2015`) %>% left_join(state_abbrev,c("GeoName"="US State")) %>%
  select("Abbreviation",everything(),-GeoName)
names(consumer_consumption_cleaned)[1] <- "State"

write.csv(consumer_consumption_cleaned,"../../data/Clean Data/consumer_spending_2015.csv")
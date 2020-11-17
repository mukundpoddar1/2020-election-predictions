library(tidyverse)
library(rvest)
library(stringr)
library(readxl)
#Voting and Registration in the Election of November 2016

# In 2016, from January through August, a total of 8,113 registered voters were asked the following:
#In politics TODAY, do you consider yourself a Republican, Democrat, or independent?

#source data: voting_registration_2016.xlsx

#https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-580.html
#https://www2.census.gov/programs-surveys/cps/tables/p20/580/table04a.xlsx

#read in table for voter registration in 2016
voter_registration <- read_xlsx("../../data/Source Data/voting_registration_2016.xlsx", skip=4)

#only take voter registration
cleaned_voter_registration <- voter_registration[c("...1","Total registered","Percent registered\r\n(Total)")]
names(cleaned_voter_registration)[1] <- "State"

#output
write.csv(cleaned_voter_registration,"voter_registration.csv")


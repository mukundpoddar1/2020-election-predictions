library(tidyverse)
library(dplyr)

# Reading data
df <- read.csv2('data/Source Data/usa-2016-presidential-election-by-county.csv')

# Changing columns to numnerica and divding by 100 to get to 88% to .88
df$Republicans.2016 <- as.numeric(df$Republicans.2016)/100
df$Democrats.2016 <- as.numeric(df$Democrats.2016)/100

df$Republicans.2012 <- as.numeric(df$Republicans.2012) /100
df$Democrats.2012 <- as.numeric(df$Democrats.2012) / 100

df$Republicans.2008 <- as.numeric(df$Republicans.2008)/100
df$Democrats.2008 <- as.numeric(df$Democrats.2008)/100

# Creating consistency variables
df1 <- df %>% 
    mutate(consistency_dem = Democrats.2012 / Democrats.2008) %>%
    mutate(consistency_rep = Republicans.2012 / Republicans.2008)

# Creating data frames
df_election <- df1 %>%
    select(Fips, Republicans.2016, Democrats.2016, Republicans.2012, Democrats.2012, Republicans.2008, Democrats.2008, consistency_dem, consistency_rep)

df_demographics_environment <- df1 %>% 
    select(Fips, Less.Than.High.School.Diploma:Elevation.Bins)

# Writing file 
write.csv(df_election, "data/Clean Data/saul_cleaned/clean_election_results_2016.csv", row.names=FALSE)
write.csv(df_demographics_environment,"data/Clean Data/saul_cleaned/uncertain_demographics_environment_2016.csv", row.names=FALSE)
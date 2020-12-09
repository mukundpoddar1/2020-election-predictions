library(tidyverse)
library(dplyr)

# Reading data
df <- read.csv2('../../data/Source Data/usa-2016-presidential-election-by-county.csv')

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
    #aggregate(Less.Than.High.School.Diploma ~ , FUN=sum)

# Creating data frames
df_election <- df1 %>%
    #filter(State != "Alaska") %>%
    select(State, Fips, Republicans.2016, Democrats.2016, Republicans.2012, Democrats.2012, Republicans.2008, Democrats.2008, consistency_dem, consistency_rep)
    
# Getting Alaska Fips
alaska_fips <- df_election %>% filter(State == "Alaska") %>%
    .$Fips

# Setting boolean values
remove_fips <- alaska_fips != 2240

# Getting fip codes I want to drop
a_fip <- alaska_fips[remove_fips]

# Dropping the codes I dont want to keep
df_election <- df_election %>%
    filter(!Fips %in% a_fip)

# Changing last county fip to 0200 
df_election$Fips[df_election$Fips == 2240] <- paste(0,2000 , sep = '')

## NJP Additions ##################
# Drop rando Virgina county
df_election <- df_election[df_election$Fips != "51515", ] 

# Manually input Alaska data for 2008 and 2016 (pulled from NPR)
df_election[df_election$Fips == "02000",]$Republicans.2016 <-  .529
df_election[df_election$Fips == "02000",]$Democrats.2016 <-  .377
# Pulled from Politico
df_election[df_election$Fips == "02000",]$Republicans.2012 <- .553
df_election[df_election$Fips == "02000",]$Democrats.2012 <- .413
#Pulled from NYTimes
df_election[df_election$Fips == "02000",]$Republicans.2008 <- .598
df_election[df_election$Fips == "02000",]$Democrats.2008 <- .38

df_election[df_election$Fips == "02000",]$consistency_dem <- df_election[df_election$Fips == "02000",]$Democrats.2012/df_election[df_election$Fips == "02000",]$Democrats.2008
df_election[df_election$Fips == "02000",]$consistency_rep <- df_election[df_election$Fips == "02000",]$Republicans.2012/df_election[df_election$Fips == "02000",]$Republicans.2008
####################################


# Subsetting demographic and environmental data
df_demographics_environment <- df1 %>% 
    select(State, Fips, Less.Than.High.School.Diploma:precip)

# Writing file 
write.csv(df_election, "../../data/Clean Data/saul_cleaned/clean_election_results_2016.csv", row.names=FALSE)
write.csv(df_demographics_environment,"../../data/Clean Data/saul_cleaned/uncertain_demographics_environment_2016.csv", row.names=FALSE)
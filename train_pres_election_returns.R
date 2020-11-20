library(dplyr)
library(tidyverse)

# Reading in the data
df_election <- read_csv2("data/Source Data/usa-2016-presidential-election-by-county.csv")

df_train <- read_csv("data/Clean Data/final_pres_2016_dataset.csv")

View(df_train)

df_merged <- merge(df_train, df_election, by.x = "counties_merge", by.y = "County", all.x = TRUE)

length(df_merged$county)
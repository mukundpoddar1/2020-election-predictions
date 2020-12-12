library(tidyverse)
library(tidyr)
library(dplyr)

# Loading in the dataframe
df <- read_csv("../../data/Source Data/president_general_polls_2016.csv")

# Selecting columns, dropping national polls, grouping by state, finding the mean, and renaming
df1 <- df %>%
    select(state, rawpoll_clinton, rawpoll_trump, adjpoll_clinton, adjpoll_trump) %>%
    filter(state != 'U.S.') %>%
    group_by(state) %>%
    summarize_all(mean) %>%
    rename(
        dem_poll_mean = rawpoll_clinton, 
        rep_poll_mean = rawpoll_trump,
        adj_dem_poll_mean = adjpoll_clinton,
        adj_rep_poll_mean = adjpoll_trump 
    )

# Selecting columns, dropping national polls, grouping by state, finding the median, and renaming
df2 <- df %>%
    select(state, rawpoll_clinton, rawpoll_trump, adjpoll_clinton, adjpoll_trump) %>%
    filter(state != 'U.S.') %>%
    group_by(state) %>%
    summarize_all(median) %>%
    rename(
        dem_poll_median = rawpoll_clinton, 
        rep_poll_median = rawpoll_trump,
        adj_dem_poll_median = adjpoll_clinton,
        adj_rep_poll_median = adjpoll_trump 
    )


# Selecting columns, dropping national polls, grouping by state, finding the standard deviation, and renaming
df3 <- df %>%
    select(state, rawpoll_clinton, rawpoll_trump, adjpoll_clinton, adjpoll_trump) %>%
    filter(state != 'U.S.') %>%
    group_by(state) %>%
    summarize_all(sd) %>%
    rename(
        dem_poll_sd = rawpoll_clinton, 
        rep_poll_sd = rawpoll_trump,
        adj_dem_poll_sd = adjpoll_clinton,
        adj_rep_poll_sd = adjpoll_trump 
    )

# Merging two of the dataframes
temp_merge <- merge(df1, df2, by.x = "state", by.y = 'state')

# Merging the merged with the final dataframe
final_df <- merge(temp_merge, df3, by.x = "state", by.y = 'state')

# Writing file to csv
write.csv(final_df, '../../data/Clean Data/saul_cleaned/clean_polls_2016.csv', row.names = FALSE)
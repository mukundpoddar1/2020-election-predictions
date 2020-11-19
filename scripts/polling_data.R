library(tidyverse)
library(tidyr)

# Loading in the dataframe
df <- read_csv("data/Source Data/president_polls.csv")

# Grouping by state and party, taking the mean, median and standard deviation
df_updated_mean <- df %>% group_by(state, candidate_party) %>%
    summarise(pct_mean = mean(pct))

df_updated_median <- df %>% group_by(state, candidate_party) %>%
    summarise(pct_median = median(pct))

df_updated_std <- df %>% group_by(state, candidate_party) %>%
    summarise(pct_sd = sd(pct))


# Merging the calculations together 
df_updated <- merge(df_updated_mean, df_updated_median, by = c('state', 'candidate_party'))
df_updated <- merge(df_updated, df_updated_std, by = c('state', 'candidate_party'))

# Changing each each poll that does not vary with 0s 
df_updated[is.na(df_updated)] <- 0 

# Altering the dataframe to wide 
#df_polls_state <- spread(df_updated, pct_mean, pct_median, pct_sd)
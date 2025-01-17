library(tidyverse)
library(tidyr)

# Loading in the dataframe
df <- read_csv("../../data/Source Data/president_polls2020.csv")

# Grouping by state and party, taking the mean, median and standard deviation
df_updated_mean <- df %>% group_by(state, candidate_party) %>%
    summarise(poll_mean = mean(pct))

df_updated_median <- df %>% group_by(state, candidate_party) %>%
    summarise(poll_median = median(pct))

df_updated_std <- df %>% group_by(state, candidate_party) %>%
    summarise(poll_sd = sd(pct))


# Merging the calculations together
df_updated <- merge(df_updated_mean, df_updated_median, by = c('state', 'candidate_party'))
df_updated <- merge(df_updated, df_updated_std, by = c('state', 'candidate_party'))

# Changing each each poll that does not vary with 0s
df_updated[is.na(df_updated)] <- 0

# Altering the dataframe to long, creating keys column, and altering to wide
df_polls_state <- gather(df_updated, calculation, values, poll_mean:poll_sd, factor_key = TRUE) %>%
    mutate(candidate_party = tolower(candidate_party)) %>%
    mutate(keys = paste(.$candidate_party, .$calculation, sep = '_')) %>%
    subset(select = -c(candidate_party, calculation)) %>%
    spread(keys, values) %>%
    subset(select = c(state, dem_poll_mean, dem_poll_median, dem_poll_sd, rep_poll_mean, rep_poll_median, rep_poll_sd))

# Dropping state == 0, other
df_polls_state <- df_polls_state[!(df_polls_state$state == 0),]

# Writing file
write.csv(df_polls_state, "../../data/Clean Data/clean_polls_2020.csv", row.names = FALSE)

# Reading in main dataset
df_merge <- read_csv("data/Clean Data/merged_final_dataset.csv")

# Adding county state key for later merging
df_merge <- df_merge %>%
    mutate(counties_merge = paste(df_merge$ctyname.x, df_merge$stname.x, sep = ", ")) 

# Merging
df_merged <- merge(df_merge, df_polls_state, by.x = 'stname.x', by.y = 'state', all.x = TRUE )

# Dropping congress districts
df_merged <- df_merged %>%
    select(-c(congress_district.x, congress_district.y))

# Dropping duplicate rows
df_merged <- df_merged %>% distinct()

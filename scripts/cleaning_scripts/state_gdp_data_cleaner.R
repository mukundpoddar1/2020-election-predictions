#########################
# The source data was downloaded from https://apps.urban.org/features/state-economic-monitor/
# This script combines the state GDP values (% changes and raw values) for the 
# election years by taking a weighted average of the previous quarters, 
# assigning more weights to more recent quarters.
# Author: Mukund Poddar
#########################


library(tidyverse)

state_gdp=read.csv('../../data/Source Data/state_gdp_yoy_percent_change.csv') %>% filter(.$Geography != 'United States')
weights=c(1,1,1,1,1,1,1,2,3,4)
state_gdp_2020 = state_gdp %>% mutate('gdp_change'=apply(state_gdp %>% select(c('X2018.Q1','X2018.Q2','X2018.Q3','X2018.Q4','X2019.Q1','X2019.Q2','X2019.Q3','X2019.Q4', 'X2020.Q1', 'X2020.Q2')),
                                                    1,weighted.mean,weights), 'year'=2020)
state_gdp_2016 = state_gdp %>% mutate('gdp_change'=apply(state_gdp %>% select(c('X2014.Q1','X2014.Q2','X2014.Q3','X2014.Q4','X2015.Q1','X2015.Q2','X2015.Q3','X2015.Q4', 'X2016.Q1', 'X2016.Q2')),
                                                    1,weighted.mean,weights), 'year'=2016)
gdp_changes=rbind(state_gdp_2016,state_gdp_2020)[,c('Geography','year','gdp_change')]

state_gdp=read.csv('../../data/Source Data/state_gdp_raw_in_millions.csv') %>% filter(.$Geography != 'United States')
state_gdp_2020 = state_gdp %>% mutate('raw_gdp'=apply(state_gdp %>% select(c('X2018.Q1','X2018.Q2','X2018.Q3','X2018.Q4','X2019.Q1','X2019.Q2','X2019.Q3','X2019.Q4', 'X2020.Q1', 'X2020.Q2')),
                                                    1,weighted.mean,weights), 'year'=2020)
state_gdp_2016 = state_gdp %>% mutate('raw_gdp'=apply(state_gdp %>% select(c('X2014.Q1','X2014.Q2','X2014.Q3','X2014.Q4','X2015.Q1','X2015.Q2','X2015.Q3','X2015.Q4', 'X2016.Q1', 'X2016.Q2')),
                                                    1,weighted.mean,weights), 'year'=2016)
raw_gdp=rbind(state_gdp_2016,state_gdp_2020)[,c('Geography','year','raw_gdp')]

state_gdp=left_join(gdp_changes,raw_gdp,by=c('Geography','year'))

write.csv(state_gdp, file='../../data/Clean Data/state_gdp.csv', sep=",", row.names=FALSE)

#join state_gdp with the county names by state name. Then make a column that multiplies the number with pop/total pop

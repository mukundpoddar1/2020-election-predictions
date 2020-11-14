#########################
# The source data was downloaded from https://apps.urban.org/features/state-economic-monitor/
# This script combines the state GDP values (% changes and raw values) for the 
# election years by taking a weighted average of the previous quarters, 
# assigning more weights to more recent quarters.
# Author: Mukund Poddar
#########################


library(tidyverse)

state_gdp=read.csv('../data/Source Data/state_gdp_yoy_percent_change.csv')
weights=c(1,1,1,1,1,1,1,2,3,4)
state_gdp$change_2020=apply(state_gdp %>% select(c('X2018.Q1','X2018.Q2','X2018.Q3','X2018.Q4','X2019.Q1','X2019.Q2','X2019.Q3','X2019.Q4', 'X2020.Q1', 'X2020.Q2')),1,weighted.mean,weights)
state_gdp$change_2016=apply(state_gdp %>% select(c('X2014.Q1','X2014.Q2','X2014.Q3','X2014.Q4','X2015.Q1','X2015.Q2','X2015.Q3','X2015.Q4', 'X2016.Q1', 'X2016.Q2')),1,weighted.mean,weights)
gdp_changes=state_gdp[,c('Geography','change_2016','change_2020')]

state_gdp=read.csv('../data/Source Data/state_gdp_raw_in_millions.csv')
state_gdp$raw_2020=apply(state_gdp %>% select(c('X2018.Q1','X2018.Q2','X2018.Q3','X2018.Q4','X2019.Q1','X2019.Q2','X2019.Q3','X2019.Q4', 'X2020.Q1', 'X2020.Q2')),1,weighted.mean,weights)
state_gdp$raw_2016=apply(state_gdp %>% select(c('X2014.Q1','X2014.Q2','X2014.Q3','X2014.Q4','X2015.Q1','X2015.Q2','X2015.Q3','X2015.Q4', 'X2016.Q1', 'X2016.Q2')),1,weighted.mean,weights)
state_gdp=left_join(gdp_changes,state_gdp,by='Geography') %>% select(c('Geography','raw_2016','raw_2020','change_2016','change_2020'))

write.csv(state_gdp, file='../data/Clean Data/state_gdp.csv')

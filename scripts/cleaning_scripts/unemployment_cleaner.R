#########################
# The source data was downloaded from https://apps.urban.org/features/state-economic-monitor/
# This script combines the state unemployment rates for the 
# election years by taking a weighted average of the last 18 months, 
# assigning more weights to more recent months.
# Author: Mukund Poddar
#########################

library(tidyverse)

state_unemployment=read.csv('../data/Source Data/unemployment_rate_raw.csv') %>% filter(.$Geography != 'United States')
weights=c(1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4)
first="01"
months=c('01','02','03','04','05','06','07','08','09','10','11','12')
head_20=c()
head_20=c(head_20,paste(paste0('X',"2019"),months[4:12],first, sep="."))
head_20=c(head_20,paste(paste0('X',"2020"),months[1:9],first, sep="."))
head_16=c()
head_16=c(head_16,paste(paste0('X',"2015"),months[4:12],first, sep="."))
head_16=c(head_16,paste(paste0('X',"2016"),months[1:9],first, sep="."))

state_unemployment = state_unemployment %>% mutate('2020'=apply(state_unemployment %>% select(head_20),1,weighted.mean,weights))
state_unemployment = state_unemployment %>% mutate('2016'=apply(state_unemployment %>% select(head_16),1,weighted.mean,weights))
state_unemployment=state_unemployment %>% select(c('Geography','2020','2016')) %>% pivot_longer(!'Geography','Year','Unemployment')

write.csv(state_unemployment,file='../data/Clean Data/state_unemployment.csv', sep=",", row.names=FALSE)

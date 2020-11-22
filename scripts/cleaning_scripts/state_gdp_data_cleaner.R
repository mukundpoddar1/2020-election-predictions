#########################
# The source data was downloaded from https://apps.urban.org/features/state-economic-monitor/
# This script combines the state GDP values (% changes and raw values) for the 
# election years by taking a weighted average of the previous quarters, 
# assigning more weights to more recent quarters.
# It then divides the state GDP into counties taking populations into account appropriately
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

# Currently our data is at the state level but we need it to be at the county level
# So we now divide the raw GDP value among the counties weighted by populations,
# and replicate % change across the counties

fips_codes = read.csv('../../data/Clean Data/base_county_state_fips_lkp.csv') %>% select('stname', 'fips')
county_pop = read.csv('../../data/Clean Data/census_clean_2016.csv') %>% select('fips','popestimate2016') %>% 
                  setNames(c('fips','population')) %>% mutate(year=rep(2016,nrow(.)))
county_pop = rbind(county_pop, read.csv('../../data/Clean Data/census_clean_2019.csv') %>% select('fips','popestimate2019') %>% 
                     setNames(c('fips','population')) %>% mutate(year=rep(2020,nrow(.))))
county_gdp = left_join(state_gdp, fips_codes, by=c('Geography'='stname'))
county_gdp = left_join(county_gdp, county_pop, by = c('fips','year'))

county_gdp = county_gdp %>% group_by(Geography) %>% mutate(raw_gdp=raw_gdp*population/sum(population)) %>% ungroup()

write.csv(county_gdp %>% filter(year==2016) %>% select(fips,raw_gdp,gdp_change), 
          file='../../data/Clean Data/county_gdp_2016.csv', sep=",", row.names=FALSE)
write.csv(county_gdp %>% filter(year==2020) %>% select(fips,raw_gdp,gdp_change), 
          file='../../data/Clean Data/county_gdp_2020.csv', sep=",", row.names=FALSE)


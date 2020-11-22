#########################
# This file uses the APIs from the FEC to pull the campaign finance data.
# More can be read at https://api.open.fec.gov/developers/
# Author: Mukund Poddar
#########################

library(RJSONIO)
library(dplyr)
finance_json=fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?api_key=DEMO_KEY&per_page=100&candidate_id=P00000002&election_year=2020")
dem_finances=data.frame(state=sapply(finance_json$results, function(index) index$contribution_state),
                        dem_amount=sapply(finance_json$results, function(index) index$contribution_receipt_amount))

finance_json=fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?api_key=DEMO_KEY&per_page=100&candidate_id=P00000003&election_year=2020")
rep_finances=data.frame(state=sapply(finance_json$results, function(index) index$contribution_state),
                        rep_amount=sapply(finance_json$results, function(index) index$contribution_receipt_amount))
total_finances=left_join(dem_finances, rep_finances)
total_finances=total_finances %>% mutate(year=2020)
finance_json=fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?api_key=DEMO_KEY&per_page=100&candidate_id=P00000002&election_year=2016")
dem_finances=data.frame(state=sapply(finance_json$results, function(index) index$contribution_state),
                        dem_amount=sapply(finance_json$results, function(index) index$contribution_receipt_amount))

finance_json=fromJSON("https://api.open.fec.gov/v1/presidential/contributions/by_state/?api_key=DEMO_KEY&per_page=100&candidate_id=P00000003&election_year=2016")
rep_finances=data.frame(state=sapply(finance_json$results, function(index) index$contribution_state),
                        rep_amount=sapply(finance_json$results, function(index) index$contribution_receipt_amount))
total_finances= rbind(total_finances, left_join(dem_finances, rep_finances) %>% mutate(year=2016))

# Currently our data is at the state level but we need it to be at the county level
# So we now divide the amount raised among the counties weighted by populations
# This carries with it the assumption that each county donates similarly to the parties
# when controlled for state.

fips_codes = read.csv('../../data/Clean Data/base_county_state_fips_lkp.csv') %>% select('stname', 'fips')
fips_codes = left_join(fips_codes, read_csv("../../data/Source Data/state_codes.csv"), by=c('stname'='state'))
county_pop = read.csv('../../data/Clean Data/census_clean_2016.csv') %>% select('fips','popestimate2016') %>% 
  setNames(c('fips','population')) %>% mutate(year=rep(2016,nrow(.)))
county_pop = rbind(county_pop, read.csv('../../data/Clean Data/census_clean_2019.csv') %>% select('fips','popestimate2019') %>% 
                     setNames(c('fips','population')) %>% mutate(year=rep(2020,nrow(.))))
county_finances = left_join(total_finances, fips_codes, by=c('state'='abbreviation'))
county_finances = left_join(county_finances, county_pop, by = c('fips','year'))

county_finances = county_finances %>% group_by(state) %>% 
                      mutate(dem_amount=dem_amount*population/sum(population),
                             rep_amount=rep_amount*population/sum(population)) %>% ungroup()

write.csv(county_finances %>% filter(year==2016) %>% select(fips,dem_amount,rep_amount), 
          file='../../data/Clean Data/campaign_raised_2016.csv', sep=",", row.names=FALSE)
write.csv(county_finances %>% filter(year==2020) %>% select(fips,dem_amount,rep_amount), 
          file='../../data/Clean Data/campaign_raised_2020.csv', sep=",", row.names=FALSE)

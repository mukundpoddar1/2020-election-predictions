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
write.csv(total_finances, file='../../data/Clean Data/CampaignFinances.csv', sep=",", row.names=FALSE)

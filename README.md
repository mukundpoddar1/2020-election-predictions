# 2020-election-predictions
An attempt to predict the 2020 US Presidential Elections


## Data Sources:  

**Election Returns:**  
1. _usa-2016-presidential-election-by-county.csv_, Election Results and Demographic/Environmental data by county from the 2012 and 2016 Presidential Elections. 

Source: https://public.opendatasoft.com/explore/dataset/usa-2016-presidential-election-by-county/table/?disjunctive.state. 

2. _election_returns_2020.csv_: Election results from the 2020 election scraped from NPR the week of 11/15-11/21. 

Source: https://apps.npr.org/elections20-interactive/


**Voting Registration:**  

**Demographics:**  
1. _census_clean_2016.csv_ and _census_clean_2019_: Population estimates by county and state in 2016 and 2019. Includes net migration rates for 2016 and 2019. Net migration is the net migrants per 1,000 people in the population. Also include race identifying information and age brackets.
  
Sources: U.S. Census Bureau, Population Division: Annual Resident Population Estimates, Estimated Components of Resident Population Change, and Rates of the Components of Resident Population Change for States and Counties: April 1, 2010 to July 1, 2019; U.S. Census Bureau, Population Division: Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019  


Field name | Description  
--- | ---  
fips | Combined state and county FIPS codes
popestimate2016 | 7/1/2016 resident total population estimate
popestimate2019 | 7/1/2019 resident total population estimate
netmig2016 | Net migration in period 7/1/2015 to 6/30/2016
netmig2019 | Net migration in period 7/1/2018 to 6/30/2019    
tot_male | Total male population (normalized using popestimate fields) 
tot_female | Total female population (normalized using popestimate fields) 
wa_male | White alone male population (normalized using tot_male fields)
wa_female | White alone female population (normalized using tot_female fields)
ba_male | Black or African American alone male population (normalized using tot_male fields)
ba_female | Black or African American alone female population (normalized using tot_female fields)
h_male | Hispanic male population (normalized using tot_male fields)
h_female | Hispanic female population (normalized using tot_female fields)
aac_male | Asian alone or in combination male population (normalized using tot_male fields)
aac_female | Asian alone or in combination female population (normalized using tot_female fields)
age_x_to_y_male_agegrp | Number of male population in x to y age group (normalized using tot_male fields)
age_x_to_y_female_agegrp | Number of female population in x to y age group (normalized using tot_female fields)


**Polling Data** 
1. _president_general_polls_2016.csv_, polling data from 2016 polls. Contains raw and adjusted polling data. 
2. _president_polls2020.csv_, polling data from 2020 polls. Contains only raw polling data.

Source: 538's website: (2020) https://projects.fivethirtyeight.com/polls/ , 
(2016) https://projects.fivethirtyeight.com/2016-election-forecast/national-polls/

**COVID-19:**

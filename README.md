# 2020-election-predictions
An attempt to predict the 2020 US Presidential Elections


## Data Sources:  

**Election Returns:**  
1. _election_returns_2012_2016.csv_, _senate_election_returns_2016.csv_, and _senate_election_returns_2018.csv_: Election results by county from the 2012 and 2016 Presidential Elections. 

Source: MIT Electin Data + Science Lab. Harvard Dataverse https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
https://dataverse.harvard.edu/dataverse/medsl_senate

**Voting Registration:**  

**Demographics:**  
1. _county_populations.rds_ and _state_populations.rds_: Population estimates by county and state in 2012, 2016, and 2019. Includes net migration rates for 2012, 2016, and 2019. Net migration is the net migrants per 1,000 people in the population.   
  
Source: U.S. Census Bureau, Population Division: Annual Resident Population Estimates, Estimated Components of Resident Population Change, and Rates of the Components of Resident Population Change for States and Counties: April 1, 2010 to July 1, 2019  


Field name | Description  
--- | ---  
region | Census Region code
division | Census Division code
state | State FIPS code
county | County FIPS code
stname | State name
ctyname | County name
popestimate2012 | 7/1/2012 resident total population estimate
popestimate2016 | 7/1/2016 resident total population estimate
popestimate2019 | 7/1/2019 resident total population estimate
netmig2012 | Net migration in period 7/1/2011 to 6/30/2012
netmig2016 | Net migration in period 7/1/2015 to 6/30/2016
netmig2019 | Net migration in period 7/1/2018 to 6/30/2019    

2. _county_race_gender.rds_ and _state_race_gender.rds_: Population estimates for gender (only male/female), and race by county and state in 2012, 2016, and 2019.
  
Source: U.S. Census Bureau, Population Division: Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019

Field name | Description  
--- | ---  
year | 5 = 2012 9 = 2016, 12 = 2019  
state | State FIPS code
county | County FIPS code
stname | State name
ctyname | County name
tot_pop | Total population
tot_male | Total male population  
tot_female | Total female population
wa_male | White alone male population
wa_female | White alone female population
ba_male | Black or African American alone male population
ba_female | Black or African American alone female population
h_male | Hispanic male population
h_female | Hispanic female population
aac_male | Asian alone or in combination male population
aac_female | Asian alone or in combination female population    

2. _county_age.rds_ and _state_age.rds_: Population estimates for age groups by county and state in 2012, 2016, and 2019.
  
Source: U.S. Census Bureau, Population Division: Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019

Field name | Description  
--- | ---  
state | State FIPS code
stname | State name
county | County FIPS code
ctyname | County name
year | 5 = 2012 9 = 2016, 12 = 2019 
agegrp | Age Category
tot_pop | Total population
tot_male | Total male population 
tot_female | Total female population


**COVID-19:**
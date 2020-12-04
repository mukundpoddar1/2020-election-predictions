# 2020-election-predictions
An attempt to predict the 2020 US Presidential Elections  

## Authors:  

* Ben Shea
* Mukund Poddar
* Nellie Ponarul
* Saul Holding

## Contents
1. [Raw Data Sources](#sources)
2. [Merged Data](#merge)
3. [Final Analysis](#final)


## Raw Data Sources:  <a name = "sources">

**Election Returns:**  
1. Election Results and Demographic/Environmental data by county from the 2012 and 2016 Presidential Elections 
    
    * Cleaned file: _usa-2016-presidential-election-by-county.csv_ 
    * Source: https://public.opendatasoft.com/explore/dataset/usa-2016-presidential-election-by-county/table/?disjunctive.state
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/election_demographic_environment.R)

2. Election results from the 2020 election 

    * Cleaned file: _election_returns_2020.csv_
    * Source: https://apps.npr.org/elections20-interactive/ (scraped the week of 11/15-11/21)
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/cleaning_scripts/election_returns_2020.R)

**Demographics:**  
1. Population estimates, age demographics, race demographics and net migration by county in 2016 and 2019. 

    * Cleaned files: _census_clean_2016.csv_ and _census_clean_2019_: 
    * Sources: 
      * U.S. Census Bureau, Population Division: Annual Resident Population Estimates, Estimated Components of Resident Population Change, and Rates of the Components of Resident Population Change for States and Counties: April 1, 2010 to July 1, 2019
        * https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/
      * U.S. Census Bureau, Population Division: Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019
        * https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/cleaning_scripts/census_clean.R)
  
**Polling Data** 
1. Polling data from 2016 polls. Contains raw and adjusted polling data.  

    * Cleaned file: _president_general_polls_2016.csv_
    * Source: https://projects.fivethirtyeight.com/2016-election-forecast/national-polls/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/polling_data_2016.R)    
    
2. Polling data from 2020 polls. Contains only raw polling data.  

    * Cleaned file: _president_polls2020.csv_
    * Source: https://projects.fivethirtyeight.com/polls/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/polling_2020.R)  

**Electoral College**
1. Electoral College vote breakout by state

    * Cleaned file: _electoral_college.csv_
    * Source: https://www.britannica.com/topic/United-States-Electoral-College-Votes-by-State-1787124
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/get_electoral_counts.R)  
    
2. Maine and Nebraska electoral votes. Vote breakout was obtained by manually mapping counties to congressional districts using descriptions on Ballotpedia

    * Cleaned file: _electoral_votes_main_nebraska.csv_
    * Source: https://ballotpedia.org/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/get_electoral_counts.R)  

**Economic Data**
1. Consumer spending data
    * Cleaned files: _consumer_spending_2016.csv_, _consumer_spending_2020.csv_
    * Source: https://apps.bea.gov/regional/histdata/ 
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/cleaning_scripts/consumer_expenditure.R)

2. GDP
    * Cleaned files: _county_gdp_2016.csv_, _county_gdp_2020.csv_
    * Source: https://apps.urban.org/features/state-economic-monitor/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/cleaning_scripts/state_gdp_data_cleaner.R)

3. Unemployment
    * Cleaned files: _unemployment_2016.csv_, _unemployment_2020.csv_
    * Source: https://apps.urban.org/features/state-economic-monitor/
    * [Data cleaning script](https://apps.urban.org/features/state-economic-monitor/)

4. Campaign Finance
    * Cleaned files: _campaign_raised_2016.csv_, _campaign_raised_2020.csv_
    * Source: https://api.open.fec.gov/developers/
    * [Data cleaning script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/cleaning_scripts/campaign_finance_scraper.R)
    
    
## Merged Data:  <a name = "merge">
We merged all of our cleaned data sources into two files, one for 2016 data and one for 2020 data. For some data sources, we used data from 2019 as a proxy for 2020.
  
  * [Merging script](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/merge_cleaned_data.R)
  
### Final Data Fields  

Field name | Description  
--- | ---  
fips | Combined state and county FIPS codes
dem_amount | Amount raised by Democrat candidate
rep_amount | Amount raised by Republican candidate
popestimate | resident total population estimate in year
netmig | Net migration in year   
race_white | White alone population (normalized using popestimate)
race_black | Black or African American alone population (normalized using popestimate)
race_hispanic | Hispanic population (normalized using popestimate)
race_aac | Asian alone or in combination population (normalized using popestimate)
age_x_to_y_years | Estimated population in x to y age group (normalized using popestimate)  
financial.services.and.insurance | Consumer spending
gasoline.and.other.energy.goods | Consumer spending
health.care | Consumer spending
other.nondurable.goods | Consumer spending
personal.consumption.expenditures | Consumer spending
food | Consumer spending
household | Consumer spending
nonprofit | Consumer spending
nondurable_goods | Consumer spending
durable_goods | Consumer spending
goods_clothing_footwear | Consumer spending
services | Consumer spending
recreation | Consumer spending
transportation | Consumer spending
raw_gdp | GDP per county
gdp_change | GDP change
unemployment | Unemployment by county
dem_poll_mean | Percentage polled for Democrat candidate; mean (by state)
dem_poll_median | Percentage polled for Democrat candidate; median (by state)
dem_poll_sd | Percentage polled for Democrat candidate; standard deviation (by state)
rep_poll_mean | Percentage polled for Republican candidate; mean (by state)
rep_poll_median | Percentage polled for Republican candidate; median (by state)
rep_poll_sd | Percentage polled for Republican candidate; standard deviation (by state)
consistency_dem | Ratio of percentage voted for Democrat candidate in current election to percentage who voted for Democrat candidate in previous election
consistency_rep | Ratio of percentage voted for Republican candidate in current election to percentage who voted for Republican candidate in previous election
dem_rep_ratio | Ratio of percentage voted for Democrat candidate in current election to percentage who voted for Republican candidate in current election **(response variable)**

## Final Analysis: <a name = "final">    
We created predictions of the 2020 election outcome by predicting the ratio of percent democrat votes to percent republican votes for each county and using these to calculated the electoral college returns.  

  * [Modeling Analysis](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/ml_models.Rmd): Here we perform all of our modeling analysis
  * [Exploratory Analysis and Maps](https://github.com/mukundpoddar1/2020-election-predictions/blob/main/scripts/ml_models.Rmd): Here we have all of our exploratoratory data analyses and mapped visualizations of our predictions.

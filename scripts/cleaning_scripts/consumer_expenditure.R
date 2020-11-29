#########################
# The source data was downloaded from https://apps.bea.gov/regional/histdata/releases/1019pce/pce1019.zip
# original site: https://apps.bea.gov/regional/histdata/ 
# This script changes the data from long to wide format for consumer spending per capita on specific types of goods
# county population data from census.gov (United States link): https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres.xlsx
# Author: Ben Shea
#########################

#original data SAEXP1__ALL_AREAS_1997_2018.csv

library(tidyverse)
library(readxl)

######DATA SOURCES
#read in table for consumer spending from 2015
consumer_consumption <- read_csv("../../data/Source Data/SAEXP1__ALL_AREAS_1997_2018.csv")

#read in state name to abbreviation crosswalk
state_abbrev <- read_xlsx("../../data/Source Data/state name to abbrev crosswalk.xlsx")

#read in county/state crosswalk
base_county_state_fips <- read_csv("../../data/Clean Data/base_county_state_fips_lkp.csv")

#read in county population size table and clean the data
county_pop <- read_xlsx("../../data/Source Data/co-est2019-annres.xlsx",skip = 3)
county_pop$...1 <- str_remove(county_pop$...1,".")



consumption_func <- function(county_population, year){
   #DATA WRANGLING
   county_df <- county_population %>% separate(...1, c("County","State"), sep = ", ") %>% slice(2:3143) %>% select(County, State, year) %>%
      rename(population = year)
   
   county_df <- county_df %>%
      group_by(State) %>%
      mutate(countT= sum(population)) %>%
      group_by(County, add=TRUE) %>%
      mutate(percent=population/countT) %>% select(everything(),-c(population,countT))
   
   consumer_consumption_cleaned <- consumer_consumption %>% select(GeoName, Description, year) %>%
      filter(GeoName %in% state_abbrev$`US State`) %>% spread(Description, year) %>% rename(stname=GeoName)
   
   #######MERGING
   
   #merges state consumption onto counties
   consumer_consumption_county <- base_county_state_fips %>% left_join(consumer_consumption_cleaned, by="stname") %>%
      left_join(county_df, by=c("ctyname"="County","stname"="State"))
   
   #multiply state consumption by percent of population for each county
   list <- names(consumer_consumption_county) %in% c("state","county","stname","ctyname","fips")
   consumer_consumption_county[!list] <- consumer_consumption_county[!list] %>% mutate_each(funs(.*percent))
   
   #remove percent column
   consumer_consumption_county <- select(consumer_consumption_county, everything(),-percent)
   consumer_consumption_county = subset(consumer_consumption_county, select = -c(state,county,stname,ctyname))
   
   #edit for Alaska
   alaska_row <- consumer_consumption %>% filter(GeoName=="Alaska") %>% select(GeoName, Description, year) %>%
      spread(Description, year) %>% rename(fips=GeoName)
   alaska_row$fips[alaska_row$fips == "Alaska"] <- "02000"
   
   #remove old 02000 row and add new Alaska row
   consumer_consumption_county<-consumer_consumption_county[!(consumer_consumption_county$fips=="02000"),]
   consumer_consumption_county <- rbind(consumer_consumption_county,alaska_row)
   
   return(consumer_consumption_county)
}


consumer_consumption_county_16 <- consumption_func(county_pop,"2016")
consumer_consumption_county_18 <- consumption_func(county_pop,"2018")
 
#combine columns together in similar groups

combine_columns <- function(df){
   df$Food <- as.numeric(unlist(df["Food and beverages purchased for off-premises consumption"]+
                                   df["Food services and accommodations"]))
   df$Household <- as.numeric(unlist(df["Household consumption expenditures (for services)"]+
      df["Housing and utilities"]+
      df["Furnishings and durable household equipment"]))
   df$Nonprofit <- as.numeric(
      unlist(
         df["Gross output of nonprofit institutions"]+
            df["Less: Receipts from sales of goods and services by nonprofit institutions"]+
            df["Final consumption expenditures of nonprofit institutions serving households (NPISHs)"]
         )
      )
   
   df$nondurable_goods <- as.numeric(
      unlist(
         df["Nondurable goods"]+df["Other nondurable goods"]
      )
   )
   
   df$durable_goods <- as.numeric(
      unlist(
         df["Durable goods"]+df["Other durable goods"]
      )
   )
   
   #combine goods and clothing and footwear because they're highly correlated
   df$goods_clothing_footwear <- as.numeric(
      unlist(
         df["Goods"]+df["Clothing and footwear"]
      )
   )

   df$services <- as.numeric(unlist(df["Services"]+df["Other services"]))
   df$Recreation<- as.numeric(unlist(df["Recreation services"]+df["Recreational goods and vehicles"]))
   df$transportation <- as.numeric(unlist(df["Motor vehicles and parts"]+df["Transportation services"]))
   return(select(df, everything(),-c(`Food and beverages purchased for off-premises consumption`,
                                     `Food services and accommodations`,
                                     `Household consumption expenditures (for services)`,
                                     `Housing and utilities`,`Gross output of nonprofit institutions`,
                                     `Less: Receipts from sales of goods and services by nonprofit institutions`,
                                     `Furnishings and durable household equipment`,
                                     `Gross output of nonprofit institutions`,
                                     `Nondurable goods`, `Other durable goods`,
                                     `Durable goods`,`Other durable goods`,
                                     `Services`,`Other services`,
                                     `Recreation services`,`Recreational goods and vehicles`,
                                     `Final consumption expenditures of nonprofit institutions serving households (NPISHs)`,
                                     `Motor vehicles and parts`,`Transportation services`,`Goods`,`Clothing and footwear`)))
   }


consumption_18 <- combine_columns(consumer_consumption_county_18)
consumption_16 <- combine_columns(consumer_consumption_county_16)

per_capita <- function(year, consumption_df){
county_pop_map <- base_county_state_fips %>% left_join(
   (county_pop %>% separate(...1, c("County","State"), sep = ", ") %>% slice(2:3143) %>% select(County, State,year) %>%
      rename(population = year)), by=c("ctyname"="County","stname"="State")
   ) %>% select(population, fips)
consumption_18_pop <- consumption_df %>% select(fips) %>% left_join(county_pop_map,by="fips")
consumption_18_per_capita <- as.matrix(consumption_df %>% select(everything(),-c(fips))) %>% sweep(2,consumption_18_pop$population,FUN='/')
return(data.frame(consumption_18_per_capita,fips=consumption_df$fips))
}

consumption_16_per_capita <- per_capita("2016",consumption_16)
consumption_18_per_capita <- per_capita("2018",consumption_18)


#correlation matrices
library(corrplot)
 
correlation_18 <- subset(consumption_18_per_capita, select = -c(fips))
correlation_16 <- subset(consumption_16_per_capita, select = -c(fips))

corrplot(cor(correlation_18, use = "pairwise.complete.obs"),method="number")
corrplot(cor(correlation_16, use = "pairwise.complete.obs"),method="number")

#Combine all consumer spending if needed
# consumption_16_final <- data.frame(fips=consumption_16$fips,consumer_exp=
#                                       rowSums(correlation_16))
# 
# consumption_18_final <- data.frame(fips=consumption_16$fips,consumer_exp=
#                                       rowSums(correlation_18))

######OUTPUT
head(consumption_18_per_capita)
head(consumption_16_per_capita)
write_csv(consumption_16_per_capita,"../../data/Clean Data/consumer_spending_2016.csv")
write_csv(consumption_18_per_capita,"../../data/Clean Data/consumer_spending_2018.csv")

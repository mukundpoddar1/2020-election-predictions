#read in table for party affiliation in 2016

library(tidyverse)
library(rvest)
library(stringr)


#2016 Party Identification Detailed Tables

# In 2016, from January through August, a total of 8,113 registered voters were asked the following:
#In politics TODAY, do you consider yourself a Republican, Democrat, or independent?

#source data: 09-13-16-Party-ID-Detailed-Tables.csv

#https://www.pewresearch.org/politics/2016/09/13/2016-party-identification-detailed-tables/

#read party_id csv from website
party_id <- read_csv("https://assets.pewresearch.org/wp-content/uploads/sites/5/2016/09/09-13-16-Party-ID-Detailed-Tables.csv")

head(party_id)

#subset to only region section
region <- party_id[61:72,2:6]
names(region) <- c("Region","Rep","Dem","Ind","DK")
region <- region[which(str_detect(region$Region,"[(]")),]

#split regions into states
states <- unlist(str_split(region$Region, " ")) %>% 
  str_replace_all(c("[(]"="", "[] )]"=""))
states <- states[which(str_length(states)==2)]
states

#break down party id by state
party <- c()
for(i in seq(length(states))){
  party <- rbind(party,region[which(str_detect(region$Region, states[i])),2:5])
}
party

#combine state column with party id distribution
cleaned_df <- data.frame(state=states, party)

#output
write_csv(cleaned_df, "../../data/Clean Data/demographics/state_party_id_2016.csv")

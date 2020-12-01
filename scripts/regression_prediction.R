#########################
# Title: BST 260 Project
# Purpose:Regression prediction attempt - Get a prediction of the 2020 electoral college map using regression on the 2020 results alone
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

# load 2020 data
elections_2020 <- read_csv("../data/merged_final_2020.csv")

# electoral college
elect_tbl <- read_csv("../data/electoral_college.csv")

# FIT MODELS --------------------------------------------------------------

#####################################################################

# LOGISTIC ON 2020 --------------------------------------------------------
# Keep complete cases
elections_2020_model <- na.omit(elections_2020[, c()])

# outcome variable
elections_2020$party_outcome <- ifelse(elections_2020$democrats_pct > elections_2020$republicans_pct, 1, 0)

covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "financial.services.and.insurance", "gasoline.and.other.energy.goods", "health.care", "other.nondurable.goods", "personal.consumption.expenditures", "food", "household", "nonprofit", "nondurable_goods", "durable_goods", "goods_clothing_footwear", "services", "recreation", "transportation",  "republicans_pct", "democrats_pct", "party_outcome")]

# Keep complete cases
elections_2020_model <- na.omit(elections_2020[, c("fips", covs, "party_outcome")])

cov_statement <- paste0(covs, collapse = " + ")

# Logistic model
mod.log <- glm(eval(paste0("party_outcome ~ ", cov_statement)), data = elections_2020_model, family = binomial())

# Use the fitted probabilities from the logistic model as percentages
outcome <- data.frame(fips = elections_2020_model$fips, popestimate = elections_2020_model$popestimate, fitted_pct = fitted(mod.log))

# Generate population outcomes with fitted probabilities
outcome$dems_pop <- outcome$fitted_pct*outcome$popestimate
outcome$reps_pop <- (1-outcome$fitted_pct)*outcome$popestimate

# Roll up to state and match with electoral college.
outcome %>% 
  mutate(
    state_fips = str_sub(fips, 1,2)
  )%>% 
  # Remove Maine and Nebraska
  filter(
    !state_fips %in% c("23", "31")
  ) %>% 
  group_by(state_fips) %>% 
  summarize(
    pct_dem = sum(dems_pop, na.rm = T)/sum(popestimate),
    pct_rep = sum(reps_pop, na.rm = T)/sum(popestimate)
  ) %>% 
  mutate(
    dem_ind = ifelse(pct_dem >= pct_rep, 1, 0)
  ) %>% 
  inner_join(
    elect_tbl,
    by = c("state_fips" = "fips")
  ) %>% 
  group_by(dem_ind) %>% 
  summarize(total_votes = sum(elect_votes))  



#####################################################################

# MLR to predict Democrat vote counts 
# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020")]

cov_statement <- paste0(covs, collapse = " + ")

# Get democrat counts
elections_2020$dems_county_pop <- elections_2020$democrats.2020*elections_2020$popestimate2019

# Plot outcome - normality of observations
elections_2020 %>% 
  ggplot(aes(x = log(dems_county_pop))) +
  geom_histogram()+
  theme_bw()


# fit a linear model
mod_dem <- lm(eval(parse(text = paste0("log(dems_county_pop) ~", cov_statement))), data = elections_2020)
summary(mod_dem)

# MLR to predict Republican vote counts
# Get democrat counts
elections_2020$reps_county_pop <- elections_2020$republicans.2020*elections_2020$popestimate2019

# Plot outcome - normality of observations
elections_2020 %>% 
  ggplot(aes(x = log(reps_county_pop))) +
  geom_histogram()+
  theme_bw()


# fit a linear model
mod_rep <- lm(eval(parse(text = paste0("log(reps_county_pop) ~", cov_statement))), data = elections_2020)
summary(mod_rep)


# CALCULATE ELECTORAL VOTES -----------------------------------------------

# Get predicted counts
preds <- na.omit(elections_2020) %>% 
  select(fips, dems_county_pop, reps_county_pop, popestimate2019) %>% 
  bind_cols(data.frame(pred_dem = exp(fitted(mod_dem)), pred_rep = exp(fitted(mod_rep))))


# Calculate electoral votes for both actual and predictions
preds %>% 
  mutate(
    state_fips = str_sub(fips, 1,2)
  ) %>% 
  # Remove Maine and Nebraska
  filter(
    !state_fips %in% c("23", "31")
  ) %>% 
  group_by(state_fips) %>% 
  summarize(
    pct_dem_pred = sum(pred_dem, na.rm = T)/sum(popestimate2019),
    actual_dem = sum(dems_county_pop, na.rm = T)/sum(popestimate2019),
    pct_rep_pred = sum(pred_rep, na.rm = T)/sum(popestimate2019),
    actual_rep = sum(reps_county_pop, na.rm = T)/sum(popestimate2019)
  ) %>% 
  mutate(
    dem_ind_actual = ifelse(actual_dem > actual_rep, 1, 0),
    dem_ind_pred = ifelse(pct_dem_pred > pct_rep_pred, 1, 0)
  ) %>% 
  inner_join(
    elect_tbl,
    by = c("state_fips" = "fips")
  ) %>% 
  group_by(dem_ind_pred) %>% 
  summarize(total_votes = sum(elect_votes)) # off by 30 votes (30/538 is about 5.5%)


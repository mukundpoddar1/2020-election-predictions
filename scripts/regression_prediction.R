#########################
# Title: BST 260 Project
# Purpose:Regression prediction attempt - Get a prediction of the 2020 electoral college map using regression on the 2020 results alone
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(caret)

# LOAD DATA ---------------------------------------------------------------

# load 2016 data
elections_2016 <- read_csv("../data/merged_final_2016.csv")

# load 2020 data
elections_2020 <- read_csv("../data/merged_final_2020.csv")

# electoral college
elect_tbl <- read_csv("../data/electoral_college.csv")
maine_neb <- read_csv("../data/electoral_votes_main_nebraska.csv")

# FIT MODELS --------------------------------------------------------------

#####################################################################

# LOGISTIC ON 2020 --------------------------------------------------------
# Keep complete cases

# outcome variable
elections_2016$party_outcome <- ifelse(elections_2016$democrats_pct > elections_2016$republicans_pct, 1, 0)
elections_2020$party_outcome <- ifelse(elections_2020$democrats_pct > elections_2020$republicans_pct, 1, 0)


train = elections_2016 %>% select(-democrats_pct, -republicans_pct)
test = elections_2020 %>% select(-democrats_pct, -republicans_pct)
train = train[complete.cases(train),]
x_train = train[c(2:(length(train)-1))]
y_train = train[length(train)]
colnames(y_train) = 'actual'


test = test[complete.cases(test),]
x_test = test[c(2:(length(test)-1))]
y_test = test[length(test)]
colnames(y_test) = 'actual'

x_train = scale(x_train) %>% data.frame() %>% bind_cols(y_train)
x_test = scale(x_test) %>% data.frame() %>% bind_cols(y_test)

# Train the model
covs <- names(x_train)[names(x_train) != "actual"]
covs <- paste0(covs, collapse = " + ")

mod.log <- glm(eval(paste0("actual ~ ", covs)), data = x_train, family = binomial)

# Perform backwards selection
# mod.log_back <- step(mod.log, direction = "back")
# summary(mod.log_back)
# Result of backwards step
mod.log_back <- glm(formula = actual ~ dem_amount + rep_amount + popestimate + 
                      netmig + race_white + race_black + race_hispanic + race_aac + 
                      age_0_to_19_years + age_20_to_39_years + age_40_to_59_years + 
                      age_60_to_79_years + financial.services.and.insurance + transportation + 
                      gdp_change + unemployment + dem_poll_mean + dem_poll_median + 
                      rep_poll_mean + rep_poll_sd + consistency_dem + consistency_rep, 
                    family = binomial, data = x_train)

# Predict 2020
x_test$pred <- predict(mod.log_back, x_test, type = "response")

#MSE
mean((x_test$pred - x_test$actual) ^2) # 0.09476399

# Plot
x_test %>% 
  ggplot(aes(x = actual, y = pred, group = actual)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,1, by =  .1)) +
  theme_bw() 

x_test %>% 
  ggplot(aes(x = na.omit(elections_2020)$democrats_pct, y = pred)) +
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = F)+
  xlab("Actual % Democrat") +
  ylab("Predicted % Democrat")+
  theme_bw()

# CALCULATE ELECTORAL COLLEGE --------------------------------------------

# Use the fitted probabilities from the logistic model as percentages
outcome <- data.frame(fips = na.omit(elections_2020)$fips, popestimate = x_test$popestimate, fitted_pct = round(x_test$pred,3))
# outcome$fitted_pct <- round(ifelse(outcome$fitted_pct > 1, 1, outcome$fitted_pct),4)
# Generate population outcomes with fitted probabilities
outcome$dems_pop <- outcome$fitted_pct*outcome$popestimate
outcome$reps_pop <- (1-outcome$fitted_pct)*outcome$popestimate

# Roll up to state and match with electoral college.
outcome %>%
  mutate(
    state_fips = str_sub(fips, 1,2)
  )%>%
  # Remove Max`ine and Nebraska
  # filter(
  #   !state_fips %in% c("23", "31")
  # ) %>%
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

# A tibble: 2 x 2
# dem_ind total_votes
# <dbl>       <dbl>
#   1       0         188
#   2       1         351

# Maine and Nebraska Congressional votes

outcome %>% 
  mutate(
    state_fips = str_sub(fips, 1,2)
  )%>%
  # Maine and Nebraska
  filter(
    state_fips %in% c("23", "31")
  ) %>%
  left_join(
    maine_neb %>% mutate(fips = str_sub(paste0("0", fips), -5), state_fips = as.character(state_fips)),
    by = c("fips", "state_fips")
  ) %>% 
  group_by(state_fips,congress_district) %>%
  summarize(
    pct_dem = sum(dems_pop, na.rm = T)/sum(popestimate),
    pct_rep = sum(reps_pop, na.rm = T)/sum(popestimate)
  ) %>%
  mutate(
    dem_ind = ifelse(pct_dem >= pct_rep, 1, 0)
  )

# A tibble: 5 x 5
# Groups:   state_fips [2]
# state_fips congress_district pct_dem pct_rep dem_ind
# <chr>      <chr>               <dbl>   <dbl>   <dbl>
# 1 23         CD-1               0.149    0.851       0
# 2 23         CD-2               0.0226   0.977       0
# 3 31         CD-1              -0.0353   1.04        0
# 4 31         CD-2               0.389    0.611       0
# 5 31         CD-3               0.0246   0.975       0
# covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "financial.services.and.insurance", "gasoline.and.other.energy.goods", "health.care", "other.nondurable.goods", "personal.consumption.expenditures", "food", "household", "nonprofit", "nondurable_goods", "durable_goods", "goods_clothing_footwear", "services", "recreation", "transportation",  "republicans_pct", "democrats_pct", "party_outcome")]
# 
# # Keep complete cases
# elections_2016_model <- na.omit(elections_2016[, c("fips", covs, "party_outcome", "democrats_pct", "republicans_pct")])
# elections_2020_model <- na.omit(elections_2020[, c("fips", covs, "party_outcome", "democrats_pct", "republicans_pct")])
# 
# # Split training data
# train <- createDataPartition(y = elections_2016_model$party_outcome, p = .6, list = F)
# training <- elections_2016_model[train, ]
# testing <- elections_2016_model[-train, ]
# 
# 
# # Logistic model
# cov_statement <- paste0(covs, collapse = " + ")
# mod.log <- glm(eval(paste0("party_outcome ~ ", cov_statement)), data = training, family = binomial())
# summary(mod.log)
# 
# # Test
# testing$predicted <- predict(mod.log, testing, type = "response")
# # not sure how to interpret these predictions
# testing$predicted_out <- ifelse(testing$predicted > 0.6, 1, 0)
# count(testing, party_outcome, predicted_out)
# # A tibble: 4 x 3
# # party_outcome predicted_out     n
# # <dbl>         <dbl> <int>
# # 1             0             0  1016
# # 2             0             1    26
# # 3             1             0    62
# # 4             1             1   140
# 
# # Check on 2016 on 2020 - model prediction is way off
# elections_2020_model$prediction <- predict(mod.log, elections_2020_model, type = "response")
# 
# 
# # elections_2020_model$prediction <- ifelse(elections_2020_model$prediction > 0.5, 1, 0)
# # count(elections_2020_model, party_outcome, prediction)
# 




# #####################################################################
# 
# # MLR to predict Democrat vote counts 
# # Get list of covariates
# covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020")]
# 
# cov_statement <- paste0(covs, collapse = " + ")
# 
# # Get democrat counts
# elections_2020$dems_county_pop <- elections_2020$democrats.2020*elections_2020$popestimate2019
# 
# # Plot outcome - normality of observations
# elections_2020 %>% 
#   ggplot(aes(x = log(dems_county_pop))) +
#   geom_histogram()+
#   theme_bw()
# 
# 
# # fit a linear model
# mod_dem <- lm(eval(parse(text = paste0("log(dems_county_pop) ~", cov_statement))), data = elections_2020)
# summary(mod_dem)
# 
# # MLR to predict Republican vote counts
# # Get democrat counts
# elections_2020$reps_county_pop <- elections_2020$republicans.2020*elections_2020$popestimate2019
# 
# # Plot outcome - normality of observations
# elections_2020 %>% 
#   ggplot(aes(x = log(reps_county_pop))) +
#   geom_histogram()+
#   theme_bw()
# 
# 
# # fit a linear model
# mod_rep <- lm(eval(parse(text = paste0("log(reps_county_pop) ~", cov_statement))), data = elections_2020)
# summary(mod_rep)
# 
# 
# # CALCULATE ELECTORAL VOTES -----------------------------------------------
# 
# # Get predicted counts
# preds <- na.omit(elections_2020) %>% 
#   select(fips, dems_county_pop, reps_county_pop, popestimate2019) %>% 
#   bind_cols(data.frame(pred_dem = exp(fitted(mod_dem)), pred_rep = exp(fitted(mod_rep))))
# 
# 
# # Calculate electoral votes for both actual and predictions
# preds %>% 
#   mutate(
#     state_fips = str_sub(fips, 1,2)
#   ) %>% 
#   # Remove Maine and Nebraska
#   filter(
#     !state_fips %in% c("23", "31")
#   ) %>% 
#   group_by(state_fips) %>% 
#   summarize(
#     pct_dem_pred = sum(pred_dem, na.rm = T)/sum(popestimate2019),
#     actual_dem = sum(dems_county_pop, na.rm = T)/sum(popestimate2019),
#     pct_rep_pred = sum(pred_rep, na.rm = T)/sum(popestimate2019),
#     actual_rep = sum(reps_county_pop, na.rm = T)/sum(popestimate2019)
#   ) %>% 
#   mutate(
#     dem_ind_actual = ifelse(actual_dem > actual_rep, 1, 0),
#     dem_ind_pred = ifelse(pct_dem_pred > pct_rep_pred, 1, 0)
#   ) %>% 
#   inner_join(
#     elect_tbl,
#     by = c("state_fips" = "fips")
#   ) %>% 
#   group_by(dem_ind_pred) %>% 
#   summarize(total_votes = sum(elect_votes)) # off by 30 votes (30/538 is about 5.5%)


#########################
# Title: BST 260 Project
# Purpose: XgBoost prediction attempt - Get a prediction of the 2020 electoral college map using XGBoost on the 2016 results alone
#########################

# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(xgboost)
library(Metrics)


# LOAD DATA ---------------------------------------------------------------

# load 2020 data
train <- read_csv("../data/merged_final_2016.csv")
dim(train)
train_complete <- train[complete.cases(train), ]
dim(train_complete)

# load 2020 data
test <- read_csv("../data/merged_final_2020.csv")
dim(test)
test_complete <- test[complete.cases(test), ]
dim(test_complete)

#read in county/state crosswalk
base_county_state_fips <- read_csv("../data/Clean Data/base_county_state_fips_lkp.csv")

# electoral college
elect_tbl <- read_csv("../data/electoral_college.csv")
maine_neb <- read_csv("../data/electoral_votes_main_nebraska.csv")

# FIT MODELS --------------------------------------------------------------

#remove outcome variables and fips code
train.boost <- as.matrix(train_complete[,c(4:(length(train_complete)-2))])
test.boost <- as.matrix(test_complete[c(4:(length(train_complete)-2))])

x_train = scale(train.boost)
x_test = scale(test.boost)

train_final <- matrix(as.numeric(data.matrix(x_train)), nrow = nrow(train_complete))
test_final <- matrix(as.numeric(data.matrix(x_test)), nrow = nrow(test_complete))
party_outcome <- ifelse(train_complete$democrats_pct>train_complete$republicans_pct,1,0)
#party_outcome <- train_complete$democrats_pct
outcome <- matrix(as.numeric(data.matrix(party_outcome)))



# GRID SEARCH -------------------------------------------------------------------------------
searchGridSubCol <- expand.grid(subsample = c(0.5,0.7,1), 
                                colsample_bytree = c(0.5,0.7,1),
                                max_depth = c(3, 4,5,6),
                                min_child = seq(1), 
                                eta = c(0.1,0.2,0.3,0.4)
)

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgb_model <- xgboost(data =  train_final, label=outcome, nrounds = 100, 
                         verbose = 1,"max.depth" = currentDepth, "eta" = currentEta,                               
                         "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                         , print_every_n = 20, "min_child_weight" = currentMinChild,
                         early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgb_model$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    print(rmse)
    print(trmse)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))

output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "currentMinChild","eta")
names(output) <- varnames
head(output)
# -----------------------------------------------------------------------------------------------

#https://xgboost.readthedocs.io/en/latest/parameter.html
xgb_model = xgboost(data=train_final, 
                    label=outcome, 
                    missing = NaN,
                    nrounds=1000,
                    verbosity=1, 
                    eta=0.3, 
                    max_depth=6, 
                    subsample=1, 
                    min_child=1,
                    colsample_bytree=1,
                    objective="reg:squarederror", #binary:logistic 
                    eval_metric="rmse"
                    )

train_preds <- predict(xgb_model, train_final, missing = NaN)
rmse(train_complete$democrats_pct,train_preds)
plot(train_complete$democrats_pct,train_preds)

test_preds <- predict(xgb_model, test_final, missing = NaN)
rmse(test_complete$democrats_pct,test_preds)
plot(test_complete$democrats_pct,test_preds)

# roll up to State level ----------------------------------------


test_pred_df <- data.frame(fips = test_complete$fips, percent_dem =test_preds, percent_rep = 1-test_preds, 
                           pop_estimate = test_complete$popestimate) %>% mutate(dem_vote = percent_dem * pop_estimate, 
                                                                                rep_vote = percent_rep * pop_estimate) %>%
  left_join(base_county_state_fips[c("stname","fips")], by="fips")

#most of electoral college
elect_rollup <- test_pred_df %>% left_join(elect_tbl, by = c("stname" ="state")) %>% 
  group_by(stname) %>% summarize(total_dem_vote = sum(dem_vote),
                                total_rep_vote = sum(rep_vote)) %>%
  mutate(state_win=ifelse(total_dem_vote>total_rep_vote,1,0))

final_elect_rollup <- elect_rollup %>% 
  left_join(elect_tbl %>% 
              select(state, elect_votes), by=c("stname" ="state")) %>%
  mutate(dem_electoral_votes = state_win*elect_votes,rep_electoral_votes = elect_votes - dem_electoral_votes)

#maine and nebraska
maine_neb$fips <- as.character(maine_neb$fips)
elect_rollup_mn <- maine_neb %>% left_join(test_pred_df, by = "fips")
elect_rollup_mn <- elect_rollup_mn[complete.cases(elect_rollup_mn), ] %>% 
  group_by(congress_district) %>% summarize(total_dem_vote = sum(dem_vote),
                                 total_rep_vote = sum(rep_vote)) %>%
  mutate(state_win=ifelse(as.numeric(total_dem_vote)>as.numeric(total_rep_vote),1,0))

final_elect_rollup_mn <- elect_rollup_mn %>% 
  mutate(dem_electoral_votes = state_win,rep_electoral_votes = 1-dem_electoral_votes)

final_elect_rollup %>% summarise(total_dem_elect_votes = sum(dem_electoral_votes), total_rep_elect_votes = sum(rep_electoral_votes))
final_elect_rollup_mn
# -----------------------------------------------------------------

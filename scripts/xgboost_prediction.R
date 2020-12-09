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
train.boost <- as.matrix(train_complete[,c(4:(length(train_complete)-1))])
test.boost <- as.matrix(test_complete[c(4:(length(train_complete)-1))])

x_train = scale(train.boost)
x_test = scale(test.boost)

train_final <- matrix(as.numeric(data.matrix(x_train)), nrow = nrow(train_complete))
test_final <- matrix(as.numeric(data.matrix(x_test)), nrow = nrow(test_complete))
#train_party_outcome <- ifelse(train_complete$democrats_pct>train_complete$republicans_pct,1,0)
train_party_outcome <- train_complete$dem_rep_ratio
train_outcome <- matrix(as.numeric(data.matrix(train_party_outcome)))



# GRID SEARCH -------------------------------------------------------------------------------
#https://www.kaggle.com/silverstone1903/xgboost-grid-search-r

searchGridSubCol <- expand.grid(subsample = c(0.5,0.7,1), 
                                colsample_bytree = c(0.5,0.7,1),
                                max_depth = c(3,4,5,6),
                                eta = c(0.1,0.2,0.3,0.4)
)

obj<- "reg:squarederror"

weightsData <- scales::rescale(train_complete$popestimate, to=c(0,1))

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- 1
    xgb_model <- xgboost(data =  train_final, label=train_outcome, nrounds = 100, 
                         verbose = 0,"max.depth" = currentDepth, "eta" = currentEta,                               
                         "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                         , print_every_n = 20, "min_child_weight" = currentMinChild,
                         early_stopping_rounds = 10,objective=obj, eval_metric="rmse", #weights = weightsData
                         )
    
    xvalidationScores <- as.data.frame(xgb_model$evaluation_log)
    trmse <- tail(xvalidationScores$train_rmse,1)
    output <- return(c(trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))

output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TrainRMSE", "SubSampRate", "ColSampRate", "Depth","eta","currentMinChild")
names(output) <- varnames
head(output)
best_params <- output[which.min(output$TrainRMSE),]
best_params
# -----------------------------------------------------------------------------------------------

#https://xgboost.readthedocs.io/en/latest/parameter.html
xgb_model = xgboost(data=train_final, 
                    label=train_outcome, 
                    missing = NaN,
                    nrounds=100,
                    verbosity=1, 
                    eta=best_params$eta, 
                    max_depth=best_params$Depth, 
                    subsample=best_params$SubSampRate, 
                    min_child=best_params$currentMinChild,
                    colsample_bytree=best_params$ColSampRate,
                    objective=obj, 
                    eval_metric="rmse"
                    #,weights = weightsData
                    )

train_preds <- predict(xgb_model, train_final, missing = NaN)
rmse(train_complete$dem_rep_ratio,train_preds)
plot(train_complete$dem_rep_ratio,train_preds)

test_preds <- predict(xgb_model, test_final, missing = NaN)
rmse(test_complete$dem_rep_ratio,test_preds)
plot(test_complete$dem_rep_ratio,test_preds)

# roll up to State level ----------------------------------------


test_pred_df <- data.frame(fips = test_complete$fips, dem_rep_ratio =test_preds,
                           pop_estimate = test_complete$popestimate) %>%
  mutate(dem_rep_ratio_votes = dem_rep_ratio * pop_estimate) %>%
  left_join(base_county_state_fips[c("stname","fips")], by="fips")

#most of electoral college
elect_rollup <- test_pred_df %>% left_join(elect_tbl, by = c("stname" ="state")) %>% 
  group_by(stname) %>% summarize(total_dem_rep_ratio_votes = sum(dem_rep_ratio_votes),
                                total_pop = sum(pop_estimate)) %>%
  mutate(state_win=ifelse(total_dem_rep_ratio_votes>total_pop,1,0))

final_elect_rollup <- elect_rollup %>% 
  left_join(elect_tbl %>% 
              select(state, elect_votes), by=c("stname" ="state")) %>%
  mutate(dem_electoral_votes = state_win*elect_votes,rep_electoral_votes = elect_votes - dem_electoral_votes)

#maine and nebraska
maine_neb$fips <- as.character(maine_neb$fips)
elect_rollup_mn <- maine_neb %>% left_join(test_pred_df, by = c("fips","state"="stname"))
elect_rollup_mn <- elect_rollup_mn[complete.cases(elect_rollup_mn), ] %>% 
  group_by(congress_district) %>% summarize(total_dem_rep_ratio_votes = sum(dem_rep_ratio_votes),
                                            total_pop = sum(pop_estimate)) %>%
  mutate(state_win=ifelse(as.numeric(total_dem_rep_ratio_votes)>as.numeric(total_pop),1,0))

final_elect_rollup_mn <- elect_rollup_mn %>% 
  mutate(dem_electoral_votes = state_win,rep_electoral_votes = 1-dem_electoral_votes)

final_elect_rollup %>% summarise(total_dem_elect_votes = sum(dem_electoral_votes), total_rep_elect_votes = sum(rep_electoral_votes))
final_elect_rollup_mn
# -----------------------------------------------------------------

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

# electoral college
elect_tbl <- read_csv("../data/electoral_college.csv")

# FIT MODELS --------------------------------------------------------------

#remove outcome variables and fips code
train.boost <- as.matrix(train_complete[,c(4:(length(train_complete)-2))])
test.boost <- as.matrix(test_complete[c(4:(length(train_complete)-2))])

#look into why some rows have NA values

train_final <- matrix(as.numeric(data.matrix(train.boost)), nrow = nrow(train_complete))
test_final <- matrix(as.numeric(data.matrix(test.boost)), nrow = nrow(test_complete))
outcome <- matrix(as.numeric(data.matrix(train_complete$democrats.2016)))

#look to implement gridSearch: https://www.kaggle.com/silverstone1903/xgboost-grid-search-r

#https://xgboost.readthedocs.io/en/latest/parameter.html
xgb_model = xgboost(data=train_final, 
                         label=outcome, 
                         missing = NaN,
                         nrounds=200,
                         verbosity=1, 
                         eta=0.3, 
                         max_depth=6, 
                         subsample=1, 
                         colsample_bytree=1,
                         objective="binary:logistic", 
                         eval_metric="rmse"
                         )
test_preds <- predict(xgb_model, test_final, missing = NaN)
rmse(test_complete$democrats.2020,test_preds)

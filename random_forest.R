library(randomForest)
library(MASS)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)


# scale data, but don't want to scale y. Split, then issue with randomForest
# split data into val and train
# tune
# select best model
# Do we check state predictions ? 
# then retrain
# then predict on test 
# get actual predictions

seed = 42

train <- read_csv("../data/merged_final_2016.csv")
test <- read_csv("../data/merged_final_2020.csv")
# train <- train[complete.cases(train),]
# train <- data.frame(train)

train = train[complete.cases(train),]
x_train = train[c(2:(length(train)-1))]
y_train = train[length(train)]
colnames(y_train) = 'actual'


test = test[complete.cases(test),]
x_test = test[c(2:(length(test)-1))]
y_test = test[length(test)]
colnames(y_test) = 'actual'


train_scaled = scale(x_train) 
test_scaled = scale(x_test)
train_scaled = train_scaled %>% data.frame() %>% bind_cols(y_train)


number_predictors <- dim(train_scaled)[2] - 1

# # Splitting
# inTrain   <- createDataPartition(y = train_scaled$actual, p = 0.7)
# 
# train_set <- slice(train_scaled, inTrain$Resample1)
# val_set <- slice(train_scaled, -inTrain$Resample1)
# 
# 
# y_actual_val <- val_set %>%
#     select(actual)


### Base model ###
#Setting the default tuning parameters
control <- trainControl(method='repeatedcv', number=10, repeats=3)
mtry <- sqrt(number_predictors)
tunegrid <- expand.grid(.mtry=mtry)

# Initializing and fitting the random forest
rf_model_base <- train(actual ~ ., data = train_scaled, method = 'rf',
                      metric='RMSE', tuneGrid=tunegrid, trControl = control)

# Getting the predictions for the validation set
preds_rf_base <- predict(rf_model_base)

# Getting results into a dataframe
df <- data.frame(preds_rf_base, train_scaled$actual)

# Plotting the results
df %>% ggplot(aes(preds_rf_base, train_scaled.actual))+
    geom_point() +
    geom_abline(intercept=0, slope = 1, color = 'red') +
    ggtitle("Base Validation Analysis") +
    xlab("Predictions from Base Model") +
    ylab("Acual Validation Ratios")


### Tuning type 1
control <- trainControl(method='repeatedcv', number=10, repeats=3)
tunegrid_tune <- expand.grid(.mtry= c(sqrt(number_predictors), log2(number_predictors), number_predictors/3))
# Initializing and fitting the random forest
rf_model_tuned1 <- train(actual ~ ., data = train_scaled, method = 'rf',
                       metric='RMSE', tuneGrid=tunegrid_tune, trControl = control)

 
plot(rf_model_tuned1)


### Tuned Model - Grid Search ###

# mtry
# trees
# minimum number observations per node
# 
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(seq(2,18, 2)))
# set.seed(seed)
# 
# 
# rf <- train(actual ~ ., data = train_set, method = 'rf',
#              metric='RMSE', tuneGrid=tunegrid, trControl = control, ntree=2000)



# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid_tune <- expand.grid(.mtry= c(sqrt(number_predictors), log2(number_predictors), number_predictors/3))
modellist_new <- list()
seed <- 42

for (ntree in c(seq(40,600, 80))) {
    for (nodesize in c(10, 30, 60, 90)) {

    print(paste(ntree, nodesize))

    set.seed(seed)
    fit <- train(actual ~ ., data = train_scaled, method = 'rf',
                 metric='RMSE', tuneGrid=tunegrid_tune, trControl = control, ntree=ntree, nodesize=nodesize)
    key <- toString(ntree)
    modellist_new[[key]] <- fit
    }
}
# compare results
results <- resamples(modellist_new)
summary(results)
dotplot(results)







 
                        
                       # .ntree = c(seq(50,2050, 100)), .nodesize=c(seq(5,255,20)))
#tunegrid <- expand.grid(.mtry=c(1:15), .ntrees=c(50,800), .nodesize=c(50,200))



# preds_rf_grid <- predict(rf_model_grid, newdata = val_set)
# 
# df_grid <- data.frame(preds_rf_grid, y_actual_val$dem_rep_ratio)
# 
# # Plotting the results
# df_grid %>% ggplot(aes(preds_rf_grid, y_actual_val.dem_rep_ratio))+
#     geom_point() +
#     geom_abline(intercept=0, slope = 1, color = 'red') +
#     ggtitle("Tuned Validation Analysis") +
#     xlab("Predictions from Base Model") +
#     ylab("Acual Validation Ratios")
# 

#
#
# # Response Variable
# preds_rf_search_train <- predict(rf_random_search_dem, newdata = train_set_dem)
# preds_rf_search_val <- predict(rf_random_search_dem, newdata = val_dem)
#
# df_search_train <- data.frame(preds_rf_search_train, train_set_dem$democrats_pct)
# df_search_val <- data.frame(preds_rf_search_val, val_dem$democrats_pct)
#
# df_search_train %>% ggplot(aes(preds_rf_search_train, train_set_dem.democrats_pct)) +
#     geom_point() +
#     geom_abline(intercept=0, slope = 1)
#
# df_search_val %>% ggplot(aes(preds_rf_search_val, val_dem.democrats_pct)) +
#     geom_point() +
#     geom_abline(intercept=0, slope = 1)
#
#
#
#
# ### Scaling
# x_train = scale(x_train)
# x_test = scale(x_test, center=attr(X_train, "scaled:center"),
#               scale=attr(X_train, "scaled:scale"))
#
# test_dem <- test %>% select(!republicans_pct)
# test_rep <- test %>% select(!democrats_pct)
#
#
# test_dem[,which(!names(test_dem) %in% c("democrats_pct"))] <- scale(test_dem[,which(!names(test_dem) %in% c("democrats_pct"))],
#                                                                    center=attr(train_dem[,which(!names(train_dem) %in% c("democrats_pct"))], "scaled:center"),
#                                                                 scale=attr(train_dem[,which(!names(train_dem) %in% c("democrats_pct"))], "scaled:scale"))
#
#
#

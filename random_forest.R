library(randomForest)
library(MASS)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)

# Setting seed
seed = 42

# Rrading in the training and test data
train <- read_csv("../data/merged_final_2016.csv")
test <- read_csv("../data/merged_final_2020.csv")
# train <- train[complete.cases(train),]
# train <- data.frame(train)

# Dropping NAs and seperating xtrain and y train
train = train[complete.cases(train),]
x_train = train[c(2:(length(train)-1))]
y_train = train[length(train)]

# renaming
colnames(y_train) = 'actual'

# 
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


### Base model
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

 
print(rf_model_tuned1)



### Manual Search
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



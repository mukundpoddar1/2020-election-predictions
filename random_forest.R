library(randomForest)
library(MASS)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Metrics)
library(mlbench)
library(caret)



train <- read_csv("data/merged_final_2016.csv")
test <- read_csv("data/merged_final_2020.csv")
# train <- train[complete.cases(train),]
# train <- data.frame(train)

x_train = train[c(2:(length(train)-2))]
y_train = train[length(train)-1]

train <- train[complete.cases(train),]
test <- test[complete.cases(test),]

train_dem <- train %>% select(!republicans_pct)
train_rep <- train %>% select(!democrats_pct)

test_dem <- test %>% select(!republicans_pct)
test_rep <- test %>% select(!democrats_pct)

test_response_dem <- test %>% select(democrats_pct)



#x_train <- (train[c(2:(length(train)-2))])
y_train_dem <- train[length(train)-1]
y_train_rep <- train[length(train)]

#x_test <- test[c(2:(length(test)-2))]
y_test_dem <- test[length(test)-1]
y_test_rep <- test[length(test)]

#x_train = scale(x_train)
#x_test = scale(x_test, center=attr(X_train, "scaled:center"),
#               scale=attr(X_train, "scaled:scale"))

# test_dem <- test %>% select(!republicans_pct)
# test_rep <- test %>% select(!democrats_pct)


#test_dem[,which(!names(test_dem) %in% c("democrats_pct"))] <- scale(test_dem[,which(!names(test_dem) %in% c("democrats_pct"))], 
#                                                                    center=attr(train_dem[,which(!names(train_dem) %in% c("democrats_pct"))], "scaled:center"),
#                                                                   scale=attr(train_dem[,which(!names(train_dem) %in% c("democrats_pct"))], "scaled:scale"))


inTrain_dem   <- createDataPartition(y = train_dem$democrats_pct, p = 0.7)
inTrain_rep <- createDataPartition(y = train_rep$republicans_pct, p = 0.7)

train_set_dem <- slice(train_dem, inTrain_dem$Resample1)
train_set_rep <- slice(train_rep, inTrain_rep$Resample1)

val_dem <- slice(train_dem, -inTrain_dem$Resample1)
val_rep <- slice(train_rep, -inTrain_rep$Resample1)


# Base model
#rf_model_base <- randomForest(democrats_pct ~ ., data = train_dem)


# preds_rf_base <- predict(rf_model_base, newdata = test_dem)
# 
# df <- data.frame(preds_rf_base, test_response_dem$democrats_pct)
# 
# df %>% ggplot(aes(preds_rf_base, test_response_dem.democrats_pct))+
#     geom_point() +
#     geom_abline(intercept=0, slope = 1)


control <- trainControl(method='repeatedcv', number=10, repeats=3, search='random')
set.seed(42)
mtry <- sqrt(ncol(train_set_dem))
rf_random_search <- randomForest(democrats_pct ~ ., data = train_set_dem, metric="Accuracy", tuneLength=15, trControl=control)

preds_rf_search_train <- predict(rf_random_search, newdata = train_set_dem)
preds_rf_search_val <- predict(rf_random_search, newdata = val_dem)

df_search_train <- data.frame(preds_rf_search_train, train_set_dem$democrats_pct)
df_search_val <- data.frame(preds_rf_search_val, val_dem$democrats_pct)

df_search_train %>% ggplot(aes(preds_rf_search_train, train_set_dem.democrats_pct)) +
    geom_point() + 
    geom_abline(intercept=0, slope = 1)


df_search_val %>% ggplot(aes(preds_rf_search_val, val_dem.democrats_pct)) +
    geom_point() + 
    geom_abline(intercept=0, slope = 1)





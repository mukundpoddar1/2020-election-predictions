#########################
# Title: BST 260 Project - Regression models
# Purpose: Run base models on data to determine predictability
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
# library(foreign)
# library(MASS)
# library(leaps)
library(glmnet)


# LOAD DATA ---------------------------------------------------------------

elections_2020 <- read_csv("../data/merged_final_2020.csv")


# MLR ---------------------------------------------------------------------

# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020")]

cov_statement <- paste0(covs, collapse = " + ")

# Fitting the Full Regression Model
full.mod <- lm(eval(parse(text = paste0("democrats.2020 ~", cov_statement))), data=elections_2020)
summary(full.mod)

# Performing backwards selection
mod_back <- step(full.mod, direction = "backward")
summary(mod_back)


# LASSO -------------------------------------------------------------------

# Perform LASSO
# Following this:
# https://rstatisticsblog.com/data-science-in-action/machine-learning/lasso-regression/

model_data <- elections_2020 %>% dplyr::select(-fips)
x_vars <- model.matrix(democrats.2020 ~., model_data)[, -1]
y_var <- model_data$democrats.2020
lambda_seq <- 10^seq(2,-2, by = -.1)

set.seed(1)
train <- sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test <- (-train)
y_test <- y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_seq, nfolds = 5)

best_lam <- cv_output$lambda.min

# Using this value, train lasso model again
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test, ])

coef(lasso_best) # removes only consistency_rep, republicans.2020 and rep_poll_median

# Elastic net - article says to rerun with alpha = 0.5
elastic_best <- glmnet(x_vars[train,], y_var[train], alpha = 0.5, lambda = best_lam)
pred1 <- predict(elastic_best, s = best_lam, newx = x_vars[x_test, ])

coef(elastic_best) # removes only popestimate2019, age_20_to_39_years_tot_female_agegrp, rep_poll_mean, rep_poll_median, consistency_rep, republicans.2020 

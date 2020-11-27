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

# Logistic (dems > 50%) ---------------------------------------------------------------------

# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020")]

cov_statement <- paste0(covs, collapse = " + ")

elections_2020$dems_over_50pct <- ifelse(elections_2020$democrats.2020 > 0.5, 1, 0)

# Fitting the Full Regression Model
full.mod <- glm(eval(parse(text = paste0("dems_over_50pct ~", cov_statement))), data=elections_2020, family = binomial)
summary(full.mod)

# Performing backwards selection
mod_back <- step(full.mod, direction = "backward")
summary(mod_back)

AIC(full.mod) # 1131.408
AIC(mod_back) # 1118.379

model_results <- tibble(
  model_type = c("logistic", "logistic"),
  model_selection = c("full model", "backwards selection"),
  outcome = c("dems over 50%", "dems over 50%"),
  AIC = c(AIC(full.mod), AIC(mod_back)),
  BIC = c(BIC(full.mod), BIC(mod_back))
)

#### LASSO

# Perform LASSO
# Following this:
# https://rstatisticsblog.com/data-science-in-action/machine-learning/lasso-regression/

model_data <- elections_2020 %>% dplyr::select(-fips)
x_vars <- model.matrix(dems_over_50pct ~., model_data)[, -1]
y_var <- model_data$dems_over_50pct
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

coef(lasso_best) # removes only tot_male, raw_gdp, consistency_rep, republicans.2020

# Elastic net - article says to rerun with alpha = 0.5
elastic_best <- glmnet(x_vars[train,], y_var[train], alpha = 0.5, lambda = best_lam)
pred1 <- predict(elastic_best, s = best_lam, newx = x_vars[x_test, ])

coef(elastic_best) # removes only rep_amount, tot_male, age_20_to_39_years_tot_female_agegrp , age_40_to_59_years_tot_male_agegrp, raw_gdp, consistency_rep, republicans.2020 

# Re-do model with elastic best covariates:
# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020", "rep_amount", "raw_gdp", "dem_amount", "dems_over_50pct")]

cov_statement <- paste0(covs, collapse = " + ")

# Fitting the Full Regression Model
full.mod <- glm(eval(parse(text = paste0("dems_over_50pct ~", cov_statement))), data=elections_2020, family = binomial)
summary(full.mod)

AIC(full.mod) #1165.644 -- Full mod does better
model_results <- model_results %>% 
  bind_rows(
    tibble(
    model_type = c("logistic"),
    model_selection = c("elastic net"),
    outcome = c("dems over 50%"),
    AIC = c(AIC(full.mod)),
    BIC = c(BIC(full.mod)) 
    )
  )


# Democrats mult county pop -----------------------------------------------

# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020", "dems_over_50pct", "popestimate2019")]

cov_statement <- paste0(covs, collapse = " + ")

elections_2020$dems_county_pop <- elections_2020$democrats.2020*elections_2020$popestimate2019

# Plot outcome
elections_2020 %>% 
  ggplot(aes(x = dems_county_pop)) +
  geom_histogram()+
  theme_bw()


# fit a linear model
full.mod <- lm(eval(parse(text = paste0("dems_county_pop ~", cov_statement))), data = elections_2020)
summary(full.mod)

# Model evaluation:
par(mfrow = c(2, 2))
hist(full.mod$residuals, main = "Histogram of Residuals", xlab = "Residuals") 
qqnorm(residuals(full.mod), pch = 20, col = "tomato", main = "Unstandardized Residuals") # th qqline(residuals(lm3C)) #NJP Added line
plot(fitted(full.mod), residuals(full.mod), main = "Plot of Residuals against Fitted Values",xlab = "Fitted Values", ylab = "Residuals") 
abline(h = 0, col = "cornflowerblue", lwd = 2)

# Residuals don't look fully scattered 

AIC(full.mod) #72062.87

model_results <- model_results %>% 
  bind_rows(
    tibble(
    model_type = c("linear"),
    model_selection = c("full model"),
    outcome = c("% dems * county population"),
    AIC = c(AIC(full.mod)),
    BIC = c(BIC(full.mod))
    )
  )


# Democrats mult county pop (log) -----------------------------------------------

# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020", "dems_over_50pct", "dems_county_pop")]

cov_statement <- paste0(covs, collapse = " + ")

# Plot outcome - normality of observations
elections_2020 %>% 
  ggplot(aes(x = log(dems_county_pop))) +
  geom_histogram()+
  theme_bw()


# fit a linear model
full.mod <- lm(eval(parse(text = paste0("log(dems_county_pop) ~", cov_statement))), data = elections_2020)
summary(full.mod)

# Model evaluation:
par(mfrow = c(2, 2))
hist(full.mod$residuals, main = "Histogram of Residuals", xlab = "Residuals") 
qqnorm(residuals(full.mod), pch = 20, col = "tomato", main = "Unstandardized Residuals") # th qqline(residuals(lm3C)) #NJP Added line
plot(fitted(full.mod), residuals(full.mod), main = "Plot of Residuals against Fitted Values",xlab = "Fitted Values", ylab = "Residuals") 
abline(h = 0, col = "cornflowerblue", lwd = 2)

# Residuals don't look fully scattered 

AIC(full.mod) # 8292.731 - 8 times higher than logistic model

model_results <- model_results %>% 
  bind_rows(
    tibble(
    model_type = c("linear"),
    model_selection = c("full model"),
    outcome = c("log(% dems * county population)"),
    AIC = c(AIC(full.mod)),
    BIC = c(BIC(full.mod))
    )
  )


# Republicans mult county pop (log) -----------------------------------------------

# Get list of covariates
covs <- names(elections_2020)[!names(elections_2020) %in% c("fips", "democrats.2020", "republicans.2020", "dems_over_50pct", "dems_county_pop")]

cov_statement <- paste0(covs, collapse = " + ")

# Get republican counts
elections_2020$reps_county_pop <- elections_2020$republicans.2020*elections_2020$popestimate2019

# Plot outcome - normality of observations
elections_2020 %>% 
  ggplot(aes(x = log(reps_county_pop))) +
  geom_histogram()+
  theme_bw()


# fit a linear model
full.mod <- lm(eval(parse(text = paste0("log(reps_county_pop) ~", cov_statement))), data = elections_2020)
summary(full.mod)

# Model evaluation:
par(mfrow = c(2, 2))
hist(full.mod$residuals, main = "Histogram of Residuals", xlab = "Residuals") 
qqnorm(residuals(full.mod), pch = 20, col = "tomato", main = "Unstandardized Residuals") # th qqline(residuals(lm3C)) #NJP Added line
plot(fitted(full.mod), residuals(full.mod), main = "Plot of Residuals against Fitted Values",xlab = "Fitted Values", ylab = "Residuals") 
abline(h = 0, col = "cornflowerblue", lwd = 2)

# Residuals don't look fully scattered 

AIC(full.mod) # 7096.327

model_results <- model_results %>% 
  bind_rows(
    tibble(
      model_type = c("linear"),
      model_selection = c("full model"),
      outcome = c("log(% reps * county population)"),
      AIC = c(AIC(full.mod)),
      BIC = c(BIC(full.mod))
    )
  )




# Model Selection results
# A tibble: 6 x 5
# model_type model_selection     outcome                            AIC    BIC
# <chr>      <chr>               <chr>                            <dbl>  <dbl>
# 1 logistic   full model          dems over 50%                    1123.  1329.
# 2 logistic   backwards selection dems over 50%                    1110.  1261.
# 3 logistic   elastic net         dems over 50%                    1161.  1348.
# 4 linear     full model          % dems * county population      72871. 73077.
# 5 linear     full model          log(% dems * county population)  8293.  8504.
# 6 linear     full model          log(% reps * county population)  7096.  7308.


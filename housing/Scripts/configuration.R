
library(MASS)
library(tidyverse)
library(data.table)
library(ggplot2)
library(glmnet)
library(car)

set.seed(123)

# Edit to local path
setwd("~/R-Learning/Data Science/housing")
source("Scripts/regressions.R")

# Model for house price - choice of Boston dataset (default) or external one found
## can do get_data("") for other dataset
d <- get_data()

# EDA
check_eda <- get_eda()

# Variable Selection
vars <- d %>% regression_VarSelection

# Model Diagnostics Executed
## For elimination of collinear variables:
## vars <- support_RedefineVars("households")
d_filt <- vars %>% regression_ModelFit %>% regression_Diagnostics

# Check Regression Assumptions (Assume Independence)
assumptions <- regression_ModelFit(vars, d_filt) %>%
  regression_Assumptions(., d_filt)

# RMSE Evaluation + Final Model
rmse <- regression_CV(vars, d_filt)
final_model <- regression_ModelFit(vars, d_filt)
summary(final_model)

# Order (is this right?): 
## EDA
## Variable Selection (with 10-fold x-validation)
## Model Diagnostics / Multicollinearity (filter if needed)
## Check Model Assumptions
## Final Model fit / evaluation


# Notes:

## LASSO can be preceded by step sel to fine tune. LASSO penalizes addtl terms

## Is the final Model overfit? How do I recover a CV version? Can also do on 80% train df
## CV is for model comparison -> best test error for that model
## Can compare train & test error when overfitting --> when train error drops and test error rises, overfit
### Want to be at minima of test error
### Can take my model, take more complex and error --> see how train & test error behaves, be at min of test error

# Can run on train only; can look at pred vs. actual plots

# Neural nets --> needs to iter to fit, look at train and test over # of runs
## Easiest example for overfitting

# Bias-variance trade-off
## Want a less complex model and same results

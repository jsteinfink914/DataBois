
library(tidyverse)
library(data.table)
library(caret)
library(doParallel)
library(scales)

# getModelInfo() and modelLookup() extremely useful CARET functions
setwd("~/R-Learning/Data Science/loan")
source("Scripts/data_munge.R")

set.seed(123)

dt <- get_data() %>% data_ImputeMissings # %>% data_AllInteractions

cl <- makePSOCKcluster(3) # n cores - 1
registerDoParallel(cl)

best <- dt %>% analyze_TuneRF(., tunegrid = expand.grid(.mtry = 2:9))

### Bests:
# 'rfRules' computing more iters than expected - why?

## Without interactions:
# rf: mtry == 2, ntrees == 200

## With interactions:
# rf: mtry == 5, ntrees == 600

best_tune <- data.frame(mtry = 2, ntrees = 200)

final_model <- dt %>% fit_rf(., tuning_params = best_tune)
stopCluster(cl) 


############ Computational modelling for the Horizon task ##############

rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(here)
theme_set(theme_classic(base_size = 14))
here()

session <- 1

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

horizon <- load_and_prep_bandit_data(session)$horizon


###### Hierachical Bayesian Implementation of Standard Wilson model ############

# in orig wilson paper they fit the two horizons separately so let's do that here

# this function also saves the model objects as "analysis/bandits/modellingResults/fitHorizonSession[session]Wilson_full_horizon[horizon]only.Rda"
res_list5 <- recovery_horizon(horizon[horizon$Horizon == -0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = T)
res_list10 <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = T)
res_list5
res_list10


############  Hierarchical Bayesian Implementation of UCB ######################

res_list1 <- recovery_horizon(horizon[horizon$Horizon == -0.5, ], "UCB", bayesian = T, full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
res_list1

res_list2 <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "UCB", bayesian = T, full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
res_list2




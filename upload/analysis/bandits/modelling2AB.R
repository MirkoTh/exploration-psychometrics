################## computational modelling for the two-armed bandit #############

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

sam <- load_and_prep_bandit_data(session)$sam

sam <- load_and_prep_bandit_data(session)$sam

######### standard hybrid model ##########

# this also saves the model object fitted to the data as analysis/bandits/modellingResults/fitSamSession[session]hybrid_hierarchical.Rda
res_list1 <- recovery_sam(sam, "hybrid", hierarchical = T, it = 8000, no_intercept = F, save = T, use_saved = T, iterative = F)
res_list1


######## UCB #####################

res_list2 <- recovery_sam(sam, "UCB", hierarchical = T, it = 8000, no_intercept = F, save = T, use_saved = T, iterative = F)
res_list2




######## fit models ##########

library(tidyverse)
library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

#setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("~/expPsychoCluster/banditsWave1.Rda")

source("~/expPsychoCluster/recovery_utils.R")
sam <- get_KL_into_df(sam)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

############### Horizon task ############

horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2


#####  UCB but bayesian model

res_list <- recovery_horizon(horizon, "UCB", bayesian = T, full = T)

save(res_list, file = "~/expPsychoCluster/HorizonUCB.Rda")


################# Sam's task ########

## hybrid model
res_list2 <- recovery_sam(sam, "hybrid", hierarchical = T)

save(res_list2, file = "~/expPsychoCluster/SamUCB.Rda")


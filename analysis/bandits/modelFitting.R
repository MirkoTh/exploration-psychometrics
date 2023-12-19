######## fit models ##########


library(tidyverse)
library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")

load("data/pilot/bandits.Rda")

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

############### Horizon task ############

###### Hierachical Bayesian Implementation of Standard Wilson model 
res_list <- recovery_horizon(horizon, "Wilson", full = F)

## plot estimates
trueParams <- res_list[[1]]
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodel)))`
simParams <- res_list[[2]]
simParams$estimate <- simParams$`colMeans(as.data.frame(posterior_samples(recovModel)))`
pars <- rbind(trueParams[ ,2:3], simParams[ ,2:3])
pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)

ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")


###### UCB subject-level GLMs

res_list <- recovery_horizon(horizon, "UCB", bayesian = F)
res_list

trueParams <- res_list[[1]]
simParams <- res_list[[2]]

## visualise parameter distributions

pars <- pivot_longer(trueParams, cols = 2:6, names_to = "parameter", values_to = "estimate")
pars <- rbind(pars, pivot_longer(simParams, cols = 2:6, names_to = "parameter", values_to = "estimate"))

pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)

pars <- subset(pars, bothConverged)

ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(parameter), scale= "free")


##### bayes + UCB but bayesian model

res_list <- recovery_horizon(horizon, "UCB", bayesian = T, full = F)
res_list

trueParams <- res_list[[1]]
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodelUCB)))`
simParams <- res_list[[2]]
simParams$estimate <- simParams$`colMeans(as.data.frame(posterior_samples(recovModelUCB)))`
pars <- rbind(trueParams[ ,2:3], simParams[ ,2:3])
pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)

ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")


################# Sam's task ########

## hybrid model
res_list <- recovery_sam(sam, "hybrid")
# get parameters fitted to actual data
trueParams <- res_list[[1]]
# get parameters fitted to simulated data
simParams <- res_list[[2]]

# view recovery plot
res_list[[3]]

pars <- pivot_longer(trueParams, cols = 2:3, names_to = "parameter", values_to = "estimate")
pars <- rbind(pars, pivot_longer(simParams, cols = 2:3, names_to = "parameter", values_to = "estimate"))

pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)

pars <- subset(pars, bothConverged)

ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(parameter), scale= "free")






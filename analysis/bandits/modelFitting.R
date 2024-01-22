######## fit models ##########


library(tidyverse)
library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")

load("data/wave1/bandits.Rda")

source("analysis/recovery_utils.R")
sam <- get_KL_into_df(sam)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

############### Horizon task ############

horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2


###### Hierachical Bayesian Implementation of Standard Wilson model 
res_list <- recovery_horizon(horizon, "Wilson", full = T)
res_list
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


### check whether the recovery looks better if I remove outliers

simParams <- subset(simParams, V > mean(V)-1*sd(V) &  V < mean(V)+1*sd(V)) # outlier datapoints are the same subjects irrespective of variable

trueParams <- subset(trueParams, V > mean(V)-1*sd(V) &  V < mean(V)+1*sd(V), ID != 10) # outlier datapoints are the same subjects irrespective of variable

cors <- data.frame(true = rep(c("RU", "V", "Horizon", "RUH", "VH"), 5),
                   recovered =  rep(c("RU", "V", "Horizon", "RUH", "VH"), each = 5),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) 


#####  UCB but bayesian model

res_list <- recovery_horizon(horizon, "UCB", bayesian = T, full = T)
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
res_list <- recovery_sam(sam, "hybrid", hierarchical = T)
# get parameters fitted to actual data
trueParams <- res_list[[1]]
# get parameters fitted to simulated data
simParams <- res_list[[2]]

# view recovery plot
res_list[[3]]

# hierarchical

pars <- rbind(trueParams, simParams)
pars$estimate <- pars$`colMeans(as.data.frame(posterior_samples(trueModel)))`
pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)
ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")


# subject-level

pars <- pivot_longer(trueParams, cols = 2:3, names_to = "parameter", values_to = "estimate")
pars <- rbind(pars, pivot_longer(simParams, cols = 2:3, names_to = "parameter", values_to = "estimate"))

pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)

pars <- subset(pars, bothConverged)

ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(parameter), scale= "free")




################# bootstrapped parameter recovery ###############

# desired N
N <- 200

########### Horizon task + UCB non-bayesian implementation
res_list <- recovery_horizon(horizon, "UCB", bayesian = F)

trueParams <- res_list[[1]]

cor(trueParams[ ,2:6])

# take out incredible outliers

trueParams <- subset(trueParams, V > mean(V)-1*sd(V) &  V < mean(V)+1*sd(V))
cor(trueParams[ ,2:6])

res_list2 <- recover_bootstrapped_estimates_glm(N = N, trueParams = trueParams, model = "UCB", task = "Horizon", data = horizon)
res_list2

bootParams <- res_list2[[1]]
cor(bootParams[ ,2:6])


########### Sam + UCB non bayesian
res_list <- recovery_sam(sam, "UCB")
trueParams <- res_list[[1]]

res_list2 <- recover_bootstrapped_estimates_glm(N = N, trueParams = trueParams, model = "UCB", task = "Sam", data = sam)
res_list2



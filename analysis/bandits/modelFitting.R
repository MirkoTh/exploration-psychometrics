######## fit models ##########


library(tidyverse)
library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/bandits/banditsWave1.Rda")

source("analysis/recovery_utils.R")

if (!is.element("KLM0", colnames(sam))) {
  sam <- get_KL_into_df(sam) 
  
  save(horizon, sam, restless, file = "analysis/bandits/banditsWave1.Rda")
}

if (!is.element("bayMeanL", colnames(horizon))) {
  horizon$bayMeanL <- NA
  horizon$bayMeanR <- NA
  horizon$bayVarL <- NA
  horizon$bayVarR <- NA
  horizon$row <- 1:nrow(horizon)
  
  for (i in horizon$row[horizon$trial == 5]){
    horizon[horizon$row == i, grep("bay", colnames(horizon))] <- bayIncrAtOnce(i, horizon)
  }
  
  horizon$V <- scale(getV(horizon$bayMeanL, horizon$bayMeanR))
  horizon$RU <- scale(getRU(horizon$bayVarL, horizon$bayVarR))
  
  
  save(horizon, sam, restless, file = "analysis/bandits/banditsWave1.Rda")
  
  
}

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}




############### Horizon task ############

horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2


###### Hierachical Bayesian Implementation of Standard Wilson model 
res_list <- recovery_horizon(horizon, "Wilson", full = T, it = 1000)
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

pars <- subset(pars, bothConverged & abs(estimate) < 10)

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
save(res_list, file = "analysis/bandits/recovHorizonFull.Rda")

load("analysis/bandits/recovHorizonReduced.Rda")


trueParams <- res_list[[1]]
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodelUCB)))`

# distribution of fitted and recovered parameters
# 
# simParams <- res_list[[2]]
# simParams$estimate <- simParams$`colMeans(as.data.frame(posterior_samples(recovModelUCB)))`
# pars <- rbind(trueParams[ ,2:3], simParams[ ,2:3])
# pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)
# 
# ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")

# distribution only of fitting parameters

ggplot(trueParams, aes(estimate)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free") + geom_vline(aes(xintercept = 0))


################# Sam's task ########

## UCB
res_list1 <- recovery_sam(sam, "hybrid", hierarchical = T, it = 1000)
# get parameters fitted to actual data
# trueParams <- res_list[[1]]
# # get parameters fitted to simulated data
# simParams <- res_list                                                                                                                                                                                                          [[2]]
# 

load("analysis/bandits/recovSamhybrid.Rda")
# view recovery plot
res_list1[[3]]

# hierarchical

trueParams <- res_list1[[1]]
# # get parameters fitted to simulated data
simParams <- res_list1[[2]]

pars <- rbind(trueParams, simParams)
pars$estimate <- pars$`colMeans(as.data.frame(posterior_samples(trueModel)))`
pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)
ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")

# 
# # subject-level
# 
# pars <- pivot_longer(trueParams, cols = 2:3, names_to = "parameter", values_to = "estimate")
# pars <- rbind(pars, pivot_longer(simParams, cols = 2:3, names_to = "parameter", values_to = "estimate"))
# 
# pars$source <- rep(c("observed", "recovered"), each = nrow(pars)/2)
# 
# pars <- subset(pars, bothConverged)
# 
# ggplot(pars, aes(estimate, fill = source)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(parameter), scale= "free")
# 
# 
# 
# 
# ################# bootstrapped parameter recovery ###############
# 
# # desired N
# N <- 200
# 
# ########### Horizon task + UCB non-bayesian implementation
# res_list <- recovery_horizon(horizon, "UCB", bayesian = F)
# 
# trueParams <- res_list[[1]]
# 
# cor(trueParams[ ,2:6])
# 
# # take out incredible outliers
# 
# trueParams <- subset(trueParams, V > mean(V)-1*sd(V) &  V < mean(V)+1*sd(V))
# cor(trueParams[ ,2:6])
# 
# res_list2 <- recover_bootstrapped_estimates_glm(N = N, trueParams = trueParams, model = "UCB", task = "Horizon", data = horizon)
# res_list2
# 
# bootParams <- res_list2[[1]]
# cor(bootParams[ ,2:6])
# 
# 
# ########### Sam + UCB non bayesian
# res_list <- recovery_sam(sam, "UCB")
# trueParams <- res_list[[1]]
# 
# res_list2 <- recover_bootstrapped_estimates_glm(N = N, trueParams = trueParams, model = "UCB", task = "Sam", data = sam)
# res_list2
# 



############# 4 ARB ############

restlessParams <- readRDS("analysis/4arlb-overview.rds")

ggplot(restlessParams, aes(beta)) + geom_density(alpha = 0.6, fill = "purple") + geom_jitter(aes(y = -0.1), height = 0.05) + 
  labs(title = "Beta parameter estimate from UCB",
       y = "density")

ggplot(restlessParams, aes(gamma_ucb)) + geom_density(alpha = 0.6, fill = "purple") + geom_jitter(aes(y = -0.1), height = 0.05) + 
  labs(title = "Gamma parameter estimate from UCB",
       y = "density")



############# correlations amongst predictors #############


###### Horizon

# equation: chosen ~ delta_mean*Horizon + info*Horizon

horizon$mean_L <- NA
horizon$mean_R <- NA

horizon$row <- 1:nrow(horizon)
horizon$mean_L[horizon$trial == 5] <- apply(as.array(horizon$row[horizon$trial == 5]), 1, function(x) meann(horizon$reward[horizon$ID == horizon$ID[x]&
                                                                                                              horizon$block == horizon$block[x] &
                                                                                                              horizon$chosen == 0 & 
                                                                                                              horizon$trial < 5]))
horizon$mean_R[horizon$trial == 5] <- apply(as.array(horizon$row[horizon$trial == 5]), 1, function(x) meann(horizon$reward[horizon$ID == horizon$ID[x]&
                                                                                                              horizon$block == horizon$block[x] &
                                                                                                              horizon$chosen == 1& 
                                                                                                              horizon$trial < 5]))
## calculate deltas
horizon$delta_mean <- scale(horizon$mean_L - horizon$mean_R)

horizon$delta_meanHorizon <- horizon$Horizon * horizon$delta_mean

horizon$infoHorizon <- horizon$info * horizon$Horizon


preds <- c("Horizon", "delta_mean", "info", "delta_meanHorizon", "infoHorizon")

cors <- data.frame(predictor1 = rep(preds, length(preds)),
                   predictor2 = rep(preds, each = length(preds)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(horizon[ ,colnames(horizon) == cors$predictor1[x]],
                                                             horizon[ ,colnames(horizon) == cors$predictor2[x]], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = predictor1, y = predictor2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))


### within subject (only for 1 subject bc should not differ bc task setup is same for everyone)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(horizon[horizon$ID == 1,colnames(horizon) == cors$predictor1[x]],
                                                             horizon[horizon$ID == 1,colnames(horizon) == cors$predictor2[x]], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = predictor1, y = predictor2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))



############ Sam
preds <- c("V", "RU")

cors <- data.frame(predictor1 = rep(preds, length(preds)),
                   predictor2 = rep(preds, each = length(preds)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(sam[ ,colnames(sam) == cors$predictor1[x]],
                                                             sam[ ,colnames(sam) == cors$predictor2[x]], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = predictor1, y = predictor2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))



### within subject 

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(sam[sam$ID == 200 ,colnames(sam) == cors$predictor1[x]],
                                                             sam[sam$ID == 200 ,colnames(sam) == cors$predictor2[x]], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = predictor1, y = predictor2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

library(plyr)
cors_sam <- ddply(sam, ~ID, summarise, cor = cor(V,RU))

ggplot(cors_sam, aes(cor)) + geom_histogram()


############### simulate data using predetermined parameters #########

####### UCB Horizon ##########

#### parameters

V = seq(-2, 1, 1)
RU = seq(-2, 2, 1)
Horizon = seq(-2,2,1)
VH = seq(-2,2,1)
RUH = seq(-2,2,1)

pars = data.frame(V = rep(V, each = length(RU)*length(Horizon)*length(VH)* length(RUH)),
                  RU = rep(RU, each = length(Horizon)*length(VH)*length(RUH)),
                  Horizon = rep(Horizon, each = length(VH)*length(RUH)),
                  VH = rep(Horizon, each = length(RUH)),
                  RUH = RUH)

pars$ID <- 1:nrow(pars)
for (i in pars$ID){
  simdat <- subset(horizon, trial == 5 & ID == 1, -chosen) # ID does not matter here, everyone observed the same fixed choices anyway
  simdat$chosen <- pars$V[i] * simdat$V + pars$RU[i] * simdat$RU + pars$Horizon[i] * simdat$Horizon +
    pars$VH[i] * (simdat$V * simdat$Horizon) + pars$RUH[i] * (simdat$RU * simdat$Horizon) # setting intercept to 0 bc used scaled params so should approximately be ok
  
  simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
  
}

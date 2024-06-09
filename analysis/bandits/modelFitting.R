######## fit models ##########
rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

session <- 2

load(paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

if (!is.element("KLM0", colnames(sam))) {
  sam <- get_KL_into_df(sam) 
  
  save(horizon, sam, restless, file = paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))
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
  
  
  save(horizon, sam, restless, file = paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))
  
  
}

if (!is.element("delta_mean", colnames(horizon))){
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
  
  save(horizon, sam, restless, file = paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))
}



### remove the person that has no data

horizon <- subset(horizon,!is.na(info))
sam <- subset(sam, !is.na(chosen))



############### Horizon task ############

unique(horizon$Horizon)
unique(horizon$info)

horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2




###### Hierachical Bayesian Implementation of Standard Wilson model 
res_list <- recovery_horizon(horizon, "Wilson", full = T, it = 6000, save = T)
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

res_list <- recovery_horizon(horizon, "UCB", bayesian = T)
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


############  UCB but bayesian model ########

#test <- recovery_horizon(horizon, model = "UCB",bayesian = T, full = T, it = 200, no_horizon = T, save = F, no_intercept = T)

## version with just the long horizon
horiz <- subset(horizon, Horizon == 0.5)

res_list <- recovery_horizon(horiz, "UCB", bayesian = T, full = T, it = 4000, no_horizon = T, no_intercept = T) # this should save the output as .Rda as well
res_list

res_list <- recovery_horizon(horizon, "UCB", bayesian = T, full = T, it = 6000, no_intercept = F, save = T)
res_list



#load("analysis/bandits/recovHorizonReduced.Rda")

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

## just fitting the model

# modelfit <- fit_model_sam(sam, "UCB", hierarchical = T, it = 4000)
# 
# estims <- modelfit[[2]]
# ggplot(estims, aes(estimate)) + geom_histogram(alpha = 0.5, position = "identity") + facet_wrap(vars(predictor), scale= "free")
# 
# save(modelfit, file = paste("analysis/bandits/modelFitSamWave", session, ".Rda", sep = ""))

#test <- recovery_sam(sam, "UCB", hierarchical = T, it = 200, save = F, no_intercept = T)

## UCB
res_list1 <- recovery_sam(sam, "UCB", hierarchical = T, it = 4000, no_intercept = T)
res_list1
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

############## UCB Horizon 

#### parameters

V = seq(-3, 0, 1)
RU = seq(-2, 2, 1)
Horizon = seq(-2,2,1)
VH = seq(-2,2,1)
RUH = seq(-2,2,1)

Nsims = 10

pars = data.frame(V = rep(V, each = length(RU)*length(Horizon)*length(VH)* length(RUH)),
                  RU = rep(RU, each = length(Horizon)*length(VH)*length(RUH)),
                  Horizon = rep(Horizon, each = length(VH)*length(RUH)),
                  VH = rep(Horizon, each = length(RUH)),
                  RUH = RUH,
                  reward = NA,
                  se = NA)

pars$ID <- 1:nrow(pars)


### if this is for wave 2 we don't have data for that yet so load the reward set

load("task/rewardsHorizon2.Rda")

horizon <- rewards
horizon$ID <- 1
horizon$chosen <- 99 # need this to be non na
horizon$chosen[horizon$Horizon == 5 & horizon$trial > 5] <- NA
horizon$reward <- NA

horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2

horizon$bayMeanL <- NA
horizon$bayMeanR <- NA
horizon$bayVarL <- NA
horizon$bayVarR <- NA



fixed <- jsonlite::fromJSON("task/fixedChoices2.json")# rounds incl practice * fixed choices matrix

for (i in 1:nrow(fixed)){
  
  horizon$chosen[horizon$block == i & horizon$trial < 5] <- fixed[i, ] 
  horizon$reward[horizon$block == i & horizon$trial < 5] <- ifelse(horizon$chosen[horizon$block == i & horizon$trial < 5] == 0, horizon$reward1[horizon$block == i & horizon$trial < 5], horizon$reward2[horizon$block == i& horizon$trial < 5])
  
}

########################

for (i in pars$ID){
  ## little progress bar
  
  if(i %% 50 == 0){print(paste(i, "of", length(pars$ID)))}
  
  temp <- rep(NA, Nsims)
  
  for (k in 1:Nsims){
    
    simdat <- subset(horizon, ID == 1 & !is.na(chosen)) # ID does not matter here, everyone observed the same fixed choices anyway
    
    
    simdat$row <- 1:nrow(simdat)
    
    for (j in simdat$row[simdat$trial > 4]){
      simdat[simdat$row == j, grep("bay", colnames(simdat))] <- bayIncrAtOnce(j, simdat)
    }

    simdat$V <- scale(getV(simdat$bayMeanL, simdat$bayMeanR))
    simdat$RU <- scale(getRU(simdat$bayVarL, simdat$bayVarR))
    
    simdat$chosen[simdat$trial > 4] <- NA
    simdat$chosen[simdat$trial == 5] <- pars$V[i] * simdat$V[simdat$trial == 5] + pars$RU[i] * simdat$RU[simdat$trial == 5] + pars$Horizon[i] * simdat$Horizon[simdat$trial == 5] +
      pars$VH[i] * (simdat$V[simdat$trial == 5] * simdat$Horizon[simdat$trial == 5]) + pars$RUH[i] * (simdat$RU[simdat$trial == 5] * simdat$Horizon[simdat$trial == 5]) # setting intercept to 0 bc used scaled params so should approximately be ok
    # logistic function
    simdat$chosen[simdat$trial == 5] <- 1/(1+exp(-simdat$chosen[simdat$trial == 5]))
    
    simdat$chosen[simdat$trial == 5] <- ifelse(simdat$chosen[simdat$trial == 5] < runif(nrow(simdat[simdat$trial ==5, ])), 0, 1)
    
    simdat$reward[simdat$trial == 5] <- ifelse(simdat$chosen[simdat$trial == 5] == 0, simdat$reward1[simdat$trial == 5], simdat$reward2[simdat$trial == 5])
    
    # update baymean and bayvar for trial 6 based on what was chosen on first free choice (5) such that can then exploit based on that outcome
    for (j in simdat$row[simdat$trial == 6]){
      simdat[simdat$row == j, grep("bay", colnames(simdat))] <- bayIncrAtOnce(j, simdat)
    }
    
    
    ## make it choose optimally after trial 5
    simdat$chosen[simdat$trial  == 6] <- ifelse(simdat$bayMeanL[simdat$trial == 6] > simdat$bayMeanR[simdat$trial == 6], 0, 1)
    
    simdat$chosen[simdat$trial > 6] <- rep(simdat$chosen[simdat$trial == 6], each = 4)
    
    simdat$best <- ifelse(simdat$reward1 > simdat$reward2, 0,1)
    simdat$reward <- NA
    simdat$reward <- ifelse(simdat$chosen == simdat$best, 1, 0)

    temp[k] = mean(simdat$reward[simdat$trial > 4])
    
  }
  
  pars$reward[i] <- mean(temp)
  pars$se[i] <- se(temp)
  
}



hist(pars$reward)

df <- pivot_longer(pars, cols = c(1:5), names_to = "parameter", values_to = "estimate")

ggplot(df, aes(estimate, reward)) + facet_wrap(vars(parameter)) +
  stat_summary(geom = "point", fun.y = mean) + stat_summary(geom = "line", fun.y = mean)


ggplot(pars, aes(V,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red", high = "blue")

ggplot(pars, aes(RU,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red",  high = "blue")

ggplot(pars, aes(VH,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red",  high = "blue")


####################### now for Sam

#### parameters

V = seq(-2, 0.1, 0.4)
RU = seq(-4, 1, 0.4)

Nsims = 10

pars = data.frame(V = rep(V, each = length(RU)),
                  RU = rep(RU, length(V)),
                  reward = NA,
                  se = NA)

pars$ID <- 1:nrow(pars)


## if this is for wave 2 where we don't have data yet then need to import the reward set


load("task/rewardsSam2.Rda")
sam <- rewards
sam$ID <- 1
sam$chosen <- NA
sam$reward <- NA

for (i in pars$ID){
  
  temp <- rep(NA, Nsims)
  
  for (k in 1:Nsims){
    
    simdat <- subset(sam, ID == 1, -chosen) # ID does not matter here, everyone observed the same fixed choices anyway
    
    simdat <- sim_data_sam(simdat, pars, i, bootstrapped = T, newRewards = F)
    
    simdat$best <- ifelse(simdat$reward1 > simdat$reward2, 0, 1)
    
    simdat$reward <- ifelse(simdat$chosen == simdat$best, 1, 0)
    
    
   temp[k] <- mean(simdat$reward)
    
    
  }
  
  pars$reward[i] <- mean(temp)
  pars$se[i] <- se(temp)
  
}

ggplot(pars, aes(V, RU, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red", high = "blue") + geom_text(aes(label = round(reward, digits = 2)))

df <- pivot_longer(pars, cols = 1:2, values_to = "estimate", names_to = "parameter")

ggplot(df, aes(estimate, reward)) + geom_smooth() + facet_wrap(vars(parameter))


################ simulating how to get the best rewards in other data sets #######################

############ Horizon

horizon <- read.csv("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl/data.csv")

horizon$chosen <- ifelse(horizon$Choice == 0, 1, 0)
horizon$trial <- horizon$Trial
horizon$block <- horizon$Block
horizon$ID <- horizon$Subject
horizon$reward <- horizon$Outcome


V = seq(-2, 1, 1)
RU = seq(-2, 2, 1)
Horizon = seq(-2,2,1)
VH = seq(-2,2,1)
RUH = seq(-2,2,1)

Nsim = 10

pars = data.frame(V = rep(V, each = length(RU)*length(Horizon)*length(VH)* length(RUH)),
                  RU = rep(RU, each = length(Horizon)*length(VH)*length(RUH)),
                  Horizon = rep(Horizon, each = length(VH)*length(RUH)),
                  VH = rep(Horizon, each = length(RUH)),
                  RUH = RUH,
                  reward = NA,
                  se = NA)

pars$ID <- 1:nrow(pars)

for (i in pars$ID){
  
  ## little progress bar
  
  if(i %% 50 == 0){print(paste(i, "of", length(pars$ID)))}
  
  temp <- rep(NA, Nsim)
  
  for (k in 1:Nsim){
    simdat <- subset(horizon, ID == sample(horizon$ID, 1) & !is.na(chosen)) 
    
    
    simdat$row <- 1:nrow(simdat)
    
    simdat$bayMeanL <- NA
    simdat$bayMeanR <- NA
    simdat$bayVarL <- NA
    simdat$bayVarR <- NA
    
    for (j in simdat$row[simdat$trial == 5]){
      simdat[simdat$row == j, grep("bay", colnames(simdat))] <- bayIncrAtOnce(j, simdat)
    }
    
    simdat$V <- scale(getV(simdat$bayMeanL, simdat$bayMeanR))
    simdat$RU <- scale(getRU(simdat$bayVarL, simdat$bayVarR))
    

    simdat$row <- 1:nrow(simdat)
    
    # for (j in simdat$row[simdat$trial > 5]){
    #   simdat[simdat$row == j, grep("bay", colnames(simdat))] <- bayIncrAtOnce(j, simdat)
    # }
    
    simdat$V <- scale(getV(simdat$bayMeanL, simdat$bayMeanR))
    simdat$RU <- scale(getRU(simdat$bayVarL, simdat$bayVarR))
    
    simdat$chosen[simdat$trial > 4] <- NA
    simdat$chosen[simdat$trial == 5] <- pars$V[i] * simdat$V[simdat$trial == 5] + pars$RU[i] * simdat$RU[simdat$trial == 5] + pars$Horizon[i] * simdat$Horizon[simdat$trial == 5] +
      pars$VH[i] * (simdat$V[simdat$trial == 5] * simdat$Horizon[simdat$trial == 5]) + pars$RUH[i] * (simdat$RU[simdat$trial == 5] * simdat$Horizon[simdat$trial == 5]) # setting intercept to 0 bc used scaled params so should approximately be ok
    # logistic function
    simdat$chosen[simdat$trial == 5] <- 1/(1+exp(-simdat$chosen[simdat$trial == 5]))
    
    simdat$chosen[simdat$trial == 5] <- ifelse(simdat$chosen[simdat$trial == 5] < runif(nrow(simdat[simdat$trial ==5, ])), 0, 1)
    
    simdat$reward[simdat$trial == 5] <- ifelse(simdat$chosen[simdat$trial == 5] == 0, simdat$mu_L[simdat$trial == 5], simdat$mu_R[simdat$trial == 5])
    
    # update baymean and bayvar for trial 6 based on what was chosen on first free choice (5) such that can then exploit based on that outcome
    for (j in simdat$row[simdat$trial == 6]){
      simdat[simdat$row == j, grep("bay", colnames(simdat))] <- bayIncrAtOnce(j, simdat)
    }
    
    
    ## make it choose optimally after trial 5
    simdat$chosen[simdat$trial  == 6] <- ifelse(simdat$bayMeanL[simdat$trial == 6] > simdat$bayMeanR[simdat$trial == 6], 0, 1)
    
    simdat$chosen[simdat$trial > 6] <- rep(simdat$chosen[simdat$trial == 6], each = 4)
    
    ## we don't know what is best so let's just compare rewards based on ground truth rewards
    simdat$best <- ifelse(ifelse(is.na(simdat$bayMeanL), simdat$mu_L, simdat$bayMeanL) > ifelse(is.na(simdat$bayMeanR), simdat$mu_R, simdat$bayMeanR), 0, 1)
    
    
    simdat$reward <- NA
    simdat$reward <- ifelse(simdat$chosen == simdat$best, 1, 0)
    
    temp[k] <- mean(simdat$reward[simdat$trial > 4])
    
    
  }
  
  
  
  
  pars$reward[i] <- mean(temp)
  pars$se[i] <- se(temp)
  
}



hist(pars$reward)

df <- pivot_longer(pars, cols = c(1:5), names_to = "parameter", values_to = "estimate")

ggplot(df, aes(estimate, reward))  + facet_wrap(vars(parameter)) +
  stat_summary(geom = "point", fun.y = mean) + stat_summary(geom = "line", fun.y = mean)


ggplot(pars, aes(V,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red", high = "blue")

ggplot(pars, aes(RU,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red",  high = "blue")

ggplot(pars, aes(VH,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red",  high = "blue")





##################### Sam's task

sam <- read.csv("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl/exp1_bandit_task_scale.csv")

sam$chosen <- ifelse(sam$C == 0, 1, 0)
sam$ID <- sam$sub



V = seq(-1.5, 0.1, 0.1)
RU = seq(-0.7, 1.5, 0.1)

Nsim = 10

pars = data.frame(V = rep(V, each = length(RU)),
                  RU = rep(RU, length(V)),
                  reward = NA)

pars$ID <- 1:nrow(pars)

for (i in pars$ID){
  temp <- rep(NA, Nsim)
  
  for (k in 1:Nsim){
    simdat <- subset(sam, ID == sample(sam$ID, 1), -chosen) # ID does not matter here, everyone observed the same fixed choices anyway
    
    simdat <- sim_data_sam(simdat, pars, i, bootstrapped = T, newRewards = F)
    
    simdat$best <- ifelse(simdat$reward1 > simdat$reward2, 0, 1)
    
    simdat$reward <- ifelse(simdat$chosen == simdat$best, 1, 0)
    
    
    temp[k] <- mean(simdat$reward)
    
    
  }
  
  
  # simdat <- subset(sam, ID == 1, -chosen ) # ID does not matter here, everyone observed the same fixed choices anyway
  # 
  # simdat$chosen <- pars$V[i] * simdat$V + pars$RU[i] * simdat$RU  # setting intercept to 0 bc used scaled params so should approximately be ok
  # # logistic function
  # simdat$chosen <- 1/(1+exp(-simdat$chosen))
  # 
  # simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
  # 
  # simdat$reward <- NA
  # simdat$reward <- ifelse(simdat$chosen == 0, simdat$reward1, simdat$reward2)
  # 
  # simdat$max <- ifelse(simdat$reward1 > simdat$reward2, simdat$reward1, simdat$reward2)
  # 
  # simdat$best <- ifelse(simdat$reward == simdat$max, 1, 0)
  # 
   pars$reward[i] <- mean(temp)
  
  
  
}

ggplot(pars, aes(V, RU, fill = reward)) + geom_raster() + scale_fill_gradient(low = "red", high = "blue") + geom_text(aes(label = round(reward, digits = 2)))

df <- pivot_longer(pars, cols = 1:2, values_to = "estimate", names_to = "parameter")

ggplot(df, aes(estimate, reward)) + geom_smooth() + facet_wrap(vars(parameter))


############### parameter identifiability ##########

############## Horizon

load(sprintf("analysis/bandits/modellingResults/fitHorizonSession%iUCBfullno_intercept.Rda", session))

baymodel

simdat <- subset(horizon, trial == 5, -chosen)
simdat$chosen <- predict(baymodel)[ ,1]
simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)

out <- fit_model_horizon(simdat, "UCB", full = T, 2000, save = F)
recovModel <- out[[1]]
recoveredParams <- out[[2]]
recovModel



############ visualise fixed effects ##############

models <- list.files("analysis/bandits/modellingResults")

models

models <- subset(models, !grepl("legacy", models))
models <- subset(models, !grepl("recovery", models))

models


count <- 1
for (i in models){
  sess <- readr::parse_number(i)
  load(paste("analysis/bandits/modellingResults/", i, sep = ""))
  
  if (grepl("Horizon", i)){
    model <- baymodel
  } else {
    model <- trueModel
  }
  fixed <- summary(model)$fixed
  
  fixed$var <- rownames(fixed)
  fixed$color <- ifelse(sign(fixed$`l-95% CI`) == sign(fixed$`u-95% CI`), "red", "grey")
  
  p <- ggplot(fixed, aes(var, Estimate, fill = color)) + geom_col() +
    geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.5)+
    scale_fill_manual(values = c("grey", "red"))+
    theme(legend.position = "none") +
    ggtitle(i)
  
  assign(paste("p", count, sep = ""), p)
  
  count = count +1
  
}

ggpubr::ggarrange(p1, p2, p3, p4, p5, p6)
ggpubr::ggarrange(p7, p8, p9, p10, p11, p12)


################# identifiability of the interaction effect ############

model <- brm(chosen ~ V + RU + Horizon + (V + RU + Horizon | ID),
             data = horizon[horizon$trial == 5, ],
             family = "bernoulli",
             chains = 2,
             cores = 2,
             it = 3000)
model

simdat <- subset(horizon, trial == 5, -c(chosen))

simdat$chosen_pred <- predict(model)[ ,1]
simdat$chosen <- ifelse(runif(nrow(simdat),0, 1) > simdat$chosen_pred, 0, 1)

print(plyr::ddply(simdat, ~chosen, summarise, pred = mean(chosen_pred)))

recov_model <- brm(chosen ~ V*Horizon + RU*Horizon + (V*Horizon + RU*Horizon | ID),
                   data = simdat,
                   family = "bernoulli",
                   chains = 2,
                   cores = 2,
                   it = 3000)

recov_model


### only taking out 1 interaction at a time

model <- brm(chosen ~ V*Horizon + RU  + (V*Horizon + RU + Horizon | ID),
             data = horizon[horizon$trial == 5, ],
             family = "bernoulli",
             chains = 2,
             cores = 2,
             it = 3000)
model

simdat <- subset(horizon, trial == 5, -c(chosen))

simdat$chosen_pred <- predict(model)[ ,1]
simdat$chosen <- ifelse(runif(nrow(simdat),0, 1) > simdat$chosen_pred, 0, 1)

print(plyr::ddply(simdat, ~chosen, summarise, pred = mean(chosen_pred)))

recov_model <- brm(chosen ~ V + RU*Horizon + (V + RU*Horizon | ID),
                   data = simdat,
                   family = "bernoulli",
                   chains = 2,
                   cores = 2,
                   it = 3000)

recov_model

recov_model <- brm(chosen ~ V*Horizon + RU*Horizon + (V*Horizon + RU*Horizon | ID),
                   data = simdat,
                   family = "bernoulli",
                   chains = 2,
                   cores = 2,
                   it = 3000)

recov_model

#####
model <- brm(chosen ~ V + RU*Horizon  + (V + RU*Horizon | ID),
             data = horizon[horizon$trial == 5, ],
             family = "bernoulli",
             chains = 2,
             cores = 2,
             it = 3000)
model

simdat <- subset(horizon, trial == 5, -c(chosen))

simdat$chosen_pred <- predict(model)[ ,1]
simdat$chosen <- ifelse(runif(nrow(simdat),0, 1) > simdat$chosen_pred, 0, 1)

print(plyr::ddply(simdat, ~chosen, summarise, pred = mean(chosen_pred)))

recov_model <- brm(chosen ~ V*Horizon + RU + (V*Horizon + RU | ID),
                   data = simdat,
                   family = "bernoulli",
                   chains = 2,
                   cores = 2,
                   it = 3000)

recov_model



recov_model <- brm(chosen ~ V*Horizon + RU*Horizon + (V*Horizon + RU*Horizon | ID),
                   data = simdat,
                   family = "bernoulli",
                   chains = 2,
                   cores = 2,
                   it = 3000)

recov_model


################# is it that the predictors are correlated? ######

df <- data.frame(V = horizon$V[horizon$trial == 5],
                 V_Horizon = horizon$V[horizon$trial == 5]*horizon$Horizon[horizon$trial == 5],
                 RU = horizon$RU[horizon$trial == 5],
                 R_Horizon = horizon$RU[horizon$trial == 5] * horizon$Horizon[horizon$trial == 5])

df <- pivot_longer(df,cols = 1:ncol(df), names_to = "predictor", values_to = "value")
cors <- data.frame(x = rep(colnames(df), each = length(colnames(df))),
                   y = rep(colnames(df), length(colnames(df))),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(df$value[df$predictor == cors$x[x]],
                                                             df$value[df$predictor == cors$y[x]]))


ggplot(cors, aes(x = x, y = y, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

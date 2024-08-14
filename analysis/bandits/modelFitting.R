######## fit models ##########
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


############### Horizon task ############
horizon <- load_and_prep_bandit_data(session)$horizon


###### Hierachical Bayesian Implementation of Standard Wilson model 

res_list <- recovery_horizon(horizon, "Wilson", full = T, it = 8000, save = T, use_saved = T)
res_list

# in orig wilson paper they fit the two horizons separately so let's do that here

res_list2 <- recovery_horizon(horizon[horizon$Horizon == -0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = F)
res_list3 <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = F)
res_list2
res_list3

########## are V and RU equivalent to delta_mean and info?

p1 <- ggplot(horizon[horizon$trial == 5, ], aes(delta_mean, V)) + geom_jitter(alpha = 0.3)+
  geom_smooth(method = "lm")+
  #coord_cartesian(xlim = c(min(c(horizon$V, horizon$delta_mean), na.rm = T), min(c(horizon$V, horizon$delta_mean), na.rm = T)),
                 # ylim = c(min(c(horizon$V, horizon$delta_mean), na.rm = T), min(c(horizon$V, horizon$delta_mean), na.rm = T)))+
  geom_label(label = sprintf("r = %.2f", cor(horizon$V[horizon$trial == 5], horizon$delta_mean[horizon$trial == 5])), x = Inf, y = -Inf, hjust = "inward", vjust = "inward")+
  labs(title = "Difference in posterior means (V) by mean difference",
       x = "mean difference",
       y = "V")

p2 <- ggplot(horizon[horizon$trial == 5, ], aes(as.factor(info), RU)) + 
  geom_boxplot(width = 0.2)+
  scale_x_discrete(breaks = c(-0.5, 0, 0.5), labels = c(-1, 0, 1))+
  labs(title = "Difference in posterior variance (RU) by information condition",
       x = "information condition",
       y = "RU")

ggpubr::ggarrange(p1, p2, labels = "AUTO")


############  Hierarchical Bayesian Implementation of UCB


res_list <- recovery_horizon(horizon, "UCB", bayesian = T, full = T, it = 8000, no_horizon = F, no_intercept = F, save = T, use_saved = T) # this should save the output as .Rda as well
res_list

res_list1 <- recovery_horizon(horizon[horizon$Horizon == -0.5, ], "UCB", bayesian = T, full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
res_list1

res_list2 <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "UCB", bayesian = T, full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
res_list2

### horizon 10 but using all free choices


for (i in horizon$row[horizon$trial > 5]){
  if (i %% 1000 == 0){print(sprintf("%.2f percent done", (i/nrow(horizon)*100)))}
  horizon[horizon$row == i, grep("bay", colnames(horizon))] <- bayIncrAtOnce(i, horizon)
}

horizon$V <- scale(getV(horizon$bayMeanL, horizon$bayMeanR))
horizon$RU <- scale(getRU(horizon$bayVarL, horizon$bayVarR))

res_list <- fit_model_horizon(horizon[horizon$Horizon == 0.5 & horizon$trial > 4, ], model = "UCB", full = T, it = 8000, no_horizon = T, save = F, no_intercept = F, use_saved = F, use_all = T)

res_list[[1]]

save(res_list, file = sprintf("analysis/bandits/modellingResults/fitHorizon10allChoicesSession%i.Rda", session))

############ how does the modelfit of UCB compare to the classic wilson model?

# model fit of wilson model
out <- fit_model_horizon(horizon[horizon$Horizon == -0.5, ], model = "Wilson", full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
wilson_5 <- out[[1]]
wilson_5 <- add_criterion(wilson_5, "loo")
wilson_5$criteria

modelFits <- data.frame(model = rep(c("Wilson", "UCB"), each = 2),
                        horizon = rep(c(5,10), 2),
                        loo = NA,
                        se = NA)

modelFits[modelFits$model == "Wilson" & modelFits$horizon == 5, c(3,4)] <- wilson_5$criteria$loo$estimates[3, ]

out <- fit_model_horizon(horizon[horizon$Horizon == 0.5, ], model = "Wilson", full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
wilson_10 <- out[[1]]
wilson_10 <- add_criterion(wilson_10, "loo")
wilson_10$criteria

modelFits[modelFits$model == "Wilson" & modelFits$horizon == 10, c(3,4)] <- wilson_10$criteria$loo$estimates[3, ]

# model fit of UCB
out <- fit_model_horizon(horizon[horizon$Horizon == -0.5, ], model = "UCB", full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
ucb_5 <- add_criterion(out[[1]], "loo")
ucb_5$criteria

modelFits[modelFits$model == "UCB" & modelFits$horizon == 5, c(3,4)] <- ucb_5$criteria$loo$estimates[3, ]

out <- fit_model_horizon(horizon[horizon$Horizon == 0.5, ], model = "UCB", full = T, it = 8000, no_intercept = F, no_horizon = T, save = T, use_saved = T)
ucb_10 <- add_criterion(out[[1]], "loo")
ucb_10$criteria

modelFits[modelFits$model == "UCB" & modelFits$horizon == 10, c(3,4)] <- ucb_10$criteria$loo$estimates[3, ]

modelFits$horizon <- factor(modelFits$horizon, levels = c(5, 10), labels = c("short", "long"))

p3 <- ggplot(modelFits, aes(model, loo, fill = horizon)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = loo -se, ymax = loo+se), width = 0.3, position = position_dodge(0.9))+
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"), name = "Horizon")+
  labs(title = "Model comparison",
       subtitle = "UCB and the classic Wilson model do not differ in their model fit",
       y = "Leave one out information criterion")
  
p3
ggpubr::ggarrange(p1, p2, p3, ncol = 3, labels = "AUTO")

########## is UCB fitting the horizons separately identifiable?

## Horizon 5

# no V

fit <- brm(chosen ~ RU + (RU | ID),
           data = horizon[horizon$Horizon == -0.5 & horizon$trial == 5, ],
           family = "bernoulli",
           chains = 2,
           cores = 2,
           iter = 6000)

simdat <- subset(horizon, trial == 5 & Horizon == -0.5, -chosen)

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)

refit <- brm(chosen ~ V + RU + (V + RU | ID),
             data = simdat,
             family = "bernoulli",
             chains = 2,
             cores = 2,
             iter = 6000)

refit
# no RU

fit <- brm(chosen ~ V + (V | ID),
           data = horizon[horizon$Horizon == -0.5 & horizon$trial == 5, ],
           family = "bernoulli",
           chains = 2,
           cores = 2,
           iter = 6000)

simdat <- subset(horizon, trial == 5 & Horizon == -0.5, -chosen)

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)

refit <- brm(chosen ~ V + RU + (V + RU | ID),
             data = simdat,
             family = "bernoulli",
             chains = 2,
             cores = 2,
             iter = 6000)

refit


################### same question for the Wilson model

fit <- brm(chosen ~ delta_mean,
           data = horizon[horizon$Horizon == 0.5, ],
           family = "bernoulli",
           chains = 2,
           cores = 2,
           iter = 6000)

simdat <- subset(horizon, Horizon == 0.5 & trial == 5, -chosen)

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)


refit <- brm(chosen ~ delta_mean + info,
             data = simdat,
             family = "bernoulli",
             chains = 2,
             cores = 2,
             iter = 6000)

fit

refit
############ Zaller et al

zaller <- read.csv("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl/data.csv")

zaller <- zaller %>% 
  rename(chosen = Choice,
         ID = Subject,
         block = Block,
         trial = Trial,
         info = Info,
         reward = Outcome) %>% 
  mutate(Horizon = recode(Horizon, `5` = -0.5, `10` = 0.5),
         info = info/2)

zaller$row <- 1:nrow(zaller)
zaller$mean_L[zaller$trial == 5] <- apply(as.array(zaller$row[zaller$trial == 5]), 1, function(x) meann(zaller$reward[zaller$ID == zaller$ID[x]&
                                                                                                                        zaller$block == zaller$block[x] &
                                                                                                                        zaller$chosen == 0 & 
                                                                                                                        zaller$trial < 5]))
zaller$mean_R[zaller$trial == 5] <- apply(as.array(zaller$row[zaller$trial == 5]), 1, function(x) meann(zaller$reward[zaller$ID == zaller$ID[x]&
                                                                                                                        zaller$block == zaller$block[x] &
                                                                                                                        zaller$chosen == 1& 
                                                                                                                        zaller$trial < 5]))

zaller$delta_mean <- zaller$mean_L - zaller$mean_R


zaller$session <- 1

out_5 <- recovery_horizon(zaller[zaller$Horizon == -0.5, ],
                        model = "Wilson",
                        it = 8000,
                        no_horizon = T,
                        save = F)

out_5
out_10 <- recovery_horizon(zaller[zaller$Horizon == 0.5, ],
                          model = "Wilson",
                          it = 8000,
                          no_horizon = T,
                          save = F)

out_10
#
################# Sam's task ########
sam <- load_and_prep_bandit_data(session)$sam

res_list1 <- recovery_sam(sam, "hybrid", hierarchical = T, it = 8000, no_intercept = F, save = T, use_saved = T, iterative = F)
res_list1

res_list2 <- recovery_sam(sam, "UCB", hierarchical = T, it = 8000, no_intercept = F, save = T, use_saved = T, iterative = F)
res_list2

################ test identifiability of hybrid model ###########

### leaving out VTU

fit <- fit_model_sam(sam, "UCB", hierarchical = T, it = 8000, save = F, use_saved = T)[[1]]

simdat <- data.frame()# for hierarchical model bc that one needs the simdat of all subjects at once

## iterate through subjects to simulate for each subject
for (i in unique(sam$ID)){
  if (i %% 10 == 0) {print(paste("subject", i, "of", max(sam$ID)))}
  
  ### simulate data
  
  # create data
  simdat_temp <- sim_data_sam(sam, fit, i, hierarchical= T, bootstrapped = F, newRewards = F, iterative = F)
  simdat <- rbind(simdat, simdat_temp)
  
}



refit <- fit_model_sam(simdat, "hybrid", hierarchical = T, it = 8000, save = F, use_saved = F)[[1]]

refit

# compared to when fit to regular data
fit_model_sam(sam, "hybrid", hierarchical = T, it = 8000, save = F, use_saved = T)[[1]]



### leaving out V (and simulating using the old method that retains the old kalman filter output)

fit <- brm(chosen ~ RU + VTU + (RU + VTU | ID),
           data = sam,
           family = "bernoulli",
           iter = 4000,
           cores = 2,
           chains = 2)

simdat <- sam

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)

refit <- fit_model_sam(simdat, "hybrid", hierarchical = T, it = 4000, save = F, use_saved = F)[[1]]

refit

############ same thing for fan et al data

fan <- read.csv("~/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl/exp1_bandit_task_scale.csv")

# cond: 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS

fan$cond <- factor(fan$cond, levels = c(1,2,3,4), labels = c("FS", "SF", "FF", "SS"))

# they rescaled V, VTU, RU but the on-rescaled variables are still in the dataframe called _old
fan$VTU <- fan$V_old / fan$TU_old
fan$V <- fan$V_old
fan$RU <- fan$RU_old

fan$ID <- fan$sub

fan$chosen <- fan$C

### leaving out VTU

fit <- fit_model_sam(fan, "UCB", hierarchical = T, it = 2000, save = F, use_saved = F)[[1]]

simdat <- fan

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)

refit <- fit_model_sam(simdat, "hybrid", hierarchical = T, it = 4000, save = F, use_saved = F)[[1]]

refit

# compared to when fit to regular data
hybFit <-  fit_model_sam(fan, "hybrid", hierarchical = T, it = 2000, save = F, use_saved = F)[[1]]
hybFit

save(fit, refit, hybFit, file = "analysis/bandits/identifyVTUFan.Rda")

### leaving out V and RU

## subsampling data to speed this up
N = 100
inclSubs <- sample(unique(fan$sub), N, replace = F)

fan_subset <- subset(fan, is.element(sub, inclSubs))

fit <- brm(chosen ~  VTU + (VTU | ID),
           data = fan_subset,
           family = "bernoulli",
           iter = 2000,
           cores = 2,
           chains = 2)

simdat <- fan_subset

simdat$chosen <- predict(fit)[ ,1]

hist(simdat$chosen)

simdat$chosen <- ifelse(runif(nrow(simdat), 0, 1) > simdat$chosen, 0, 1)

refit <- fit_model_sam(simdat, "hybrid", hierarchical = T, it = 2000, save = F, use_saved = F)[[1]]

refit


########### comparative model fit of hybrid vs ucb

# hybrid
out <- fit_model_sam(sam, model = "hybrid", hierarchical = T, it = 8000, save = T, no_intercept = F, use_saved = T)
hybrid <- add_criterion(out[[1]], "loo")
hybrid$criteria

modelFits <- data.frame(model = rep(c("hybrid", "UCB")),
                        loo = NA,
                        se = NA)

modelFits[modelFits$model == "hybrid", c(2,3)] <- hybrid$criteria$loo$estimates[3, ]

# model fit of UCB
out <- fit_model_sam(sam, model = "ucb", hierarchical = T, it = 8000, save = T, no_intercept = F, use_saved = T)
ucb <- add_criterion(out[[1]], "loo")
ucb$criteria

modelFits[modelFits$model == "UCB", c(2,3)] <- ucb$criteria$loo$estimates[3, ]


p3 <- ggplot(modelFits, aes(model, loo)) +
  geom_col(fill = "#66C2A5")+
  geom_errorbar(aes(ymin = loo -se, ymax = loo+se), width = 0.3)+
  labs(title = "Model comparison",
       subtitle = "UCB and the classic hybrid model do not differ in their model fit",
       y = "Leave one out information criterion")

p3


#################### making a csv with all model parameters ###############
## Sam 

params <- list()
fixed <- list()
for (s in 1:2){
  
  data <- load_and_prep_bandit_data(s)$sam
  out <- fit_model_sam(data, model = "hybrid", hierarchical = T, use_saved = T)
  
  # inspect the model to ensure everything is nicely converged and stuff
  
  print(out[[1]])
  params[[s]] <- out[[2]]
  fixed[[s]] <- as.data.frame(summary(out[[1]])$fixed)
  
  
}

sam_params <- params %>% bind_rows(.id = "session") %>% 
  mutate(estimate = -1*estimate) # already flipped to be larger number more seeking
sam_fixed <- fixed %>% bind_rows(.id = "session") %>% 
  mutate(Estimate = -1*Estimate, 
         low = -1*`u-95% CI`, 
         `u-95% CI` = -1*`l-95% CI`,
         `l-95% CI`= low) %>% 
  subset(select = -low)# when recoding this here we have to flip upper and lower CI, need temporary variable to avoid recoded versions influencing each other


## Horizon
params <- list()
fixed <- list()

for (s in 1:2){
  
  p <- list()
  f <- list()
  for (h in c(-0.5, 0.5)){
    
    data <- load_and_prep_bandit_data(s)$horizon
    out <- fit_model_horizon(data[data$Horizon == h, ], model = "Wilson", bayesian = T, full = T, no_horizon = T, use_saved = T)
    
    # inspect the model to ensure everything is nicely converged and stuff
    
    print(out[[1]])
    p <- append(p, list(out[[2]]))
    f <- append(f,list(as.data.frame(summary(out[[1]])$fixed)))
    
  }
  params[[s]] <- p %>% bind_rows(.id = "horizon") %>% 
    mutate(horizon = recode(horizon, `1` = "short", `2` = "long"))
  
  fixed[[s]] <- f %>% bind_rows(.id = "horizon") %>% 
    mutate(horizon = recode(horizon, `1` = "short", `2` = "long"))
}

horizon_params <- params %>% bind_rows(.id = "session") %>% 
  mutate(estimate = if_else(predictor == "delta_mean", -1 * estimate, estimate)) # here we recode only for reward
horizon_fixed <- fixed %>% bind_rows(.id = "session")
horizon_fixed$row_names <- rownames(horizon_fixed)
horizon_fixed <- horizon_fixed %>%
  mutate(Estimate = if_else(str_detect(row_names, "delta_mean"), -1 * Estimate, Estimate),
         low = if_else(str_detect(row_names, "delta_mean"), -1 * `u-95% CI`, `l-95% CI`), # this evaluates one after the other so need intermediate variable to avoid one row influencing the next
         `u-95% CI` = if_else(str_detect(row_names, "delta_mean"), -1 * `l-95% CI`, `u-95% CI`),
         `l-95% CI` = low) %>% 
  subset(select = -c(row_names, low))

## combine
params <- list(sam = sam_params, horizon = horizon_params) %>% 
  bind_rows(.id = "task")%>% 
  subset(select = c("predictor", "estimate", "horizon", "task", "session"))
params$ID <- parse_number(rownames(params)) 

fixed <- list(sam = sam_fixed, horizon = horizon_fixed) %>% 
  bind_rows(.id = "task")
split_vec <- strsplit(rownames(fixed), "\\...")
# Extract the part before '...' which is the first element after split
fixed$predictor <- sapply(split_vec, '[', 1)

### restless

restless <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")

rest_params <- restless %>% 
  pivot_longer(cols = -ID, values_to = "estimate", names_to = "predictor") %>% 
  mutate(session = as.character(parse_number(predictor)),
         predictor = ifelse(grepl("ru", predictor), "RU", "V"),
         horizon = NA,
         task = "restless")

rest_fixed <- rest_params %>% 
  group_by(predictor, session, task, horizon) %>% 
  summarise(Estimate = mean(estimate),
            `Est.Error` = se(estimate),
            `l-95% CI` = mean(estimate) - 1.96*se(estimate),
            `u-95% CI` = mean(estimate) + 1.96*se(estimate)) %>% 
  mutate(Rhat = NA,
         Bulk_ESS = NA,
         Tail_ESS = NA)

## combine all

fixed <- list(fixed, rest_fixed) %>% 
  bind_rows()


params <- list(params, rest_params) %>% 
  bind_rows()


## save
write.csv(params,"analysis/bandits/AllModelParameters.csv")
write.csv(fixed, "analysis/bandits/AllFixedEffects.csv")

saveRDS(params, file ="analysis/bandits/allParams.rds")
saveRDS(fixed, fil = "analysis/bandits/allFixed.rds")

#################### correlations among VTU, V, RU across trials in our data vs Fan et al ##########
our_cor <- plyr::ddply(sam[sam$trial > 1, ], ~trial, summarise, V_VTU = cor(V, VTU), V_RU = cor(V, RU), VTU_RU = cor(VTU, RU))

fan <- read.csv("~/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl/exp1_bandit_task_scale.csv")

# cond: 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS

fan$cond <- factor(fan$cond, levels = c(1,2,3,4), labels = c("FS", "SF", "FF", "SS"))

# they rescaled V, VTU, RU but the on-rescaled variables are still in the dataframe called _old
fan$VTU <- fan$V_old / fan$TU_old

fan_cor <-  plyr::ddply(fan[fan$trial > 1, ], ~trial, summarise, V_VTU = cor(V_old, VTU), V_RU = cor(V_old, RU_old), VTU_RU = cor(VTU, RU_old))

our_cor$source <- "session1"

fan_cor$source <- "fan"

cors <- rbind(our_cor, fan_cor)

cors <- pivot_longer(cors, cols = grep("V", colnames(cors)), names_to = "predictors", values_to = "cor")

ggplot(cors, aes(trial, cor, color = predictors)) + geom_point()+
  geom_line()+
  facet_grid(cols = vars(source))


### split up by condition

our_cor <- plyr::ddply(sam[sam$trial > 1, ], ~trial+cond, summarise, V_VTU = cor(V, VTU), V_RU = cor(V, RU), VTU_RU = cor(VTU, RU))

fan_cor <-  plyr::ddply(fan[fan$trial > 1, ], ~trial+cond, summarise, V_VTU = cor(V_old, VTU), V_RU = cor(V_old, RU_old), VTU_RU = cor(VTU, RU_old))

our_cor$source <- "session1"

fan_cor$source <- "fan"

cors <- rbind(our_cor, fan_cor)

cors <- pivot_longer(cors, cols = grep("V", colnames(cors)), names_to = "predictors", values_to = "cor")

ggplot(cors, aes(trial, cor, color = predictors)) + geom_point()+
  geom_line()+
  facet_grid(cols = vars(source), rows = vars(cond))




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

session <- 1
set.seed(123)
#### parameters

V = seq(-4, 0, 1)
RU = seq(-3, 3, 1)
Horizon = seq(-3,3,1)
VH = seq(-3,3,1)
RUH = seq(-3,3,1)

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

load(sprintf("task/rewardsHorizon%i.Rda", session))

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



fixed <- jsonlite::fromJSON(sprintf("task/fixedChoices%i.json", session))# rounds incl practice * fixed choices matrix

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
    
    simdat <- subset(horizon, !is.na(chosen)) 
    
    
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
    
    # calculate proportion of best choices
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


ggplot(pars, aes(V,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(high = "#66C2A5", low = "#FC8D62")

ggplot(pars, aes(RU,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(high = "#66C2A5", low = "#FC8D62")

ggplot(pars, aes(VH,RUH, fill = reward)) + geom_raster() + scale_fill_gradient(high = "#66C2A5", low = "#FC8D62")


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

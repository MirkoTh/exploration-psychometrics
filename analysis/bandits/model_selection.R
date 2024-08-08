################## Model selection ##########

rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(here)
theme_set(theme_classic(base_size = 14))

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


############### Horizon task ##########

horizon_1 <- load_and_prep_bandit_data(1)$horizon
horizon_2 <- load_and_prep_bandit_data(2)$horizon

###### Hierachical Bayesian Implementation of Standard Wilson model 

res_list_5 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)
res_list_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)

dfs <- list(short = res_list_5[[2]], long = res_list_10[[2]])

hf_params <- dfs %>% bind_rows(.id = 'horizon')
hf_params$ID <- parse_number(rownames(hf_params))
hf_params <- subset(hf_params, select = c("predictor", "estimate", "horizon", "ID"))

####### subject-level implementation of Standard Wilson model

subj_level_5 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], 
                                  model = "Wilson",
                                  bayesian = F,
                                  full = T,
                                  save = F,
                                  use_saved = F,
                                  no_horizon = T)

subj_level_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], 
                                  model = "Wilson",
                                  bayesian = F,
                                  full = T,
                                  save = F,
                                  use_saved = F,
                                  no_horizon = T)

short <- subj_level_5[[2]]
long <- subj_level_10[[2]]

convergedIDs <- intersect(unique(short$ID[short$converged]), unique(long$ID[long$converged]))

dfs <- list(short = short, long = long)

sl_params <- dfs %>% bind_rows(.id = 'horizon')
sl_params <- sl_params %>% relocate(horizon, .after = last_col())

sl_params <- subset(sl_params, is.element(ID, convergedIDs), select = -converged)

sl_params <- pivot_longer(sl_params, cols = 1:(ncol(sl_params)-2),names_to = "predictor", values_to = "estimate")

hf_params <- subset(hf_params, is.element(ID, convergedIDs))

sl_params$subject_level <- sl_params$estimate
params <- subset(sl_params,predictor != "X.Intercept.", select = -estimate)

hf_params <- subset(hf_params, predictor !="Intercept")
params$hierarchical <- hf_params$estimate[match(paste(params$ID, params$predictor, params$horizon), paste(hf_params$ID, hf_params$predictor, hf_params$horizon))]


######### plot correlation between subject-level estimates (T1)

# Calculate correlations for each subset
params_cor <- params %>%
  group_by(predictor, horizon) %>%
  summarize(cor = cor(subject_level, hierarchical))

# Merge correlations back to original data
params <- left_join(params, params_cor, by = c("predictor", "horizon"))


lims = c(min(c(params$subject_level, params$hierarchical)), max(c(params$subject_level, params$hierarchical)))

# Plot
ggplot(params, aes(subject_level, hierarchical)) + 
  geom_jitter(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  coord_cartesian(xlim = lims, ylim = lims) + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("cor = %.2f", cor)), hjust = "inward", vjust = "inward") +
  facet_grid(cols = vars(predictor), rows = vars(horizon))+
  theme(strip.background = element_rect(fill = "white")) 


############## compare reliability ########

res_list_5 <- fit_model_horizon(horizon_2[horizon_2$Horizon == -0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)
res_list_10 <- fit_model_horizon(horizon_2[horizon_2$Horizon == 0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)

dfs <- list(short = res_list_5[[2]], long = res_list_10[[2]])

hf_params2 <- dfs %>% bind_rows(.id = 'horizon')
hf_params2$ID <- parse_number(rownames(hf_params2))
hf_params2 <- subset(hf_params2, select = c("predictor", "estimate", "horizon", "ID"))

## combine the two sessions

hf_params <- hf_params %>% rename(session1 = estimate)
hf_params2 <- hf_params2 %>% rename(session2 = estimate)

hf_params_all <- hf_params  %>% full_join(hf_params2, by = c("predictor", "horizon", "ID"))


####### subject-level implementation of Standard Wilson model

subj_level_5 <- fit_model_horizon(horizon_2[horizon_2$Horizon == -0.5, ], 
                                  model = "Wilson",
                                  bayesian = F,
                                  full = T,
                                  save = F,
                                  use_saved = F,
                                  no_horizon = T)

subj_level_10 <- fit_model_horizon(horizon_2[horizon_2$Horizon == 0.5, ], 
                                   model = "Wilson",
                                   bayesian = F,
                                   full = T,
                                   save = F,
                                   use_saved = F,
                                   no_horizon = T)

short <- subj_level_5[[2]]
long <- subj_level_10[[2]]

convergedIDs <- intersect(unique(short$ID[short$converged]), unique(long$ID[long$converged]))

dfs <- list(short = short, long = long)

sl_params2 <- dfs %>% bind_rows(.id = 'horizon')
sl_params2 <- sl_params2 %>% relocate(horizon, .after = last_col())

sl_params2 <- subset(sl_params2, is.element(ID, convergedIDs), select = -converged)

sl_params2 <- pivot_longer(sl_params2, cols = 1:(ncol(sl_params2)-2),names_to = "predictor", values_to = "session2")

sl_params <- sl_params %>% rename(session1 = estimate)

sl_params_all <- hf_params  %>% full_join(sl_params2, by = c("predictor", "horizon", "ID"))

dfs <- list(sl = sl_params_all, hb = hf_params_all)

params <- dfs %>% bind_rows(.id = 'method')

params <- subset(params, is.element(ID, convergedIDs) & !grepl("intercept", predictor, ignore.case = T))


cors <- params %>%
  group_by(method, predictor, horizon) %>%
  summarize(correlation = cor(session1, session2, use = "pairwise.complete.obs"))



################# are the fixed effects consistent ? ########


##### hierarchical bayesian fixed effects

hb_fixed_5 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)[[1]]
hb_fixed_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson", full = T, it = 8000, save = T, use_saved = T, no_horizon = T)[[1]]

short <- summary(hb_fixed_5)$fixed %>% rename(lower = "l-95% CI", upper = "u-95% CI")
short$predictor <- rownames(short)
long <- summary(hb_fixed_10)$fixed %>% rename(lower = "l-95% CI", upper = "u-95% CI")
long$predictor <- rownames(long)

hb <- list(short = short, long = long) %>% bind_rows(.id = "horizon")

# this is coded weirdly so I will recode it to be higher number = more value seeking and more information seeking
hb$Estimate[hb$predictor == "delta_mean"] <- -1 * hb$Estimate[hb$predictor == "delta_mean"]
hb$lower[hb$predictor == "delta_mean"] <- -1 * hb$lower[hb$predictor == "delta_mean"]
hb$upper[hb$predictor == "delta_mean"] <- -1 * hb$upper[hb$predictor == "delta_mean"]


##### subject-level effects

sl_5 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson",bayesian = F, full = T, save = F, use_saved = F, no_horizon = T)[[2]]
sl_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson",bayesian = F, full = T, save = F, use_saved = F, no_horizon = T)[[2]]

convergedIDs <- intersect(sl_5$ID[sl_5$converged], sl_10$ID[sl_10$converged])

sl_by_subject <- list(short = sl_5, long = sl_10) %>% bind_rows(.id = "horizon")

sl_by_subject <- pivot_longer(sl_by_subject, cols = -c("ID", "converged", "horizon"), names_to = "predictor", values_to = "estimate")

sl_by_subject <- subset(sl_by_subject, is.element(ID, convergedIDs), -c(converged))

# this is coded weirdly so I will recode it to be higher number = more value seeking and more information seeking
sl_by_subject$estimate[sl_by_subject$predictor == "delta_mean"] <- -1 * sl_by_subject$estimate[sl_by_subject$predictor == "delta_mean"]

sl <- plyr::ddply(sl_by_subject, ~predictor+horizon, summarise, lower = mean(estimate) - 1.96*se(estimate), upper = mean(estimate) + 1.96*se(estimate), Estimate = mean(estimate))


fixed <- list(hb = subset(hb, select = c("horizon", "predictor", "Estimate", "lower", "upper")), sl = sl) %>% bind_rows(.id = "method")


fixed <- subset(fixed, !grepl("intercept", predictor, ignore.case = T))

ggplot(fixed, aes(predictor, Estimate, fill = horizon)) + geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.9), width = 0.25)+
  facet_wrap(vars(method)) + scale_fill_manual(values = c("#66C2A5", "#FC8D62"))




################## model comparison with second session as held-out set #################

############# Horizon task #############

## subsample to only use the participants where session 1 converged in the subject-level models for both the long and the short horizon

sl1_05 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson", bayesian = F, full = T,no_horizon = T, save = F, use_saved = F)[[2]]
sl1_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson", bayesian = F, full = T,no_horizon = T, save = F, use_saved = F)[[2]]

convergedIDs <- intersect(sl1_05$ID[sl1_05$converged], sl1_10$ID[sl1_10$converged])

horizon_2 <- subset(horizon_2, is.element(ID, convergedIDs))
horizon_2 <- subset(horizon_2, is.element(ID, horizon_1$ID))

length(unique(horizon_2$ID)) # only 128 of the subjects that did session two had their models converge in session 1 in both horizons
####### hierarchical bayesian

### get model fit to session 1

hb1_05 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson", bayesian = T, full = T,no_horizon = T, save = T, use_saved = T)[[1]]
hb1_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson", bayesian = T, full = T,no_horizon = T, save = T, use_saved = T)[[1]]

### predict responses on session 2

simdat <- subset(horizon_2, trial == 5)

## get log_lik for each sample of MCMC and transform to average log lik as described by Alireza

h5 <- log_lik(hb1_05, newdata = subset(simdat, Horizon == -0.5))
# average over all observations (keep 1 datapoint per MCMC sample)
h5 <- rowSums(h5)
get_avg_loglik <- function(obs) {
  logp <- max(obs) + log(sum(exp(obs-max(obs)))) - log(length(obs))
}

logp_5 <- get_avg_loglik(h5)
# repeat for long horizon
h10 <- rowSums(log_lik(hb1_10, newdata = subset(simdat, Horizon == 0.5)))
logp_10 <- get_avg_loglik(h10)

hb <- data.frame(Horizon = c(5, 10),
                 log_lik = c(logp_5, logp_10))

############### subject-level 

# get model fit to session 1

sl1_05 <- fit_model_horizon(horizon_1[horizon_1$Horizon == -0.5, ], model = "Wilson", bayesian = F, full = T,no_horizon = T, save = F, use_saved = F)[[1]]
sl1_10 <- fit_model_horizon(horizon_1[horizon_1$Horizon == 0.5, ], model = "Wilson", bayesian = F, full = T,no_horizon = T, save = F, use_saved = F)[[1]]

session1IDs <- unique(horizon_1$ID)

log_lik_collect_5 <- c()
log_lik_collect_10 <- c()

for (id in unique(horizon_2$ID)){
  
  # horizon 5
  model <-sl1_05[[which(session1IDs == id)]]
  newdata <- subset(horizon_2, ID == id & Horizon == -0.5&trial == 5)
  newdata$prob <- predict(model, newdata = newdata, type = "response")
  
  # Calculate log-likelihood
  log_lik <- sum(log(newdata$prob*newdata$chosen + (1-newdata$prob)*(1-newdata$chosen)))
  
  log_lik_collect_5 <- c(log_lik_collect_5, log_lik)
  
  # horizon 10
  model <-sl1_10[[which(session1IDs == id)]]
  newdata <- subset(horizon_2, ID == id & Horizon == 0.5&trial == 5)
  newdata$prob <- predict(model, newdata = newdata, type = "response")
  
  # Calculate log-likelihood
  log_lik <- sum(log(newdata$prob*newdata$chosen + (1-newdata$prob)*(1-newdata$chosen)))
  
  log_lik_collect_10 <- c(log_lik_collect_10, log_lik)
  
}
sl <- data.frame(Horizon = c(5,10),
                 log_lik = c(sum(log_lik_collect_5), sum(log_lik_collect_10)))

log_liks <- list(hb = hb, sl = sl) %>% bind_rows(.id = "method")

##################### Sam's task #############

########## model comparison for hybrid vs ucb in predicting session 2 from session 1 #######
sam_1 <- load_and_prep_bandit_data(1)$sam
sam_2 <- load_and_prep_bandit_data(2)$sam

ucb <- fit_model_sam(data = sam_1, model = "UCB", hierarchical = T, use_saved = T)[[1]]
hybrid <- fit_model_sam(data = sam_1, model = "hybrid", hierarchical = T, use_saved = T)[[1]]


sam_2 <- subset(sam_2, is.element(ID, sam_1$ID))

# the sum(-1*log(prob)) route yields Inf so I need to go the difficult route

get_avg_loglik <- function(obs) {
  logp <- max(obs) + log(sum(exp(obs-max(obs)))) - log(length(obs))
}

logp_ucb <- rowSums(log_lik(ucb, newdata = sam_2)) # average over all observations (keep 1 datapoint per MCMC sample)

logp_u <- get_avg_loglik(logp_ucb)
# repeat for long horizon
logp_hybrid <- rowSums(log_lik(hybrid, newdata = sam_2))
logp_h <- get_avg_loglik(logp_hybrid)

print(sprintf("log likelihood UCB: %.2f log likelihood hybrid: %.2f", logp_u, logp_h))


########## comparing the test-retest reliability of the model parameters ########

# get model parameters
ucb_1 <- fit_model_sam(data = sam_1, model = "UCB", hierarchical = T, use_saved = T)[[2]]
hybrid_1 <- fit_model_sam(data = sam_1, model = "hybrid", hierarchical = T, use_saved = T)[[2]]

ucb_2 <- fit_model_sam(data = sam_2, model = "UCB", hierarchical = T, use_saved = T)[[2]]
hybrid_2 <- fit_model_sam(data = sam_2, model = "hybrid", hierarchical = T, use_saved = T)[[2]]

ucb_1$ID <- parse_number(rownames(ucb_1))
ucb_2$ID <- parse_number(rownames(ucb_2))
hybrid_1$ID <- parse_number(rownames(hybrid_1))
hybrid_2$ID <- parse_number(rownames(hybrid_2))

ucb <- ucb_1 %>% rename(estimate_1 = estimate) %>% left_join(ucb_2, by = c("ID", "predictor"))
hybrid <- hybrid_1 %>% rename(estimate_1 = estimate) %>% left_join(hybrid_2, by = c("ID", "predictor"))

# get test-retest reliability

ucb_cors <- ucb %>% group_by(predictor) %>% summarise(cor = cor(estimate_1, estimate, use = "pairwise.complete.obs"))
ucb_cors

hybrid_cors <- hybrid %>% group_by(predictor) %>% summarise(cor = cor(estimate_1, estimate, use = "pairwise.complete.obs"))
hybrid_cors

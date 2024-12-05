############ compiling the outputs from the computational modelling ################


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


############ original modelling approaches from the literature ############

######## 2AB

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
  mutate(estimate = -1*estimate,
         ID = parse_number(rownames(.))) # already flipped to be larger number more seeking

sam_fixed <- fixed %>% bind_rows(.id = "session") %>%
  mutate(Estimate = -1*Estimate,
         low = -1*`u-95% CI`,
         `u-95% CI` = -1*`l-95% CI`,
         `l-95% CI`= low) %>%
  subset(select = -low) # when recoding this here we have to flip upper and lower CI, need temporary variable to avoid recoded versions influencing each other
split_vec <- strsplit(rownames(sam_fixed), "\\...")
sam_fixed <- sam_fixed %>% 
  mutate(predictor = sapply(split_vec, '[', 1)) %>% 
  select(session, predictor, Estimate, `u-95% CI`, `l-95% CI`)



HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}


## Horizon
params <- list()
samples <- list()
for (s in 1:2){
  
  p <- list()
  samp <- list()
  for (h in c(-0.5, 0.5)){
    
    data <- load_and_prep_bandit_data(s)$horizon
    out <- fit_model_horizon(data[data$Horizon == h, ], model = "Wilson", bayesian = T, full = T, no_horizon = T, use_saved = T)
    
    # inspect the model to ensure everything is nicely converged and stuff
    
    print(out[[1]])
    p <- append(p, list(out[[2]]))
    t <- out[[1]]
    samp <- append(samp, list(as.data.frame(posterior_samples(t, pars = "b_"))))
    
  }
  
  # subject-level parameters
  params[[s]] <- p %>% bind_rows(.id = "horizon") %>% 
    mutate(horizon = recode(horizon, `1` = "short", `2` = "long"),
           ID = parse_number(rownames(.))) %>% 
    pivot_wider(id_cols = c("ID", "predictor"), names_from = "horizon", values_from = "estimate") %>% 
    mutate(estimate = long - short) %>% 
    subset(select = -c(long, short))
  
  # posterior samples of fixed effects
  
  samples[[s]] <- left_join(samp[[1]] %>% mutate(ind = 1:nrow(.)),
                            samp[[2]] %>% mutate(ind = 1:nrow(.)), by = "ind") %>% 
    mutate(Intercept = b_Intercept.y - b_Intercept.x,# subtract posterior samples of short horizon from long
           delta_mean = b_delta_mean.y - b_delta_mean.x,
           info = b_info.y - b_info.x) %>% 
    select(Intercept, delta_mean, info) %>% 
    pivot_longer(cols = (1:3), names_to = "predictor", values_to = "estimate") %>% 
    group_by(predictor) %>% 
    summarise(Estimate = mean(estimate),
              `l-95% CI` = HDIofMCMC(estimate)[1],
              `u-95% CI` = HDIofMCMC(estimate)[2])
  
}

horizon_params <- params %>% bind_rows(.id = "session")
horizon_fixed <- samples %>% bind_rows(.id = "session")

## combine
params <- list(sam = sam_params, horizon = horizon_params) %>% 
  bind_rows(.id = "task")%>% 
  subset(select = c("predictor", "estimate", "task", "session","ID"))

### restless

# extract subject-level
restless <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")

rest_params <- restless %>% 
  pivot_longer(cols = -ID, values_to = "estimate", names_to = "predictor") %>% 
  mutate(session = as.character(parse_number(predictor)),
         predictor = ifelse(grepl("ru", predictor), "RU", "V"),
         task = "restless")

# extract fixed
fixed <- list()
for (s in c(1,2)){
  fixed[[s]] <- readRDS(sprintf("analysis/bandits/restless-hierarchical-model-posterior-s%i.RDS", s)) %>% 
    select(mu_beta, mu_tau) %>% 
    pivot_longer(cols = c(1:2), names_to = "predictor", values_to = "estimate") %>% 
    group_by(predictor) %>% 
    summarise(Estimate = mean(estimate),
              `l-95% CI` = HDIofMCMC(estimate)[1],
              `u-95% CI` = HDIofMCMC(estimate)[2])
  
}

rest_fixed <- fixed %>% 
  bind_rows(.id = "session")%>% 
  mutate(predictor = recode(predictor, "mu_tau" = "V", "mu_beta" = "RU"))

## combine all

params <- list(params, rest_params) %>% 
  bind_rows()

fixed <- list("horizon" = horizon_fixed, "sam" = sam_fixed, "restless" = rest_fixed) %>% 
  bind_rows(.id = "task")

## save
write.csv(params,"analysis/bandits/AllModelParameters.csv")
write.csv(fixed, "analysis/bandits/AllFixedEffects.csv")

saveRDS(params, file ="analysis/bandits/allParams.rds")
saveRDS(fixed, fil = "analysis/bandits/allFixed.rds")


############### improved modelling ##############

### 2AB

params <- list()
fixed <- list()
for (s in 1:2){
  
  data <- load_and_prep_bandit_data(s)$sam
  out <- fit_model_sam(data, model = "UCB", hierarchical = T, use_saved = T)
  
  # inspect the model to ensure everything is nicely converged and stuff
  
  print(out[[1]])
  params[[s]] <- out[[2]]
  fixed[[s]] <- as.data.frame(summary(out[[1]])$fixed)
  
}

sam_params <- params %>% bind_rows(.id = "session") %>% 
  mutate(estimate = -1*estimate,
         ID = parse_number(rownames(.))) # already flipped to be larger number more seeking
sam_fixed <- fixed %>% bind_rows(.id = "session") %>%
  mutate(Estimate = -1*Estimate,
         low = -1*`u-95% CI`,
         `u-95% CI` = -1*`l-95% CI`,
         `l-95% CI`= low) %>%
  subset(select = -low)# when recoding this here we have to flip upper and lower CI, need temporary variable to avoid recoded versions influencing each other
split_vec <- strsplit(rownames(sam_fixed), "\\...")
sam_fixed <- sam_fixed %>% 
  mutate(predictor = sapply(split_vec, '[', 1)) %>% 
  select(session, predictor, Estimate, `u-95% CI`, `l-95% CI`)

## Horizon
params <- list()
fixed <- list()
for (s in 1:2){
  
  p <- list()
  
  h <- 0.5
  data <- load_and_prep_bandit_data(s)$horizon
  out <- fit_model_horizon(data[data$Horizon == h, ], model = "Wilson", bayesian = T, full = T, no_horizon = T, use_saved = T)
  
  # inspect the model to ensure everything is nicely converged and stuff
  
  print(out[[1]])
  p <- out[[2]]
  
  
  params[[s]] <- p %>% 
    mutate(ID = parse_number(rownames(.)))
  
  fixed[[s]] <- as.data.frame(summary(out[[1]])$fixed)
  
}

horizon_params <- params %>% bind_rows(.id = "session") %>% 
  mutate(estimate = if_else(predictor == "delta_mean", -1 * estimate, estimate)) # here we recode only for reward

horizon_fixed <- fixed %>% bind_rows(.id = "session") %>%
  mutate(row_names = rownames(.),
         Estimate = if_else(str_detect(row_names, "delta_mean"), -1 * Estimate, Estimate),
         low = if_else(str_detect(row_names, "delta_mean"), -1 * `u-95% CI`, `l-95% CI`), # this evaluates one after the other so need intermediate variable to avoid one row influencing the next
         `u-95% CI` = if_else(str_detect(row_names, "delta_mean"), -1 * `l-95% CI`, `u-95% CI`),
         `l-95% CI` = low) 
split_vec <- strsplit(rownames(horizon_fixed), "\\...")
horizon_fixed <- horizon_fixed %>% 
  mutate(predictor = sapply(split_vec, '[', 1)) %>% 
  select(session, predictor, Estimate, `u-95% CI`, `l-95% CI`)

## combine
params <- list(sam = sam_params, horizon = horizon_params) %>% 
  bind_rows(.id = "task")%>% 
  subset(select = c("predictor", "estimate", "task", "session","ID"))

### restless

restless <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")

rest_params <- restless %>% 
  pivot_longer(cols = -ID, values_to = "estimate", names_to = "predictor") %>% 
  mutate(session = as.character(parse_number(predictor)),
         predictor = ifelse(grepl("ru", predictor), "RU", "V"),
         task = "restless")

# extract fixed
fixed <- list()
for (s in c(1,2)){
  fixed[[s]] <- readRDS(sprintf("analysis/bandits/restless-hierarchical-model-posterior-s%i.RDS", s)) %>% 
    select(mu_beta, mu_tau) %>% 
    pivot_longer(cols = c(1:2), names_to = "predictor", values_to = "estimate") %>% 
    group_by(predictor) %>% 
    summarise(Estimate = mean(estimate),
              `l-95% CI` = HDIofMCMC(estimate)[1],
              `u-95% CI` = HDIofMCMC(estimate)[2])
  
}

rest_fixed <- fixed %>% 
  bind_rows(.id = "session") %>% 
  mutate(predictor = recode(predictor, "mu_tau" = "V", "mu_beta" = "RU"))

## combine all

params <- list(params, rest_params) %>% 
  bind_rows()

fixed <- list("horizon" = horizon_fixed, "sam" = sam_fixed, "restless" = rest_fixed) %>% 
  bind_rows(.id = "task")


## save
write.csv(params,"analysis/bandits/AllModelParameters_improved.csv")
write.csv(fixed, "analysis/bandits/AllFixedEffects_improved.csv")

saveRDS(params, file ="analysis/bandits/allParams_improved.rds")
saveRDS(fixed, fil = "analysis/bandits/allFixed_improved.rds")

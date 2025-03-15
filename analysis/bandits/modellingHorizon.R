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



################### simulating and with OG implementation and recoving with ours #############


OG_model <- function(df, info_weight, temp, side_bias){
  
  p_a <- 1/(1+exp((df$delta_mean + info_weight*df$info + side_bias)/temp))
  
  return(p_a)
  
}

info_weights <- seq(-2, 2, 0.4)
temps <- seq(0.1, 2, 0.4)
side_biases <- seq(-1,1,0.4)

params_df <- data.frame(info_weight = rep(info_weights, length(temps)*length(side_biases)),
                        temp = rep(rep(temps, each = length(info_weights)), length(side_biases)),
                        side_bias = rep(side_biases, each = length(info_weights)*length(temps))) 

table(params_df)
params_df$ID <- 1:nrow(params_df)

get_choices <- function(df, params, horizon){
  
  df <- subset(df, Horizon == horizon & trial == 5)
  
  for (i in 1:nrow(params)){
    
    temp <- df
    temp$ID <- i
    temp$info_weight <- params$info_weight[i]
    temp$temp <- params$temp[i]
    temp$side_bias <- params$side_bias[i]
    temp$chosen_p <- NA
    
    for (choice in 1:nrow(df)){
      temp$chosen_p[choice] <- OG_model(temp[choice, ], info_weight = params$info_weight[i], 
                                      temp = params$temp[i], side_bias = params$side_bias[i])
      
      temp$chosen[choice] <- ifelse(runif(1) > temp$chosen_p[choice], 0,1)
    }
    
    if(i == 1){
      out_df <- temp
    } else {
      out_df <- rbind(out_df, temp)
    }
    
  }
  
  return(out_df)
  
}

long_h <- get_choices(horizon[horizon$ID == 2, ], params_df, horizon = 0.5)

short_h <- get_choices(horizon[horizon$ID==2, ], params_df, horizon = -0.5)

out_long <- fit_model_horizon(data = long_h, model = "Wilson", bayesian = T, full = T, it = 2000, no_horizon = T, no_intercept = F, use_saved = F, save = F)

out_short <- fit_model_horizon(data = short_h, model = "Wilson", bayesian = T, full = T, it = 2000, no_horizon = T, no_intercept = F, use_saved = F, save = F)

recovs <- out_short[[2]] %>% 
  bind_rows(out_long[[2]], .id = "Horizon") %>% 
  mutate(ID = parse_number(rownames(.))) %>% 
  left_join(params_df, by = "ID") %>% 
  select(-`colMeans(as.data.frame(posterior_samples(baymodel)))`) %>% 
  pivot_wider(id_cols = c(ID, Horizon, info_weight, side_bias, temp), names_from = predictor, values_from = estimate)


ggplot(recovs, aes(info_weight, info, color = Horizon)) + geom_jitter(alpha = 0.3)+
  geom_smooth(method = "lm") + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("r = %.2f", cor(info_weight, info))), hjust = "inward", vjust = "inward")

ggplot(recovs, aes(side_bias, Intercept, color = Horizon)) + geom_jitter(alpha = 0.3)+
  geom_smooth(method = "lm") + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("r = %.2f", cor(side_bias, Intercept))), hjust = "inward", vjust = "inward")

ggplot(recovs, aes(temp, delta_mean, color = Horizon)) + geom_jitter(alpha = 0.3)+
  geom_smooth(method = "lm") + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("r = %.2f", cor(temp, delta_mean))), hjust = "inward", vjust = "inward")





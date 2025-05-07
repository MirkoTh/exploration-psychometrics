#################### what are optimal parameter estimates in the bandits for our reward sets? ################


rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(here)
library(ggpubr)
theme_set(theme_classic(base_size = 14))
here()

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}



set.seed(123)

################ Horizon task ##################
#### parameters
n_steps = 15

delta_mean = seq(-15, 0, length.out = n_steps)
info = seq(-5, 5, length.out = n_steps)
intercept = seq(-2,2,length.out = n_steps)

Nsims = 10

pars = data.frame(ID = rep(1:(n_steps^3), each = 2),
                  horizon = rep(c(-0.5, 0.5), n_steps^3),
                  delta_mean = rep(delta_mean, each = (n_steps^2)*2),
                  info = rep(info, each = n_steps*2),
                  intercept = rep(rep(intercept, each = 2), n_steps^2),
                  regret = NA,
                  se = NA)



pars = list(pars,pars,pars) %>% 
  bind_rows(.id = "session")

for (session in c(1,2,3)){# 3 means other reward set
  
  if (session < 3){
load(sprintf("task/rewardsHorizon%i.Rda", session))
  } else {
    
    rewards <- read.csv("analysis/bandits/ZjakowskiEtAl_TMS_horizonTask.csv") %>% 
      rename(Horizon = gameLength,
             ID = subjectID,
             block = game) %>% 
      mutate(info = -4*(rowMeans(select(., c1, c2, c3, c4)) - 1.5)) %>% 
      pivot_longer(cols = matches("^r\\d+$"),  # More robust column selection
                   names_to = "reward_trial", 
                   values_to = "reward") %>% 
      mutate(trial = parse_number(reward_trial))%>%
      rowwise() %>%
      mutate(
        choice = ifelse(trial == 1, c1, ifelse(
                        trial == 2, c2, ifelse(
                          trial == 3, c3, ifelse(
                            trial == 4, c4, ifelse(
                              trial == 5, c5, ifelse(
                                trial == 6, c6, ifelse(
                                  trial == 7, c7, ifelse(
                                    trial == 8, c8, ifelse(
                                      trial == 9, c9, c10 )   )  ) ))   ))))) %>%
      ungroup() %>% 
      mutate(reward1 = ifelse(choice == 1, reward, m1),
             reward2 = ifelse(choice == 2, reward, m2),
             chosen = choice-1)
  }

horizon <- rewards
horizon$Horizon <- ifelse(horizon$Horizon == 5, -0.5, 0.5)
horizon$info <- horizon$info/2

if (session < 3){
  
  horizon$ID = 1
horizon$reward <- NA
fixed <- jsonlite::fromJSON(sprintf("task/fixedChoices%i.json", session))# rounds incl practice * fixed choices matrix
horizon$chosen <- 99 # need this to be non na
horizon$chosen[horizon$Horizon == 5 & horizon$trial > 5] <- NA
for (i in 1:nrow(fixed)){

  
  horizon$chosen[horizon$block == i & horizon$trial < 5] <- fixed[i, ] 
  horizon$reward[horizon$block == i & horizon$trial < 5] <- ifelse(horizon$chosen[horizon$block == i & horizon$trial < 5] == 0, 
                                                                   horizon$reward1[horizon$block == i & horizon$trial < 5], 
                                                                   horizon$reward2[horizon$block == i& horizon$trial < 5])
  
  
  
  
}
}
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

######## test often which option seems better changes after exploring the less explored arm in the first free choice

df <- horizon %>% 
  mutate(best = ifelse(mean_L > mean_R, 0,1)) %>% 
  subset(info != 0 & Horizon == 0.5) 

# choose the less chosen one on each round

df$chosen[df$trial == 5] <- ifelse(df$info[df$trial == 5] == -0.5, 0, 1)


# update means according to this new information
df <- df %>% 
  rowwise() %>% 
  mutate(reward = ifelse(chosen == 0, reward1, reward2)) %>% 
  group_by(ID, block) %>%
  mutate(
    mean_L = if_else(trial == 5,
                     mean(reward[trial < 6 & chosen == 0], na.rm = TRUE),
                     NA_real_
    ),
    mean_R = if_else(trial == 5,
                     mean(reward[trial < 6 & chosen == 1], na.rm = TRUE),
                     NA_real_
    )
  ) %>% 
  ungroup() %>% 
  mutate(new_best = ifelse(mean_L > mean_R, 0, 1))


df$new_best <- ifelse(df$mean_L > df$mean_R, 0, 1)

table(df$new_best == df$best)/sum(table(df$new_best == df$best))

########################

for (i in unique(pars$ID)){
#for (i in 1){
  
  if (session < 3){
    id = 1
  } else {
    id = sample(unique(horizon$ID), size = 1)
  }
#for (i in 1){
  ## little progress bar
  
  if(i %% 50 == 0){print(paste(i, "of", length(unique(pars$ID))))}
  
  for (ho in c(-0.5, 0.5)){
  
  temp <- rep(NA, Nsims)
  
   p <- pars[pars$ID == i & pars$horizon == ho & pars$session == session, ]
  
  for (k in 1:Nsims){
    
    simdat <- subset(horizon, !is.na(chosen) & Horizon == ho&ID == id) 
    
    simdat$row <- 1:nrow(simdat)
    
    simdat$chosen[simdat$trial > 4] <- NA
    
    simdat$chosen[simdat$trial == 5] <- p$intercept+ 
      p$delta_mean * simdat$delta_mean[simdat$trial == 5] + 
      p$info * simdat$info[simdat$trial == 5]
    # logistic function
    simdat$chosen[simdat$trial == 5] <- 1/(1+exp(-simdat$chosen[simdat$trial == 5]))
    
    simdat$chosen[simdat$trial == 5] <- ifelse(simdat$chosen[simdat$trial == 5] < runif(nrow(simdat[simdat$trial ==5, ])), 0, 1)
    
    
    # update delta_mean for trial 6 based on what was chosen on first free choice (5) such that can then exploit based on that outcome
    for (j in simdat$row[simdat$trial == 6]){
      simdat$mean_L[simdat$row == j] <-apply(as.array(simdat$row[simdat$row == j]), 1, function(x) meann(simdat$reward[simdat$ID == simdat$ID[x]&
                                                                                             simdat$block == simdat$block[x] &
                                                                                             simdat$chosen == 0 & 
                                                                                             simdat$trial < 6]))
      simdat$mean_R[simdat$row == j] <-apply(as.array(simdat$row[simdat$row == j]), 1, function(x) meann(simdat$reward[simdat$ID == simdat$ID[x]&
                                                                                                          simdat$block == simdat$block[x] &
                                                                                                          simdat$chosen == 1 & 
                                                                                                          simdat$trial < 6]))
    }
    
    
    ## make it choose optimally after trial 5
    simdat$chosen[simdat$trial  == 6] <- ifelse(simdat$mean_L[simdat$trial == 6] > simdat$mean_R[simdat$trial == 6], 0, 1)
    
    simdat$chosen[simdat$trial > 6] <- rep(simdat$chosen[simdat$trial == 6], each = 4)
    simdat$reward <- ifelse(simdat$chosen == 0, simdat$reward1, simdat$reward2)
    
    # calculate regret
    # simdat$bestR <- ifelse(simdat$reward1 > simdat$reward2, simdat$reward1,simdat$reward2)
    # simdat$reward <- simdat$bestR - simdat$reward 
    
    #temp[k] = mean(simdat$reward[simdat$trial > 4])
    temp[k] = sum(simdat$reward[simdat$trial > 4])
    
  }
  
  pars$regret[pars$ID == i & pars$horizon == ho & pars$session == session] <- mean(temp)
  pars$se[pars$ID == i & pars$horizon == ho & pars$session == session] <- se(temp)
  
  }
  
}

}


hist(pars$regret)

df <- pars %>% 
  group_by(delta_mean, info, horizon, session) %>% 
  summarise(regret = mean(regret)) %>% 
  mutate(horizon = factor(horizon, levels = c(-0.5, 0.5),
                                 labels = c("short", "long")),
          delta_mean = -1*delta_mean,
         info = -1*info,
         session = factor(session, levels = c(1,2,3),
                          labels = c("Session 1", "Session 2", "Zajkowski et al. 2017")))

grid <- ggplot(df, aes(delta_mean, info, fill = regret)) + geom_raster() + scale_fill_viridis_c(guide = "none") +
  facet_grid(vars(horizon), vars(session)) +
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = "value-guided",
       y = "directed")

grid
# delta_mean_df <- pars %>% 
#   group_by(delta_mean, horizon) %>% 
#   summarise(outcome = mean(regret), se = se(regret)) %>% 
#   rename(estimate = delta_mean)
# 
# info_df <- pars %>% 
#   group_by(info, horizon) %>% 
#   summarise(outcome = mean(regret), se = se(regret)) %>% 
#   rename(estimate = info)
# 
# intercept_df <- pars %>% 
#   group_by(intercept, horizon) %>% 
#   summarise(outcome = mean(regret), se = se(regret)) %>% 
#   rename(estimate = intercept)
# 
# df <- list(delta_mean = delta_mean_df, info = info_df, intercept = intercept_df) %>% 
#   bind_rows(.id = "param") %>% 
#   mutate(param = recode(param, "delta_mean" = "value-guided",
#                         "info" = "directed"),
#          horizon = factor(horizon, levels = c(-0.5, 0.5),
#                           labels = c("short", "long"))) %>% 
#   subset(param != "intercept")



df <- pars %>% 
  pivot_longer(cols = c(delta_mean, info, intercept), names_to = "param", values_to = "estimate") %>% 
  mutate(param = recode(param, "delta_mean" = "value-guided",
                        "info" = "directed"),
         horizon = factor(horizon, levels = c(-0.5, 0.5),
                          labels = c("short", "long")),
         estimate = -1*estimate) %>% # recode in the same way we do for all other paper plots (to aid interpretability) 
  subset(param != "intercept")

## add in participant parameters
for (session in c(1,2)){
  load(sprintf("analysis/bandits/modellingResults/fitHorizonSession%iWilson_full_horizon5only.Rda", session))
  short_pp <- trueParams
  load(sprintf("analysis/bandits/modellingResults/fitHorizonSession%iWilson_full_horizon10only.Rda", session))
  long_pp <- trueParams
  
  if (session == 1){
    
  
  pp <- list("short" = short_pp, "long" = long_pp) %>% 
    bind_rows(.id = "horizon") %>% 
    select(horizon, predictor, estimate) %>% 
    group_by(predictor, horizon) %>%
    summarise(se = sd(estimate),
              estimate = mean(estimate)) %>%
    mutate(param = recode(predictor, "delta_mean" = "value-guided",
                          "info" = "directed")) %>% 
    subset(param != "Intercept", select = -predictor)
  } else {
    ppp <- list("short" = short_pp, "long" = long_pp) %>% 
      bind_rows(.id = "horizon") %>% 
      select(horizon, predictor, estimate) %>% 
      group_by(predictor, horizon) %>%
      summarise(se = sd(estimate),
                estimate = mean(estimate)) %>%
      mutate(param = recode(predictor, "delta_mean" = "value-guided",
                            "info" = "directed")) %>% 
      subset(param != "Intercept", select = -predictor)
    
    pp <- list(pp, ppp) %>% 
      bind_rows(.id = "session") %>% 
      mutate(estimate = estimate * -1)
    
  }
  
}
  
p1 <- ggplot(df, aes(estimate, regret))+ #geom_point()+geom_line()+
  #geom_linerange(aes(ymin = outcome -se, ymax = outcome +se))+
  geom_smooth(aes(color = session))+
  geom_point(data = pp, aes(estimate, 0, color = session)) +
  #geom_density(data = pp, aes(estimate, y = after_stat(density)), color = "#66C2A5", fill = "#66C2A5", alpha = 0.3)+
  geom_linerange(data = pp, aes(xmin = estimate - se, xmax = estimate + se, y = 0, color = session))+
  facet_grid(cols = vars(param), rows = vars(horizon), scales = "free")+
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom"
  ) +
  labs(y = "regret ±95%CI",
       x = "estimate (±SD)")+
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"), labels = c("session 1", "session 2", "Zajkowski et al. 2017"), 
                     name = element_blank())

p1

hor_plot <- ggarrange(grid, p1, ncol = 1, nrow = 2, heights = c(0.9,1))
hor_plot


ggsave(hor_plot, file = "figures/figures-ms/submission-1/SIoptimimalityHorizon.png",
       height = 8, width = 7)






####################### Two-armed bandit #########################


#### parameters

n_steps = 15

V = seq(-3, 0.1, length.out = n_steps)
RU = seq(-5, 1, length.out = n_steps)

Nsims = 15

pars = data.frame(V = rep(V, each = n_steps),
                  RU = rep(RU, n_steps),
                  reward = NA,
                  se = NA)

pars$ID <- 1:nrow(pars)
pars <- list(pars,pars,pars) %>% 
  bind_rows(.id = "session")





for (session in c(1,2,3)) {
  
if (session < 3) {
  load(sprintf("task/rewardsSam%i.Rda", session))
  rewards$ID <- 1
  rewards$chosen <- NA
  rewards$reward <- NA
  rewards <- get_KL_into_df(rewards)
} else {
  rewards <- read.csv("analysis/bandits/FanEtAlexp1_bandit_task_scale.csv") %>% 
    rename(ID = sub,
           KLM0 = est_m1,
           KLM1 = est_m2,
           KLV0 = est_s1,
           KLV1 = est_s2) %>% 
    mutate(chosen = NA,
           reward = NA,
           V = KLM0 - KLM1,
           RU = getRU(KLV0, KLV1))
}


sam <- rewards

print(paste("session: ", session))

for (i in unique(pars$ID)){
  
  temp <- rep(NA, Nsims)
  
  for (k in 1:Nsims){
    
    id <- ifelse(session < 3, 1, sample(unique(sam$ID), size = 1))
    simdat <- subset(sam, ID == id) # ID does not matter here, everyone observed the same fixed choices anyway
  
    simdat <- sim_data_sam(simdat, pars, i, bootstrapped = T, newRewards = F)
    
    # simdat$best <- ifelse(simdat$reward1 > simdat$reward2, 0, 1)
    # 
    # simdat$reward <- ifelse(simdat$chosen == simdat$best, 0, abs(simdat$reward1 - simdat$reward2))
    simdat$reward <- ifelse(simdat$chosen == 0, simdat$reward1, simdat$reward2)
    
    #temp[k] <- mean(simdat$reward)
    temp[k] <- sum(simdat$reward)
    
    
  }
  
  pars$reward[pars$ID == i & pars$session == session] <- meann(temp)
  pars$se[pars$ID == i & pars$session == session] <- se(temp)
  
}

}

df <- pars %>% 
  mutate(session = factor(session, levels = c(1,2,3),
                          labels = c("Session 1", "Session 2", "Fan et al. 2023")),
         V = -1*V,
         RU = -1 * RU)

grid <- ggplot(df, aes(V, RU, fill = reward)) + geom_raster() + scale_fill_viridis_c(guide = "none") + 
  #geom_text(aes(label = round(reward, digits = 2))) + 
  facet_grid(cols = vars(session))  +
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = "value-guided",
       y = "directed")

grid

df <- pivot_longer(pars, cols = c(V, RU), values_to = "estimate", names_to = "param") %>% 
  mutate(param = recode(param, "V" = "value-guided",
                       "RU" = "directed"),
         estimate = estimate * -1) %>% # bc is coded in reverse 
  subset(param != "intercept")

ggplot(df, aes(estimate, reward)) + geom_smooth() + 
  facet_grid(cols = vars(param), rows = vars(session), scale = "free")


## add in participant data

for (session in 1:2){
  load(sprintf("analysis/bandits/modellingResults/fitSamSession%iUCB_hierarchical.Rda", session))
  
  if (session == 1){
    pp <- trueParams %>% 
      select(predictor, estimate) %>% 
      group_by(predictor) %>%
      summarise(se = sd(estimate),
                estimate = mean(estimate)) %>%
      mutate(param = recode(predictor, "V" = "value-guided",
                            "RU" = "directed")) %>% 
      subset(param != "Intercept", select = -predictor)
  } else {
    ppp <- trueParams %>% 
      select(predictor, estimate) %>% 
      group_by(predictor) %>%
      summarise(se = sd(estimate),
                estimate = mean(estimate)) %>%
      mutate(param = recode(predictor, "V" = "value-guided",
                            "RU" = "directed")) %>% 
      subset(param != "Intercept", select = -predictor)
    
    pp <- list(pp, ppp) %>% 
      bind_rows(.id = "session") %>% 
      mutate(estimate = -1*estimate)
  }

  
}




p1 <- ggplot(df, aes(estimate, reward, color = session))+ geom_smooth( )+
  geom_point(data = pp, aes(estimate, 0), color = "#66C2A5") +
  #geom_density(data = pp, aes(estimate, y = after_stat(density)), color = "#66C2A5", fill = "#66C2A5", alpha = 0.3)+
  geom_linerange(data = pp, aes(xmin = estimate - se, xmax = estimate + se, y = 0), color= "#66C2A5" )+
  facet_wrap(vars(param), scales = "free")+
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom"
  ) +
  labs(y = "regret ±95%CI",
       x = "estimate (±SD)")+
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"), labels = c("session 1", "session 2", "Fan et al. 2023"), 
                     name = element_blank())

p1

sam_plot <- ggarrange(grid, p1, ncol = 1, nrow = 2, heights = c(0.9,1))
sam_plot

ggsave(sam_plot, file = "figures/figures-ms/submission-1/SIoptimimalitySam.png",
       height = 5, width = 7)

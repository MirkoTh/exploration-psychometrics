############ plot for Bob Wilson meeting ###################

rm(list = ls())
library(grid)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(brms)
library(here)
library(patchwork)
theme_set(theme_bw(base_size = 14))

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

self_cor <- function(df){
  cors <- df %>% 
    select(-ID) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    as.data.frame() %>% 
    mutate(x = rownames(.)) %>% 
    pivot_longer(cols = -x, names_to = "y", values_to = "cor")
  
  return(cors)
  
}


horizon1 <- load_and_prep_bandit_data(1)$horizon
horizon2 <- load_and_prep_bandit_data(2)$horizon

df <- list(horizon1, horizon2) %>% 
  bind_rows(.id = "session") %>% 
  mutate(chosen_recoded = ifelse(info == -0.5, abs(chosen-1), 
                                 ifelse(info == 0 & reward1 < reward2, abs(chosen-1), chosen)),
         info_rec = ifelse(info == 0, "P(low mean)", "P(high info)")) %>% 
  subset(trial == 5) %>% 
  group_by(Horizon, ID, session, info_rec) %>% 
  summarise(P_exp = mean(chosen_recoded)) %>% 
  pivot_wider(id_cols = c(Horizon, ID, info_rec), names_from = session, values_from = P_exp) %>% 
  rename(session1 = `1`,
        session2  = `2`) %>% 
  subset(!is.na(session1) & !is.na(session2)) %>% 
  mutate(Horizon = factor(Horizon, levels = c(-0.5, 0.5), labels = c("short Horizon", "long Horizon")))

cors <- df %>% 
  group_by(Horizon, info_rec) %>% 
  summarise(cor = cor(session1, session2))

df <- left_join(df, cors, by = c("Horizon", "info_rec"))

# seperated by horizons
ggplot(df, aes(session1, session2)) + geom_jitter(color = "blue") +
  geom_smooth(method = "lm", color = "blue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  facet_grid(rows = vars(info_rec), cols = vars(Horizon))+
  geom_label(aes(label = sprintf("r = %.2f", cor)), x = -Inf, y = Inf, hjust = "inward", vjust = "inward")



# delta between horizons

df <- df %>% 
  pivot_longer(cols = c(session1, session2), names_to = "session", values_to = "P_exp") %>% 
  pivot_wider(id_cols = c(ID, info_rec, session), names_from = Horizon, values_from = "P_exp") %>% 
  mutate(P_exp = `long Horizon` - `short Horizon`) %>% 
  select(-c(`short Horizon`, `long Horizon`)) %>% 
  pivot_wider(id_cols = c(ID, info_rec), names_from = session, values_from = P_exp)

cors <- df %>% 
  group_by(info_rec) %>% 
  summarise(cor = cor(session1, session2))

df <- left_join(df, cors, by = c("info_rec"))

ggplot(df, aes(session1, session2)) + geom_jitter(color = "blue") +
  geom_smooth(method = "lm", color = "blue") +
  #coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  facet_grid(rows = vars(info_rec))+
  geom_label(aes(label = sprintf("r = %.2f", cor)), x = -Inf, y = Inf, hjust = "inward", vjust = "inward")





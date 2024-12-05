############# external validity ############

rm(list = ls())

library(plyr)
library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(ggridges)
theme_set(theme_bw(base_size = 14))
library(here)

session <- 2

source("analysis/recovery_utils.R")
se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}



####### prepping data #########

# task-based measures
horizon <- load_and_prep_bandit_data(session)$horizon
sam <- load_and_prep_bandit_data(session)$sam
restless <- load_and_prep_bandit_data(session)$restless

best <- function(df){
  rewards <- subset(df, select = grepl("reward", colnames(df)))
  rewards <- subset(rewards, select = -reward)
  
  best <- apply(as.array(1:nrow(rewards)), 1, function(x) max(rewards[x,]))
  optimal <- ifelse(df$reward == best, 1, 0)
  return(optimal)
  
} 

Switch <- function(df){
  df$prev <- c(NA, df$chosen[1:nrow(df)-1])
  df$switch <- ifelse(df$chosen == df$prev, 0, 1)
  df$switch[df$trial == 1] <- NA
  return(df)
}


horizon <- Switch(horizon)
sam <- Switch(sam)
restless <- Switch(restless)
horizon$best <- best(horizon)
sam$best <- best(sam)
restless$best <- best(restless)

hos <- horizon %>% 
  group_by(ID) %>% 
  subset(trial > 4) %>% 
  summarise(Poptimal = meann(best),
            Pswitch = meann(switch))

sos <- sam %>% 
  group_by(ID)%>% 
  summarise(Poptimal = meann(best),
            Pswitch = meann(switch))

ros <- restless %>% 
  group_by(ID)%>% 
  summarise(Poptimal = meann(best),
            Pswitch = meann(switch))

task_based <- list("horizon" = hos, "2AB" = sos, "restless" = ros) %>% 
  bind_rows(.id = "task") %>% 
  pivot_longer(cols = c("Poptimal", "Pswitch"), names_to = "predictor", values_to = "estimate") %>% 
  mutate(predictor = paste(task, predictor, sep = "_")) %>% 
  select(ID, predictor, estimate)

# model parameters
s <- session
params <- readRDS("analysis/bandits/allParams.rds") %>% 
  mutate(task = recode(task,"sam" = "2AB"),
         predictor = paste(task, predictor, sep = "_")) %>% 
  subset(!grepl("ntercept", predictor) & session == s,
         select = c("ID", "predictor", "estimate"))


# questionnaire data
load(sprintf("analysis/qdat_session%i.Rda", session))

avg <- avg %>% 
  mutate(estimate = score,
         predictor = Measure) %>% 
  select(ID, estimate, predictor)

# WM performance
wm <- read.csv("data/wm-performance.csv") %>% 
  pivot_longer(cols = -c("participant_id"), names_to = "predictor", values_to = "estimate") %>% 
  mutate(session = parse_number(predictor)+1,
         ID = as.numeric(as.character(participant_id))) %>% 
  subset(grepl("recall", predictor) & session == s)


all <- list(params, task_based, wm, avg) %>% 
  bind_rows() %>% 
  pivot_wider(id_cols = "ID", names_from = "predictor", values_from = "estimate")


############### correlation matrix ###########


cors <- all %>% 
  subset(select = -ID) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame()

selected_columns <- colnames(cors) %>%
  str_subset("^(?!.*(2AB|restless|horizon)).*$")

# Select rows that contain "2AB", "restless", or "horizon"
selected_rows <- rownames(cors) %>%
  str_subset("2AB|restless|horizon")

# Filter the correlation matrix
cors <- cors[selected_rows, selected_columns]

save(cors, file = sprintf("analysis/external_validity_cors_session%i.Rda", s))
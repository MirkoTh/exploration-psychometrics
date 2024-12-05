rm(list = ls())


# note sigma_prior refers to variance (i.e., sigma squared) and not to sd


# Import Packages and Load Data -------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(mvtnorm)
library(zoo)
library(TTR)
library(future)
library(furrr)
library(ggbeeswarm)
library(reactable)
library(reactablefmtr)

home_grown <- c("utils/analysis-utils.R", "utils/modeling-utils.R", "utils/plotting-utils.R")
walk(home_grown, source)

for (i in 1:2) {
  tbl_rb <- read_csv(str_c("data/finalRestlessSession", i, ".csv"))
  tbl_exclude2 <- read_csv(file = "data/exclusions2_noPID.csv")
  tbl_exclude1 <- read_csv(file = "data/exclusions1_noPID.csv")
  
  
  # seems like two people were invited to session 2, who should not have been
  tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
    left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
    filter(exclude_2 == 0 & exclude_1 == 0)
  
  tbl_rb <- tbl_rb %>%
    inner_join(tbl_exclude[, c("ID")], by = "ID") %>%
    rename(choices = chosen, rewards = reward) %>%
    mutate(choices = choices + 1)
  l_participants <- tbl_rb %>% split(., .$"ID")
  
  # generate randomly walking arms
  nr_trials <- nrow(l_participants[[1]])
  nr_participants <- length(l_participants)
  
  # reward set is the same for all participants, just take one of them
  tbl_rewards <- tbl_rb  %>%
    filter(ID == 2) %>%
    select(starts_with("reward")) %>%
    rename(
      "Arm 1" = reward1,
      "Arm 2" = reward2,
      "Arm 3" = reward3,
      "Arm 4" = reward4
    ) %>% select(- rewards)
  
  # generating values from actual stimulus set
  sigma_xi_sq <- 7.84
  sigma_epsilon_sq <- 16
  sigma_prior <- 1000
  mu_prior <- 50
  decay_center <- 50
  
  l_choices_made <- map(l_participants, "choices")
  
  ## UCB with Softmax -------------------------------------------------------
  
  bds_ucb <- list(gamma = list(lo = 0, hi = 1), beta = list(lo = -10, hi = 10))

  plan(multisession, workers = availableCores() - 1)
  l_kalman_ucb_no_variance <- furrr:::future_map(
    l_participants, fit_ucb_no_variance_wrapper,
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_ucb,
    decay_center = decay_center,
    .progress = TRUE
  )
  plan("sequential")
  saveRDS(l_kalman_ucb_no_variance, file = str_c("data/4arlb-kalman-ucb-fit", i, ".rds"))


  # Softmax only ------------------------------------------------------------

  
  bds_sm <- list(gamma = list(lo = 0, hi = 1))
  
  l_kalman_sm_no_variance <- furrr:::future_map(
    l_participants, fit_softmax_no_variance_wrapper,
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_sm, 
    decay_center = decay_center,
    .progress = TRUE
  )
  plan("sequential")
  saveRDS(l_kalman_sm_no_variance, file = str_c("data/4arlb-kalman-sm-fit", i, ".rds"))

}


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


fit_or_load <- "fit"
tbl_rb <- read_csv("data/finalRestlessSession1.csv")
tbl_exclude2 <- read_csv(file = "data/exclusions2.csv")
tbl_exclude1 <- read_csv(file = "data/exclusions1.csv")


# seems like two people were invited to session 2, who should not have been
tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
  left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
  filter(exclude_2 == 0 & exclude_1 == 0)

# for testing, just take 4 subjects
tbl_exclude <- tbl_exclude %>% filter(ID <= 6)

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



my_participants_tbl_kalman <- function(l_params_decision, sim_d, v_choices) {
  tibble(
    sigma_prior = 1000,
    mu_prior = 50,
    sigma_xi_sq = 7.84,
    sigma_epsilon_sq = 16,
    lambda = .9836,
    decay_center = decay_center,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(nr_participants, 100000, 1000)),
    choices_made = v_choices
  )
}

my_participants_tbl_delta <- function(l_params_decision, delta, sim_d, v_choices) {
  tibble(
    mu_prior = 50,
    delta = delta,
    lambda = .9836,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(nr_participants, 100000, 1000)),
    choices_made = v_choices
  )
}

# generating values from actual stimulus set
sigma_xi_sq <- 7.84
sigma_epsilon_sq <- 16
sigma_prior <- 1000
mu_prior <- 50
decay_center <- 50

l_choices_made <- map(l_participants, "choices")



# Fit, Simulate, & Recover Parameters -------------------------------------


## Softmax no variance ----------------------------------------------------

bds_sm <- list(gamma = list(lo = 0, hi = .5))


if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_kalman_softmax_no_variance <- furrr::future_map(
    l_participants, fit_softmax_no_variance_wrapper, 
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_sm,
    decay_center = decay_center,
    .progress = TRUE
  )
  plan("sequential")
  saveRDS(l_kalman_softmax_no_variance, file = "data/empirical-parameter-recovery-kalman-softmax-fit.rds")
  
  tbl_kalman_softmax_no_variance <- reduce(l_kalman_softmax_no_variance, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(gamma = V1, ll = V2)
  
  l_params_decision <- map(
    tbl_kalman_softmax_no_variance$gamma, 
    ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
  )
  
  # recovery just for selected stimulus sets
  tbl_participants_kalman_softmax <- my_participants_tbl_kalman(l_params_decision, FALSE, l_choices_made)
  tbl_results_kalman_softmax <- simulate_and_fit_softmax(
    tbl_participants_kalman_softmax, nr_vars = 0, 
    cond_on_choices = TRUE, nr_trials = nr_trials,
    bds = bds_sm, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_kalman_softmax, file = "data/empirical-parameter-recovery-kalman-softmax-recovery.rds")
} else if (fit_or_load == "load") {
  l_kalman_softmax_no_variance <- readRDS(file = "data/empirical-parameter-recovery-kalman-softmax-fit.rds")
  tbl_results_kalman_softmax <- readRDS(file = "data/empirical-parameter-recovery-kalman-softmax-recovery.rds")
}


tbl_recovery_kalman_softmax <- tbl_results_kalman_softmax %>%
  unnest_wider(params_decision) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_0var_long <- tbl_recovery_kalman_softmax %>%
  rename("Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma))


## UCB with Softmax -------------------------------------------------------

bds_ucb <- list(gamma = list(lo = 0, hi = .5), beta = list(lo = -10, hi = 10))

if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_kalman_ucb_no_variance <- furrr:::future_map(
    l_participants, fit_ucb_no_variance_wrapper,
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_ucb, 
    decay_center = mu_prior,
    
    .progress = TRUE
  )
  plan("sequential")
  saveRDS(l_kalman_ucb_no_variance, file = "data/empirical-parameter-recovery-kalman-ucb-fit.rds")
  
  tbl_kalman_ucb_no_variance <- reduce(l_kalman_ucb_no_variance, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(gamma = V1, beta = V2, ll = V3)
  
  l_params_decision <- map2(
    tbl_kalman_ucb_no_variance$gamma, tbl_kalman_ucb_no_variance$beta,
    ~ list(gamma = ..1, beta = ..2, choicemodel = "ucb", no = 4)
  )
  
  tbl_participants_kalman_ucb <- my_participants_tbl_kalman(l_params_decision, FALSE, l_choices_made)
  tbl_results_kalman_ucb <- simulate_and_fit_ucb(
    tbl_participants_kalman_ucb, nr_vars = 0, cond_on_choices = TRUE, 
    nr_trials = nr_trials, bds = bds_ucb, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_kalman_ucb, file = "data/empirical-parameter-recovery-kalman-ucb-recovery.rds")
} else if (fit_or_load == "load") {
  l_kalman_ucb_no_variance <- readRDS(file = "data/empirical-parameter-recovery-kalman-ucb-fit.rds")
  tbl_results_kalman_ucb <- readRDS(file = "data/empirical-parameter-recovery-kalman-ucb-recovery.rds")
}


tbl_results_kalman_ucb <- tbl_results_kalman_ucb %>%
  unnest_wider(params_decision)
tbl_recovery_kalman_ucb <- tbl_results_kalman_ucb %>%
  #filter(between(beta, -3, 3)) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml),
    r_gamma_beta = cor(gamma, beta_ml),
    r_beta_gamma = cor(beta, gamma_ml)
  ) %>% ungroup()

tbl_recovery_kalman_ucb_long <- tbl_recovery_kalman_ucb  %>% 
  mutate(
    beta_mn = "empirical",
    gamma_mn = "empirical"
  ) %>%
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta,
    "Gamma in Beta out" = r_gamma_beta,
    "Beta in Gamma out" = r_beta_gamma
  ) %>%
  pivot_longer(cols = c(Gamma, Beta, `Gamma in Beta out`, `Beta in Gamma out`)) %>%
  mutate(
    param_in = rep(c("Gamma", "Beta"), 2),
    param_out = c("Gamma", "Beta", "Beta", "Gamma")
  )

pl_recov_ucb_sm <- ggplot(tbl_recovery_kalman_ucb_long, aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(name = "") +
  geom_label(aes(label = str_c("r = ", round(value, 2)))) +
  theme_bw() +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Parameter In", y = "Parameter Out")



save_my_pdf_and_tiff(
  pl_recov_ucb_sm, "figures/4arlb-ucb-softmax-param-correlations-empirical", 5.5, 3
)

tbl_results_kalman_ucb %>%
  filter(between(beta, -3, 3)) %>%
  pivot_longer(c(gamma, beta), names_to = "Parameter In", values_to = "p_val_in") %>%
  pivot_longer(c(gamma_ml, beta_ml), names_to = "Parameter Out", values_to = "p_val_out") %>%
  ggplot(aes(p_val_in, p_val_out)) +
  geom_abline(linewidth = 1, color = "grey") +
  geom_point(shape = 1) +
  facet_wrap(`Parameter In` ~ `Parameter Out`, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value In", y = "Parameter Value Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


## Mixture Thompson Sampling with UCB -------------------------------------


bds_mix_th_ucb <- list(gamma = list(lo = 0, hi = .5), beta = list(lo = -10, hi = 10), w_mix = list(lo = 0, hi = 1))

if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_kalman_ucb_thompson <- furrr:::future_map(
    l_participants, fit_mixture_no_variance_wrapper,
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    f_fit = fit_kalman_ucb_thompson_no_variance,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_mix_th_ucb, 
    decay_center = decay_center,
    .progress = TRUE
  )
  plan("sequential")
  
  saveRDS(l_kalman_ucb_thompson, file = "data/empirical-parameter-recovery-kalman-ucb-thompson-fit.rds")
  
  tbl_kalman_ucb_thompson <- reduce(l_kalman_ucb_thompson, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(gamma = V1, beta = V2, w_mix = V3, ll = V4)
  
  l_params_decision <- pmap(
    tbl_kalman_ucb_thompson[, c("gamma", "beta", "w_mix")],
    ~ list(gamma = ..1, beta = ..2, w_mix = ..3, choicemodel = "ucb_thompson", no = 4)
  )
  
  tbl_participants_kalman_ucb_thompson <- my_participants_tbl_kalman(l_params_decision, FALSE, l_choices_made)
  tbl_results_kalman_ucb_thompson <- simulate_and_fit_mixture(
    tbl_participants_kalman_ucb_thompson, nr_vars = 0, cond_on_choices = TRUE, 
    nr_trials = nr_trials, bds = bds_mix_th_ucb, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_kalman_ucb_thompson, file = "data/empirical-parameter-recovery-kalman-ucb-thompson-recovery.rds")
  
} else if (fit_or_load == "load") {
  l_kalman_ucb_thompson <- readRDS(file = "data/empirical-parameter-recovery-kalman-ucb-thompson-fit.rds")
  tbl_results_kalman_ucb_thompson <- readRDS(file = "data/empirical-parameter-recovery-kalman-ucb-thompson-recovery.rds")
}


tbl_results_kalman_ucb_thompson <- tbl_results_kalman_ucb

tbl_results_kalman_ucb_thompson <- tbl_results_kalman_ucb_thompson %>%
  unnest_wider(params_decision)
tbl_recovery_kalman_ucb_thompson <- tbl_results_kalman_ucb_thompson %>%
  #filter(between(beta, -3, 3)) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml),
    r_w_mix = cor(w_mix, w_mix_ml),
    r_gamma_beta = cor(gamma, beta_ml),
    r_gamma_w_mix = cor(gamma, w_mix_ml),
    r_beta_gamma = cor(beta, gamma_ml),
    r_beta_w_mix = cor(beta, w_mix_ml),
    r_w_mix_gamma = cor(w_mix, gamma_ml),
    r_w_mix_beta = cor(w_mix, beta_ml)
  ) %>% ungroup()

tbl_recovery_kalman_ucb_thompson_long <- tbl_recovery_kalman_ucb_thompson  %>% 
  mutate(
    beta_mn = "empirical",
    gamma_mn = "empirical",
    w_mix_mn = "empirical"
  ) %>%
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta,
    "w(mix)" = r_w_mix,
    "Gamma in Beta out" = r_gamma_beta,
    "Gamma in w(mix) out" = r_gamma_w_mix,
    "Beta in Gamma out" = r_beta_gamma,
    "Beta in w(mix) out" = r_beta_w_mix,
    "w(mix) in Gamma out" = r_w_mix_gamma,
    "w(mix) in Beta out" = r_w_mix_beta
  ) %>%
  pivot_longer(cols = c(Gamma, Beta, `w(mix)`, `Gamma in Beta out`, `Gamma in w(mix) out`, `Beta in Gamma out`, `Beta in w(mix) out`, `w(mix) in Gamma out`, `w(mix) in Beta out`)) %>%
  mutate(
    param_in = rep(c("Gamma", "Beta", "w(mix)"), 3),
    param_out = c("Gamma", "Beta", "w(mix)",  "Beta", "w(mix)",  "Gamma", "w(mix)",  "Gamma", "Beta")
  )

pl_recov_ucb_thompson <- ggplot(tbl_recovery_kalman_ucb_thompson_long, aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(name = "") +
  geom_label(aes(label = str_c("r = ", round(value, 2)))) +
  theme_bw() +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Parameter In", y = "Parameter Out")



save_my_pdf_and_tiff(
  pl_recov_ucb_thompson, "figures/4arlb-ucb-thompson-param-correlations-empirical", 5.5, 3
)

tbl_results_kalman_ucb_thompson %>%
  #filter(between(beta, -3, 3)) %>%
  pivot_longer(c(gamma, beta, w_mix), names_to = "Parameter In", values_to = "p_val_in") %>%
  pivot_longer(c(gamma_ml, beta_ml, w_mix_ml), names_to = "Parameter Out", values_to = "p_val_out") %>%
  ggplot(aes(p_val_in, p_val_out)) +
  geom_abline(linewidth = 1, color = "grey") +
  geom_point(shape = 1) +
  facet_wrap(`Parameter In` ~ `Parameter Out`, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value In", y = "Parameter Value Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


## Mixture Thompson Sampling with RU -------------------------------------


bds_mix_th_ru <- list(beta = list(lo = -10, hi = 10), w_mix = list(lo = 0, hi = 1))

if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_kalman_ru_thompson <- furrr:::future_map(
    l_participants, fit_mixture_no_variance_wrapper,
    tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
    f_fit = fit_kalman_ru_thompson_no_variance,
    sigma_xi_sq, sigma_epsilon_sq,
    sigma_prior, mu_prior,
    bds = bds_mix_th_ru, 
    decay_center = decay_center,
    .progress = TRUE, .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  saveRDS(l_kalman_ru_thompson, file = "data/empirical-parameter-recovery-kalman-ru-thompson-fit.rds")
  
  tbl_kalman_ru_thompson <- reduce(l_kalman_ru_thompson, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(beta = V1, w_mix = V2, ll = V3)
  
  l_params_decision <- pmap(
    tbl_kalman_ru_thompson[, c("beta", "w_mix")],
    ~ list(beta = ..1, w_mix = ..2, choicemodel = "ru_thompson", no = 4)
  )
  
  tbl_participants_kalman_ru_thompson <- my_participants_tbl_kalman(l_params_decision, FALSE, l_choices_made)
  tbl_results_kalman_ru_thompson <- simulate_and_fit_mixture(
    tbl_participants_kalman_ru_thompson, nr_vars = 0, cond_on_choices = TRUE, 
    nr_trials = nr_trials, bds = bds_mix_th_ru, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_kalman_ru_thompson, file = "data/empirical-parameter-recovery-kalman-ru-thompson-recovery.rds")
  
} else if (fit_or_load == "load") {
  l_kalman_ru_thompson <- readRDS(file = "data/empirical-parameter-recovery-kalman-ru-thompson-fit.rds")
  tbl_results_kalman_ru_thompson <- readRDS(file = "data/empirical-parameter-recovery-kalman-ru-thompson-recovery.rds")
}


tbl_results_kalman_ru_thompson <- tbl_results_kalman_ru_thompson %>%
  unnest_wider(params_decision)
tbl_recovery_kalman_ru_thompson <- tbl_results_kalman_ru_thompson %>%
  #filter(between(beta, -3, 3)) %>%
  summarize(
    r_beta = cor(beta, beta_ml),
    r_w_mix = cor(w_mix, w_mix_ml),
    r_beta_w_mix = cor(beta, w_mix_ml),
    r_w_mix_beta = cor(w_mix, beta_ml)
  ) %>% ungroup()

tbl_recovery_kalman_ru_thompson_long <- tbl_recovery_kalman_ru_thompson  %>% 
  mutate(
    beta_mn = "empirical",
    w_mix_mn = "empirical"
  ) %>%
  rename(
    "Beta" = r_beta,
    "w(mix)" = r_w_mix,
    "Beta in w(mix) out" = r_beta_w_mix,
    "w(mix) in Beta out" = r_w_mix_beta
  ) %>%
  pivot_longer(cols = c(Beta, `w(mix)`, `Beta in w(mix) out`, `w(mix) in Beta out`)) %>%
  mutate(
    param_in = rep(c("Beta", "w(mix)"), 2),
    param_out = c("Beta", "w(mix)",  "w(mix)",  "Beta")
  )

pl_recov_ru_thompson <- ggplot(tbl_recovery_kalman_ru_thompson_long, aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(name = "") +
  geom_label(aes(label = str_c("r = ", round(value, 2)))) +
  theme_bw() +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Parameter In", y = "Parameter Out")



save_my_pdf_and_tiff(
  pl_recov_ru_thompson, "figures/4arlb-ru-thompson-param-correlations-empirical", 5.5, 3
)

tbl_results_kalman_ru_thompson %>%
  #filter(between(beta, -3, 3)) %>%
  pivot_longer(c(beta, w_mix), names_to = "Parameter In", values_to = "p_val_in") %>%
  pivot_longer(c(beta_ml, w_mix_ml), names_to = "Parameter Out", values_to = "p_val_out") %>%
  ggplot(aes(p_val_in, p_val_out)) +
  geom_abline(linewidth = 1, color = "grey") +
  geom_point(shape = 1) +
  facet_wrap(`Parameter In` ~ `Parameter Out`, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value In", y = "Parameter Value Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")





# for delta and decay, check that prior mu is set to 50 in all functions

# for comparability with Kalman recovery, only re-fit choices for recovery data set

## Delta Rule -----------------------------------------------------------

bds_delta <- list(delta = list(lo = 0, hi = 1), gamma = list(lo = 0, hi = .5))


if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_delta <- furrr:::future_map(
    l_participants, fit_delta_softmax_wrapper,
    tbl_rewards = tbl_rewards, is_decay = FALSE,
    condition_on_observed_choices = TRUE,
    mu_prior = mu_prior,
    bds = bds_delta, 
    .progress = TRUE, .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  saveRDS(l_delta, file = "data/empirical-parameter-recovery-delta-fit.rds")
  
  tbl_delta <- reduce(l_delta, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)
  
  l_params_decision <- pmap(
    tbl_delta[, c("gamma")],
    ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
  )
  
  tbl_participants_delta <- my_participants_tbl_delta(l_params_decision, tbl_delta$delta, FALSE, l_choices_made)
  tbl_results_delta <- simulate_and_fit_delta(
    tbl_participants_delta, is_decay = FALSE, cond_on_choices = TRUE, 
    nr_trials = nr_trials, bds = bds_delta, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_delta, file = "data/empirical-parameter-recovery-delta-recovery.rds")
  
} else if (fit_or_load == "load") {
  l_delta <- readRDS(file = "data/empirical-parameter-recovery-delta-fit.rds")
  tbl_results_delta <- readRDS(file = "data/empirical-parameter-recovery-delta-recovery.rds")
}

tbl_results_delta <- tbl_results_delta %>%
  unnest_wider(params_decision)
tbl_recovery_delta <- tbl_results_delta %>%
  #filter(between(beta, -3, 3)) %>%
  summarize(
    r_delta = cor(delta, delta_ml),
    r_gamma = cor(gamma, gamma_ml),
    r_delta_gamma = cor(delta, gamma_ml),
    r_gamma_delta = cor(gamma, delta_ml)
  ) %>% ungroup()

tbl_recovery_delta_long <- tbl_recovery_delta  %>% 
  mutate(
    delta_mn = "empirical",
    gamma_mn = "empirical"
  ) %>%
  rename(
    "Delta" = r_delta,
    "Gamma" = r_gamma,
    "Delta in Gamma out" = r_delta_gamma,
    "Gamma in Delta out" = r_gamma_delta
  ) %>%
  pivot_longer(cols = c(Delta, Gamma, `Delta in Gamma out`, `Gamma in Delta out`)) %>%
  mutate(
    param_in = rep(c("Delta", "Gamma"), 2),
    param_out = c("Delta", "Gamma",  "Gamma",  "Delta")
  )

pl_recov_delta <- ggplot(tbl_recovery_delta_long, aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(name = "") +
  geom_label(aes(label = str_c("r = ", round(value, 2)))) +
  theme_bw() +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Parameter In", y = "Parameter Out")



save_my_pdf_and_tiff(
  pl_recov_delta, "figures/4arlb-delta-param-correlations-empirical", 5.5, 3
)

tbl_results_delta %>%
  #filter(between(beta, -3, 3)) %>%
  pivot_longer(c(delta, gamma), names_to = "Parameter In", values_to = "p_val_in") %>%
  pivot_longer(c(delta_ml, gamma_ml), names_to = "Parameter Out", values_to = "p_val_out") %>%
  ggplot(aes(p_val_in, p_val_out)) +
  geom_abline(linewidth = 1, color = "grey") +
  geom_point(shape = 1) +
  facet_wrap(`Parameter In` ~ `Parameter Out`, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value In", y = "Parameter Value Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")




## Decay Rule ----------------------------------------------------------


if (fit_or_load == "fit") {
  plan(multisession, workers = availableCores() - 1)
  l_decay <- furrr:::future_map(
    l_participants, fit_delta_softmax_wrapper,
    tbl_rewards = tbl_rewards, is_decay = TRUE,
    condition_on_observed_choices = TRUE,
    mu_prior = mu_prior,
    bds = bds_delta, 
    .progress = TRUE, .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  saveRDS(l_decay, file = "data/empirical-parameter-recovery-decay-fit.rds")
  
  tbl_decay <- reduce(l_decay, rbind) %>%
    as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)
  
  l_params_decision <- pmap(
    tbl_decay[, c("gamma")],
    ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
  )
  
  tbl_participants_decay <- my_participants_tbl_delta(l_params_decision, tbl_decay$delta, FALSE, l_choices_made)
  tbl_results_decay <- simulate_and_fit_delta(
    tbl_participants_decay, is_decay = TRUE, cond_on_choices = TRUE, 
    nr_trials = nr_trials, bds = bds_delta, tbl_rewards = tbl_rewards
  )
  
  saveRDS(tbl_results_decay, file = "data/empirical-parameter-recovery-decay-recovery.rds")
  
} else if (fit_or_load == "load") {
  l_decay <- readRDS(file = "data/empirical-parameter-recovery-decay-fit.rds")
  tbl_results_decay <- readRDS(file = "data/empirical-parameter-recovery-decay-recovery.rds")
}


tbl_results_decay <- tbl_results_decay %>%
  unnest_wider(params_decision)
tbl_recovery_decay <- tbl_results_decay %>%
  #filter(between(beta, -3, 3)) %>%
  summarize(
    r_delta = cor(delta, delta_ml),
    r_gamma = cor(gamma, gamma_ml),
    r_delta_gamma = cor(delta, gamma_ml),
    r_gamma_delta = cor(gamma, delta_ml)
  ) %>% ungroup()

tbl_recovery_decay_long <- tbl_recovery_decay  %>% 
  mutate(
    delta_mn = "empirical",
    gamma_mn = "empirical"
  ) %>%
  rename(
    "Delta" = r_delta,
    "Gamma" = r_gamma,
    "Delta in Gamma out" = r_delta_gamma,
    "Gamma in Delta out" = r_gamma_delta
  ) %>%
  pivot_longer(cols = c(Delta, Gamma, `Delta in Gamma out`, `Gamma in Delta out`)) %>%
  mutate(
    param_in = rep(c("Delta", "Gamma"), 2),
    param_out = c("Delta", "Gamma",  "Gamma",  "Delta")
  )

pl_recov_decay <- ggplot(tbl_recovery_decay_long, aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(name = "") +
  geom_label(aes(label = str_c("r = ", round(value, 2)))) +
  theme_bw() +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Parameter In", y = "Parameter Out")



save_my_pdf_and_tiff(
  pl_recov_decay, "figures/4arlb-decay-param-correlations-empirical", 5.5, 3
)

tbl_results_decay %>%
  #filter(between(beta, -3, 3)) %>%
  pivot_longer(c(delta, gamma), names_to = "Parameter In", values_to = "p_val_in") %>%
  pivot_longer(c(delta_ml, gamma_ml), names_to = "Parameter Out", values_to = "p_val_out") %>%
  ggplot(aes(p_val_in, p_val_out)) +
  geom_abline(linewidth = 1, color = "grey") +
  geom_point(shape = 1) +
  facet_wrap(`Parameter In` ~ `Parameter Out`, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value In", y = "Parameter Value Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")



# Summarize Results -------------------------------------------------------

wrangle_recoveries <- function(my_tbl, modelname) {
  tbl_summary <- crossing(
    pars = c("r_gamma", "r_beta", "r_delta", "r_w_mix"),
    simulate_data = c(TRUE, FALSE)
  )
  out <- tbl_summary %>% left_join(
    my_tbl %>% 
      mutate(model = modelname) %>%
      pivot_longer(cols = -c(model, simulate_data)),
    by = c("pars" = "name", "simulate_data" = "simulate_data")
  ) %>%
    pivot_wider(names_from = pars, values_from = value)
  out[!is.na(out$model), ]
}

km_sm <- wrangle_recoveries(tbl_recovery_kalman_softmax, "Kalman Softmax")
km_ucb <- wrangle_recoveries(tbl_recovery_kalman_ucb, "Kalman UCB")
km_ucb_thompson <- wrangle_recoveries(tbl_recovery_kalman_ucb_thompson, "Kalman UCB & Thompson")
km_ru_thompson <- wrangle_recoveries(tbl_recovery_kalman_ru_thompson, "Kalman RU & Thompson")
delta_sm <- wrangle_recoveries(tbl_recovery_delta_softmax, "Delta Softmax")
decay_sm <- wrangle_recoveries(tbl_recovery_decay_softmax, "Decay Softmax")

tbl_summary_pars <- rbind(km_sm, km_ucb, km_ucb_thompson, km_ru_thompson, delta_sm, decay_sm)
# visualize summary recovery of four models with reactable table
badtogood_cols <- c('#d65440', '#ffffff', "forestgreen")

colnames(tbl_summary_pars) <- c("simulate_data", "Model", "Beta", "Delta", "Gamma", "w_mix")
tbl_summary_pars$Model <- fct_inorder(factor(tbl_summary_pars$Model))
tbl_summary_pars <- tbl_summary_pars %>% relocate(Gamma, .before = Beta)
tbl_summary_pars[, c("Gamma", "Beta", "Delta", "w_mix")] <- map(tbl_summary_pars[, c("Gamma", "Beta", "Delta", "w_mix")], ~ round(.x, digits = 2))
tbl_summary_pars$simulate_data <- factor(tbl_summary_pars$simulate_data)
levels(tbl_summary_pars$simulate_data) <- c("One Fixed Set", "By Participant")
tbl_summary_pars <- tbl_summary_pars %>% rename("Random Walk" = simulate_data)
tbl_summary_pars_long <- tbl_summary_pars %>% pivot_longer(-c(Model, "Random Walk"))
tbl_summary_pars_long$name <- factor(tbl_summary_pars_long$name)
levels(tbl_summary_pars_long$name) <- c("Beta", "Delta", "Gamma", "w_mix")
tbl_summary_pars_long$name <- fct_relevel(tbl_summary_pars_long$name, "Beta", after = 2)
ggplot(tbl_summary_pars_long, aes(name, fct_rev(Model))) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(high = "aquamarine2", low = "tomato", mid = .5, guide = "none") +
  labs(x = "", y = "") +
  facet_wrap(~ `Random Walk`)


tbl_gammas <- tbl_results_kalman_softmax %>%
  select(gamma_ml) %>%
  mutate(Model = "Kalman Softmax") %>%
  rbind(
    tbl_results_kalman_ucb %>%
      select(gamma_ml) %>%
      mutate(Model = "Kalman UCB") 
  )  %>%
  rbind(
    tbl_results_kalman_ru_thompson %>%
      select(gamma_ml) %>%
      mutate(Model = "Kalman RU & Thompson") 
  )   %>%
  rbind(
    tbl_results_kalman_ucb_thompson %>%
      select(gamma_ml) %>%
      mutate(Model = "Kalman UCB & Thompson") 
  ) %>%
  rbind(
    tbl_results_delta_softmax %>%
      select(gamma_ml) %>%
      mutate(Model = "Delta Softmax") 
  ) %>%
  rbind(
    tbl_results_decay_softmax %>%
      select(gamma_ml) %>%
      mutate(Model = "Decay Softmax") 
  ) %>% rename(gamma = gamma_ml)

tbl_gammas$Model <- factor(tbl_gammas$Model)
tbl_gammas$Model <- fct_inorder(tbl_gammas$Model)
pl_gammas <- ggplot(tbl_gammas, aes(Model, gamma, group = Model)) +
  geom_violin(aes(fill = Model), alpha = .25) +
  geom_quasirandom(aes(color = Model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = Model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Gamma")



tbl_deltas <- tbl_results_delta_softmax %>%
  select(delta_ml) %>%
  mutate(Model = "Delta Softmax") %>%
  rbind(
    tbl_results_decay_softmax %>%
      select(delta_ml) %>%
      mutate(Model = "Decay Softmax") 
  ) %>% rename(delta = delta_ml)

tbl_deltas$Model <- factor(tbl_deltas$Model)
tbl_deltas$Model <- fct_inorder(tbl_deltas$Model)
pl_deltas <- ggplot(tbl_deltas, aes(Model, delta, group = Model)) +
  geom_violin(aes(fill = Model), alpha = .25) +
  geom_quasirandom(aes(color = Model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = Model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Delta")

tbl_beta <- tbl_results_kalman_ucb %>%
  select(beta_ml) %>%
  mutate(Model = "Kalman UCB") %>%
  rename(beta = beta_ml)  %>%
  rbind(
    tbl_results_kalman_ru_thompson %>%
      select(beta_ml) %>%
      mutate(Model = "Kalman RU & Thompson") %>%
      rename(beta = beta_ml)
  )   %>%
  rbind(
    tbl_results_kalman_ucb_thompson %>%
      select(beta_ml) %>%
      mutate(Model = "Kalman UCB & Thompson") %>%
      rename(beta = beta_ml)
  ) 

tbl_beta$Model <- factor(tbl_beta$Model)
tbl_beta$Model <- fct_inorder(tbl_beta$Model)
pl_beta <- ggplot(tbl_beta, aes(Model, beta, group = Model)) +
  geom_violin(aes(fill = Model), alpha = .25) +
  geom_quasirandom(aes(color = Model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = Model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Beta")



tbl_w_mix <- tbl_results_kalman_ru_thompson %>%
  select(w_mix_ml) %>%
  mutate(Model = "Kalman RU & Thompson") %>%
  rename(w_mix = w_mix_ml) %>%
  rbind(
    tbl_results_kalman_ucb_thompson %>%
      select(w_mix_ml) %>%
      mutate(Model = "Kalman UCB & Thompson") %>%
      rename(w_mix = w_mix_ml)
  )

tbl_w_mix$Model <- factor(tbl_w_mix$Model)
tbl_w_mix$Model <- fct_inorder(tbl_w_mix$Model)
pl_w_mix <- ggplot(tbl_w_mix, aes(Model, w_mix, group = Model)) +
  geom_violin(aes(fill = Model), alpha = .25) +
  geom_quasirandom(aes(color = Model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = Model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "w Thompson (vs. RU)")


pl_params_empirical <- arrangeGrob(pl_gammas, pl_w_mix, pl_deltas, pl_beta, nrow = 1, widths = c(1, .25, .5, .5))
save_my_pdf(pl_params_empirical, "figures/estimated-parameters-empirical.pdf", 19.5, 5.5)
#save_my_pdf(pl_params_empirical, "figures/estimated-parameters-empirical-individual-stimuli.pdf", 12, 4)



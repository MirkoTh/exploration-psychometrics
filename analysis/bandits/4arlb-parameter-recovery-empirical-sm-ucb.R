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
load("analysis/bandits/banditsWave1.Rda")
tbl_exclude <- readRDS(file = "analysis/wm/subjects-excl-wm.rds")
tbl_rb <- as_tibble(restless) %>%
  left_join(
    tbl_exclude[, c("participant_id", "excl_subject")], 
    by = c("ID" = "participant_id")
    ) %>%
  filter(excl_subject == 0 & ID != 168) %>%
  rename(choices = chosen, rewards = reward) %>%
  mutate(choices = choices + 1)
l_participants <- tbl_rb %>% split(., .$"ID")

# generate randomly walking arms
nr_trials <- nrow(l_participants[[1]])
nr_participants <- length(l_participants)

tbl_rewards <- tbl_rb  %>%
  filter(ID == 1) %>%
  select(starts_with("reward")) %>%
  rename(
    "Arm 1" = reward1,
    "Arm 2" = reward2,
    "Arm 3" = reward3,
    "Arm 4" = reward4
  ) %>% select(- rewards)



my_participants_tbl_kalman <- function(l_params_decision, sim_d) {
  tibble(
    sigma_prior = 1000,
    mu_prior = 50,
    sigma_xi_sq = 7.84,
    sigma_epsilon_sq = 16,
    lambda = 9999, # irrelevant because stimulus set known
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(nr_participants, 100000, 1000))
  )
}

my_participants_tbl_delta <- function(l_params_decision, delta, sim_d) {
  tibble(
    delta = delta,
    lambda = 9999, # irrelevant because stimulus set known
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(nr_participants, 100000, 1000))
  )
}

# values from actual stimulus set
sigma_xi_sq <- 7.84
sigma_epsilon_sq <- 16
sigma_prior <- 1000
mu_prior <- 50



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
  tbl_participants_kalman_softmax <- my_participants_tbl_kalman(l_params_decision, FALSE)
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
  
  tbl_participants_kalman_ucb <- my_participants_tbl_kalman(l_params_decision, FALSE)
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
  filter(between(beta, -3, 3)) %>%
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



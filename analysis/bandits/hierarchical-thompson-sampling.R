rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(cmdstanr)
library(mvtnorm)


is_fit <- FALSE#TRUE#

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)

# Read and Preprocess Data ------------------------------------------------


load(file = "analysis/bandits/banditsWave1.Rda")
restless1 <- restless
horizon1 <- horizon
sam1 <- sam
load(file = "analysis/bandits/banditsWave2.Rda")
restless2 <- restless
horizon2 <- horizon
sam2 <- sam

tbl_exclude2 <- read_csv(file = "data/exclusions2.csv")
tbl_exclude1 <- read_csv(file = "data/exclusions1.csv")

# seems like two people were invited to session 2, who should not have been
tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
  left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
  filter(exclude_2 == 0 & exclude_1 == 0)


tbl_restless1 <- read_csv("data/finalRestlessSession1.csv")
tbl_restless2 <- read_csv("data/finalRestlessSession2.csv")


# combine data from session 1 and session 2
tbl_restless <- rbind(tbl_restless1, tbl_restless2) %>%
  inner_join(tbl_exclude, by = "ID") %>%
  arrange(ID, session, trial) %>%
  group_by(ID, session) %>%
  mutate(
    chosen_prev = lag(chosen, 1),
    is_repetition = chosen == chosen_prev
  ) %>%
  group_by(ID, trial, session) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2, reward3, reward4),
    is_max = reward == max_reward_trial,
    chosen = chosen + 1
  ) %>% ungroup() %>%
  rename(
    choices = chosen,
    rewards = reward
  )

tbl_restless_s1 <- tbl_restless %>% 
  filter(session == 1) %>%
  arrange(ID, trial)
tbl_restless_s2 <- tbl_restless %>% 
  filter(session == 2) %>%
  arrange(ID, trial)



# subset for testing
n_subjects <- c(10, 175)[1]
all_ids <- unique(tbl_restless$ID)
tbl_restless_s1 <- tbl_restless_s1 %>% filter(ID %in% all_ids[1:n_subjects] & trial <= 100) # 
tbl_restless_s2 <- tbl_restless_s2 %>% filter(ID %in% all_ids[1:n_subjects] & trial <= 100) # 


# approximate thompson choice probabilities outside of stan
# because multivariate normal cdf is not available
# only fit inv temp, expl bonus, and mixture probability of thompson & ucb

no <- 4
m0 <- 50
v0 <- 1000
sigma_epsilon_sq <- 16
sigma_xi_sq <- 7.84
lambda <- 0.9836
decay_center <- 50

thompson_choice_probs_by_participant <- function(tbl_df) {
  
  max_trials <- max(tbl_df$trial)
  tbl_learned <- kalman_learning(tbl_df, no, sigma_xi_sq, sigma_epsilon_sq, m0, v0, lambda, decay_center)
  ms <- tbl_learned %>% select(starts_with("m_")) %>% as.matrix()
  vs <- tbl_learned %>% select(starts_with("v_")) %>% as.matrix()
  tbl_choice_probs <- thompson_choice_prob_map(ms, vs %>% as.matrix(), no)
  return(list(
    m = ms,
    v = vs,
    th_ch_probs = tbl_choice_probs[1:max_trials, ]
  )
  )
}

prepare_stan_data <- function(tbl_df) {
  
  l_thompson_choice_prob <- map(split(tbl_df, tbl_df$ID), thompson_choice_probs_by_participant)
  max_trials <- max(tbl_df$trial)
  
  l_data <- list(
    nSubjects = length(unique(tbl_df$ID)),
    nTrials = max(tbl_df$trial),
    choice = pivot_wider(tbl_df[, c("ID", "trial", "choices")], names_from = "trial", values_from = "choices") %>% select(-ID) %>% as.matrix(),
    reward = pivot_wider(tbl_df[, c("ID", "trial", "rewards")], names_from = "trial", values_from = "rewards") %>% select(-ID) %>% as.matrix(),
    th_ch_pr = map(l_thompson_choice_prob, ~ .x$th_ch_probs[1:max_trials, ]),
    m = map(l_thompson_choice_prob, ~ .x$m[1:max_trials, ]),
    var_mean = map(l_thompson_choice_prob, ~ .x$v[1:max_trials, ])
  )
}

l_data_s1 <- prepare_stan_data(tbl_restless_s1)
l_data_s2 <- prepare_stan_data(tbl_restless_s2)


pars_interest <- c("beta", "tau", "w_mix")
pars_group <- c("mu_beta", "mu_tau", "mu_w_mix")
pars_pred <- c("choice_pred", "log_lik")


ids_sample <- tibble(
  ID = unique(tbl_restless$ID),
  id_stan = 1:length(unique(tbl_restless$ID))
)


file_loc_hc_s1<- "data/rl-hc-thompson-ucb-mixture-s1.RDS"
th_ucb_stan_hc_txt <- mix_thompson_ucb_stan_hierarchical_generate()
mod_th_ucb_stan_hc <- cmdstan_model(th_ucb_stan_hc_txt)


init_fun <- function() list(
  mu_tau = 5.25,
  mu_beta = 0,
  mu_w_mix = .05,
  sigma_tau = .5,
  sigma_beta = 1,
  sigma_w_mix = .1,
  tau = rnorm(n_subjects, 5.5, .01),
  beta = rnorm(n_subjects, 0, .01),
  w_mix = rnorm(n_subjects, .04, .01)
)

if (is_fit) {
  
  # session 1
  fit_restless_thompson_ucb_mix_hc_s1 <- mod_th_ucb_stan_hc$sample(
    data = l_data_s1, iter_sampling = 400, iter_warmup = 200, chains = 3, 
    parallel_chains = 3, init = init_fun
  )
  
  tbl_draws_hc_s1 <- fit_restless_thompson_ucb_mix_hc_s1$draws(
    variables = c(pars_interest, pars_group), format = "df"
    )
  tbl_summary_hc_s1 <- fit_restless_thompson_ucb_mix_hc_s1$summary(variables = c(pars_interest, pars_group))
  tbl_summary_hc_s1 %>% arrange(desc(rhat))
  # , pars_pred
  saveRDS(tbl_draws_hc_s1, file_loc_hc_s1)
  
}
if (!is_fit) {
  tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
  tbl_draws_hc_s2 <- readRDS(file_loc_hc_s2)
}


l_posterior_hc_1 <- posteriors_and_maps_bandits(tbl_draws_hc_s1, 1, c("beta", "tau", "w_mix"), ids_sample)
l_posterior_hc_1[[2]] %>% ggplot(aes(map)) + geom_histogram() + facet_wrap(~ parameter, scales = "free_x")

l_posterior_hc_1[[1]] %>% ggplot(aes(value, group = ID)) + geom_histogram() + facet_grid(ID ~ parameter, scales = "free_x")


















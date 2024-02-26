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



# generate fixed stimulus sets --------------------------------------------


# values from actual stimulus set
sigma_xi_sq <- 7.84
sigma_epsilon_sq <- 16
sigma_prior <- 1000
mu_prior <- 50
# and values for generating similar stimulus sets
lambda <- .9836
nr_trials <- 200
center_decay <- 50


my_two_seeds <- c(997733, 49015499)
set.seed(my_two_seeds[1])
mu_init <- rnorm(4, 50, 3)
tbl_rewards_w1 <- generate_restless_bandits(
  sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq, mu1 = mu_init, 
  lambda = lambda, nr_trials = nr_trials, center_decay = center_decay
) %>% mutate_if(is.double, round)

set.seed(my_two_seeds[2])
mu_init <- rnorm(4, 50, 3)
tbl_rewards_w2 <- generate_restless_bandits(
  sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq, mu1 = mu_init, 
  lambda = lambda, nr_trials = nr_trials, center_decay = center_decay
) %>% mutate_if(is.double, round)


# expected rewards on fixed stimulus sets ---------------------------------



l_params_decision <- list(
  gamma = .2,
  beta = .05,
  choicemodel = "ucb",
  no = 4
)
bds_ucb <- list(gamma = list(lo = 0, hi = .5), beta = list(lo = -10, hi = 10))
n_iter <- 5


gamma <- seq(.01, 1, length.out = 3)
beta <- seq(-3, 3, length.out = 3)
tbl_params <- crossing(gamma, beta)

# parallelization in inner loop
# outer loop computed sequentially
l_results_w1 <- list()
l_results_w2 <- list()

for (i in 1:nrow(tbl_params)) {
  l_params_decision$gamma <- tbl_params$gamma[i]
  l_params_decision$beta <- tbl_params$beta[i]
  plan(multisession, workers = availableCores() - 2)
  l_results_w1[[i]] <- future_map(1:n_iter, ~ simulate_kalman(
    sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, 
    lambda, nr_trials, l_params_decision, simulate_data = FALSE,
    seed = .x, tbl_rewards = tbl_rewards_w1 %>% select(-trial_id)
  ), seed = NULL, .progress = TRUE)
  l_results_w2[[i]] <- future_map(1:n_iter, ~ simulate_kalman(
    sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, 
    lambda, nr_trials, l_params_decision, simulate_data = FALSE,
    seed = .x, tbl_rewards = tbl_rewards_w2 %>% select(-trial_id)
  ), seed = NULL, .progress = TRUE)
  plan("sequential")
}

tbl_results_w1 <- map(
  l_results_w1, ~ map_dbl(
    .x, ~ sum(.x$tbl_return$rewards)
  )) %>% reduce(rbind) %>% as.data.frame() %>%
  as_tibble() %>% mutate(stim_set = "Wave 1")
colnames(tbl_results_w1) <- c(str_c("it", 1:(ncol(tbl_results_w1) - 1)), "stim_set")
tbl_results_w1 <- cbind(tbl_params, tbl_results_w1) %>%
  pivot_longer(starts_with("it"))

tbl_results_w2 <- map(
  l_results_w2, ~ map_dbl(
    .x, ~ sum(.x$tbl_return$rewards)
  )) %>% reduce(rbind) %>% as.data.frame() %>%
  as_tibble() %>% mutate(stim_set = "Wave 2")
colnames(tbl_results_w2) <- c(str_c("it", 1:(ncol(tbl_results_w2) - 1)), "stim_set")
tbl_results_w2 <- cbind(tbl_params, tbl_results_w2) %>%
  pivot_longer(starts_with("it"))

tbl_results <- rbind(tbl_results_w1, tbl_results_w2)

grouped_agg(tbl_results, c(gamma, beta, stim_set), value) %>%
  ggplot(aes(gamma, beta)) +
  geom_tile(aes(fill = mean_value)) +
  facet_wrap(~ stim_set)


# expected rewards on random stimulus sets --------------------------------




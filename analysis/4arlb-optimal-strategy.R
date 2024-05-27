rm(list = ls())


# note sigma_prior refers to variance (i.e., sigma squared) and not to sd


# Import Packages and Load Data -------------------------------------------


library(tidyverse)
library(rutils)
library(future)
library(furrr)
library(grid)
library(gridExtra)

home_grogridExtrahome_grown <- c("utils/analysis-utils.R", "utils/modeling-utils.R", "utils/plotting-utils.R")
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


my_two_seeds <- c(997733, 5247222)
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

ggplot(
  rbind(tbl_rewards_w1 %>% mutate(wave = 1), tbl_rewards_w2 %>% mutate(wave = 2)) %>%
    pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)),
  aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), linewidth = .75) +
  facet_wrap(~ wave) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")



# expected rewards on fixed stimulus sets ---------------------------------


l_params_decision <- list(
  gamma = .2,
  beta = .05,
  choicemodel = "ucb",
  no = 4
)
n_iter <- 50


gamma <- c(seq(.01, .25, length.out = 4), .5, 1, 2, 3, 4.5)
beta <- sort(c(0, seq(-3, 3, length.out = 10)))
tbl_params <- crossing(gamma, beta)

# parallelization in inner loop
# outer loop computed sequentially
l_results_w1 <- list()
l_results_w2 <- list()

is_fit <- TRUE
# takes approx. 17 mins for n_iter = 10, and 10x10 parameter grid
if (is_fit) {
  plan(multisession, workers = availableCores() - 3)
  pb <- progress::progress_bar$new(total = nrow(tbl_params))
  for (i in 1:nrow(tbl_params)) {
    l_params_decision$gamma <- tbl_params$gamma[i]
    l_params_decision$beta <- tbl_params$beta[i]
    l_results_w1[[i]] <- future_map(1:n_iter, ~ simulate_kalman(
      sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, 
      lambda, nr_trials, l_params_decision, simulate_data = FALSE,
      seed = .x, tbl_rewards = tbl_rewards_w1 %>% select(-trial_id),
      decay_center = center_decay
    ), seed = NULL)
    l_results_w2[[i]] <- future_map(1:n_iter, ~ simulate_kalman(
      sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, 
      lambda, nr_trials, l_params_decision, simulate_data = FALSE,
      seed = .x, tbl_rewards = tbl_rewards_w2 %>% select(-trial_id),
      decay_center = center_decay
    ), seed = NULL)
    pb$tick()
  }
  # save results
  l_results_both <- list(
    l_results_w1, l_results_w2
  )
  saveRDS(l_results_both, "data/4arlb-optimal-2-fixed.rds")
  plan("sequential")
} else if (!is_fit) {
  l_results_both <- readRDS("data/4arlb-optimal-2-fixed.rds")
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



# expected rewards on random stimulus sets --------------------------------

l_results_random <- list()
if (is_fit) {
  plan(multisession, workers = availableCores() - 3)
  pb <- progress::progress_bar$new(total = nrow(tbl_params))
  for (i in 1:nrow(tbl_params)) {
    l_params_decision$gamma <- tbl_params$gamma[i]
    l_params_decision$beta <- tbl_params$beta[i]
    l_results_random[[i]] <- future_map(1:n_iter, ~ simulate_kalman(
      sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, 
      lambda, nr_trials, l_params_decision, simulate_data = TRUE,
      seed = .x, tbl_rewards = tbl_rewards_w1 %>% select(-trial_id),
      mu_init = "sample", decay_center = center_decay
    ))
    pb$tick()
  }
  # save results
  saveRDS(l_results_random, "data/4arlb-optimal-random.rds")
  plan("sequential")
} else if (!is_fit) {
  l_results_random <- readRDS("data/4arlb-optimal-random.rds")
}

tbl_results_random <- map(
  l_results_random, ~ map_dbl(
    .x, ~ sum(.x$tbl_return$rewards)
  )) %>% reduce(rbind) %>% as.data.frame() %>%
  as_tibble() %>% mutate(stim_set = "Random Stimulus Sets")
colnames(tbl_results_random) <- c(str_c("it", 1:(ncol(tbl_results_random) - 1)), "stim_set")
tbl_results_random <- cbind(tbl_params, tbl_results_random) %>%
  pivot_longer(starts_with("it"))



# plot results ------------------------------------------------------------

tbl_results <- rbind(tbl_results_w1, tbl_results_w2, tbl_results_random)
tbl_results$gamma <- factor(round(tbl_results$gamma, 2))
tbl_results$beta <- factor(round(tbl_results$beta, 2))

pl_optimal_strategy_2d <- grouped_agg(tbl_results, c(gamma, beta, stim_set), value) %>%
  group_by(stim_set) %>%
  mutate(mean_value_prop = mean_value/max(mean_value)) %>%
  ggplot(aes(gamma, beta)) +
  geom_tile(aes(fill = mean_value_prop)) +
  geom_text(aes(label = round(mean_value_prop, 3)), color = "white") + 
  facet_wrap(~ stim_set) +
  scale_fill_viridis_c(guide = "none") +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = expression(gamma), y = expression(beta)) + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  )

pd <- position_dodge(width = .15)
tbl_results_agg <- grouped_agg(tbl_results, c(gamma, stim_set), value) %>%
  # group_by(stim_set) %>%
  # mutate(mean_value_shifted = mean_value - min(mean_value)) %>%
  pivot_longer(gamma) %>%
  rbind(
    grouped_agg(tbl_results, c(beta, stim_set), value) %>%
      # group_by(stim_set) %>%
      # mutate(mean_value_shifted = mean_value - min(mean_value)) %>%
      pivot_longer(beta)
  ) %>% ungroup()
tbl_results_agg$value <- as.factor(as.numeric(as.character(tbl_results_agg$value)))
tbl_results_agg$name <- factor(tbl_results_agg$name, labels = c("beta", "gamma"))

pl_optimal_strategy_1d <- ggplot(tbl_results_agg, aes(value, mean_value, group = stim_set)) +
  geom_line(aes(color = stim_set), position = pd) +
  geom_errorbar(aes(
    ymin = mean_value - 1.96 * se_value, 
    ymax = mean_value + 1.96 * se_value, 
    color = stim_set
  ), width = .15, position = pd) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = stim_set), position = pd) +
  facet_wrap(~ name, scales = "free_x", labeller = label_parsed) +
  theme_bw() +
  scale_x_discrete(expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.01, 0), labels = scales::comma) +
  labs(x = "Parameter Value", y = "Cum. Rewards") + 
  theme(
    text = element_text(size = 22),
    axis.text.y = element_text(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white")
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4", "gold"), name = "")


pl_optimal_stratagy_1d_2d <- arrangeGrob(pl_optimal_strategy_2d, pl_optimal_strategy_1d, nrow = 2)
save_my_pdf_and_tiff(
  pl_optimal_stratagy_1d_2d,
  "figures/4arlb-optimal-strategy", 16, 10
)

test_sum_bandits <- function(idx) {
  tmp <- generate_restless_bandits(
    sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq, mu1 = mu_init, 
    lambda = lambda, nr_trials = nr_trials, center_decay = center_decay
  ) %>% mutate_if(is.double, round) %>%
    pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`))
  sum(tmp$value)
  
}
r <- map_dbl(1:100, test_sum_bandits)

w1_long <- tbl_rewards_w1 %>% mutate_if(is.double, round) %>%
  pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`))

w2_long <- tbl_rewards_w2 %>% mutate_if(is.double, round) %>%
  pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`))

mean(r)
sum(w1_long$value)
sum(w2_long$value)




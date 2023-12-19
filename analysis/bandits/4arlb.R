rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)

dir_data_rel <- c("data/pilot/", "data/2023-11-lab-pilot/")[2]

load(file = str_c(dir_data_rel, "bandits.Rda"))

tbl_restless <- as_tibble(restless)
tbl_restless <- tbl_restless %>%
  group_by(ID, trial) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2, reward3, reward4),
    is_max = reward == max_reward_trial,
    chosen = chosen + 1
  ) %>% ungroup() %>%
  rename(
    choices = chosen,
    rewards = reward
  )

tbl_4arlb_performance <- grouped_agg(tbl_restless, c(ID, session), is_max) %>% 
  ungroup() %>%
  rename(prop_correct_4arlb = mean_is_max)

tbl_sam <- as_tibble(sam)
tbl_horizon <- as_tibble(horizon)


tbl_sam <- tbl_sam %>%
  group_by(ID, block, trial) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2),
    is_max = reward == max_reward_trial
  ) %>% ungroup()

tbl_sam_performance <- grouped_agg(tbl_sam, c(ID, session), is_max) %>% 
  ungroup() %>%
  rename(prop_correct_sam = mean_is_max)

tbl_horizon_trial1 <- tbl_horizon %>%
  filter(trial >= 5 & !is.na(reward)) %>%
  group_by(ID, block, trial) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2),
    is_max = reward == max_reward_trial
  ) %>% ungroup()

tbl_horizon_performance <- grouped_agg(tbl_horizon_trial1, c(ID, session), is_max) %>% 
  ungroup() %>%
  rename(prop_correct_horizon = mean_is_max)


tbl_performance_3tasks <- tbl_4arlb_performance %>%
  left_join(tbl_sam_performance[, c("ID", "prop_correct_sam")], by = "ID") %>%
  left_join(tbl_horizon_performance[, c("ID", "prop_correct_horizon")], by = "ID") %>%
  arrange(prop_correct_4arlb) %>%
  mutate(ID = fct_inorder(as.character(ID)))

tbl_performance_3tasks <- tbl_performance_3tasks %>%
  pivot_longer(c(prop_correct_4arlb, prop_correct_sam, prop_correct_horizon))
tbl_performance_3tasks$name <- factor(tbl_performance_3tasks$name, labels = c("4ARLB", "Sam", "Horizon"))
tbl_hline <- tibble(
  name = c("4ARLB", "Horizon", "Sam"),
  y_ic = c(.25, .5, .5)
)

ggplot(tbl_performance_3tasks, aes(ID, value)) +
  geom_hline(data = tbl_hline, aes(yintercept = y_ic, group = name), linetype = "dotdash", color = "tomato3", size = 1) +
  geom_text(aes(label = ID), size = 5) +
  facet_wrap(~ name) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(.04, 0), breaks = seq(1, 10, by = 1)) +
  labs(x = "Participant ID", y = "Prop. Best Arm") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 18)
  )


x <- c(.2, 2)
bds <- list(gamma = list(lo = 0, hi = 1), beta = list(lo = -5, hi = 5))
params_init <- pmap_dbl(
  list(x, map_dbl(bds, "lo"), map_dbl(bds, "hi")),
  upper_and_lower_bounds
)
tbl_results <- tbl_restless
nr_options <- 4
sigma_xi_sq <- 16 # 36 in stimulus set of first prolific pilot; 16 in lab pilot
sigma_epsilon_sq <- 16
tbl_results1 <- filter(tbl_results, ID == 1)
l_tbl_results <- split(tbl_results, tbl_results$ID)


l_fits <- list()
l_inits <- list()
n_init_vals <- 10


t_start <- Sys.time()
future::plan(future::multisession, workers = future::availableCores() - 2)

for (i in 1:n_init_vals) {
  params_init <- c(runif(1, bds$gamma$lo, bds$gamma$hi), runif(1, bds$beta$lo, bds$beta$hi))
  l_fit <- furrr::future_map(
    l_tbl_results, fit_ucb_no_variance_wrapper, 
    tbl_rewards = NULL, 
    condition_on_observed_choices = TRUE,
    sigma_xi_sq = sigma_xi_sq, 
    sigma_epsilon_sq = sigma_epsilon_sq, 
    bds = bds, 
    params_init = params_init
  )
  l_fits[[i]] <- l_fit
  l_inits[[i]] <- params_init
}
future::plan("default")
beepr::beep()
t_end <- Sys.time()
round(t_end - t_start, 1)

tbl_fits_4arlb <- map2(
  l_fits, 1:length(l_fits), 
  ~ reduce(.x, rbind) %>% as.data.frame() %>% mutate(it = .y, ID = 1:max(tbl_results$ID)) %>%
    rename(sum_ll = V3, beta = V2, gamma = V1) %>%
    relocate(ID, .before = gamma)
) %>% reduce(rbind) %>%
  arrange(ID) %>%
  group_by(ID) %>%
  mutate(rank = row_number(sum_ll)) %>%
  ungroup()



fit_ucb_no_variance_wrapper(l_tbl_results[[1]], tbl_rewards = NULL, condition_on_observed_choices = TRUE,
                            sigma_xi_sq = 36, sigma_epsilon_sq = 16, los = los, his = his)



tau <- .031
m1 <- seq(0, 100, by = .1)
m2 <- 50
prob1 <- exp(m1 * tau)
prob2 <- rep(exp(m2 * tau), length(prob1))
tbl_softmax <- tibble(
  m = m1,
  p = prob1 / (prob1 + prob2)
)
ggplot(tbl_softmax, aes(m, p)) + geom_point() + scale_x_continuous(breaks = seq(0, 100, by = 5))

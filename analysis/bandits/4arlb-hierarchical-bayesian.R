rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(gridExtra)
library(cmdstanr)


is_fit <- TRUE#FALSE#
is_hierarchical <- TRUE#FALSE#

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)



# Read and Preprocess Data ------------------------------------------------


load(file = "analysis/bandits/banditsWave1full.Rda")
restless1 <- restless
horizon1 <- horizon
sam1 <- sam
load(file = "analysis/bandits/banditsWave2full.Rda")
restless2 <- restless
horizon2 <- horizon
sam2 <- sam

tbl_wm <- readRDS("data/all-data/tbl-performance-wm.rds")
#tbl_exclude <- readRDS(file = "analysis/wm/subjects-excl-wm.rds")

# some IDs do not yet have data for session 1 because of loading restless data from temporary files (work in progress)
# in the meantime, just exclude IDs without complete data sets for both sessions

n_valid_session1 <- restless1 %>% group_by(ID) %>% 
  summarize(n_valid_1 = sum(chosen >= 0)) %>% ungroup() %>% 
  replace_na(list(n_valid_1 = 0)) %>% arrange(n_valid_1)
n_valid_session2 <- restless2 %>% group_by(ID) %>% 
  summarize(n_valid_2 = sum(chosen >= 0)) %>% ungroup() %>% 
  replace_na(list(n_valid_2 = 0)) %>% arrange(n_valid_2)

tbl_valid_restless <- tibble(ID = unique(n_valid_session1$ID, n_valid_session2$ID)) %>%
  left_join(n_valid_session1, by = "ID") %>%
  left_join(n_valid_session2, by = "ID") %>%
  replace_na(list(n_valid_1 = 0, n_valid_2 = 0)) %>%
  filter(n_valid_1 == 200 & n_valid_2 == 200)
restless1 <- restless1 %>% inner_join(tbl_valid_restless %>% select(ID), by = "ID")
restless2 <- restless2 %>% inner_join(tbl_valid_restless %>% select(ID), by = "ID")



tbl_excl_bandits_1 <- read_csv("data/exclusions1.csv")
tbl_excl_bandits_2 <- read_csv("data/exclusions2.csv")
tbl_excl_wm <- readRDS("analysis/wm/subjects-excl-wm.rds")




tbl_exclude <- tbl_excl_wm %>%
  left_join(tbl_excl_bandits_1 %>% select(ID, exclude), by = c("participant_id" = "ID")) %>%
  left_join(tbl_excl_bandits_2 %>% select(ID, exclude), by = c("participant_id" = "ID"), suffix = c("_1", "_2")) %>%
  mutate(exclude_all = ifelse((excl_subject + exclude_1 + exclude_2) > 0, TRUE, FALSE))


# combine data from session 1 and session 2
tbl_restless <- as_tibble(rbind(restless1, restless2)) %>%
  left_join(tbl_exclude, by = c("ID" = "participant_id")) %>%
  filter(exclude_all == 0) %>%
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


l_data_s1 <- list(
  nSubjects = length(unique(tbl_restless_s1$ID)),
  nTrials = max(tbl_restless_s1$trial),
  choice = pivot_wider(tbl_restless_s1[, c("ID", "trial", "choices")], names_from = "trial", values_from = "choices") %>% select(-ID) %>% as.matrix(),
  reward = pivot_wider(tbl_restless_s1[, c("ID", "trial", "rewards")], names_from = "trial", values_from = "rewards") %>% select(-ID) %>% as.matrix()
)
l_data_s2 <- list(
  nSubjects = length(unique(tbl_restless_s2$ID)),
  nTrials = max(tbl_restless_s2$trial),
  choice = pivot_wider(tbl_restless_s2[, c("ID", "trial", "choices")], names_from = "trial", values_from = "choices") %>% select(-ID) %>% as.matrix(),
  reward = pivot_wider(tbl_restless_s2[, c("ID", "trial", "rewards")], names_from = "trial", values_from = "rewards") %>% select(-ID) %>% as.matrix()
)




# by-participant ----------------------------------------------------------


ucb_stan_txt <- ucb_stan()
mod_ucb_stan <- cmdstan_model(ucb_stan_txt)

file_loc_s1 <- "data/restless-hierarchical-model-posterior.RDS"
file_loc_s2 <- "data/restless-model-posterior-s2.RDS"
pars_interest <- c("beta", "tau")

if (is_fit & !is_hierarchical) {
  
  # session 1
  fit_restless_ucb_s1 <- mod_ucb_stan$sample(
    data = l_data_s1, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_s1 <- fit_restless_ucb_s1$draws(variables = pars_interest, format = "df")
  tbl_summary_s1 <- fit_restless_ucb_s1$summary(variables = pars_interest)
  tbl_summary_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_s1, file_loc_s1)
  
  
  # session 2
  fit_restless_ucb_s2 <- mod_ucb_stan$sample(
    data = l_data_s2, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_s2 <- fit_restless_ucb_s2$draws(variables = pars_interest, format = "df")
  tbl_summary_s2 <- fit_restless_ucb_s2$summary(variables = pars_interest)
  tbl_summary_s2 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_s2, file_loc_s2)
  
} else if (!is_fit) {
  tbl_draws_s1 <- readRDS(file_loc_s1)
  tbl_draws_s2 <- readRDS(file_loc_s2)
}

ids_sample <- tibble(
  ID = unique(tbl_restless$ID),
  id_stan = 1:length(unique(tbl_restless$ID))
)

posteriors_and_maps <- function(tbl_draws, s, pars_interest) {
  
  tbl_posterior <- tbl_draws %>% 
    dplyr::select(starts_with(pars_interest), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(pars_interest), names_to = "parameter", values_to = "value") %>%
    mutate(
      id_stan = as.integer(str_extract(parameter, "[0-9]+")),
      parameter = str_extract(parameter, "^[a-z]+")
    ) %>%
    left_join(ids_sample, by = "id_stan") %>%
    relocate(ID, .before = parameter) %>%
    select(-c(id_stan))
  
  tbl_map <- tbl_posterior_s1 %>% group_by(ID, parameter) %>% 
    summarize(map = mean(value)) %>%
    ungroup() %>%
    mutate(
      parameter = factor(parameter, labels = c("ru", "v")),
      session = s
    )
  
  return(list(tbl_posterior = tbl_posterior, tbl_map = tbl_map))
  
}


l_posterior_1 <- posteriors_and_maps(tbl_draws_s1, 1, c("beta", "tau"))
l_posterior_2 <- posteriors_and_maps(tbl_draws_s2, 2, c("beta", "tau"))



# plausibilize with ml fit
tbl_max_likely <- readRDS("data/4arlb-overview.rds")
tbl_max_likely <- tbl_max_likely %>% ungroup() %>% 
  filter(session == 1) %>% 
  select(ID, beta, gamma_ucb) %>%
  mutate(ID = as.numeric(as.character(ID))) %>%
  rename(ru = beta, v = gamma_ucb) %>%
  pivot_longer(c(ru, v))

l_posterior_1$tbl_map %>% 
  inner_join(tbl_max_likely, by = c("ID", "parameter" = "name")) %>%
  ggplot(aes(map, value, group = parameter)) +
  geom_abline() +
  geom_point(aes(color = parameter)) +
  facet_wrap(~ parameter) +
  coord_cartesian(xlim = c(-.5, .5), ylim = c(-.5, .5)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "MAP", y = "Maximum Likelihood") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")




# hierarchical ------------------------------------------------------------



ucb_stan_hc_txt <- ucb_stan_hierarchical()
mod_ucb_stan_hc <- cmdstan_model(ucb_stan_hc_txt)

file_loc_hc_s1 <- "data/restless-hierarchical-model-posterior-s1.RDS"
file_loc_hc_s2 <- "data/restless-hierarchical-model-posterior-s2.RDS"

if (is_fit & is_hierarchical) {
  
  # session 1
  fit_restless_ucb_hc_s1 <- mod_ucb_stan_hc$sample(
    data = l_data_s1, iter_sampling = 1000, iter_warmup = 500, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s1 <- fit_restless_ucb_hc_s1$draws(variables = c(pars_interest, pars_group), format = "df")
  tbl_summary_hc_s1 <- fit_restless_ucb_hc_s1$summary(variables = c(pars_interest, pars_group))
  tbl_summary_hc_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s1, file_loc_hc_s1)
  
  
  # session 2
  fit_restless_ucb_hc_s2 <- mod_ucb_stan_hc$sample(
    data = l_data_s2, iter_sampling = 1000, iter_warmup = 500, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s2 <- fit_restless_ucb_hc_s2$draws(variables = c(pars_interest, pars_group), format = "df")
  tbl_summary_hc_s2 <- fit_restless_ucb_hc_s2$summary(variables = c(pars_interest, pars_group))
  tbl_summary_hc_s2 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s2, file_loc_hc_s2)
  
} else if (!is_fit) {
  tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
  tbl_draws_hc_s2 <- readRDS(file_loc_hc_s2)
}

l_posterior_hc_1 <- posteriors_and_maps(tbl_draws_hc_s1, 1, c("beta", "tau"))
l_posterior_hc_2 <- posteriors_and_maps(tbl_draws_hc_s2, 2, c("beta", "tau"))

pars_group <- c("mu_beta", "mu_tau")


group_posteriors <- function(tbl_draws) {
  tbl_posterior <- tbl_draws %>% 
    dplyr::select(starts_with(pars_group), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(pars_group), names_to = "parameter", values_to = "value")
  
  l <- sd_bfs(tbl_posterior, pars_group, .5, limits = c(.025, .975))
  bfs <- l[[1]]
  tbl_thx <- l[[2]]
  bfs <- bfs[names(bfs) %in% pars_group]
  tbl_thx <- tbl_thx %>% filter(parameter %in% pars_group)
  
  # plot the posteriors and the bfs
  l_pl <- map(as.list(pars_group), plot_posterior, tbl_posterior, tbl_thx, bfs)
  
  return(list(tbl_posterior = tbl_posterior, l_pl = l_pl))
}

l_post_hc_1 <- group_posteriors(tbl_draws_hc_s1)
l_post_hc_2 <- group_posteriors(tbl_draws_hc_s2)

grid.arrange(
  l_post_hc_1$l_pl[[1]] + ggtitle("S1: Group Beta"), l_post_hc_1$l_pl[[2]] + ggtitle("S1: Group Inv. Temp."),
  l_post_hc_2$l_pl[[1]] + ggtitle("S2: Group Beta"), l_post_hc_2$l_pl[[2]] + ggtitle("S2: Group Inv. Temp."),
  nrow = 2, ncol = 2
)



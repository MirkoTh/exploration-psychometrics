rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
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



# subset for testing
# all_ids <- unique(tbl_restless$ID)
# tbl_restless_s1 <- tbl_restless_s1 %>% filter(ID %in% all_ids[1:20])
# tbl_restless_s2 <- tbl_restless_s2 %>% filter(ID %in% all_ids[1:20])


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

pars_interest <- c("beta", "tau")
pars_group <- c("mu_beta", "mu_tau")
pars_pred <- c("choice_pred", "log_lik")



# by-participant ----------------------------------------------------------


ucb_stan_txt <- ucb_stan()
mod_ucb_stan <- cmdstan_model(ucb_stan_txt)

file_loc_s1 <- "data/restless-participant-model-posterior-s1.RDS"
file_loc_s2 <- "data/restless-participant-model-posterior-s2.RDS"



if (is_fit & !is_hierarchical) {
  
  # session 1
  fit_restless_ucb_s1 <- mod_ucb_stan$sample(
    data = l_data_s1, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_s1 <- fit_restless_ucb_s1$draws(variables = c(pars_interest, pars_pred), format = "df")
  tbl_summary_s1 <- fit_restless_ucb_s1$summary(variables = c(pars_interest, pars_pred))
  tbl_summary_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_s1, file_loc_s1)
  
  
  # session 2
  fit_restless_ucb_s2 <- mod_ucb_stan$sample(
    data = l_data_s2, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_s2 <- fit_restless_ucb_s2$draws(variables = c(pars_interest, pars_pred), format = "df")
  tbl_summary_s2 <- fit_restless_ucb_s2$summary(variables = c(pars_interest, pars_pred))
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

posteriors_and_maps <- function(tbl_draws, s, pars_interest, ids_sample) {
  
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
  
  tbl_map <- tbl_posterior %>% group_by(ID, parameter) %>% 
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
    data = l_data_s1, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s1 <- fit_restless_ucb_hc_s1$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s1 <- fit_restless_ucb_hc_s1$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s1, file_loc_hc_s1)
  
  
  # session 2
  fit_restless_ucb_hc_s2 <- mod_ucb_stan_hc$sample(
    data = l_data_s2, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s2 <- fit_restless_ucb_hc_s2$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s2 <- fit_restless_ucb_hc_s2$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s2 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s2, file_loc_hc_s2)
  
} else if (!is_fit) {
  tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
  tbl_draws_hc_s2 <- readRDS(file_loc_hc_s2)
}

l_posterior_hc_1 <- posteriors_and_maps(tbl_draws_hc_s1, 1, c("beta", "tau"), ids_sample)
l_posterior_hc_2 <- posteriors_and_maps(tbl_draws_hc_s2, 2, c("beta", "tau"), ids_sample)

# test-retest reliability
tbl_map_hc <- l_posterior_hc_1[[2]] %>% select(-session) %>% 
  left_join(
    l_posterior_hc_2[[2]] %>% select(-session),
    by = c("ID", "parameter"), suffix = c("_1", "_2")
  ) %>% 
  pivot_wider(names_from = parameter, values_from = c(map_1, map_2))
saveRDS(tbl_map_hc, "data/4arlb-maps-hierarchical.RDS")

tbl_map_hc %>%
  summarize(
    r_v = cor(map_1_v, map_2_v),
    r_ru = cor(map_1_ru, map_2_ru)
  )

# distribution of maps
hist_maps <- l_posterior_hc_1[[2]] %>% select(-session) %>% 
  left_join(
    l_posterior_hc_2[[2]] %>% select(-session),
    by = c("ID", "parameter"), suffix = c("_1", "_2")
  ) %>% pivot_longer(cols = c(map_1, map_2)) %>%
  mutate(name = factor(name, labels = c("Session 1", "Session 2"))) %>%
  ggplot(aes(value)) +
  geom_histogram(fill = "skyblue2", color = "black") +
  facet_grid(name ~ parameter, scales = "free_x") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "MAP", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  )

save_my_pdf_and_tiff(hist_maps, "figures/4arlb-hierarchical-histograms-maps",  6, 5.5)

## fixed effects
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
  l_post_hc_1$l_pl[[1]] + ggtitle("S1: Group Beta") + coord_cartesian(xlim = c(-1.5, .1)),
  l_post_hc_1$l_pl[[2]] + ggtitle("S1: Group Inv. Temp.") + coord_cartesian(xlim = c(-.1, .25)),
  l_post_hc_2$l_pl[[1]] + ggtitle("S2: Group Beta") + coord_cartesian(xlim = c(-1.5, .1)),
  l_post_hc_2$l_pl[[2]] + ggtitle("S2: Group Inv. Temp.") + coord_cartesian(xlim = c(-.1, .25)),
  nrow = 2, ncol = 2
)



# recoverability ----------------------------------------------------------


# get predictions from bayesian model and append to original data
my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
my_bayesian_preds <- function(tbl_draws) {
  tbl_pred <- tbl_draws %>% select(contains("choice_pred"))
  randomly_drawn_ids <- sample(1:nrow(tbl_pred), ncol(tbl_pred), replace = TRUE)
  
  tbl_preds <- tibble(
    choice_pred = map2_dbl(randomly_drawn_ids, tbl_pred, ~ .y[.x]),
    choice_pred_map = map_dbl(tbl_pred, my_mode)
  )
  
  return(tbl_preds)
}

tbl_restless_s1 <- cbind(tbl_restless_s1, my_bayesian_preds(tbl_draws_hc_s1))
tbl_restless_s2 <- cbind(tbl_restless_s2, my_bayesian_preds(tbl_draws_hc_s2))
tbl_restless_s1$reward_pred_map <- pmap_dbl(
  tbl_restless_s1[, c("reward1", "reward2", "reward3", "reward4", "choice_pred_map")], 
  ~ c(..1, ..2, ..3, ..4)[..5]
)
tbl_restless_s2$reward_pred_map <- pmap_dbl(
  tbl_restless_s2[, c("reward1", "reward2", "reward3", "reward4", "choice_pred_map")], 
  ~ c(..1, ..2, ..3, ..4)[..5]
)

# fit same model again on predicted data
l_data_s1$choice <- pivot_wider(tbl_restless_s1[, c("ID", "trial", "choice_pred_map")], names_from = "trial", values_from = "choice_pred_map") %>% select(-ID) %>% as.matrix()
l_data_s1$reward  <- pivot_wider(tbl_restless_s1[, c("ID", "trial", "reward_pred_map")], names_from = "trial", values_from = "reward_pred_map") %>% select(-ID) %>% as.matrix()

l_data_s2$choice <- pivot_wider(tbl_restless_s2[, c("ID", "trial", "choice_pred_map")], names_from = "trial", values_from = "choice_pred_map") %>% select(-ID) %>% as.matrix()
l_data_s2$reward  <- pivot_wider(tbl_restless_s2[, c("ID", "trial", "reward_pred_map")], names_from = "trial", values_from = "reward_pred_map") %>% select(-ID) %>% as.matrix()

file_loc_hc_s1_recovery <- "data/restless-hierarchical-model-recovery-posterior-s1.RDS"
file_loc_hc_s2_recovery <- "data/restless-hierarchical-model-recovery-posterior-s2.RDS"

if (is_fit & is_hierarchical) {
  
  # session 1
  fit_restless_ucb_hc_s1_recovery <- mod_ucb_stan_hc$sample(
    data = l_data_s1, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s1_recovery <- fit_restless_ucb_hc_s1_recovery$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s1_recovery <- fit_restless_ucb_hc_s1_recovery$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s1_recovery %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s1_recovery, file_loc_hc_s1_recovery)
  
  
  # session 2
  fit_restless_ucb_hc_s2_recovery <- mod_ucb_stan_hc$sample(
    data = l_data_s2, iter_sampling = 300, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s2_recovery <- fit_restless_ucb_hc_s2_recovery$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s2_recovery <- fit_restless_ucb_hc_s2_recovery$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s2_recovery %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s2_recovery, file_loc_hc_s2_recovery)
  
} else if (!is_fit) {
  tbl_draws_hc_s1_recovery <- readRDS(file_loc_hc_s1_recovery)
  tbl_draws_hc_s2_recovery <- readRDS(file_loc_hc_s2_recovery)
}

# compute correlations between input maps and output maps

by_participant_maps <- function(tbl_draws, f_time) {
  tbl_draws %>% select(starts_with(pars_interest)) %>%
    mutate(n_sample = 1:nrow(.)) %>%
    pivot_longer(-n_sample) %>%
    mutate(
      ID = as.integer(str_extract(name, "[0-9]+")),
      parameter = str_extract(name, "^[a-z]*")
    ) %>%
    group_by(ID, parameter) %>%
    summarize(map = mean(value)) %>%
    ungroup() %>%
    mutate(fit_time = f_time)
}

map_cor <- function(tbl_draws_hc, tbl_draws_hc_recovery) {
  
  l_tbl_draws <- list(tbl_draws_hc, tbl_draws_hc_recovery)
  l_tbl_maps <- map2(l_tbl_draws, c("data", "preds"), by_participant_maps)
  
  tbl_recovery <- reduce(
    l_tbl_maps, 
    ~ left_join(.x, .y, by = c("ID", "parameter"), suffix = c("_data", "_preds"))
  )
  cor_recovery <- tbl_recovery %>%
    pivot_wider(names_from = parameter, values_from = c(map_data, map_preds)) %>%
    select(-c(fit_time_data, fit_time_preds)) %>%
    summarize(
      beta_in_beta_out = cor(map_data_beta, map_preds_beta),
      tau_in_tau_out = cor(map_data_tau, map_preds_tau),
      beta_in_tau_out = cor(map_data_beta, map_preds_tau),
      tau_in_beta_out = cor(map_data_tau, map_preds_beta)
    )
  
  return(list(tbl_recovery = tbl_recovery, cor_recovery = cor_recovery))
}

recovery_heatmap <- function(l_recovery, ttl) {
  tbl_in_out <- l_recovery$cor_recovery %>%
    pivot_longer(colnames(.)) %>%
    mutate(
      param_in = str_match(name, "([a-z]*)_")[, 2],
      param_out = str_match(name, "_([a-z]*)_out")[, 2]
    )
  ggplot(tbl_in_out, aes(param_in, param_out)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 2)), size = 6, color = "white") +
    theme_bw() +
    scale_x_discrete(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis_c(guide = "none") +
    labs(x = "MAP (Data)", y = "MAP (Prediction)", title = ttl) + 
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22)
    ) + 
    scale_color_gradient(name = "", low = "skyblue2", high = "tomato4")
}

l_recovery_s1 <- map_cor(tbl_draws_hc_s1, tbl_draws_hc_s1_recovery)
l_recovery_s2 <- map_cor(tbl_draws_hc_s2, tbl_draws_hc_s2_recovery)
pl_heatmap_s1 <- recovery_heatmap(l_recovery_s1, "Session 1")
pl_heatmap_s2 <- recovery_heatmap(l_recovery_s2, "Session 2")

pl_heatmaps <- arrangeGrob(
  pl_heatmap_s1, pl_heatmap_s2 + scale_fill_viridis_c(), 
  nrow = 1, widths = c(1, 1.3)
)

save_my_pdf_and_tiff(
  pl_heatmaps, "figures/4arlb-hierarchical-ucb-recovery", 10, 4
)  

rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(cmdstanr)
library(mvtnorm)


is_fit <- FALSE#TRUE#
is_hierarchical <- TRUE#FALSE#

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)


posteriors_and_maps <- function(tbl_draws, s, pars_interest, ids_sample) {
  
  n_params <- length(pars_interest)
  if (n_params == 2) filt <- 1:2 else filt <- 2
  
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
      parameter = factor(parameter, labels = c("ru", "v")[filt]),
      session = s
    )
  
  return(list(tbl_posterior = tbl_posterior, tbl_map = tbl_map))
  
}


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


ids_sample <- tibble(
  ID = unique(tbl_restless$ID),
  id_stan = 1:length(unique(tbl_restless$ID))
)

# by-participant ----------------------------------------------------------


file_loc_s1 <- "data/restless-participant-model-posterior-s1.RDS"
file_loc_s2 <- "data/restless-participant-model-posterior-s2.RDS"


if (is_fit & !is_hierarchical) {
  
  
  ucb_stan_txt <- ucb_stan()
  mod_ucb_stan <- cmdstan_model(ucb_stan_txt)
  
  
  
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
  
}
if (!is_fit) {
  tbl_draws_s1 <- readRDS(file_loc_s1)
  tbl_draws_s2 <- readRDS(file_loc_s2)
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




# hierarchical ucb --------------------------------------------------------


file_loc_hc_s1 <- "data/restless-hierarchical-model-posterior-s1.RDS"
file_loc_hc_s2 <- "data/restless-hierarchical-model-posterior-s2.RDS"
loo_loc_ucb_s1 <- "data/loo-ucb-hc-1.RDS"
loo_loc_ucb_s2 <- "data/loo-ucb-hc-2.RDS"

ucb_stan_hc_txt <- ucb_stan_hierarchical_generate()
mod_ucb_stan_hc <- cmdstan_model(ucb_stan_hc_txt)

if (is_fit & is_hierarchical) {
  
  # session 1
  fit_restless_ucb_hc_s1 <- mod_ucb_stan_hc$sample(
    data = l_data_s1, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s1 <- fit_restless_ucb_hc_s1$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s1 <- fit_restless_ucb_hc_s1$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s1, file_loc_hc_s1)
  
  loo_ucb_1 <- fit_restless_ucb_hc_s1$loo(variables = "log_lik")
  saveRDS(loo_ucb_1, file = loo_loc_ucb_s1)
  
  
  # session 2
  fit_restless_ucb_hc_s2 <- mod_ucb_stan_hc$sample(
    data = l_data_s2, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s2 <- fit_restless_ucb_hc_s2$draws(variables = c(pars_interest, pars_group, pars_pred), format = "df")
  tbl_summary_hc_s2 <- fit_restless_ucb_hc_s2$summary(variables = c(pars_interest, pars_group, pars_pred))
  tbl_summary_hc_s2 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s2, file_loc_hc_s2)
  
  loo_ucb_2 <- fit_restless_ucb_hc_s2$loo(variables = "log_lik")
  saveRDS(loo_ucb_2, file = loo_loc_ucb_s2)
  
}
if (!is_fit) {
  tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
  tbl_draws_hc_s2 <- readRDS(file_loc_hc_s2)
  loo_ucb_1 <- readRDS(loo_loc_ucb_s1)
  loo_ucb_2 <- readRDS(loo_loc_ucb_s2)
}

l_posterior_hc_ucb_1 <- posteriors_and_maps(tbl_draws_hc_s1, 1, c("beta", "tau"), ids_sample)
l_posterior_hc_ucb_2 <- posteriors_and_maps(tbl_draws_hc_s2, 2, c("beta", "tau"), ids_sample)

# test-retest reliability
tbl_map_hc <- l_posterior_hc_ucb_1[[2]] %>% select(-session) %>% 
  left_join(
    l_posterior_hc_ucb_2[[2]] %>% select(-session),
    by = c("ID", "parameter"), suffix = c("_1", "_2")
  ) %>% 
  pivot_wider(names_from = parameter, values_from = c(map_1, map_2))
tbl_save <- tbl_map_hc
colnames(tbl_save)[2:5] <- str_c("rlb_", colnames(tbl_save)[2:5])
write_csv(tbl_save, "data/4arlb-maps-hierarchical.csv")

tbl_map_hc %>%
  summarize(
    r_v = cor(map_1_v, map_2_v),
    r_ru = cor(map_1_ru, map_2_ru)
  )

# distribution of maps
hist_maps <- l_posterior_hc_ucb_1[[2]] %>% select(-session) %>% 
  left_join(
    l_posterior_hc_ucb_2[[2]] %>% select(-session),
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

params_bf <- c("RU", "V")
library(kde1d)
hdi_etc <- function(tbl_draws) {
  tbl_posterior <- tbl_draws %>% 
    dplyr::select(starts_with(c("mu_beta", "mu_tau")), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(c("mu_beta", "mu_tau")), names_to = "parameter", values_to = "value") %>%
    mutate(parameter = factor(parameter, labels = params_bf))
  kdes <- estimate_kd(tbl_posterior, params_bf)
  limits <- c(0.0025, 0.9975)
  par_lims <- limit_axes(kdes, limits = limits)
  bfs <- map2_dbl(kdes, c(dt(0, 1, 1), dnorm(0, 0, 1)), ~ (..2)/dkde1d(0, ..1))
  return(
    list(
      par_lims = par_lims,
      bfs = bfs
    )
  )
  
}

l_hdi_etc_s1 <- hdi_etc(tbl_draws_hc_s1)
l_hdi_etc_s2 <- hdi_etc(tbl_draws_hc_s2)

pl_fixed_s1 <- plot_map_hdi_bf(l_hdi_etc_s1$par_lims, l_hdi_etc_s1$bfs, "Session 1") + coord_cartesian(xlim = c(-1.25, .5))
pl_fixed_s2 <- plot_map_hdi_bf(l_hdi_etc_s2$par_lims, l_hdi_etc_s2$bfs, "Session 2") + coord_cartesian(xlim = c(-1.25, .5))
pl_fixed_both <- arrangeGrob(
  pl_fixed_s1 + scale_y_discrete(expand = c(.3, 0)),
  pl_fixed_s2 + scale_y_discrete(expand = c(.3, 0)),
  nrow = 1)


grid.draw(pl_fixed_both)

save_my_pdf_and_tiff(pl_fixed_both, "figures/4arlb-fixed-effects-posteriors", 5.5, 8)




# recoverability ----------------------------------------------------------


# get predictions from bayesian model and append to original data
my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


my_bayesian_preds <- function(tbl_draws) {
  tbl_pred <- tbl_draws %>% select(contains("choice_pred"))
  randomly_drawn_ids <- sample(1:nrow(tbl_pred), ncol(tbl_pred), replace = TRUE)
  
  tbl_subj <- tbl_pred[1, ] %>% 
    mutate(it = 1:nrow(.)) %>% 
    pivot_longer(-it, names_pattern = "\\[([0-9]+),", names_to = "ID") %>%
    mutate(ID = as.numeric(ID)) %>% select(-c(it, value))
  tbl_trial <- tbl_pred[1, ] %>% 
    mutate(it = 1:nrow(.)) %>% 
    pivot_longer(-it, names_pattern = "([0-9]+)\\]$", names_to = "trial") %>%
    mutate(trial = as.numeric(trial)) %>% select(-it)
  
  tbl_lookup_id <- tibble(
    ID_stan = sort(unique(tbl_subj$ID)),
    ID = unique(tbl_restless_s1$ID)
  )
  
  tbl_pred_arranged <- tibble(cbind(tbl_subj, trial = tbl_trial$trial))
  tbl_pred_arranged$choice_pred <- map2_dbl(randomly_drawn_ids, tbl_pred, ~ .y[.x])
  tbl_pred_arranged$choice_pred_map <- map_dbl(tbl_pred, my_mode)
  tbl_pred_arranged <- tbl_pred_arranged %>% 
    left_join(tbl_lookup_id, by = c("ID" = "ID_stan"), suffix = c("_stan", ""))
  
  return(tbl_pred_arranged)
}

tbl_preds_s1 <- my_bayesian_preds(tbl_draws_hc_s1)
tbl_preds_s2 <- my_bayesian_preds(tbl_draws_hc_s2)

tbl_restless_s1 <- as_tibble(left_join(tbl_restless_s1, tbl_preds_s1, by = c("ID", "trial")))
tbl_restless_s2 <- as_tibble(left_join(tbl_restless_s2, tbl_preds_s2, by = c("ID", "trial")))


# tbl_restless_s1$reward_pred_map <- pmap_dbl(
#   tbl_restless_s1[, c("reward1", "reward2", "reward3", "reward4", "choice_pred_map")], 
#   ~ c(..1, ..2, ..3, ..4)[..5]
# )
# tbl_restless_s2$reward_pred_map <- pmap_dbl(
#   tbl_restless_s2[, c("reward1", "reward2", "reward3", "reward4", "choice_pred_map")], 
#   ~ c(..1, ..2, ..3, ..4)[..5]
# )
# tbl_restless_s1$reward_pred <- pmap_dbl(
#   tbl_restless_s1[, c("reward1", "reward2", "reward3", "reward4", "choice_pred")], 
#   ~ c(..1, ..2, ..3, ..4)[..5]
# )
# tbl_restless_s2$reward_pred <- pmap_dbl(
#   tbl_restless_s2[, c("reward1", "reward2", "reward3", "reward4", "choice_pred")], 
#   ~ c(..1, ..2, ..3, ..4)[..5]
# )

# fit same model again on predicted data
l_data_s1$choice_gen <- pivot_wider(tbl_restless_s1[, c("ID", "trial", "choice_pred")], names_from = "trial", values_from = "choice_pred") %>% select(-ID) %>% as.matrix()
# l_data_s1$reward  <- pivot_wider(tbl_restless_s1[, c("ID", "trial", "reward_pred_map")], names_from = "trial", values_from = "reward_pred_map") %>% select(-ID) %>% as.matrix()

l_data_s2$choice_gen <- pivot_wider(tbl_restless_s2[, c("ID", "trial", "choice_pred")], names_from = "trial", values_from = "choice_pred") %>% select(-ID) %>% as.matrix()
# l_data_s2$reward  <- pivot_wider(tbl_restless_s2[, c("ID", "trial", "reward_pred_map")], names_from = "trial", values_from = "reward_pred_map") %>% select(-ID) %>% as.matrix()

file_loc_hc_s1_recovery <- "data/restless-hierarchical-model-recovery-posterior-s1.RDS"
file_loc_hc_s2_recovery <- "data/restless-hierarchical-model-recovery-posterior-s2.RDS"

# use the model just fitting the choices, without changing the learning
ucb_stan_hc_fixed_txt <- ucb_stan_hierarchical_fit_fixed_learning()
mod_ucb_stan_hc <- cmdstan_model(ucb_stan_hc_fixed_txt)

if (is_fit & is_hierarchical) {
  
  # session 1
  fit_restless_ucb_hc_s1_recovery <- mod_ucb_stan_hc$sample(
    data = l_data_s1, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s1_recovery <- fit_restless_ucb_hc_s1_recovery$draws(variables = c(pars_interest, pars_group), format = "df")
  tbl_summary_hc_s1_recovery <- fit_restless_ucb_hc_s1_recovery$summary(variables = c(pars_interest, pars_group))
  tbl_summary_hc_s1_recovery %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s1_recovery, file_loc_hc_s1_recovery)
  
  
  # session 2
  fit_restless_ucb_hc_s2_recovery <- mod_ucb_stan_hc$sample(
    data = l_data_s2, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_s2_recovery <- fit_restless_ucb_hc_s2_recovery$draws(variables = c(pars_interest, pars_group), format = "df")
  tbl_summary_hc_s2_recovery <- fit_restless_ucb_hc_s2_recovery$summary(variables = c(pars_interest, pars_group))
  tbl_summary_hc_s2_recovery %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_s2_recovery, file_loc_hc_s2_recovery)
  
} 
if (!is_fit) {
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

grid.draw(pl_heatmaps)

save_my_pdf_and_tiff(
  pl_heatmaps, "figures/4arlb-hierarchical-ucb-recovery", 10, 4
)  





tbl_sam <- as_tibble(rbind(sam1, sam2)) %>%
  inner_join(tbl_exclude %>% select(ID), by = "ID") %>%
  arrange(ID, session, block, trial) %>%
  group_by(block) %>%
  mutate(
    prev_chosen = lag(chosen, 1),
    is_repetition = chosen == prev_chosen) %>%
  rowwise() %>%
  mutate(
    is_optimal = reward == max(c(reward1, reward2)),
    regret = max(c(reward1, reward2)) - reward
  )

tbl_horizon <-  as_tibble(rbind(horizon1, horizon2)) %>%
  inner_join(tbl_exclude %>% select(ID), by = "ID") %>%
  arrange(ID, session, block, trial) %>%
  group_by(block) %>%
  mutate(
    prev_chosen = lag(chosen, 1),
    is_repetition = chosen == prev_chosen
  ) %>%
  rowwise() %>%
  mutate(
    is_optimal = reward == max(c(reward1, reward2)),
    regret = max(c(reward1, reward2)) - reward
  )
tbl_horizon$is_repetition[tbl_horizon$trial == 5] <- NA


tbl_reliability_rl <- tbl_restless %>%
  rowwise() %>%
  mutate(
    is_optimal = rewards == max(c(reward1, reward2, reward3, reward4)),
    regret = max(c(reward1, reward2, reward3, reward4)) - rewards
  ) %>%
  group_by(ID, session) %>%
  summarize(
    p_switch = mean(is_repetition, na.rm = TRUE),
    p_optimal = mean(is_optimal),
    regret = sum(regret)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
  mutate(task = "Restless")
tbl_reliability_sam <- tbl_sam %>%
  group_by(ID, session) %>%
  summarize(
    p_switch = mean(is_repetition, na.rm = TRUE),
    p_optimal = mean(is_optimal, na.rm = TRUE),
    regret = sum(regret, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
  mutate(task = "Sam")
tbl_reliability_horizon <- tbl_horizon %>%
  group_by(ID, session) %>%
  summarize(
    p_switch = mean(is_repetition, na.rm = TRUE),
    p_optimal = mean(is_optimal, na.rm = TRUE),
    regret = sum(regret, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
  mutate(task = "Horizon")

tbl_all_three <- rbind(tbl_reliability_horizon, tbl_reliability_sam, tbl_reliability_rl)

tbl_reliability_measures <- tbl_all_three %>%
  group_by(task) %>%
  summarize(
    reliability_switching = cor(p_switch_1, p_switch_2),
    reliability_optimal = cor(p_optimal_1, p_optimal_2),
    reliability_regret = cor(regret_1, regret_2)
  ) %>% pivot_longer(-task) %>%
  mutate(
    parameter = str_remove(name, "reliability_")
  )
tbl_reliability_measures$parameter <- factor(tbl_reliability_measures$parameter, labels = c("p(optimal)", "regret", "p(switch)"))
tbl_reliability_measures <- tbl_reliability_measures %>% select(-name)
write_csv(tbl_reliability_measures, file = "analysis/reliability-task-measures.csv")
write_csv(tbl_all_three, file = "analysis/bandits-task-measures.csv")

tbl_all_three %>%
  select(-c(p_optimal_1, p_optimal_2, regret_1, regret_2)) %>%
  pivot_wider(id_cols = ID, names_from = task, values_from = c(p_switch_1, p_switch_2)) %>%
  select(-ID) %>%
  cor()

write_csv(tbl_all_three, file = "analysis/bandits-task-measures.csv")


# Fixed Effects All Three Bandit Tasks ------------------------------------


load("analysis/fitHorizonSession1UCB_full.Rda")
coefs_full_s1 <- summary(baymodel)$fixed
load("analysis/fitHorizonSession2UCBfull.Rda")
coefs_full_s2 <- summary(baymodel)$fixed

tbl_ia_both <- coefs_full_s1 %>% filter(str_detect(rownames(.), ":")) %>% mutate(param = rownames(.), session = "Session 1") %>%
  rbind(
    coefs_full_s2 %>% filter(str_detect(rownames(.), ":")) %>% mutate(param = rownames(.), session = "Session 2")
  ) %>% mutate(
    Estimate = -Estimate,
    dummy = -`u-95% CI`,
    `u-95% CI` = -`l-95% CI`,
    `l-95% CI` = dummy
  ) %>% select(-dummy)

ggplot(tbl_ia_both, aes(Estimate, param)) +
  geom_segment(aes(x = `l-95% CI`, xend = `u-95% CI`), linewidth = 3.5, lineend = "round") +
  geom_vline(xintercept = 0) +
  geom_point(color = "white", size = 4) +
  geom_point(color = "skyblue2", size = 3) +
  facet_wrap(~ session) +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.05, 0)) +
  scale_y_discrete(expand = c(0.5, 0)) +
  labs(x = "", y = "") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")




tbl_fixed_sam_horizon <- read_csv("analysis/AllFixedEffects.csv") %>% select(-`...1`)
tbl_fixed_rlb <- l_hdi_etc_s1$par_lims %>% 
  pivot_wider(id_cols = parameter, names_from = variable, values_from = value) %>%
  mutate(session = 1, task = "Restless") %>%
  rbind(
    l_hdi_etc_s2$par_lims %>% mutate(session = 2) %>%
      pivot_wider(id_cols = parameter, names_from = variable, values_from = value) %>%
      mutate(session = 2, task = "Restless")
  ) %>% relocate(session, .after = parameter) %>% relocate(task, .before = parameter) %>%
  select(-c(max_dens, zero_dens))
colnames(tbl_fixed_rlb) <- c("task", "param", "session", "lower", "upper")
tbl_fixed_rlb$estim <- (tbl_fixed_rlb$upper + tbl_fixed_rlb$lower) / 2
tbl_fixed_rlb <- tbl_fixed_rlb %>% relocate(estim, .before = lower)

tbl_fixed <- rbind(tbl_fixed_sam_horizon, tbl_fixed_rlb)

# first show diagnostic effects
# horizon: diff in ru and v between short and long horizon
# sam: main effects of v and ru

tbl_fixed_sam_horizon %>% filter(task == "sam") %>%
  mutate(estim = -estim, upper_new = -lower, lower = -upper, session = factor(session, labels = c("Session 1", "Session 2"))) %>%
  mutate(upper = upper_new) %>% select(-upper_new) %>%
  ggplot(aes(estim, param)) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = lower, xend = upper), linewidth = 3.5, lineend = "round") +
  geom_point(color = "white", size = 4) +
  geom_point(color = "skyblue2", size = 3) +
  facet_wrap(~ session) +
  coord_cartesian(xlim = c(-.2, .2)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_discrete(expand = c(.2, 0)) +
  labs(x = "Posterior", y = "") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_fill_manual(values = c("skyblue2", "tomato4"), name = "")


tbl_fixed_sam_horizon %>% filter(task == "horizon_5" & param == "RU") %>%
  mutate(task = "Horizon (1)", estim = -estim, upper_new = -lower, lower = -upper) %>%
  select(-upper) %>% rename(upper = upper_new) %>%
  rbind(
    tbl_fixed_rlb %>% filter(param == "RU")
  ) %>%
  rbind(
    tbl_fixed_sam_horizon %>% filter(task == "sam" & param == "RU") %>%
      mutate(task = "Sam", estim = -estim, upper_new = -lower, lower = -upper) %>%
      select(-upper) %>% rename(upper = upper_new)
  ) %>%
  rbind(
    tbl_fixed_sam_horizon %>% filter(task == "horizon_10" & param == "RU") %>%
      mutate(task = "Horizon (6)", estim = -estim, upper_new = -lower, lower = -upper) %>%
      select(-upper) %>% rename(upper = upper_new)
  ) %>%
  mutate(session = factor(session, labels = c("Session 1", "Session 2"))) %>%
  ggplot(aes(estim, task)) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = lower, xend = upper), linewidth = 3.5, lineend = "round") +
  geom_point(color = "white", size = 4) +
  geom_point(color = "skyblue2", size = 3) +
  facet_wrap(~ session) +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_discrete(expand = c(.2, 0)) +
  labs(x = "Posterior", y = "") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_fill_manual(values = c("skyblue2", "tomato4"), name = "")



# hierarchical softmax ----------------------------------------------------



file_loc_hc_sm_s1 <- "data/restless-hierarchical-softmax-posterior-s1.RDS"
file_loc_hc_sm_s2 <- "data/restless-hierarchical-softmax-posterior-s2.RDS"
loo_loc_sm_s1 <- "data/loo-sm-hc-1.RDS"
loo_loc_sm_s2 <- "data/loo-sm-hc-2.RDS"
sm_stan_hc_txt <- softmax_stan_hierarchical_generate()
sm_stan_hc_txt <- cmdstan_model(sm_stan_hc_txt)


if (is_fit & is_hierarchical) {
  
  # session 1
  fit_restless_sm_hc_s1 <- sm_stan_hc_txt$sample(
    data = l_data_s1, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  vars_sm <- c("tau", "mu_tau", "log_lik")
  tbl_draws_hc_sm_s1 <- fit_restless_sm_hc_s1$draws(variables = vars_sm, format = "df")
  tbl_summary_hc_sm_s1 <- fit_restless_sm_hc_s1$summary(variables = vars_sm)
  tbl_summary_hc_sm_s1 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_sm_s1, file_loc_hc_sm_s1)
  
  loo_sm_1 <- fit_restless_sm_hc_s1$loo(variables = "log_lik")
  saveRDS(loo_sm_1, file = loo_loc_sm_s1)
  
  # session 2
  fit_restless_sm_hc_s2 <- sm_stan_hc_txt$sample(
    data = l_data_s2, iter_sampling = 1000, iter_warmup = 200, chains = 3, parallel_chains = 3
  )
  
  tbl_draws_hc_sm_s2 <- fit_restless_sm_hc_s2$draws(variables = vars_sm, format = "df")
  tbl_summary_hc_s2 <- fit_restless_sm_hc_s2$summary(variables = vars_sm)
  tbl_summary_hc_s2 %>% arrange(desc(rhat))
  saveRDS(tbl_draws_hc_sm_s2, file_loc_hc_sm_s2)
  
  loo_sm_2 <- fit_restless_sm_hc_s2$loo(variables = "log_lik")
  saveRDS(loo_sm_2, file = loo_loc_sm_s2)
  
}
if (!is_fit) {
  tbl_draws_hc_sm_s1 <- readRDS(file_loc_hc_sm_s1)
  tbl_draws_hc_sm_s2 <- readRDS(file_loc_hc_sm_s2)
  loo_sm_1 <- readRDS(loo_loc_sm_s1)
  loo_sm_2 <- readRDS(loo_loc_sm_s2)
}

l_posterior_hc_sm_1 <- posteriors_and_maps(tbl_draws_hc_sm_s1, 1, c("tau"), ids_sample)
l_posterior_hc_sm_2 <- posteriors_and_maps(tbl_draws_hc_sm_s2, 2, c("tau"), ids_sample)



tbl_two_tau <- l_posterior_hc_ucb_1$tbl_map %>% mutate(model = "ucb") %>%
  rbind(
    l_posterior_hc_sm_1$tbl_map %>% mutate(model = "softmax")
  ) %>% rbind(
    l_posterior_hc_ucb_2$tbl_map %>% mutate(model = "ucb") %>%
      rbind(
        l_posterior_hc_sm_2$tbl_map %>% mutate(model = "softmax")
      )
  ) %>% 
  filter(parameter == "v") %>%
  pivot_wider(id_cols = c(ID, session, parameter), names_from = model, values_from = map)


tbl_tau_cor <- tbl_two_tau %>%
  group_by(session) %>%
  summarize(r = cor(ucb, softmax))
tbl_tau_cor$session <- factor(tbl_tau_cor$session, labels = c("Session 1", "Session 2"))
tbl_two_tau$session <- factor(tbl_two_tau$session, labels = c("Session 1", "Session 2"))

pl_v_across_models <- ggplot(tbl_two_tau, aes(softmax, ucb)) +
  geom_abline() +
  geom_point() +
  geom_label(data = tbl_tau_cor, aes(.3, .1, label = str_c("r = ", round(r, 2)))) +
  facet_wrap(~ session) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "V (Softmax)", y = "V (UCB)") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )

grid.draw(pl_v_across_models)
saveRDS(pl_v_across_models, file = "data/restless-v-across-ucb-sm.Rds")




# Model Comparison --------------------------------------------------------


loo_softmax <- fit_restless_sm_hc_s1$loo(variables = "log_lik")


loo::loo_model_weights(
  list(loo_ucb_1, loo_sm_1), #, loo_mixture_group
  method = "stacking"
)

loo::loo_model_weights(
  list(loo_ucb_2, loo_sm_2), #, loo_mixture_group
  method = "stacking"
)




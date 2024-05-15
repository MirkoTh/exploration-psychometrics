rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(gridExtra)
library(cmdstanr)


is_fit <- FALSE#TRUE#

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
  ) %>% ungroup()





# stan model of kalman sme
# adopted from danwitz et al. 2022
# to be changed:
## add xi variance in kalman gain calculation
## add xi variance in variance (of the mean) calculation
## add the weighting of the compuated value with regression to the baseline


ucb_stan_txt <- ucb_stan()
mod_ucb_stan <- cmdstan_model(ucb_stan_txt)

file_loc <- "data/restless-hierarchical-model-posterior.RDS"
pars_interest <- c("beta", "tau")


# data structure required in stan model:
## int<lower=1> nSubjects;
## int<lower=1> nTrials;               
## array[nSubjects, nTrials] int choice;     
## matrix[nSubjects, nTrials] reward; 

tbl_restless_sample <- tbl_restless %>% 
  filter(
    ID %in% sort(unique(tbl_restless$ID))[1:10],
    session == 1
  )

l_data <- list(
  nSubjects = length(unique(tbl_restless_sample$ID)),
  nTrials = max(tbl_restless_sample$trial),
  choice = pivot_wider(tbl_restless_sample[, c("ID", "trial", "choices")], names_from = "trial", values_from = "choices") %>% select(-ID) %>% as.matrix(),
  reward = pivot_wider(tbl_restless_sample[, c("ID", "trial", "rewards")], names_from = "trial", values_from = "rewards") %>% select(-ID) %>% as.matrix()
)

if (is_fit) {
  
  fit_restless_ucb <- mod_ucb_stan$sample(
    data = l_data, iter_sampling = 1000, iter_warmup = 500, chains = 3, parallel_chains = 3
  )
  
  tbl_draws <- fit_restless_ucb$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_restless_ucb$summary(variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  saveRDS(tbl_draws, file_loc)
  
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}

ids_sample <- unique(tbl_restless_sample$ID)

par_lbls <- str_c(rep(c("ru", "v"), each = length(ids_sample)), "[", rep(ids_sample, 2), "]")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(pars_interest), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(pars_interest), names_to = "parameter", values_to = "value") %>%
  mutate(
    parameter_id = factor(parameter, labels = par_lbls),
    ID = as.integer(str_extract(parameter_id, "[0-9]+")),
    parameter = str_extract(parameter_id, "^[a-z]+")
    ) %>%
  relocate(ID, .before = parameter) %>%
  select(-parameter_id)


tbl_map <- tbl_posterior %>% group_by(ID, parameter) %>% summarize(map = mean(value))

tbl_max_likely <- readRDS("data/4arlb-overview.rds")
tbl_max_likely <- tbl_max_likely %>% ungroup() %>% 
  filter(ID %in% ids_sample & session == 1) %>% 
  select(ID, beta, gamma_ucb) %>%
  mutate(ID = as.numeric(as.character(ID))) %>%
  rename(ru = beta, v = gamma_ucb) %>%
  pivot_longer(c(ru, v))

tbl_map %>% 
  left_join(tbl_max_likely, by = c("ID", "parameter" = "name")) %>%
  ggplot(aes(map, value, group = parameter)) +
  geom_point(aes(color = parameter)) +
  facet_wrap(~ parameter)




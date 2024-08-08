rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(cmdstanr)
library(mvtnorm)
library(future)
library(furrr)


dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)



load(file = "analysis/bandits/banditsWave1.Rda")
restless1 <- restless
horizon1 <- horizon
sam1 <- sam



# Sam's task --------------------------------------------------------------


tbl_cor_avg_sam1 <- sam1 %>%
  filter(trial > 1) %>%
  group_by(cond, trial) %>%
  summarize(
    "V-RU" = cor(V, RU),
    "V-VTU" = cor(V, VTU),
    "RU-VTU" = cor(RU, VTU)
  ) %>% group_by(cond) %>%
  summarize(
    "V-RU" = mean(`V-RU`),
    "V-VTU" = mean(`V-VTU`),
    "RU-VTU" = mean(`RU-VTU`)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = -cond)

tbl_cor_trial_sam1 <- sam1 %>%
  filter(trial > 1) %>%
  group_by(cond, trial) %>%
  summarize(
    "V-RU" = cor(V, RU),
    "V-VTU" = cor(V, VTU),
    "RU-VTU" = cor(RU, VTU)
  ) %>% pivot_longer(cols = -c(cond, trial)) %>%
  ungroup()

ggplot(tbl_cor_trial_sam1, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_sam1, aes(6, .15 * as.numeric(as.factor(name)), label = str_c("avg. r = ", round(value, 2)), color = as.factor(name))) +
  facet_wrap(~ cond) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Correlation") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "") 



# Horizon Task ------------------------------------------------------------

horizon1 <- horizon1 %>% filter(!is.na(chosen)) %>% mutate(chosen = chosen + 1) %>% rename(choices = chosen, rewards = reward)
l_horizon1 <- split(horizon1, interaction(horizon1$block, horizon1$ID))
# no decay to decay_center, lambda = 1
kalman_learning(horizon1 %>% filter(ID == 2 & block == 1), 2, 0, 17, 50, 1000, lambda = 1, decay_center = 50)


kalman_helper <- function(...) {
  tbl_out <- kalman_learning(...)
  tbl_out[1:(nrow(tbl_out)-1), ]
}

plan(multisession, workers = availableCores() - 1)
l_learned_horizon1 <- future_map(
  l_horizon1, kalman_helper,
  2, 0, 17, 50, 1000, lambda = 1, decay_center = 50
)

i <- seq(1, 18001, by = 3000)
j <- seq(3000, 21000, by = 3000)
l_out <- future_map2(i, j, ~ reduce(l_learned_horizon1[.x:.y], rbind))
plan("sequential")

tbl_learned_horizon1 <- reduce(l_out, rbind)

horizon_learned1 <- horizon1 %>% cbind(tbl_learned_horizon1) %>%
  mutate(
    V = m_1 - m_2,
    RU = v_1 - v_2,
    TU = sqrt(v_1 + v_2),
    VTU = V / TU
  ) %>% as_tibble()

tbl_cor_avg_horizon1 <- horizon_learned1 %>%
  filter(trial > 4) %>%
  group_by(Horizon, trial) %>%
  summarize(
    "V-RU" = cor(V, RU),
    "V-VTU" = cor(V, VTU),
    "RU-VTU" = cor(RU, VTU)
  ) %>% filter(!is.na(`V-RU`)) %>%
  group_by(Horizon) %>%
  summarize(
    "V-RU" = mean(`V-RU`),
    "V-VTU" = mean(`V-VTU`),
    "RU-VTU" = mean(`RU-VTU`)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = -c(Horizon))

tbl_cor_trial_horizon1 <- horizon_learned1 %>%
  filter(trial > 4) %>%
  group_by(Horizon, trial) %>%
  summarize(
    "V-RU" = cor(V, RU),
    "V-VTU" = cor(V, VTU),
    "RU-VTU" = cor(RU, VTU)
  )  %>% filter(!is.na(`V-RU`)) %>%
  pivot_longer(cols = -c(Horizon, trial)) %>%
  ungroup()

ggplot(tbl_cor_trial_horizon1, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_horizon1, aes(
    6, .15 * as.numeric(as.factor(name)), 
    label = str_c("avg. r = ", round(value, 2)
    ), color = as.factor(name))) +
  facet_wrap( ~ Horizon) +
  theme_bw() +
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Correlation") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")



# Restless Bandit ---------------------------------------------------------

tbl_restless1 <- restless1  %>%
  rename(
    choices = chosen,
    rewards = reward) %>%
  mutate(
    choices = choices + 1
  ) %>% filter(ID < 20)
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
max_trials <- max(restless1$trial)
l_thompson_choice_prob <- map(split(tbl_restless1, tbl_restless1$ID), thompson_choice_probs_by_participant)
th_ch_pr <- map(l_thompson_choice_prob, ~ .x$th_ch_probs[1:max_trials, ])
m <- map(l_thompson_choice_prob, ~ .x$m[1:max_trials, ])
var_mean <- map(l_thompson_choice_prob, ~ .x$v[1:max_trials, ])

tbl_th_ch_pr <- reduce(th_ch_pr, rbind)
colnames(tbl_th_ch_pr) <- c("th_1", "th_2", "th_3", "th_4")
tbl_restless1_info <- tbl_restless1 %>% cbind(reduce(m, rbind)) %>%
  cbind(reduce(var_mean, rbind)) %>%
  cbind(tbl_th_ch_pr) %>%
  as_tibble()
tbl_restless1_info <- tbl_restless1_info %>%
  mutate(
    m_1_2 = m_1 - m_2,
    m_1_3 = m_1 - m_3,
    m_1_4 = m_1 - m_4,
    m_2_3 = m_2 - m_3,
    m_2_4 = m_2 - m_4,
    m_3_4 = m_3 - m_4,
    v_1_2 = v_1 - v_2,
    v_1_3 = v_1 - v_3,
    v_1_4 = v_1 - v_4,
    v_2_3 = v_2 - v_3,
    v_2_4 = v_2 - v_4,
    v_3_4 = v_3 - v_4,
    th_1_2 = th_1 - th_2,
    th_1_3 = th_1 - th_3,
    th_1_4 = th_1 - th_4,
    th_2_3 = th_2 - th_3,
    th_2_4 = th_2 - th_4,
    th_3_4 = th_3 - th_4,
  )

n_excl <- str_c(rep(c("m_", "v_", "th_"), 4), rep(1:4, each = 3))

cor(tbl_restless1_info %>% 
      select(-n_excl) %>%
      select(starts_with(c("m_", "v_", "th_")))) %>%
  as.data.frame() %>%
  mutate(var_in = rownames(.)) %>%
  pivot_longer(-var_in, names_to = "var_out") %>%
  ggplot(aes(var_in, var_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "white") +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = "Var In", y = "Var Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  ) + 
  scale_fill_gradient2(name = "", high = "#66C2A5", low = "#FC8D62", mid = "white")










rm(list = ls())
# Load Packages and Utils -------------------------------------------------


library(tidyverse)
library(rutils)
library(gridExtra)


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


shared_cols <- intersect(colnames(sam1), colnames(sam2))
tbl_sam <- as_tibble(rbind(sam1[, shared_cols], sam2[, shared_cols])) %>%
  left_join(tbl_exclude, by = c("ID" = "participant_id")) %>%
  filter(exclude_all == 0)

shared_cols <- intersect(colnames(horizon1), colnames(horizon2))
tbl_horizon <- as_tibble(rbind(horizon1[, shared_cols], horizon2[, shared_cols])) %>%
  left_join(tbl_exclude, by = c("ID" = "participant_id")) %>%
  filter(exclude_all == 0)



# aggregate by participant and session

# restless
tbl_4arlb_performance <- grouped_agg(
  tbl_restless, c(ID, session), is_max
) %>% 
  ungroup() %>%
  rename(prop_correct_4arlb = mean_is_max)
tbl_4arlb_cor <- tbl_4arlb_performance %>% 
  select(ID, session, prop_correct_4arlb) %>%
  pivot_wider(values_from = prop_correct_4arlb, names_from = session)

ggplot(tbl_4arlb_cor, aes(`1`,`2`)) +
  geom_abline() +
  geom_hline(yintercept = .25, color = "grey", linetype = "dotdash") +
  geom_vline(xintercept = .25, color = "grey", linetype = "dotdash") +
  geom_smooth(method = "lm", color = "tomato") +
  geom_point(shape = 1) +
  theme_bw() +
  scale_x_continuous(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Wave I", y = "Wave II") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


cor(tbl_4arlb_cor[complete.cases(tbl_4arlb_cor),])

# sam
tbl_sam <- tbl_sam %>%
  group_by(ID, session, block, trial) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2),
    is_max = reward == max_reward_trial
  ) %>% ungroup()

tbl_sam_performance <- grouped_agg(tbl_sam, c(ID, session), is_max) %>% 
  ungroup() %>%
  rename(prop_correct_sam = mean_is_max)

tbl_sam_performance %>% 
  select(ID, session, prop_correct_sam) %>%
  pivot_wider(values_from = prop_correct_sam, names_from = session) %>% 
  ggplot(aes(`1`,`2`)) + geom_point()

tbl_sam_cor <- 
  tbl_sam_performance %>%
  select(ID, session, prop_correct_sam) %>%
  pivot_wider(values_from = prop_correct_sam, names_from = session)
cor(tbl_sam_cor[complete.cases(tbl_sam_cor), ])

tbl_horizon_trial1 <- tbl_horizon %>%
  filter(trial >= 5 & !is.na(reward)) %>%
  group_by(ID, session, block, trial) %>%
  mutate(
    max_reward_trial = pmax(reward1, reward2),
    is_max = reward == max_reward_trial,
    first_choice = trial == 5
  ) %>% ungroup()


# horizon
tbl_horizon_performance <- grouped_agg(
  tbl_horizon_trial1, c(ID, session, first_choice), is_max
) %>% 
  ungroup() %>%
  rename(prop_correct_horizon = mean_is_max)

ggplot(tbl_horizon_performance, aes(prop_correct_horizon)) +
  geom_histogram() +
  facet_wrap(~ first_choice)

tbl_horizon_cor <- tbl_horizon_performance %>% 
  pivot_wider(id_cols = c(ID, first_choice), names_from = session, values_from = prop_correct_horizon)

ggplot(tbl_horizon_cor, aes(`1`,`2`)) + geom_point() + facet_wrap(~ first_choice)
tbl_horizon_cor[complete.cases(tbl_horizon_cor),] %>% group_by(first_choice) %>% summarize(cor(`1`,`2`))






# Model Fitting -----------------------------------------------------------



x <- c(.2, 2)
bds_ucb <- list(gamma = list(lo = 0, hi = 1), beta = list(lo = -10, hi = 10))
bds_sm <- list(gamma = list(lo = 0, hi = 1))

tbl_results <- tbl_restless %>% filter(!is.na(choices))
nr_options <- 4
sigma_prior <- 1000
mu_prior <- 50
sigma_xi_sq <- 7.84 # 36 in stimulus set of first prolific pilot; 16 in lab pilot
sigma_epsilon_sq <- 16
lambda <- .9836
decay_center <- 50
tbl_results1 <- filter(tbl_results, ID == 1)


l_fits_ucb <- list()
l_fits_sm <- list()
l_inits <- list()
n_init_vals <- 2

l_tbl_results <- split(tbl_results, interaction(tbl_results$ID, tbl_results$session, sep = "_"))
filter_use <- map_lgl(l_tbl_results, ~ nrow(.x) == 200) 
l_tbl_results <- l_tbl_results[filter_use]
tbl_learned <- kalman_learning(
  l_tbl_results[[10]], nr_options, sigma_xi_sq, sigma_epsilon_sq,
  m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
)


tbl_learned %>% 
  mutate(
    trial_id = 1:201
  ) %>%
  pivot_longer(cols = str_c("m_", 1:4)) %>%
  group_by(trial_id) %>%
  mutate(
    the_max = max(value),
    is_max = the_max == value
  ) %>%
  ggplot(aes(name, trial_id)) +
  geom_tile(aes(fill = is_max))

# fit_ucb_no_variance_wrapper(tbl_results = l_tbl_results[[1]], tbl_rewards = NULL, condition_on_observed_choices = TRUE,
#                             sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq,
#                             bds = bds, params_init = params_init)

if (is_fit) {
  t_start <- Sys.time()
  future::plan(future::multisession, workers = 2)#future::availableCores() - 2)
  
  for (i in 1:n_init_vals) {
    params_init <- c(runif(1, bds_ucb$gamma$lo, bds_ucb$gamma$hi), runif(1, bds_ucb$beta$lo/2, bds_ucb$beta$hi/2))
    l_fit_ucb <- furrr::future_map(
      l_tbl_results, fit_ucb_no_variance_wrapper, 
      tbl_rewards = NULL, 
      condition_on_observed_choices = TRUE,
      sigma_xi_sq = sigma_xi_sq, 
      sigma_epsilon_sq = sigma_epsilon_sq, 
      sigma_prior = 1000,
      mu_prior = 50,
      bds = bds_ucb, 
      params_init = params_init,
      lambda = lambda,
      decay_center = decay_center,
      .progress = TRUE
    )
    saveRDS(l_fit_ucb, file = str_c("data/4arlb-ucb-fits-it-", i, ".rds"))
    l_fit_sm <- furrr::future_map(
      l_tbl_results, fit_softmax_no_variance_wrapper, 
      tbl_rewards = NULL, 
      condition_on_observed_choices = TRUE,
      sigma_xi_sq = sigma_xi_sq, 
      sigma_epsilon_sq = sigma_epsilon_sq, 
      sigma_prior = 1000,
      mu_prior = 50,
      bds = bds_sm, 
      params_init = params_init[1],
      .progress = TRUE
    )
    saveRDS(l_fit_sm, file = str_c("data/4arlb-sm-fits-it-", i, ".rds"))
    l_fits_ucb[[i]] <- l_fit_ucb
    l_fits_sm[[i]] <- l_fit_sm
    l_inits[[i]] <- params_init
  }
  future::plan("sequential")
  beepr::beep()
  t_end <- Sys.time()
  round(t_end - t_start, 1)
  
} else {
  for (i in 1:n_init_vals) {
    l_fits_sm[[i]] <- readRDS(str_c("data/4arlb-sm-fits-it-", i, ".rds"))
    l_fits_ucb[[i]] <- readRDS(str_c("data/4arlb-ucb-fits-it-", i, ".rds"))
    
  }
}



# Postprocessing ----------------------------------------------------------


n_blocks <- 1#max(tbl_restless$block)
tbl_fits_ucb <- map2(
  l_fits_ucb, 1:length(l_fits_ucb), 
  ~ reduce(.x, rbind) %>% as.data.frame() %>% 
    mutate(
      it = .y, 
      ID = as.numeric(str_match(names(.x), "^[0-9]*")[, 1]),
      session = str_match(names(.x), "[0-9]*$")[, 1]
    ) %>%
    rename(sum_ll = V3, beta = V2, gamma = V1) %>%
    relocate(ID, .before = gamma)
) %>% reduce(rbind) %>%
  arrange(ID) %>%
  group_by(ID, session) %>%
  mutate(rank = row_number(sum_ll)) %>%
  ungroup() %>%
  filter(rank == 1)

tbl_fits_sm <- map2(
  l_fits_sm, 1:length(l_fits_sm), 
  ~ reduce(.x, rbind) %>% as.data.frame() %>% 
    mutate(
      it = .y, 
      ID = as.numeric(str_match(names(.x), "^[0-9]*")[, 1]),
      session = str_match(names(.x), "[0-9]*$")[, 1]
    ) %>%
    rename(sum_ll = V2, gamma = V1) %>%
    relocate(ID, .before = gamma)
) %>% reduce(rbind) %>%
  arrange(ID) %>%
  group_by(ID, session) %>%
  mutate(rank = row_number(sum_ll)) %>%
  ungroup() %>%
  filter(rank == 1)


tmp <- tbl_fits_ucb %>% 
  pivot_wider(id_cols = c(ID), values_from = c(gamma, beta), names_from = session) %>%
  filter(complete.cases(.))

tbl_cor <- cor(tmp %>% select(-ID)) %>%
  as.data.frame() %>%
  mutate(
    param_in = str_remove(str_remove(colnames(.), "_"), "[1-2]"),
    session_id_in = str_c("Wave ", str_extract(colnames(.), "[1-2]"))
  ) %>%
  pivot_longer(cols = -c(session_id_in, param_in), names_to = "param_out") %>%
  mutate(
    session_id_out = str_c("Wave ", str_extract(param_out, "[1-2]")),
    param_out = str_replace(param_out, "_[1-2]_", "_"),
    param_out = str_replace(param_out, "_[1-2]$", "")
  )

tbl_cor$param_in <- factor(tbl_cor$param_in)
levels(tbl_cor$param_in) <- c("Beta", "Gamma")
tbl_cor$param_out <- factor(tbl_cor$param_out)
levels(tbl_cor$param_out) <- c("Beta", "Gamma")

# between-task correlations per session
pl_between_params <- tbl_cor %>%
  filter(session_id_in == session_id_out) %>%
  ggplot(aes(param_in, param_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "darkgrey", size = 5) +
  scale_fill_viridis_c(name = "Correlation", guide = "none") +
  facet_wrap(~ session_id_in) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "", title = "Within-Session\nCorrelations") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90)
  )

# task correlations between sessions with reliabilities highlighted
tbl_between <- tbl_cor %>%
  filter(session_id_in != session_id_out & session_id_in == "Wave 1")

pl_between_sessions <- ggplot(
  tbl_between %>% mutate(session_id_both = "Both Waves"), 
  aes(param_in, param_out)
) +
  geom_tile(aes(fill = value)) +
  # highlight diagonal
  geom_tile(data = tbl_between %>% filter(param_in == param_out), aes(param_in, param_out), color = "grey30", alpha = 0, linewidth = 1.5) +
  geom_text(aes(label = round(value, 2)), color = "grey40", size = 5) +
  facet_wrap(~ session_id_both) +
  scale_fill_viridis_c(name = "Correlation", guide = "none") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "", y = "", 
    title = "Between-Session\nCorrelations"
  ) + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(size = 10)
  )

grid.draw(arrangeGrob(
  pl_between_params, pl_between_sessions,
  nrow = 1,
  widths = c(1.7, 1),
  layout_matrix = matrix(c(1, 2), nrow = 1)
))


tbl_both <- 
  tbl_fits_sm %>%
  left_join(tbl_fits_ucb, by = c("ID", "session"), suffix = c("_sm", "_ucb")) %>%
  mutate(
    session = as.numeric(session),
    bic_ucb = sum_ll_ucb + 2*log(nrow(l_tbl_results[[1]])), 
    aic_ucb = sum_ll_ucb + 4,
    bic_sm = sum_ll_sm + log(nrow(l_tbl_results[[1]])),
    aic_sm = sum_ll_sm + 2,
    ucb_wins_bic = bic_ucb < bic_sm,
    ucb_wins_aic = aic_ucb < aic_sm,
    delta_ucb_minus_sm_bic = bic_ucb - bic_sm,
    delta_ucb_minus_sm_aic = aic_ucb - aic_sm
  )

tbl_both %>%
  group_by(session) %>%
  summarize(cor(gamma_sm, gamma_ucb))

tbl_both %>% count(ucb_wins_aic)
tbl_both %>% count(ucb_wins_bic)

ggplot(
  tbl_fits_sm %>% left_join(
    tbl_fits_ucb, by = c("ID", "session"), suffix = c("_sm", "_ucb")
  ), aes(gamma_sm, gamma_ucb)
) +
  geom_abline() +
  geom_point()

tbl_both %>% pivot_longer(starts_with("delta")) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ name)


tbl_sm_gamma <- tbl_fits_sm %>% select(-c(sum_ll, it, rank)) %>% pivot_wider(names_from = session, values_from = gamma)
tbl_sm_gamma <- tbl_sm_gamma[rowSums(!is.na(tbl_sm_gamma)) == 3, ]
tbl_sm_gamma %>% summarize(r = cor(`1`,`2`))

# fit_ucb_no_variance_wrapper(l_tbl_results[[1]], tbl_rewards = NULL, condition_on_observed_choices = TRUE,
#                             sigma_xi_sq = 16, sigma_epsilon_sq = 16, bds = bds, params_init = c(.5, .1))
# fit_softmax_no_variance_wrapper(l_tbl_results[[1]], tbl_rewards = NULL, condition_on_observed_choices = TRUE,
#                                 sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq, bds = bds$gamma, params_init = c(.5))

# exemplary inverse temperature
tau <- .042
m1 <- seq(0, 100, by = .1)
m2 <- 50
prob1 <- exp(m1 * tau)
prob2 <- rep(exp(m2 * tau), length(prob1))
tbl_softmax <- tibble(
  m = m1,
  p = prob1 / (prob1 + prob2)
)
ggplot(tbl_softmax, aes(m, p)) + geom_point() + scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  coord_cartesian(ylim = c(0, 1))

# inverse temperatures across model fits
id_count <- 1
tbl_softmax <- tibble()
for (tau in tbl_fits_ucb$gamma) {
  m1 <- seq(0, 100, by = .1)
  m2 <- 50
  prob1 <- exp(m1 * tau)
  prob2 <- rep(exp(m2 * tau), length(prob1))
  tbl_softmax <- rbind(
    tbl_softmax, 
    tibble(
      ID = tbl_fits_ucb$ID[id_count],
      session = tbl_fits_ucb$session[id_count],
      m = m1,
      p = prob1 / (prob1 + prob2)
    )
  )
  id_count <- id_count + 1
}

ggplot(tbl_softmax, aes(m, p, group = interaction(ID, session, sep = " _ Wave = "))) + 
  geom_line(aes(alpha = interaction(ID, session, sep = " _ Wave = "))) + 
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  guides(alpha = "none") +
  coord_cartesian(ylim = c(0, 1))

ggplot(
  tbl_fits_ucb %>% mutate(session = str_c("Wave = ", session)),
  aes(gamma, beta)) +
  geom_hline(yintercept = 0, linetype = "dotdash", alpha = .3, linewidth = 1) +
  #ggrepel::geom_text_repel(aes(label = interaction(ID, block, sep = "_"))) +
  geom_point(shape = 1) +
  facet_wrap(~ session) +
  coord_cartesian(ylim = c(-5, 5)) +
  theme_bw() +
  labs(x = "Gamma", y = "Beta") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  )

ggplot(tbl_fits_ucb, aes(beta)) +
  geom_histogram(color = "black", fill = "skyblue2", binwidth = .25) +
  geom_vline(xintercept = 0, linewidth = 1, linetype = "dashed", color = "darkred") +
  facet_wrap(~ session) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Beta", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  )

cor(tbl_fits_ucb$gamma, tbl_fits_ucb$beta)

tbl_4arlb_performance <- tbl_4arlb_performance %>%
  left_join(tbl_both[, c("ID", "session", "gamma_sm", "gamma_ucb", "beta")], by = c("ID", "session"))
tbl_4arlb_performance %>%
  pivot_longer(c(gamma_ucb, beta)) %>%
  ggplot(aes(prop_correct_4arlb, value)) +
  geom_vline(xintercept = .25, linetype = "dotdash", alpha = .3, linewidth = 1) +
  #geom_text(aes(label = interaction(ID, session, sep = "_"))) +
  geom_point(shape = 1) +
  facet_grid(name ~ session, scales = "free_y") +
  theme_bw() +
  labs(x = "Prop. Correct", y = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  )

grouped_agg(tbl_restless, ID, rt) %>% arrange(mean_rt) %>% ggplot(aes(mean_rt)) + geom_histogram()
grouped_agg(tbl_restless %>% filter(!is.na(is_repetition)), ID, c(rt, is_repetition)) %>% 
  ggplot(aes(mean_rt, mean_is_repetition)) + geom_point()



ggplot(tbl_4arlb_performance, aes(ID, prop_correct_4arlb)) +
  geom_hline(yintercept = .25) +
  # geom_hline(data = tbl_hline, aes(yintercept = y_ic, group = name), linetype = "dotdash", color = "tomato3", size = 1) +
  geom_text(aes(label = ID), size = 5) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = .1)) +
  scale_x_discrete(expand = c(.04, 0), breaks = seq(1, 10, by = 1)) +
  labs(x = "Participant ID", y = "Prop. Best Arm") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 18)
  )

tbl_4arlb_performance %>%
  select(-se_is_max) %>%
  pivot_wider(id_cols = ID, names_from = session, values_from = prop_correct_4arlb) %>%
  ggplot(aes(`1`, `2`)) +
  geom_abline() +
  #geom_label(aes(label = ID)) +
  geom_point(shape = 1) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))



# Correlations (Performance & Model Parameters) ---------------------------

cols_req <- c(
  "participant_id",
  colnames(tbl_wm)[str_detect(colnames(tbl_wm), "_recall_")],
  colnames(tbl_wm)[str_detect(colnames(tbl_wm), "_processing_")]
)
cols_req <- cols_req[!str_detect(cols_req, "timeout")]
tbl_wm <- tbl_wm[, cols_req]

tbl_wm_wide <- tbl_wm %>% pivot_longer(cols = ends_with("0") | ends_with("1")) %>%
  mutate(
    session = str_extract(name, "[0-1]"),
    session = as.numeric(session) + 1,
    name = str_remove(name, "_[0-1]$")
  ) %>% pivot_wider(
    id_cols = c(participant_id, session)
  )


tbl_performance_3tasks_wide <- tbl_4arlb_performance %>%
  left_join(
    tbl_sam_performance[, c("ID", "session", "prop_correct_sam")],
    by = c("ID", "session")
  ) %>%
  left_join(
    tbl_horizon_performance %>% filter(first_choice) %>% select(ID, session, prop_correct_horizon), 
    by = c("ID", "session")
  ) %>%
  left_join(
    tbl_wm_wide, by = c("ID" = "participant_id", "session")
  ) %>%
  arrange(prop_correct_4arlb) %>%
  mutate(ID = fct_inorder(as.character(ID)))

saveRDS(tbl_performance_3tasks_wide %>% select(-c(nunique_is_max, se_is_max)) %>% relocate(prop_correct_4arlb, .after = beta), file = "data/4arlb-overview.rds")

m_cor_1 <- cor(
  tbl_performance_3tasks_wide %>%
    filter(session == 1) %>%
    select(
      starts_with("prop_correct") | ends_with("recall") | 
        "gamma_ucb" | "beta"
    ), use = "complete.obs"
)
tbl_cor1 <- as_tibble(m_cor_1) %>%
  mutate(task_a = rownames(m_cor_1), session = 1)

m_cor_2 <- cor(
  tbl_performance_3tasks_wide %>%
    filter(session == 2) %>%
    select(
      starts_with("prop_correct") | ends_with("recall") | 
        "gamma_ucb" | "beta"
    ), use = "complete.obs"
)
tbl_cor2 <- as_tibble(m_cor_2) %>%
  mutate(task_a = rownames(m_cor_2), session = 2)


tbl_cor_long <- tbl_cor1 %>% rbind(tbl_cor2) %>% pivot_longer(-c(session, task_a))
lvls <- c(
  "prop_correct_horizon", "prop_correct_sam", "prop_correct_4arlb",
  "OS_recall", "SS_recall", "WMU_recall", "gamma_ucb", "beta"
)
lbls <- c(
  "Horizon", "Sam", "4ARLB", "OS", "SS", "WMU", "Gamma UCB", "Beta"
)
tbl_cor_long$task_a <- factor(
  tbl_cor_long$task_a, levels = lvls, labels = lbls, ordered = TRUE)
tbl_cor_long$name <- factor(
  tbl_cor_long$name, levels = lvls, labels = lbls, ordered = TRUE)

ggplot(tbl_cor_long, aes(task_a, name)) +
  geom_tile(aes(fill = ifelse(value == 1, 0, value))) +
  geom_text(aes(label = round(value, 2), color = ifelse(between(value, -.2, .2) | value == 1, "white", "black")), size = 5) +
  facet_wrap(~ session) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0), limits = rev) +
  labs(x = "Task", y = "Task") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .2)
  ) + 
  scale_fill_viridis_c(name = "r", guide = "none") +
  scale_color_manual(values = c("black", "white"), guide = "none")

tbl_performance_3tasks <- tbl_performance_3tasks_wide %>%
  pivot_longer(c(prop_correct_4arlb, prop_correct_sam, prop_correct_horizon))
tbl_performance_3tasks$name <- factor(tbl_performance_3tasks$name, labels = c("4ARLB", "Sam", "Horizon"))
tbl_hline <- tibble(
  name = c("4ARLB", "Horizon", "Sam"),
  y_ic = c(.25, .5, .5)
)

ggplot(tbl_performance_3tasks, aes(ID, value)) +
  geom_hline(
    data = tbl_hline, aes(yintercept = y_ic, group = name),
    linetype = "dotdash", color = "tomato3", linewidth = 1
  ) +
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

ggplot(tbl_performance_3tasks, aes(value)) +
  geom_histogram(aes(fill = name), color = "black") +
  facet_wrap(~ name) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Prop. Correct", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_fill_manual(values = c("skyblue2", "tomato4", "forestgreen"), name = "")

ggplot(
  tbl_performance_3tasks_wide %>% 
    select(session, beta, gamma_ucb, prop_correct_4arlb) %>%
    pivot_longer(-c(session, prop_correct_4arlb)) %>%
    mutate(name = factor(name, levels = c("beta", "gamma_ucb"), labels = c("Beta", "Gamma"))),
  aes(value, prop_correct_4arlb)) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotdash", linewidth = 1) +
  geom_point(shape = 1) +
  geom_smooth(color = "skyblue2") +
  theme_bw() +
  facet_wrap(~ name, scales = "free_x") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter", y = "Prop. Correct 4ARLB") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


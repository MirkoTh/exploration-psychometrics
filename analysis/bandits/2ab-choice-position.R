rm(list = ls())


library(tidyverse)
library(lme4)
library(furrr)
library(future)
library(brms)
library(stringr)



session <- 1
load(paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))

sam1 <- as_tibble(sam %>% filter(session == 1))
sam1$V_z <- scale(sam1$V)[,1]
sam1$RU_z <- scale(sam1$RU)[,1]

# for testing purposes
# some_ids <- sample(unique(sam1$ID), 40)
# sam1 <- sam1 %>% filter(ID %in% some_ids & block %in% seq(1, 5, by = 1))

# position analysis

HDI_95 <- function(v) {
  v_sorted <- sort(v)
  props <- 1:length(v_sorted)/length(v_sorted)
  all <- v_sorted[between(props, .025, .975)]
  return(c(min(all), max(all)))
}

bprior <- c(
  prior(normal(0, 1), class = b, coef = V_z),
  prior(normal(0, 1), class = b, coef = RU_z),
  prior(normal(0, 1), class = Intercept)
)


model_trial_position <- function(serial_position) {
  
  # select corresponding serial position
  tbl_used <- sam1 %>% filter(trial == serial_position)
  # run hierarchical Bayesian model
  m <- brm(
    chosen ~ V_z + RU_z + (V_z + RU_z | ID), 
    data = tbl_used, family = bernoulli(),
    prior = bprior, iter = 10000, warmup = 2000,
    chains = 3, cores = 3, control = list(adapt_delta = .95)
  )
  # extract by-participant posterior means
  tbl_posterior_samples <- brms::as_draws_df(m) %>% as_tibble()
  
  v_means_fixed <- map_dbl(tbl_posterior_samples[, c("b_V_z", "b_RU_z")], mean)
  tbl_means_fixed <- tibble(
    var = "map",
    name = names(v_means_fixed),
    value = v_means_fixed
  )
  
  tbl_95_fixed <- apply(tbl_posterior_samples[, c("b_V_z", "b_RU_z")], 2, HDI_95) %>% 
    as.data.frame() %>% as_tibble() %>%
    mutate(var = c("thx_lo", "thx_hi")) %>%
    pivot_longer(-var)
  
  tbl_fixed <- rbind(tbl_means_fixed, tbl_95_fixed) %>%
    mutate(serial_position = serial_position)
  
  tbl_random_slopes <- tbl_posterior_samples %>% 
    select(
      contains("V_z") & matches("[[0-9]*]") |
        contains("RU_z") & matches("[[0-9]*]")
    )
  v_p_mn <- map_dbl(tbl_random_slopes, mean)
  tbl_posterior_means_ids <- tibble(
    ID = str_extract(names(v_p_mn), "[0-9]+"),
    param = str_match(names(v_p_mn), ",(.*)]$")[,2],
    p_mn = v_p_mn,
    serial_position = serial_position
  )
  
  return(list(
    m = m,
    tbl_fixed = tbl_fixed,
    tbl_posterior_means_ids = tbl_posterior_means_ids
  ))
}



plan(multisession, workers = availableCores() - 2)
l_models <- furrr::future_map(seq(2, 10, by = 1), model_trial_position)
plan("sequential")

saveRDS(l_models, file = "analysis/bandits/choice-position-fit-models.RDS")


extract_maps_fixed <- function(x, sp) {
  x$tbl_fixed %>%
    filter(var == "map") %>%
    mutate(serial_position = sp)
}

# extract_maps_ids <- function(x, sp) {
#   x$tbl_posterior_means_ids %>%
#     mutate(serial_position = sp)
# }


tbl_beta_fixed <- map2(l_models, 2:10, extract_maps_fixed) %>% reduce(rbind) %>% as.data.frame()
tbl_beta_fixed <- tbl_beta_fixed  %>% as_tibble() %>% mutate(value = -value)
tbl_beta_fixed$name <- fct_inorder(factor(tbl_beta_fixed$name, labels = c("RU", "V")))

ggplot(tbl_beta_fixed, aes(serial_position, value, group = name)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dotdash", linewidth = 1) +
  geom_line(aes(color = name)) +
  geom_point(size = 5, color = "white") +
  geom_point(aes(color = name)) +
  geom_point(data = tbl_beta_fixed %>% filter(serial_position == 5), size = 5, shape = 1) +
  geom_label(data = tbl_beta_fixed %>% filter(serial_position == 5), aes(y = value + .475, label = "Horizon\nTask")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 9, by = 1), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial", y = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


# check convergence


summarize_convergence <- function(serial_position, l) {
  v_rhat <- rhat(l[[(serial_position - 1)]]$m)
  tbl_rhat <- tibble(
    param = names(v_rhat),
    rhat = v_rhat
  )
  pl_rhat <- tbl_rhat %>%
    arrange(desc(rhat)) %>%
    mutate(param = fct_inorder(factor(param))) %>%
    ggplot(aes(rhat, param)) +
    geom_vline(xintercept = 1.05, color = "tomato4", linetype = "dotdash", linewidth = 1) +
    #geom_line() +
    geom_point(size = .65, shape = 1, alpha = .3) +
    geom_point(data = tbl_rhat %>% filter(rhat == max(rhat)), aes(rhat, param), size = 5, color = "tomato4", shape = 1) +
    scale_x_continuous(
      expand = c(0.01, 0),
      breaks = seq(1, 1.1, by = 0.01),     # Adjust the major gridline spacing
      minor_breaks = seq(1, 1.1, by = 0.005)
    ) +
    scale_y_discrete(expand = c(0.01, 0), breaks = c()) +
    labs(x = "Rhat", y = "Model Parameter (Rank Ordered)") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(size = 22),
      axis.text.y = element_blank(),
    ) +
    coord_cartesian(xlim = c(1, 1.055))
  problematic_rhat <- nrow(tbl_rhat %>% filter(rhat >= 1.05)) >= 1
  
  return(list(
    tbl_rhat = tbl_rhat,
    problematic_rhat = problematic_rhat,
    pl_rhat = pl_rhat
  ))
}

l_rhat <- map(seq(2, 10, by = 1), summarize_convergence, l = l_models)

# any rhat problematic?
sum(reduce(map(l_rhat, "problematic_rhat"), c))



# Parameter Recovery ------------------------------------------------------



recover_model <- function(serial_position) {
  # extract model and generate backcasts
  m_trial <- l_models[[(serial_position - 1)]]$m
  tbl_used <- sam1 %>% filter(trial == serial_position)
  tbl_used$chosen_pred <- rbinom(nrow(tbl_used), 1, predict(m_trial, tbl_used, type = "response")[,1])
  # fit model on backcasts
  m_trial_refit <- brm(
    chosen_pred ~ V_z + RU_z + (V_z + RU_z | ID), 
    data = tbl_used, family = bernoulli(), prior = bprior, 
    iter = 10000, warmup = 2000, chains = 3, 
    cores = 3, control = list(adapt_delta = .95)
  )
  # post-process
  tbl_posterior_samples <- brms::as_draws_df(m_trial_refit) %>% as_tibble()
  tbl_random_slopes <- tbl_posterior_samples %>% 
    select(
      contains("V_z") & matches("[[0-9]*]") |
        contains("RU_z") & matches("[[0-9]*]")
    )
  v_p_mn <- map_dbl(tbl_random_slopes, mean)
  tbl_posterior_means_ids <- tibble(
    ID = str_extract(names(v_p_mn), "[0-9]+"),
    param = str_match(names(v_p_mn), ",(.*)]$")[,2],
    p_mn = v_p_mn,
    serial_position = serial_position
  )
  
  return(list(
    m = m_trial_refit,
    tbl_posterior_means_ids = tbl_posterior_means_ids
  ))
}


l_recover_model <- map(seq(2, 10, by = 1), recover_model, .progress = TRUE)

saveRDS(l_recover_model, file = "analysis/bandits/choice-position-recovery-models.RDS")

l_rhat_recovery <- map(seq(2, 10, by = 1), summarize_convergence, l = l_recover_model)

# any rhat problematic?
sum(reduce(map(l_rhat_recovery, "problematic_rhat"), c))



analyze_recovery <- function(serial_position) {
  tbl_recovered <-  as.data.frame(l_recover_model[[serial_position - 1]]$tbl_posterior_means_ids) %>%
    as_tibble()
  tbl_fit <- as.data.frame(l_models[[serial_position - 1]]$tbl_posterior_means_ids) %>%
    as_tibble() %>% mutate(serial_position = serial_position)
  
  tbl_participants <- tbl_fit %>%
    left_join(tbl_recovered, by = c("ID", "param", "serial_position"), suffix = c("_fit", "_recovered")) %>%
    rename(trial = serial_position) %>%
    relocate(trial, .before = "param")
  tbl_participants$param <- fct_inorder(factor(tbl_participants$param, labels = c("RU", "V")))
  
  tbl_cor <- tbl_participants %>%
    group_by(param) %>%
    summarize(r = cor(p_mn_fit, p_mn_recovered)) %>%
    ungroup() %>%
    mutate(trial = serial_position)
  
  x_cor <- max(tbl_participants$p_mn_fit) - (max(tbl_participants$p_mn_fit) - min(tbl_participants$p_mn_fit))/5
  y_cor <- min(tbl_participants$p_mn_recovered) + (max(tbl_participants$p_mn_recovered) - min(tbl_participants$p_mn_recovered))/5
  
  
  p <- ggplot(tbl_participants %>% rename(Parameter = param), aes(p_mn_fit, p_mn_recovered, group = Parameter)) +
    geom_abline() +
    geom_point(aes(color = Parameter)) +
    geom_label(data = tbl_cor %>% rename(Parameter = param), aes(x = x_cor, y = y_cor, label = round(r, 2))) +
    facet_wrap(~ Parameter)
  
  return(list(
    tbl_participants = tbl_participants,
    tbl_cor = tbl_cor,
    pl_recovery = p
  ))
  
}

l_analysis_recovery <- map(seq(2, 10, by = 1), analyze_recovery)


tbl_recovery <- reduce(map(l_analysis_recovery, "tbl_cor"), rbind) %>% as_tibble()
saveRDS(tbl_recovery, file = "data/choice-position-recovery.RDS")

tbl_recovery %>%
  group_by(param) %>%
  summarize(mean(r))

ggplot(tbl_recovery, aes(trial, r, group = param)) +
  geom_line(aes(color = param)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = param)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial", y = "Recoverability") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "") +
  coord_cartesian(ylim = c(0, 1))


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


# get fits and ids --------------------------------------------------------

tbl_exclude2 <- read_csv(file = "data/exclusions2.csv")
tbl_exclude1 <- read_csv(file = "data/exclusions1.csv")

# seems like two people were invited to session 2, who should not have been
tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
  left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
  filter(exclude_2 == 0 & exclude_1 == 0)
tbl_lookup <- tibble(
  ID = tbl_exclude$ID,
  ID_stan = 1:nrow(tbl_exclude)
)

l_kalman_ucb_no_variance_s1 <- readRDS(file = str_c("data/4arlb-kalman-ucb-fit1.rds"))
l_kalman_ucb_no_variance_s2 <- readRDS(file = str_c("data/4arlb-kalman-ucb-fit2.rds"))
l_sm1 <- readRDS("data/4arlb-kalman-sm-fit1.rds")
l_sm2 <- readRDS("data/4arlb-kalman-sm-fit2.rds")

tbl_kalman_sm <- l_sm1 %>%
  reduce(rbind) %>%
  as.data.frame() %>% mutate(session = 1) %>%
  rbind(
    l_sm2 %>% reduce(rbind) %>% as.data.frame() %>% mutate(session = 2)
  )%>% as_tibble()
colnames(tbl_kalman_sm) <- c("V", "NLL", "Session")
tbl_kalman_sm <- tbl_kalman_sm  %>%
  mutate(ID = rep(tbl_exclude$ID, 2)) %>%
  relocate(ID, .before = V)

tbl_kalman_ucb <- l_kalman_ucb_no_variance_s1 %>%
  reduce(rbind) %>%
  as.data.frame() %>% mutate(session = 1) %>%
  rbind(
    l_kalman_ucb_no_variance_s2 %>% reduce(rbind) %>% as.data.frame() %>% mutate(session = 2)
  )%>% as_tibble()
colnames(tbl_kalman_ucb) <- c("V", "RU", "NLL", "Session")
tbl_kalman_ucb <- tbl_kalman_ucb  %>%
  mutate(ID = rep(tbl_exclude$ID, 2)) %>%
  relocate(ID, .before = V)

tbl_draws_hc_s1 <- readRDS("data/restless-hierarchical-model-posterior-s1.RDS")
tbl_draws_hc_s2 <- readRDS("data/restless-hierarchical-model-posterior-s2.RDS")

individual_posteriors <- function(tbl_df) {
  tbl_df %>% 
    select(-c(starts_with("choice_pred"), starts_with("log_lik"), "mu_beta", "mu_tau")) %>%
    pivot_longer(cols = starts_with(c("beta", "tau"))) %>%
    mutate(
      parameter = str_extract(name, "^[a-z]*"),
      ID_stan = as.numeric(str_extract(name, "[0-9]+"))
    ) %>%
    left_join(
      tbl_lookup, by = "ID_stan"
    ) %>% select(-c(ID_stan, name, .draw, .iteration, .chain)) %>%
    relocate(ID, .before = value)
}

tbl_draws_hc_s1_long <- individual_posteriors(tbl_draws_hc_s1) %>% mutate(Session = 1)
tbl_draws_hc_s2_long <- individual_posteriors(tbl_draws_hc_s2) %>% mutate(Session = 2)



# Merge Fitting Methods ---------------------------------------------------


tbl_ucb_compare <- grouped_agg(rbind(tbl_draws_hc_s1_long, tbl_draws_hc_s2_long), c(ID, Session, parameter), value) %>%
  select(-c(n, nunique_value, se_value)) %>%
  pivot_wider(names_from = parameter, values_from = mean_value) %>%
  rename(V_hcb = tau, RU_hcb = beta) %>%
  left_join(tbl_kalman_ucb, by = c("ID", "Session")) %>%
  rename(V_ml_ucb = V, RU_ml_ucb = RU, NLL_ucb = NLL) %>%
  left_join(tbl_kalman_sm, by = c("ID", "Session"))  %>%
  rename(V_ml_sm = V, NLL_sm = NLL) 

tbl_ucb_plot <- tbl_ucb_compare %>%
  select(-c(NLL_sm, V_ml_sm, NLL_ucb)) %>%
  pivot_longer(-c(ID, Session)) %>%
  mutate(
    parameter = str_extract(name, "^[A-Z]+"),
    method = str_extract(name, "[a-z]*$")
  ) %>% select(-name)
tbl_ucb_plot$method <- factor(tbl_ucb_plot$method, labels = c("Hierarch. Bayes", "Max. Lik."))
tbl_ucb_plot$Session <- factor(tbl_ucb_plot$Session, labels = c("Session 1", "Session 2"))

tbl_ucb_cor <- tbl_ucb_compare %>%
  group_by(Session) %>%
  summarize(
    cor_RU = cor(RU_hcb, RU_ml_ucb),
    cor_V = cor(V_hcb, V_ml_ucb)
  ) %>% ungroup() %>%
  pivot_longer(-Session) %>%
  mutate(parameter = str_extract(name, "[A-Z]+$")) %>%
  select(-name)
tbl_ucb_cor$Session <- factor(tbl_ucb_cor$Session, labels = c("Session 1", "Session 2"))

tbl_jitter <- tbl_ucb_plot %>%
  group_by(parameter, Session) %>%
  summarize(
    my_jitter = sd(value),
    my_base = quantile(value, .9)
  ) %>%
  ungroup()
tbl_ucb_cor <- tbl_ucb_cor %>%
  left_join(tbl_jitter, by = c("parameter", "Session"))

saveRDS(tbl_ucb_cor, "data/restless-params-across-methods-cor-data.RDS")
saveRDS(tbl_ucb_plot %>% pivot_wider(names_from = method, values_from = value), "data/restless-params-across-methods-data.RDS")


pl_params_across_methods <- tbl_ucb_plot %>% pivot_wider(names_from = method, values_from = value) %>%
  ggplot(aes(`Max. Lik.`, `Hierarch. Bayes`)) +
  geom_abline() +
  geom_point(aes(color = parameter)) +
  geom_label(data = tbl_ucb_cor, aes(my_base + .5 * my_jitter, my_base - 2*my_jitter, label = str_c("r = ", round(value, 2)))) +
  facet_wrap(Session ~ parameter, scales = "free") +
  theme_bw() +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")

grid.draw(pl_params_across_methods)
saveRDS(pl_params_across_methods, "data/restless-params-across-methods.RDS")



# Compare Reliabilities of Parameters -------------------------------------

tbl_reliabilities <- tbl_ucb_compare %>% select(-NLL) %>% pivot_longer(-c(ID, Session)) %>%
  mutate(
    parameter = str_extract(name, "^[A-Z]+"),
    method = str_extract(name, "[a-z]+$")
  ) %>% pivot_wider(names_from = Session, values_from = value) %>%
  group_by(parameter, method) %>%
  summarize(reliability = cor(`1`, `2`))
tbl_reliabilities <- tbl_reliabilities %>%
  rename(Parameter = parameter, Method = method, Reliability = reliability)

DT:::datatable(tbl_reliabilities %>% mutate(Reliability = round(Reliability, 2)))

table_reliabilities <- grid.table(
  tbl_reliabilities %>% 
    mutate(Reliability = round(Reliability, 2)),
  rows = rep("  ", 4)
)
saveRDS(tbl_reliabilities, "data/restless-reliabilities-across-methods.Rds")



# Main Qualitative Patterns -----------------------------------------------


params_bf <- c("RU", "V")
library(kde1d)
hdi_etc <- function(tbl_draws, names_orig) {
  tbl_posterior <- tbl_draws %>% 
    dplyr::select(starts_with(c(names_orig)), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(names_orig), names_to = "parameter", values_to = "value") %>%
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

by_participant_maps <- function(tbl_draws, f_time, pars_interest) {
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

tbl_maps <- by_participant_maps(tbl_draws_hc_s1, "Session 1", c("beta", "tau")) %>%
  rbind(
    by_participant_maps(tbl_draws_hc_s2, "Session 2", c("beta", "tau"))
  )

tbl_hc_prep <- tbl_draws_hc_s1 %>% select(c(mu_beta, mu_tau)) %>% summarize(RU = mean(mu_beta), V = mean(mu_tau)) %>%
  mutate(Session = "Session 1", method = "Hierarch. Bayes") %>%
  rbind(
    tbl_draws_hc_s2 %>% select(c(mu_beta, mu_tau)) %>% summarize(RU = mean(mu_beta), V = mean(mu_tau)) %>%
      mutate(Session = "Session 2", method = "Hierarch. Bayes")
  )

tbl_ml_prep <- summary_se_within(tbl_ucb_compare, "RU_ml", withinvars = "Session") %>%
  select(-c(N, RU_ml_norm, sd, se)) %>%
  rename(RU_ci = ci) %>%
  left_join(
    summary_se_within(tbl_ucb_compare, "V_ml", withinvars = "Session") %>%
      select(-c(N, V_ml_norm, sd, se)) %>%
      rename(V_ci = ci),
    by = "Session"
  ) %>% rename(RU = RU_ml, V = V_ml)



tbl_ml_prep <- tbl_ucb_compare %>% group_by(Session) %>% summarize(RU = mean(RU_ml), V = mean(V_ml)) %>% mutate(method = "Max. Lik.")
tbl_ml_prep$Session <- factor(tbl_ml_prep$Session, labels = c("Session 1", "Session 2"))
tbl_prep <- rbind(tbl_hc_prep, tbl_ml_prep)
tbl_prep$method <- factor(tbl_prep$method, levels = c("Max. Lik.", "Hierarch. Bayes"), ordered = TRUE)

saveRDS(tbl_prep %>% pivot_longer(c(RU, V)), "data/restless-group-patterns-across-methods-data.RDS")


tbl_prep_individual <- summary_se_within(tbl_ucb_compare, "RU_ml", withinvars = "Session") %>%
  select(-c(N, RU_ml_norm, sd, se)) %>%
  rename(RU_ci = ci, RU = RU_ml) %>%
  left_join(
    summary_se_within(tbl_ucb_compare, "V_ml", withinvars = "Session") %>%
      select(-c(N, V_ml_norm, sd, se)) %>%
      rename(V = V_ml, V_ci = ci),
    by = "Session"
  ) %>% mutate(method = "Max. Lik.") %>%
  rbind(
    summary_se_within(tbl_ucb_compare, "RU_hcb", withinvars = "Session") %>%
      select(-c(N, RU_hcb_norm, sd, se)) %>%
      rename(RU_ci = ci, RU = RU_hcb) %>%
      left_join(
        summary_se_within(tbl_ucb_compare, "V_hcb", withinvars = "Session") %>%
          select(-c(N, V_hcb_norm, sd, se)) %>%
          rename(V_ci = ci, V = V_hcb),
        by = "Session"
      ) %>%
      mutate(method = "Hierarch. Bayes")
  )

tbl_prep_individual <- tbl_prep_individual %>% select(-c(RU_ci, V_ci)) %>% pivot_longer(c(RU, V), names_to = "parameter", values_to = "mn") %>%
  left_join(
    tbl_prep_individual %>% select(-c(RU, V)) %>% pivot_longer(c(RU_ci, V_ci), names_to = "parameter", values_to = "ci") %>% mutate(parameter = str_extract(parameter, "^[A-Z]+")),
    by = c("Session", "method", "parameter")
  )

saveRDS(tbl_prep_individual, "data/restless-group-patterns-across-methods-data-individual.RDS")


pl_group_pattern <- ggplot(tbl_prep %>% pivot_longer(c(RU, V)), aes(name, value)) +
  geom_col(aes(color = name), fill = "white") +
  geom_line(aes(group = 1)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  facet_wrap(Session ~ method) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  labs(x = "Parameter", y = "Group Mean") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")

saveRDS(pl_group_pattern, "data/restless-group-patterns-across-methods.Rds")


# Comparison SM ML vs. UCB hierarchical -----------------------------------


tbl_sm_compare <- tbl_ucb_compare %>%
  select(-c(starts_with("NLL_"), "RU_hcb", "V_ml_ucb", "RU_ml_ucb"))
tbl_cor <- tbl_sm_compare %>%
  group_by(Session) %>%
  summarize(r = cor(V_hcb, V_ml_sm)) %>%
  ungroup()

tbl_sm_compare %>%
  ggplot(aes(V_ml_sm, V_hcb)) +
  geom_abline() +
  geom_point(color = "#66C2A5") +
  geom_label(data = tbl_cor, aes(x = .35, y = .1, label = str_c("r = ", round(r, 2)))) +
  facet_wrap(~ Session) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Max. Lik. (Softmax)", y = "Hierarch. Bayes") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )
saveRDS(tbl_sm_compare, "data/restless-params-smml-hcbucb-data.RDS")
saveRDS(tbl_cor, "data/restless-params-smml-hcbucb-cor.RDS")


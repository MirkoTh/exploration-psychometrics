rm(list = ls())


library(tidyverse)
library(lme4)
library(furrr)
library(future)


session <- 1
load(paste("analysis/bandits/banditsWave", session, ".Rda", sep = ""))

sam1 <- as_tibble(sam %>% filter(session == 1))
sam1$V_z <- scale(sam1$V)[,1]
sam1$RU_z <- scale(sam1$RU)[,1]
sam1_init <- sam1 %>% filter(trial <= 3)
sam1_end <- sam1 %>% filter(trial >=8)
sam1 %>% pivot_longer(cols = c(V_z, RU_z)) %>% ggplot(aes(value)) + geom_histogram() + facet_wrap(~ name)

# glmer(chosen ~ V_z + RU_z + (V_z | ID), data = sam1, family = binomial)
# glmer(chosen ~ V_z + RU_z + (V_z + RU_z | ID), data = sam1_init, family = binomial)
# glmer(chosen ~ V_z + RU_z + (-1 + V_z + RU_z | ID), data = sam1_end, family = binomial)

horizon1 <- as_tibble(horizon %>% filter(trial == 5))

horizon1 %>% pivot_longer(cols = c(V, RU)) %>% ggplot(aes(value)) + geom_histogram() + facet_wrap(~ name)
  glmer(chosen ~ V*RU*Horizon + (1 + V | ID), data = horizon1, family = binomial)


# position analysis

model_no_ri <- function(serial_position) {
  tbl_used <- sam1 %>% filter(trial == serial_position)
  m <- glmer(chosen ~ V_z + RU_z + (-1 + V_z + RU_z | ID), data = tbl_used, family = binomial)
  
  return(m)
}
model_fixed <- function(serial_position) {
  tbl_used <- sam1 %>% filter(trial == serial_position)
  m <- glm(chosen ~ V_z + RU_z, data = tbl_used, family = binomial)
  
  return(m)
}

plan(multisession, workers = availableCores() - 2)
l_kalman_softmax_no_variance_2_rs <- furrr::future_map(seq(2, 10, by = 1), model_no_ri)
l_kalman_softmax_fixed <- furrr::future_map(seq(2, 10, by = 1), model_fixed)
plan("sequential")

df_beta <- map(l_kalman_softmax_no_variance_2_rs, ~.x@beta) %>% reduce(rbind) %>% as.data.frame()
colnames(df_beta) <- c("Intercept", "V", "RU")
tbl_beta_random <- as_tibble(df_beta) %>% mutate(V = -V, RU = -RU)

df_beta <- map(l_kalman_softmax_fixed, ~.x$coefficients) %>% reduce(rbind) %>% as.data.frame()
colnames(df_beta) <- c("Intercept_fixed", "V_fixed", "RU_fixed")
tbl_beta_fixed <- as_tibble(df_beta) %>% mutate(V_fixed = -V_fixed, RU_fixed = -RU_fixed)

tbl_plot <- cbind(tbl_beta_random, tbl_beta_fixed) %>%
  mutate(position = 2:(nrow(.)+1)) %>%
  pivot_longer(-position) %>%
  mutate(
    parameter = str_match(name, "^([A-Za-z]*)")[,1],
    r_eff = c("random", "fixed")[as.numeric(str_detect(name, "fixed")) + 1]
  ) %>%
  select(-name) %>%
  pivot_wider(names_from = r_eff, values_from = value)

ggplot(tbl_plot, aes(random, fixed)) +
  geom_abline() +
  geom_text(aes(label = position)) +
  facet_wrap(~ parameter)

saveRDS(tbl_plot, file = "data/2ab-choice-position.RDS")

ggplot(tbl_plot %>% filter(parameter != "Intercept"), aes(position, random, group = parameter)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dotdash", linewidth = 1) +
  geom_line(aes(color = parameter)) +
  geom_point(size = 5, color = "white") +
  geom_point(aes(color = parameter)) +
  geom_point(data = tbl_plot %>% filter(parameter != "Intercept" & position == 5), size = 5, shape = 1) +
  geom_label(data = tbl_plot %>% filter(parameter != "Intercept" & position == 5), aes(y = random + .475, label = "Horizon\nTask")) +
  facet_wrap(~ parameter) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 9, by = 1), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Choice Position", y = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")







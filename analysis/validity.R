rm(list = ls())

library(tidyverse)
library(grid)
library(gridExtra)
library(lavaan)
library(lavaanPlot)

rename_and_add_id <- function(my_df, nm_model) {
  my_df %>%
    rename(recovered = str_c("colMeans(as.data.frame(posterior_samples(", nm_model, ")))")) %>%
    mutate(id = str_extract(rownames(.), "[0-9]+")) %>%
    as_tibble()
}

# exclusion criteria
tbl_excl_bandits_1 <- read_csv("data/exclusions1.csv")
tbl_excl_bandits_2 <- read_csv("data/exclusions2.csv")
tbl_excl_wm <- readRDS("analysis/wm/subjects-excl-wm.rds")

tbl_exclude <- tbl_excl_wm %>%
  left_join(tbl_excl_bandits_1 %>% select(ID, exclude), by = c("participant_id" = "ID")) %>%
  left_join(tbl_excl_bandits_2 %>% select(ID, exclude), by = c("participant_id" = "ID"), suffix = c("_1", "_2")) %>%
  mutate(exclude_all = ifelse((excl_subject + exclude_1 + exclude_2) > 0, TRUE, FALSE))

fit_intercept <- FALSE
path_horizon1 <- "analysis/bandits/modellingResults/fitHorizonSession1UCBfullno_horizon"
path_horizon2 <- "analysis/bandits/modellingResults/fitHorizonSession2UCBfullno_horizon"
path_sam1 <- "analysis/bandits/modellingResults/fitSamSession1UCBhierarchical"
path_sam2 <- "analysis/bandits/modellingResults/fitSamSession2UCBhierarchical"

if (!fit_intercept){
  h1 <- str_c(path_horizon1, "no_intercept.Rda")
  h2 <- str_c(path_horizon2, "no_intercept.Rda")
  s1 <- str_c(path_sam1, "no_intercept.Rda")
  s2 <- str_c(path_sam2, "no_intercept.Rda")
} else if (fit_intercept) {
  h1 <- str_c(path_horizon1, ".Rda")
  h2 <- str_c(path_horizon2, ".Rda")
  s1 <- str_c(path_sam1, ".Rda")
  s2 <- str_c(path_sam2, ".Rda")
}

load(h1)
tbl_horizon_1 <- rename_and_add_id(trueParams, "baymodel")
load(h2)
tbl_horizon_2 <- rename_and_add_id(trueParams, "baymodel")

# load("analysis/bandits/modellingResults/fitHorizonSession1UCBfullno_horizon.Rda")
# tbl_horizon_1 <- rename_and_add_id(trueParams, "baymodel")
# load("analysis/bandits/modellingResults/fitHorizonSession2UCBfullno_horizon.Rda")
# tbl_horizon_2 <- rename_and_add_id(trueParams, "baymodel")

tbl_horizon <- tbl_horizon_1 %>% 
  inner_join(tbl_horizon_2, by = c("id", "predictor"), suffix = c("_1", "_2")) %>%
  select(c(id, predictor, estimate_1, estimate_2)) %>%
  pivot_wider(names_from = predictor, values_from = c(estimate_1, estimate_2))


load(s1)
tbl_sam_1 <- rename_and_add_id(trueParams, "trueModel")
load(s2)
tbl_sam_2 <- rename_and_add_id(trueParams, "trueModel")

tbl_sam <- tbl_sam_1 %>% 
  inner_join(tbl_sam_2, by = c("id", "predictor"), suffix = c("_1", "_2")) %>%
  select(c(id, predictor, estimate_1, estimate_2)) %>%
  pivot_wider(names_from = predictor, values_from = c(estimate_1, estimate_2))


tbl_overview <- readRDS("data/4arlb-overview.rds")
tbl_restless <- tbl_overview %>%
  select(ID, session, gamma_ucb, beta) %>%
  arrange(ID) %>%
  pivot_wider(names_from = session, values_from = c(gamma_ucb, beta)) %>%
  rename(id = ID) %>%
  mutate(id = as.character(id))


tbl_bandits <- tbl_restless %>%
  left_join(tbl_sam, by = "id") %>%
  left_join(tbl_horizon, by = "id", suffix = c("_sam", "_horizon")) %>%
  filter(!is.na(estimate_1_V_sam)) %>%
  mutate(id = as.numeric(id))

# re-code V and RU in Sam's task and Horizon task
tbl_bandits <- tbl_bandits %>%
  mutate(
    estimate_1_V_sam = -1 * estimate_1_V_sam,
    estimate_1_RU_sam = -1 * estimate_1_RU_sam,
    estimate_2_V_sam = -1 * estimate_2_V_sam,
    estimate_2_RU_sam = -1 * estimate_2_RU_sam,
    estimate_1_V_horizon = -1 * estimate_1_V_horizon,
    estimate_1_RU_horizon = -1 * estimate_1_RU_horizon,
    estimate_2_V_horizon = -1 * estimate_2_V_horizon,
    estimate_2_RU_horizon = -1 * estimate_2_RU_horizon
  )


colnames_ic <- c("V Restless", "RU Restless", "Intercept Sam", "V Sam", "RU Sam", "Intercept Horizon", "RU Horizon", "V Horizon")
colnames_no_ic <- c("V Restless", "RU Restless", "V Sam", "RU Sam", "RU Horizon", "V Horizon")

tbl_bandits_1 <- tbl_bandits %>%
  select(contains("1"))
tbl_bandits_2 <- tbl_bandits %>%
  select(contains("2"))
if (fit_intercept){
  colnames(tbl_bandits_1) <- colnames(tbl_bandits_2) <- colnames_ic
} else if (!fit_intercept)  {
  colnames(tbl_bandits_1) <- colnames(tbl_bandits_2) <- colnames_no_ic
}

my_corr_plot <- function(cortable, x_lab, y_lab, ttl) {
  as.data.frame(cortable) %>%
    mutate(rwn = rownames(.)) %>%
    pivot_longer(-rwn) %>%
    ggplot(aes(fct_rev(rwn), name)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 2)), color = "white") +
    scale_fill_viridis_c(limits = c(0, 1)) +
    theme_bw() +
    scale_x_discrete(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    labs(x = x_lab, y = y_lab, title = ttl) + 
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22),
      axis.text.x = element_text(angle = 90)
    )
}

pl_cors_s1 <- my_corr_plot(cor(tbl_bandits_1), "Parameter 1", "Parameter 2", "Session 1")
pl_cors_s2 <- my_corr_plot(cor(tbl_bandits_2), "Parameter 1", "Parameter 2", "Session 2")

grid.draw(arrangeGrob(pl_cors_s1, pl_cors_s2, nrow = 1))



# Working Memory ----------------------------------------------------------

tbl_wm <- tbl_exclude %>% filter(!exclude_all) %>% select(participant_id) %>%
  left_join(
    tbl_overview %>% mutate(ID = as.numeric(as.character(ID))) %>%
      select(ID, session, OS_recall, SS_recall, WMU_recall, OS_processing, SS_processing),
    by = c("participant_id" = "ID")
  )
colnames(tbl_wm) <- c("participant_id", "session", "recall_OS", "recall_SS", "recall_WMU", "processing_OS", "processing_SS")

tbl_wm_wide <- tbl_wm %>%
  pivot_wider(id_cols = participant_id, names_from = session, values_from = c(starts_with("recall"), starts_with("processing")))

l_tbl_wm <- split(tbl_wm, tbl_wm$session)


pl_cors_wm_s1 <- my_corr_plot(
  cor(l_tbl_wm[[1]] %>% select(-c(participant_id, session, processing_OS, processing_SS)) %>% rename("OS" = "recall_OS", "SS" = "recall_SS", "WMU" = "recall_WMU")),
  "Measure 1", "Measure 2", "Session 1"
)
pl_cors_wm_s2 <- my_corr_plot(
  cor(l_tbl_wm[[2]] %>% select(-c(participant_id, session, processing_OS, processing_SS)) %>% rename("OS" = "recall_OS", "SS" = "recall_SS", "WMU" = "recall_WMU")),
  "Measure 1", "Measure 2", "Session 2"
)
grid.draw(arrangeGrob(pl_cors_wm_s1, pl_cors_wm_s2, nrow = 1))





# Questionnaires ----------------------------------------------------------

# questionnaires


tbl_q <- read_csv("data/all-data/questionnaireScores.csv") %>% select(-"...1")

tbl_q <- tbl_exclude %>% filter(!exclude_all) %>% select(participant_id) %>%
  left_join(
    tbl_q %>% select(ID, session, measure, score),
    by = c("participant_id" = "ID")
  )

tbl_q_wide <- tbl_q %>% 
  filter(!is.na(measure)) %>%
  pivot_wider(
    id_cols = c(participant_id, session), 
    names_from = measure, values_from = score
  ) %>% 
  pivot_wider(id_cols = participant_id, names_from = session, values_from = c(BIG_5, CEI, PANASneg, PANASpos, PHQ_9, STICSA))




# CFAs --------------------------------------------------------------------

# recode v and ru from sam and horizon to map to restless params

tbl_wm_bandits <- tbl_wm_wide %>% 
  left_join(tbl_bandits, by = c("participant_id" = "id")) %>%
  left_join(tbl_q_wide, by = "participant_id")
tbl_wm_bandits <- tbl_wm_bandits[complete.cases(tbl_wm_bandits), ]
colnames(tbl_wm_bandits) <- str_remove(colnames(tbl_wm_bandits), "estimate_")

suffixes <- str_extract(colnames(tbl_wm_bandits), "^[1-2]_")
suffixes <- str_c("_", str_remove(suffixes, "_"))

colnames(tbl_wm_bandits) <- str_remove(colnames(tbl_wm_bandits), "^[1-2]_")
colnames(tbl_wm_bandits)[!is.na(suffixes)] <- str_c(colnames(tbl_wm_bandits), suffixes)[!is.na(suffixes)]


my_cols <- colnames(tbl_wm_bandits)[!str_detect(colnames(tbl_wm_bandits), "id$")]
tbl_wm_bandits[, my_cols] <- map(tbl_wm_bandits[, my_cols], ~ scale(.x, center = TRUE, scale = TRUE)[, 1])


tbl_wm_bandits_1 <- tbl_wm_bandits %>% select(contains("_1"))
tbl_wm_bandits_2 <- tbl_wm_bandits %>% select(contains("_2"))
colnames(tbl_wm_bandits_1) <- str_remove(colnames(tbl_wm_bandits_1), "_1")
colnames(tbl_wm_bandits_2) <- str_remove(colnames(tbl_wm_bandits_1), "_2")


# specify the model

value_guided_model_2_sessions <- ' 
  g_v_1  =~ V_sam_1 + V_horizon_1 + gamma_ucb_1
  g_wm_1 =~ recall_WMU_1 + recall_OS_1 + recall_SS_1

  
  g_v_2  =~ V_sam_2 + V_horizon_2 + gamma_ucb_2
  g_wm_2 =~ recall_WMU_2 + recall_OS_2 + recall_SS_2
  
  V_horizon_1 ~~ V_horizon_2
  recall_SS_1 ~~  recall_SS_2
  recall_OS_1 ~~  recall_OS_2
  

'

# fit the model

fit_l <- cfa(value_guided_model_2_sessions, data = tbl_wm_bandits)
summary(fit_l, fit.measures = TRUE)

# plot results
lavaanPlot(
  fit_l, coefs = TRUE, covs = TRUE, 
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)



# specify the model
value_guided_model_1_session <- ' 
  g_v  =~ V_sam_1 + V_horizon_1 + gamma_ucb_1

  g_wm =~ recall_WMU_1 + recall_OS_1 + recall_SS_1
  
  CEI_1 ~~ g_v
  CEI_1 ~~ g_wm
  BIG_5_1 ~~ g_v
  BIG_5_1 ~~ g_wm

'

fit_c <- cfa(value_guided_model_1_session, data = tbl_wm_bandits)
summary(fit_l, fit.measures = TRUE)

# plot results
lavaanPlot(
  fit_c, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)




cor(tbl_wm_bandits_1)
cor(tbl_wm_bandits_2)

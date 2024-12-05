rm(list = ls())

# Load Packages and Data --------------------------------------------------


library(tidyverse)
library(grid)
library(gridExtra)
library(lavaan)
library(lavaanPlot)
library(irr)

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)



# Definition of Used Model Parameters -------------------------------------

# models as from the literature or all three tasks ucb?
is_ucb <- TRUE




# Apply Exclusion Criteria ------------------------------------------------


rename_and_add_id <- function(my_df, nm_model) {
  my_df %>%
    rename(recovered = str_c("colMeans(as.data.frame(posterior_samples(", nm_model, ")))")) %>%
    mutate(id = str_extract(rownames(.), "[0-9]+")) %>%
    as_tibble()
}

# exclusion criteria
tbl_exclude2 <- read_csv(file = "data/exclusions2_noPID.csv")
tbl_exclude1 <- read_csv(file = "data/exclusions1_noPID.csv")

# seems like two people were invited to session 2, who should not have been
tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
  left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
  filter(exclude_2 == 0 & exclude_1 == 0)



# Prepare Data ------------------------------------------------------------


if (is_ucb) {
  tbl_horizon_sam <- readRDS("analysis/bandits/allParams_improved.rds") %>% 
    pivot_wider(id_cols = "ID", names_from = c("task", "predictor", "session"), values_from = "estimate")
  colnm_prep <- str_match(colnames(tbl_horizon_sam)[2:ncol(tbl_horizon_sam)], "([A-Za-z_]*)(_[1-2]$)")
  colnames(tbl_horizon_sam)[2:ncol(tbl_horizon_sam)] <- str_c(colnm_prep[,2] , "_10", colnm_prep[,3])
} else {
  tbl_horizon_sam <- readRDS("analysis/bandits/allParams.rds") %>% 
    pivot_wider(id_cols = "ID", names_from = c("task", "predictor", "session"), values_from = "estimate")
  colnm_prep <- str_match(colnames(tbl_horizon_sam)[2:ncol(tbl_horizon_sam)], "([A-Za-z_]*)(_[1-2]$)")
  colnames(tbl_horizon_sam)[2:ncol(tbl_horizon_sam)] <- str_c(colnm_prep[,2] , "_10", colnm_prep[,3])
}

colnames(tbl_horizon_sam) <- str_replace(colnames(tbl_horizon_sam), "_short_", "_5_")
colnames(tbl_horizon_sam) <- str_replace(colnames(tbl_horizon_sam), "_long_", "_10_")
colnames(tbl_horizon_sam) <- str_replace(colnames(tbl_horizon_sam), "_info_", "_RU_")
colnames(tbl_horizon_sam) <- str_replace(colnames(tbl_horizon_sam), "_delta_mean_", "_V_")

tbl_horizon_sam <- tbl_horizon_sam %>%
  inner_join(tbl_exclude, by = "ID") %>%
  select_if(str_detect(colnames(.), "_10_|ID")) %>%
  relocate(sam_Intercept_10_1, .before = sam_V_10_1) %>%
  relocate(sam_Intercept_10_2, .before = sam_V_10_2) %>%
  relocate(horizon_Intercept_10_1, .before = horizon_V_10_1) %>%
  relocate(horizon_Intercept_10_2, .before = horizon_V_10_2) %>%
  relocate(horizon_RU_10_1, .after = horizon_V_10_1) %>%
  relocate(horizon_RU_10_2, .after = horizon_V_10_2) %>%
  relocate(restless_RU_10_1, .after = restless_V_10_1)  %>%
  relocate(restless_RU_10_2, .after = restless_V_10_2)

tbl_bandits <- tbl_horizon_sam

tbl_bandits_rel <- tbl_bandits %>% pivot_longer(-ID) %>%
  mutate(
    session = as.numeric(str_detect(name, "_2")) + 1,
    task = str_extract(name, "^[a-z]*"),
    is_v = str_detect(name, pattern = "_[vV]"),
    is_vtu = str_detect(name, pattern = "_VTU_"),
    is_v = is_v & !is_vtu,
    is_ic = str_detect(name, "_Intercept")
  )
tbl_bandits_rel$parameter <- "RU"
tbl_bandits_rel$parameter[tbl_bandits_rel$is_v] <- "V"
tbl_bandits_rel$parameter[tbl_bandits_rel$is_ic] <- "IC"
tbl_bandits_rel$parameter[tbl_bandits_rel$is_vtu] <- "VTU"




# Reliability & Validity --------------------------------------------------


calc_icc_3_1 <- function(s1, s2) {
  r <- icc(tibble(s1, s2), model = "twoway", type = "consistency", unit = "single")
  r$value
  
}

tbl_bandits_rel <- tbl_bandits_rel %>% select(-c(is_v, is_ic, is_vtu, name))
tbl_bandits_param_rel <- tbl_bandits_rel %>% 
  pivot_wider(id_cols = c(ID, task, parameter), names_from = c(session), values_from = value) %>%
  group_by(task, parameter) %>%
  summarize(
    #value = cor(`1`, `2`),
    value = calc_icc_3_1(`1`, `2`),
  ) %>%
  ungroup()
tbl_bandits_param_rel$task <- as.character(factor(tbl_bandits_param_rel$task, labels = c("Horizon", "Restless", "Sam")))


# reliability task scores
tbl_rel_task_measures <- reliability_task_measures()


tbl_rel_both <- rbind(
  tbl_bandits_param_rel %>% mutate(measure = "Parameter"),
  tbl_rel_task_measures %>% mutate(measure = "Task Measure")
)
tbl_rel_both$task[tbl_rel_both$task == "Sam"] <- "2Armed"
tbl_rel_both$task <- factor(tbl_rel_both$task, levels = c("Horizon", "2Armed", "Restless"), ordered = TRUE)

if (is_ucb) {
  lvls <- c("IC", "V", "RU", "regret", "p(optimal)", "p(switch)")
} else {
  lvls <- c("IC", "V", "RU", "VTU", "regret", "p(optimal)", "p(switch)")
  
}
tbl_rel_both$parameter <- factor(tbl_rel_both$parameter, levels = lvls, ordered = TRUE)

saveRDS(tbl_rel_both, str_c("analysis/bandits/reliabilities", c("-hybrid", "-ucb")[is_ucb + 1], ".csv"))
ggplot(tbl_rel_both %>% filter(parameter != "IC"), aes(value, parameter)) +
  geom_vline(xintercept = .5, color = "red", linewidth = 2, alpha = .3) +
  geom_vline(xintercept = .75, color = "lightgreen", linewidth = 2, alpha = .5) +
  geom_vline(xintercept = .9, color = "darkgreen", linewidth = 2, alpha = .3) +
  geom_point(aes(fill = measure), shape = 23, size = 3, color = "black") +
  facet_wrap(~ task) +
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0), breaks = seq(0, 1, by = .2)) +
  scale_y_discrete(limits = rev, expand = c(0.03, 0)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Test-Retest Reliability", y = "Measure") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .5),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("skyblue2", "tomato4"), name = "")




if (is_ucb) {
  colnames_ic <- c(
    "ID",
    "Intercept 2Armed", "V 2Armed", "RU 2Armed",
    "Intercept Horizon", "V Horizon", "RU Horizon", 
    "V Restless", "RU Restless",
    "session"
  )
} else {
  colnames_ic <- c(
    "ID",
    "Intercept 2Armed", "V 2Armed", "RU 2Armed", "VTU 2Armed",
    "Intercept Horizon", "V Horizon", "RU Horizon", 
    "V Restless", "RU Restless",
    "session"
  )
  
}

tbl_bandits_1 <- tbl_bandits %>%
  select(!contains("_2")) %>%
  mutate(session = 1)
cn <- colnames(tbl_bandits)
cn2 <- cn[!(cn %in% colnames(tbl_bandits_1))]
tbl_bandits_2 <- tbl_bandits[, c("ID", cn2)] %>% mutate(session = 2)
colnames(tbl_bandits_1) <- colnames(tbl_bandits_2) <- colnames_ic



if (is_ucb) {
  saveRDS(tbl_bandits_1, "analysis/bandits/bandits-1-ucb.RDS")
  saveRDS(tbl_bandits_2, "analysis/bandits/bandits-2-ucb.RDS")
} else {
  saveRDS(tbl_bandits_1, "analysis/bandits/bandits-1-hybrid.RDS")
  saveRDS(tbl_bandits_2, "analysis/bandits/bandits-2-hybrid.RDS")
}


pl_cors_s1 <- cor(tbl_bandits_1 %>% select(-c(ID, session, contains("Intercept")))) %>%
  as.data.frame() %>%
  mutate(v_in = rownames(.)) %>%
  pivot_longer(-c(v_in)) %>%
  rename(v_out = name) %>%
  ggplot(aes(v_in, v_out)) +
  geom_tile(aes(fill = value)) +
  geom_label(aes(label = round(value, 2)))
pl_cors_s2 <- cor(tbl_bandits_2 %>% select(-c(ID, session, contains("Intercept")))) %>%
  as.data.frame() %>%
  mutate(v_in = rownames(.)) %>%
  pivot_longer(-c(v_in)) %>%
  rename(v_out = name) %>%
  ggplot(aes(v_in, v_out)) +
  geom_tile(aes(fill = value)) +
  geom_label(aes(label = round(value, 2)))
grid.draw(arrangeGrob(pl_cors_s1, pl_cors_s2, nrow = 1))



# Working Memory ----------------------------------------------------------

tbl_wm_wide <- read_csv("data/all-data/wm-performance.csv")
tbl_wm_wide <- tbl_exclude %>% select(ID) %>%
  inner_join(tbl_wm_wide, by = c("ID" = "participant_id")) 
tbl_wm_1 <- tbl_wm_wide %>%
  select(c(ID, ends_with("_0"))) %>%
  mutate(session = 1)
colnames(tbl_wm_1) <- str_remove(colnames(tbl_wm_1), "_0$")
tbl_wm_2 <- tbl_wm_wide %>%
  select(c(ID, ends_with("_1"))) %>%
  mutate(session = 2)
colnames(tbl_wm_2) <- str_remove(colnames(tbl_wm_2), "_1$")
tbl_wm <- rbind(tbl_wm_1, tbl_wm_2)

tbl_wm_wide <- tbl_wm %>%
  pivot_wider(id_cols = ID, names_from = session, values_from = c(ends_with("recall"), "OS_processing", "SS_processing"))

l_tbl_wm <- split(tbl_wm, tbl_wm$session)

saveRDS(l_tbl_wm, "analysis/wm/wm-measures.RDS")

pl_cors_wm_s1 <- my_corr_plot(
  cor(
    l_tbl_wm[[1]] %>% 
      select(-c(ID, session, OS_processing, SS_processing, prop_timeout_os_processing, prop_timeout_ss_processing, rt_os, rt_ss)) %>%
      rename("OS" = "OS_recall", "SS" = "SS_recall", "WMU" = "WMU_recall")
  ), "Measure 1", "Measure 2", "Session 1", type = "wm"
)
pl_cors_wm_s2 <- my_corr_plot(
  cor(
    l_tbl_wm[[2]] %>% 
      select(-c(ID, session, OS_processing, SS_processing, prop_timeout_os_processing, prop_timeout_ss_processing, rt_os, rt_ss)) %>% 
      rename("OS" = "OS_recall", "SS" = "SS_recall", "WMU" = "WMU_recall")
  ), "Measure 1", "Measure 2", "Session 2", type = "wm"
)
grid.draw(arrangeGrob(pl_cors_wm_s1, pl_cors_wm_s2, nrow = 1))


tbl_wm_reliability <- tbl_wm_wide %>%
  #select(-contains("processing")) %>%
  pivot_longer(-ID, values_to = "score") %>%
  mutate(
    measure = str_extract(name, "^[A-Z]*_[a-z]*"),
    session = str_extract(name, "[1-2]$")
  ) %>% select(-name) %>%
  relocate(session, .after = ID) %>%
  relocate(score, .after = measure) %>%
  mutate(domain = "WM")


# Questionnaires ----------------------------------------------------------

# questionnaires


tbl_q <- read_csv("analysis/questionnaireScores.csv") %>% select(-"...1")

tbl_q <- tbl_exclude %>% select(ID) %>%
  inner_join(
    tbl_q %>% select(ID, session, measure, score),
    by = "ID"
  )

tbl_q_wide <- tbl_q %>% 
  filter(!is.na(measure)) %>%
  pivot_wider(
    id_cols = c(ID, session), 
    names_from = measure, values_from = score
  ) %>% 
  pivot_wider(id_cols = ID, names_from = session, values_from = c(BIG_5, CEI, PANASneg, PANASpos, PHQ_9, STICSAcog, STICSAsoma))


tbl_rel_wm_q <- tbl_q %>% mutate(domain = "Questionnaire") %>%
  rbind(tbl_wm_reliability) %>%
  pivot_wider(id_cols = c(ID, domain, measure), names_from = c(session), values_from = score) %>%
  group_by(domain, measure) %>%
  summarize(reliability = cor(`1`, `2`)) %>%
  ungroup()

tbl_rel_wm_q$measure <- factor(
  tbl_rel_wm_q$measure, levels = c(
    "CEI", "BIG_5", "PHQ_9","STICSAcog", "STICSAsoma", 
    "PANASpos", "PANASneg", "OS_processing", "OS_recall", "SS_processing", "SS_recall", "WMU_recall"
  ), ordered = TRUE, labels = c(
    "Exploration", "Openness", "Depression","Anxiety Cog.", "Anxiety Som.", 
    "Affect +", "Affect -", "O Span Processing", "O Span Recall", "S Span Processing", "S Span Recall", "WM Updating"
  )
)

ggplot(tbl_rel_wm_q %>% filter(!str_detect(measure, "Processing")), aes(reliability, measure)) +
  geom_vline(xintercept = .5, color = "red", linewidth = 2, alpha = .3) +
  geom_vline(xintercept = .75, color = "lightgreen", linewidth = 2, alpha = .5) +
  geom_vline(xintercept = .9, color = "darkgreen", linewidth = 2, alpha = .3) +
  geom_point(aes(fill = domain), shape = 23, size = 3, color = "black") +
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0), breaks = seq(0, 1, by = .2)) +
  scale_y_discrete(limits = rev, expand = c(0.03, 0)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Test-Retest Reliability", y = "Measure") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .5),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("skyblue2", "tomato4"), name = "")


# item-based questionnaire data
tbl_q_item <- read.csv("analysis/dat_for_efa_s2.csv") %>% as_tibble() %>% select(-X)

tbl_q_item <- tbl_q_item %>% mutate(
  PANAS_0 = 4 - PANAS_0,
  PANAS_2 = 4 - PANAS_2,
  PANAS_4 = 4 - PANAS_4,
  PANAS_8 = 4 - PANAS_8,
  PANAS_9 = 4 - PANAS_9,
  PANAS_11 = 4 - PANAS_11,
  PANAS_13 = 4 - PANAS_13,
  PANAS_15 = 4 - PANAS_15,
  PANAS_16 = 4 - PANAS_16,
  PANAS_18 = 4 - PANAS_18
)
# this file only imports session 2 data
colnames(tbl_q_item) <- str_c(colnames(tbl_q_item), "_2")
colnames(tbl_q_item)[1] <- "ID"


# CFAs --------------------------------------------------------------------


tbl_all_three <- read_csv("analysis/bandits-task-measures.csv")


tbl_all_three <- tbl_all_three %>%
  select(ID, p_switch_1, p_switch_2, task) %>%
  pivot_wider(id_cols = ID, names_from = task, values_from = c(p_switch_1, p_switch_2))
colnames(tbl_all_three) <- str_c(str_remove(colnames(tbl_all_three), "_[1-2]"), str_extract(colnames(tbl_all_three), "_[1-2]"))
colnames(tbl_all_three)[1] <- "ID"
colnames(tbl_all_three)[str_detect(colnames(tbl_all_three), "Sam")] <- str_replace(colnames(tbl_all_three)[str_detect(colnames(tbl_all_three), "Sam")], "Sam", "2Armed")


tbl_cor_1 <- cor(tbl_all_three %>% select(contains("_1")))
colnames(tbl_cor_1) <- c("Horizon", "Two-Armed", "Restless")
rownames(tbl_cor_1) <- colnames(tbl_cor_1)
pl_switch_s1 <- my_corr_plot(tbl_cor_1, "Measure 1", "Measure 2", "Session 1", type = "switch")
tbl_cor_2 <- cor(tbl_all_three[str_detect(colnames(tbl_all_three), "_2$")])
colnames(tbl_cor_2) <- c("Horizon", "Two-Armed", "Restless")
rownames(tbl_cor_2) <- colnames(tbl_cor_2)
pl_switch_s2 <- my_corr_plot(tbl_cor_2, "Measure 1", "Measure 2", "Session 2", type = "switch")
grid::grid.draw(gridExtra::arrangeGrob(pl_switch_s1, pl_switch_s2, nrow = 1))



# recode v and ru from sam and horizon to map to restless params

colnames(tbl_bandits_1) <- str_c("1_", colnames(tbl_bandits_1))
colnames(tbl_bandits_2) <- str_c("2_", colnames(tbl_bandits_2))
tbl_bandits_corr <- cbind(tbl_bandits_1, tbl_bandits_2) %>%
  rename(ID = "1_ID") %>%
  select(-"2_ID") %>% as_tibble()

tbl_wm_bandits <- tbl_wm_wide %>% 
  left_join(tbl_bandits_corr, by = "ID") %>%
  #left_join(tbl_q_wide, by = "ID")
  left_join(tbl_q_item, by = "ID")
tbl_wm_bandits <- tbl_wm_bandits[complete.cases(tbl_wm_bandits), ]
colnames(tbl_wm_bandits) <- str_remove(colnames(tbl_wm_bandits), "estimate_")

# all session IDs as suffixes

suffixes <- str_extract(colnames(tbl_wm_bandits), "^[1-2]_")
suffixes <- str_c("_", str_remove(suffixes, "_"))

colnames(tbl_wm_bandits) <- str_remove(colnames(tbl_wm_bandits), "^[1-2]_")
colnames(tbl_wm_bandits)[!is.na(suffixes)] <- str_c(colnames(tbl_wm_bandits), suffixes)[!is.na(suffixes)]


tbl_wm_bandits %>% 
  select(starts_with("RU 2Armed") | starts_with("RU Horizon") | starts_with("RU Restless") |
           starts_with("V 2Armed") | starts_with("V Horizon") | starts_with("V Restless")) %>% 
  mutate(rwn = 1:nrow(.)) %>% pivot_longer(-rwn) %>% 
  mutate(
    session = str_extract(name, "[1-2]$"),
    parameter = str_match(name, "(.*)_[1-2]$")[,2]
  ) %>%
  ggplot(aes(value)) + geom_histogram() + facet_grid(session~parameter, scales = "free_x")


my_cols <- colnames(tbl_wm_bandits)[!str_detect(colnames(tbl_wm_bandits), "ID$")]
tbl_wm_bandits[, my_cols] <- map(tbl_wm_bandits[, my_cols], ~ scale(.x, center = TRUE, scale = TRUE)[, 1])
colnames(tbl_wm_bandits) <- str_replace(colnames(tbl_wm_bandits), " ", "_")


tbl_wm_bandits_1 <- tbl_wm_bandits %>% select(contains("_1"))
tbl_wm_bandits_2 <- tbl_wm_bandits %>% select(contains("_2"))
colnames(tbl_wm_bandits_1) <- str_remove(colnames(tbl_wm_bandits_1), "_1")
colnames(tbl_wm_bandits_2) <- str_remove(colnames(tbl_wm_bandits_1), "_2")


# some helper functions

approximate_bf <- function(bic1, bic2) exp((bic2 - bic1)/2)

extract_indices <- function(ms, modelname) {
  out <- vector()
  out["modelname"] <- modelname
  out <- c(out, ms[c(
    "chisq", "df", "pvalue", "cfi", "rmsea", 
    "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"
  )])
  return(out)
}

extract_loadings <- function(ms, gvar) {
  ms$pe %>% filter(lhs == gvar) %>% select(c(rhs, std.all))
}


# Session 1 ---------------------------------------------------------------


# WM CFA session 1
wm_model_session1 <- ' 
  g_wm_1 =~ WMU_recall_1 + OS_recall_1 + SS_recall_1
'

fit_wm <- cfa(wm_model_session1, data = tbl_wm_bandits)
s_wm <- summary(fit_wm, fit.measures = TRUE, standardized = TRUE)
resid(fit_wm)


# value-guided CFA session 1
value_guided_model_session1 <- ' 
  g_v_1  =~ V_2Armed_1 + V_Horizon_1 + V_Restless_1
'

fit_vg <- cfa(value_guided_model_session1, data = tbl_wm_bandits)
s_vg <- summary(fit_vg, fit.measures = TRUE, standardized = TRUE)
resid(fit_vg)

# directed CFA session 1
ru_model_session1 <- ' 
  g_ru_1  =~ RU_2Armed_1 + RU_Horizon_1 + RU_Restless_1
'

fit_rug <- sem(ru_model_session1, data = tbl_wm_bandits)
s_rug <- summary(fit_rug, fit.measures = TRUE, standardized = TRUE)

# extract factor loadings


tbl_loadings <- map2(list(s_wm, s_vg, s_rug), list("g_wm_1", "g_v_1", "g_ru_1"), extract_loadings) %>%
  map2(.y = list("Working-Memory Capacity", "Value-Guided", "Directed"), function(x, y) {x$model = y; return(x)}) %>%
  reduce(rbind) %>%
  as.data.frame() %>% as_tibble() %>%
  filter(!startsWith(rhs, "g_")) %>%
  relocate(model, .before = rhs)
tbl_loadings$rhs <- c(
  "Updating", "Operation Span", "Symmetry Span",
  rep(c("Two-armed", "Horizon", "Restless"), 2)
)
tbl_loadings$rhs <- factor(
  tbl_loadings$rhs, levels = c(
    "Updating", "Operation Span", "Symmetry Span", "Horizon", "Two-armed", "Restless"
  ), ordered = TRUE
)
tbl_loadings_show <- tbl_loadings %>% arrange(desc(model), rhs)
colnames(tbl_loadings_show) <- c("Model", "Indicator", "Standardized Loading")

# vg & ru CFA session 1
# negative variance problem
vg_ru_model_session1 <- '

  g_ru_1  =~ RU_2Armed_1 + RU_Horizon_1 + RU_Restless_1
  g_v_1  =~ V_2Armed_1 + V_Horizon_1 + V_Restless_1

  
  RU_2Armed_1 ~~ V_2Armed_1
  RU_Horizon_1 ~~ V_Horizon_1
  RU_Restless_1 ~~ V_Restless_1

'
fit_vg_rug1 <- sem(vg_ru_model_session1, data = tbl_wm_bandits)
summary(fit_vg_rug1, fit.measures = TRUE, standardized = TRUE)



# Session 2 ---------------------------------------------------------------


wm_model_session2 <- ' 
  g_wm_2 =~ WMU_recall_2 + OS_recall_2 + SS_recall_2
'

fit_wm <- cfa(wm_model_session2, data = tbl_wm_bandits)
s_wm <- summary(fit_wm, fit.measures = TRUE, standardized = TRUE)
resid(fit_wm)


value_guided_model_session2 <- ' 
  g_v_2  =~ V_2Armed_2 + V_Horizon_2 + V_Restless_2
'

fit_vg <- cfa(value_guided_model_session2, data = tbl_wm_bandits)
s_vg <- summary(fit_vg, fit.measures = TRUE, standardized = TRUE)
resid(fit_vg)

# directed CFA session 1
ru_model_session2 <- ' 
  g_ru_2  =~ RU_2Armed_2 + RU_Horizon_2 + RU_Restless_2
'

fit_rug <- cfa(ru_model_session2, data = tbl_wm_bandits)
s_rug <- summary(fit_rug, fit.measures = TRUE, standardized = TRUE)




# yeo.johnson tf does not work either
tbl_wm_bandits_sqrt <- tbl_wm_bandits %>% mutate(
  RU_Restless_1 = yeo.johnson(RU_Restless_1, 1),
  RU_Horizon_1 = yeo.johnson(RU_Horizon_1, 1)
)


# vg & ru CFA session 2
vg_ru_model_session2 <- '

  g_ru_2  =~ RU_2Armed_2 + RU_Horizon_2 + RU_Restless_2
  g_v_2  =~ V_2Armed_2 + V_Horizon_2 + V_Restless_2

  
  RU_2Armed_2 ~~ V_2Armed_2
  RU_Horizon_2 ~~ V_Horizon_2
  RU_Restless_2 ~~ V_Restless_2

'

fit_vg_rug2 <- sem(vg_ru_model_session2, data = tbl_wm_bandits)
s_vg_rug2 <- summary(fit_vg_rug2, fit.measures = TRUE, standardized = TRUE)


one_factor_model <- '
  # General factor
  g_e =~ RU_2Armed_2 + RU_Horizon_2 + RU_Restless_2 + V_2Armed_2 + V_Horizon_2 + V_Restless_2
  
  # Correlations between indicators from the same method
  RU_2Armed_2 ~~ V_2Armed_2
  RU_Horizon_2 ~~ V_Horizon_2
  RU_Restless_2 ~~ V_Restless_2
'

fit_onefactor_2 <- sem(one_factor_model, data = tbl_wm_bandits)
s_onefactor <- summary(fit_onefactor_2, fit.measures = TRUE, standardized = TRUE)


# compare one- and two-factor models

tbl_indices <- map2(list(s_vg_rug2$fit, s_onefactor$fit), list("2 Factors", "1 Factor"), extract_indices) %>%
  reduce(rbind) %>%
  as.data.frame() %>% as_tibble()
cols_num <- colnames(tbl_indices)[colnames(tbl_indices) != "modelname"]
approximate_bf(as.numeric(tbl_indices$bic[1]), as.numeric(tbl_indices$bic[2]))
tbl_indices[, cols_num] <- map(tbl_indices[, cols_num], .f = function(x) round(as.numeric(x), 2))




# plot results
lavaanPlot(
  fit_vg_rug2, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)

# List of variables
vars <- c("x1", "x2", "x3")

make_pairwise_strings <- function(vars) {
  # Generate pairwise residual correlations
  resid_correlations <- combn(vars, 2, function(x) paste(x[1], "~~", x[2]), simplify = TRUE)
  
  # Combine into a single string
  resid_correlations_string <- paste(resid_correlations, collapse = "\n")
  
  # Print the result
  cat(resid_correlations_string)
}

make_pairwise_strings(c("STICSA_0_2", "STICSA_1_2", "STICSA_2_2", "STICSA_3_2", "STICSA_4_2", "STICSA_5_2", "STICSA_6_2", "STICSA_7_2",
                          "STICSA_8_2", "STICSA_10_2", "STICSA_11_2", "STICSA_12_2", "STICSA_13_2", "STICSA_14_2", "STICSA_15_2",
                          "STICSA_16_2", "STICSA_17_2", "STICSA_18_2", "STICSA_19_2", "STICSA_20_2", "STICSA_21_2", "PHQ_9_0_2",
                          "PHQ_9_1_2", "PHQ_9_2_2", "PHQ_9_3_2", "PHQ_9_4_2", "PHQ_9_5_2", "PHQ_9_7_2", "PHQ_9_8_2", "PHQ_9_9_2"))

# vg & ru CFA session 2
vg_ru_wmc_model_session2 <- '

 
  # behavioral
  g_ru_2  =~ RU_2Armed_2 + RU_Horizon_2 + RU_Restless_2
  g_v_2  =~ V_2Armed_2 + V_Horizon_2 + V_Restless_2
  g_wm_2 =~ WMU_recall_2 + OS_recall_2 + SS_recall_2
  
  RU_2Armed_2 ~~ V_2Armed_2
  RU_Horizon_2 ~~ V_Horizon_2
  RU_Restless_2 ~~ V_Restless_2
  WMU_recall_2 ~~ OS_recall_2  
  
  
  
  
  Open =~ BIG_5_0_2 + BIG_5_1_2 + BIG_5_2_2 + BIG_5_3_2 + BIG_5_4_2 + BIG_5_5_2
  
  
  negMood =~ PANAS_1_2 + PANAS_3_2 +  PANAS_5_2 + PANAS_6_2 +
  PANAS_7_2 + PANAS_10_2 +  PANAS_12_2 + PANAS_14_2 + PANAS_17_2 +  PANAS_19_2
  
  posMood =~ PANAS_0_2 + PANAS_2_2 +  PANAS_4_2 + PANAS_8_2 + PANAS_9_2 +  PANAS_11_2 +
  PANAS_13_2 + PANAS_15_2 + PANAS_16_2 + PANAS_18_2

  # questionnaire
  AxDep =~ STICSA_0_2 +
  STICSA_1_2 + STICSA_2_2 + STICSA_3_2 + STICSA_4_2 + STICSA_5_2 + STICSA_6_2 + STICSA_7_2 +
  STICSA_8_2 + STICSA_10_2 + STICSA_11_2 + STICSA_12_2 + STICSA_13_2 + STICSA_14_2 + STICSA_15_2 +
  STICSA_16_2 + STICSA_17_2 + STICSA_18_2 + STICSA_19_2 + STICSA_20_2 + STICSA_21_2 + PHQ_9_0_2 +
  PHQ_9_1_2 + PHQ_9_2_2 + PHQ_9_3_2 + PHQ_9_4_2 + PHQ_9_5_2 + PHQ_9_7_2 + PHQ_9_8_2 + PHQ_9_9_2
  
  Exp =~ CEI_0_2 +  CEI_1_2 + CEI_2_2 + CEI_3_2

  

'

fit_vg_rug_wm2 <- sem(vg_ru_wmc_model_session2, data = tbl_wm_bandits)
summary(fit_vg_rug_wm2, fit.measures = TRUE, standardized = TRUE)

tbl_latents <- as_tibble(predict(fit_vg_rug_wm2))
colnames(tbl_latents) <- c("G Directed", "G Value Guided", "WMC")
my_corr_plot(cor(tbl_latents), "", "", "Convergent Validity Exploration", "latent")

write_csv(tbl_latents, "data/behavioral-tasks-latents-s2.csv")
# plot results
lavaanPlot(
  fit_vg_rug2, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)






# specify the model

value_guided_model_2_sessions <- ' 
  g_v_1  =~ V_2Armed_1 + V_Horizon_1 + V_Restless_1
  g_wm_1 =~ WMU_recall_1 + OS_recall_1 + SS_recall_1

  
  g_v_2  =~ V_2Armed_2 + V_Horizon_2 + V_Restless_2
  g_wm_2 =~ WMU_recall_2 + OS_recall_2 + SS_recall_2

  
  OS_recall_1 ~~  OS_recall_2
  SS_recall_1 ~~  SS_recall_2
  WMU_recall_1 ~~ WMU_recall_2
  V_Horizon_1 ~~ V_Horizon_2
  V_Restless_1 ~~ V_Restless_2
  V_2Armed_1 ~~ V_2Armed_2
'

## todos
## correlation with questionnaires

# fit the model

fit_l <- cfa(value_guided_model_2_sessions, data = tbl_wm_bandits)
summary(fit_l, fit.measures = TRUE, standardized = TRUE)
resid(fit_l)

# plot results
lavaanPlot(
  fit_l, coefs = TRUE, covs = TRUE, sig = .05,
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)


# specify the model
value_guided_model_1_session <- ' 

  g_v  =~ V_2Armed_1 + V_Horizon_1 + V_Restless_1

  BIG_5_1 ~~ g_v
  #CEI_1 ~~ g_v
  #STICSAsoma_1 ~~ g_v
  #PHQ_9_1 ~~ g_v


'


fit_c <- cfa(value_guided_model_1_session, data = tbl_wm_bandits)
summary(fit_c, fit.measures = TRUE, standardized = TRUE)


directed_model_1_session <- ' 

  g_ru  =~ RU_2Armed_1 + RU_Horizon_1 + RU_Restless_1

  #BIG_5_1 ~~ g_ru
  #CEI_1 ~~ g_ru
  #STICSAsoma_1 ~~ g_ru
  PHQ_9_1 ~~ g_ru


'
fit_c <- cfa(directed_model_1_session, data = tbl_wm_bandits)
summary(fit_c, fit.measures = TRUE, standardized = TRUE)


# plot results
lavaanPlot(
  fit_c, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)

# specify the model
value_guided_wm_model_1_session <- ' 

  g_wm =~ WMU_recall_1 + OS_recall_1 + SS_recall_1
  g_v  =~ V_2Armed_1 + V_Horizon_1 + V_Restless_1

'


fit_c <- cfa(value_guided_wm_model_1_session, data = tbl_wm_bandits)
summary(fit_c, fit.measures = TRUE)

# plot results
lavaanPlot(
  fit_c, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)

tbl_bandits_switch <- tbl_wm_bandits %>% inner_join(tbl_all_three, by = "ID")

tbl_bandits_switch <- tbl_bandits_switch %>%
  mutate(
    p_switch_Horizon_1 = scale(p_switch_Horizon_1)[,1],
    p_switch_2Armed_1 = scale(p_switch_Sam_1)[,1],
    p_switch_Restless_1 = scale(p_switch_Restless_1)[,1]
  )
# specify the model
switch_model_1_session <- ' 


  g_switch_1  =~ p_switch_Horizon_1 + p_switch_Sam_1 + p_switch_Restless_1
  g_switch_2  =~ p_switch_Horizon_2 + p_switch_Sam_2 + p_switch_Restless_2
  
  p_switch_Restless_1 ~~ p_switch_Restless_2
  p_switch_Horizon_1 ~~ p_switch_Horizon_2
  p_switch_Sam_1 ~~ p_switch_Sam_2

  

'

fit_switch <- cfa(switch_model_1_session, data = tbl_bandits_switch)
summary(fit_switch, fit.measures = TRUE)

# plot results
lavaanPlot(
  fit_switch, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)



cor(tbl_wm_bandits_1)
cor(tbl_wm_bandits_2)



tbl_wm_wide %>% select(contains("processing")) %>%
  mutate(rwn = 1:nrow(.)) %>%
  pivot_longer(-rwn) %>%
  mutate(
    session = str_extract(name, "[1-2]$"),
    task = str_extract(name, "^[A-Z]*")) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(session ~ task)

# final plotting for MS

rm(list = ls())
library(grid)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(brms)
library(here)
library(patchwork)
theme_set(theme_bw(base_size = 14))

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

self_cor <- function(df){
  cors <- df %>% 
    select(-ID) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    as.data.frame() %>% 
    mutate(x = rownames(.)) %>% 
    pivot_longer(cols = -x, names_to = "y", values_to = "cor")
  
  return(cors)
  
}

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)


# Which Models? ---------------------------------------------------------

my_dir <- "figures/figures-ms/revision-1"
if (!dir.exists(my_dir)){dir.create(my_dir, recursive = TRUE)}


# how did we arrive at the used models?

## restless bd ------------------------------------------------------------

tbl_prep <- readRDS("data/restless-group-patterns-across-methods-data-individual.RDS")
tbl_prep$name <- factor(tbl_prep$parameter, labels = c("Directed", "Value-Guided"))
tbl_prep$Session <- factor(tbl_prep$Session, labels = c("Session 1", "Session 2"))
pl_restless_group_patterns <- ggplot(tbl_prep, aes(name, mn)) +
  geom_col(aes(fill = name)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin = mn - ci, ymax = mn + ci), width = .2) +
  facet_wrap(Session ~ method) +
  theme_bw() +
  scale_y_continuous(expand = c(0.1, 0)) +
  labs(x = "Parameter", y = "Group Mean", title = "a) UCB: Group-Level Patterns") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  scale_fill_brewer(palette = "Set2", name = "")

tbl_ucb_ml_hcb <- readRDS("data/restless-params-across-methods-data.RDS")
tbl_ucb_cor <- readRDS("data/restless-params-across-methods-cor-data.RDS")
tbl_ucb_ml_hcb$parameter <- factor(tbl_ucb_ml_hcb$parameter, labels = c("Directed", "Value-Guided"))
tbl_ucb_cor$parameter <- factor(tbl_ucb_cor$parameter, labels = c("Directed", "Value-Guided"))


pl_params_across_methods <- ggplot(tbl_ucb_ml_hcb, aes(`Max. Lik.`, `Hierarch. Bayes`)) +
  geom_abline() +
  geom_point(aes(color = parameter)) +
  geom_label(data = tbl_ucb_cor, aes(my_base + .5 * my_jitter, my_base - 2*my_jitter, label = str_c("r = ", round(value, 2)))) +
  facet_wrap(Session ~ parameter, scales = "free") +
  theme_bw() + labs(title = "b) UCB: Indiv. Param.") +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")

tbl_restless_reliability_across_methods <- readRDS("data/restless-reliabilities-across-methods.Rds")
tbl_restless_reliability_across_methods <- tbl_restless_reliability_across_methods %>%
  mutate(Reliability = round(Reliability, 2))
tbl_restless_reliability_across_methods$Parameter <- factor(tbl_restless_reliability_across_methods$Parameter, labels = c("Directed", "Value-Guided"))


table_rl_reliab <- tableGrob(tbl_restless_reliability_across_methods)

pl_table_rl_reliab <- ggplot() + annotation_custom(table_rl_reliab) + theme_bw() + 
  labs(title = "c) Reliability") +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )


# cor value-guided ucb hcb with sm ml
tbl_sm_compare <- readRDS("data/restless-params-smml-hcbucb-data.RDS")
tbl_cor <- readRDS("data/restless-params-smml-hcbucb-cor.RDS")
tbl_cor$Session <- factor(tbl_cor$Session, labels = c("Session 1", "Session 2"))
tbl_sm_compare$Session <- factor(tbl_sm_compare$Session, labels = c("Session 1", "Session 2"))

pl_v_smml_ucbhcb <- ggplot(tbl_sm_compare, aes(V_ml_sm, V_hcb)) +
  geom_abline() +
  geom_point(color = "#66C2A5") +
  geom_label(data = tbl_cor, aes(x = .35, y = .1, label = str_c("r = ", round(r, 2)))) +
  facet_wrap(~ Session) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Max. Lik. (Softmax)", y = "UCB: Hierarch. Bayes", title = "d) Value-Guided: Indiv. Param.") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )


pl_arrive_restless_model <- arrangeGrob(
  pl_restless_group_patterns, pl_params_across_methods, pl_table_rl_reliab, pl_v_smml_ucbhcb,
  nrow = 2, heights = c(.65, .35)
)


save_my_pdf_and_tiff_and_png(
  pl_arrive_restless_model,
  str_c(my_dir, "/arrive-restless-model"),
  12, 10
)


## horizon task ------------------------------------------------------

load("analysis/bandits/model_selection_horizon.Rda")

lims = c(min(c(params_t1$subject_level, params_t1$hierarchical)), max(c(params_t1$subject_level, params_t1$hierarchical)))

params <- params_t1 %>% 
  mutate(predictor = recode(predictor,"delta_mean" = "value-guided",
                            "info" = "directed"))

# Plot
sl <- ggplot(params, aes(subject_level, hierarchical, color = predictor)) + 
  geom_jitter(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  coord_cartesian(xlim = lims, ylim = lims) + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("cor = %.2f", cor)), hjust = "inward", vjust = "inward", color = "black") +
  facet_grid(cols = vars(predictor), rows = vars(horizon))+
  scale_color_manual(values = c("#66C2A5", "#FC8D62"))+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none") +
  labs(title = "Subject-level estimates",
       x = "subject-level implementation")
sl

fi <- fixed %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "value-guided",
                            "info" = "directed"),
         method = recode(method, "hb" = "hierarchical",
                         "sl" = "subject-level"))

fix <- ggplot(fi, aes(predictor, Estimate, fill = horizon)) + geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.9), width = 0.25)+
  facet_wrap(vars(method)) + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +
  theme(strip.background = element_rect(fill = "white"))+
  labs(title = "Group-level effects")

fix

cors <- params_all %>%
  group_by(method, predictor, horizon) %>%
  summarize(correlation = cor(session1, session2, use = "pairwise.complete.obs")) %>% 
  mutate(method = recode(method, "subject_level" = "subject-level"),
         predictor = recode(predictor, "delta_mean" = "value-guided",
                            "info" = "directed"),
         correlation = round(correlation, digits = 2))

print(cors)

selection <- ggarrange(fix, sl, ncol = 2, labels = "AUTO")
selection

save_my_pdf_and_tiff_and_png(
  selection,
  str_c(my_dir, "/horizon_model_selection"),
  14, 4
)

stargazer::stargazer(cors, summary = F, rownames = F)

print(log_liks)

logs <- log_liks %>% 
  mutate(method = recode(method, "hb" = "hierarchical",
                         "sl" = "subject-level"),
         Horizon = recode(Horizon, "5" = "short",
                          "10" = "long")) %>% 
  rename(`Log Likelihood`= log_lik)

stargazer::stargazer(logs, summary = F, rownames = F)



## two-armed bd -----------------------------------------------------------


## we just used the literature default here



# 1. Models as in the Literature ------------------------------------------


# 1.1. Recoverability -----------------------------------------------------

## Horizon task -----------------------

# takes > 1 min to run
horizon <- load_and_prep_bandit_data(session = 1)$horizon
res_list_short <- recovery_horizon(horizon[horizon$Horizon == -0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = T)
res_list_long <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = T)

true_short <- res_list_short[[1]]
recovered_short <- res_list_short[[2]]

true_long <- res_list_long[[1]]
recovered_long <- res_list_long[[2]]

# Function to prepare parameters
prepare_params <- function(trueParams1, trueParams2) {
  trueParams <- trueParams1 %>% 
    mutate(ID = parse_number(rownames(.))) %>% 
    left_join(trueParams2 %>% mutate(ID = parse_number(rownames(.))), by = c("ID", "predictor")) %>% 
    mutate(estimate = estimate.y - estimate.x)
  return(trueParams)
}

# Prepare true and recovered parameters
trueParams <- prepare_params(true_short, true_long)
recoveredParams <- prepare_params(recovered_short, recovered_long)

# Calculate correlations
params <- unique(trueParams$predictor)
cors <- expand.grid(true = params, recovered = params)
cors$cor <- mapply(function(t, r) cor(trueParams$estimate[trueParams$predictor == t], recoveredParams$estimate[recoveredParams$predictor == r]), cors$true, cors$recovered)

saveRDS(cors, file = "recovery-cors-horizon-s1.RDS")

cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("info", "delta_mean", "Intercept"),
                            labels = c("Directed", "Value-Guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "delta_mean", "info"),
                       labels = c("Intercept", "Value-Guided", "Directed"))) %>% 
  subset(recovered != "Intercept" & true != "Intercept")

p1 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Horizon task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p1

#ggsave("plots/submission1/recovery_horizon_default.png", p1)


## two-armed bd ------------------------------------
sam <- load_and_prep_bandit_data(session = 1)$sam
load("analysis/bandits/modellingResults/recoverySamSession1hybrid_hierarchical_notIterative.Rda")
saveRDS(cors, file = "recovery-cors-2ab-s1.RDS")

cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("VTU", "RU", "V", "Intercept"),
                            labels = c("Random","Directed", "Value-guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "V", "RU", "VTU"),
                       labels = c("Intercept", "Value-guided", "Directed", "Random"))) %>% 
  subset(recovered != "Intercept" & true != "Intercept")

p2 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Two-armed bandit",
       x = "Fitted parameters",
       y = "Recovered parameters")
p2

#ggsave("plots/submission1/recovery_2ab_default.png", p1)
## restless bd ------------------------------------------------------------


file_loc_hc_s1 <- "data/restless-hierarchical-model-posterior-s1.RDS"
file_loc_hc_s1_recovery <- "data/restless-hierarchical-model-recovery-posterior-s1.RDS"
tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
tbl_draws_hc_s1_recovery <- readRDS(file_loc_hc_s1_recovery)

l_recovery_s1 <- map_cor(tbl_draws_hc_s1, tbl_draws_hc_s1_recovery, c("tau", "beta"))
pl_heatmap_s1 <- recovery_heatmap(l_recovery_s1, "Restless bandit", c("Value-Guided", "Directed")) +
  labs(x = "Fitted parameters",
       y = "Recovered parameters")

pl_heatmap_s1 


recovery <- ggarrange(p1, p2, pl_heatmap_s1, widths = c(3.5,4,3), ncol = 3, labels = "AUTO",
                      common.legend = T, legend = "right")

recovery

save_my_pdf_and_tiff_and_png(
  recovery,
  str_c(my_dir, "/recovery_default"),
  14, 4
)

tbl_recovery_horizon <- readRDS("analysis/bandits/recovery-cors-horizon-s1.RDS") %>%
  mutate(
    task = "Horizon",
    true = as.character(factor(true, labels = c("Directed", "Value-Guided"))),
    recovered = as.character(factor(recovered, labels = c("Directed", "Value-Guided")))
  )
tbl_recovery_2ab <- readRDS("analysis/bandits/recovery-cors-2ab-s1.RDS") %>%
  mutate(task = "Two-Armed",
         true = as.character(factor(true, labels = c("Intercept", "Directed", "Value-Guided", "Random"))),
         recovered = as.character(factor(recovered, labels = c("Intercept", "Directed", "Value-Guided", "Random")))
  )
tbl_recovery_restless <- l_recovery_s1$cor_recovery %>%
  pivot_longer(colnames(.)) %>%
  mutate(
    true = str_extract(name, "^[a-z]*"),
    recovered = str_match(name, ".*_.*_([a-z]*)_")[, 2],
    true = factor(true, labels = c("Value-Guided", "Directed")),
    recovered = factor(recovered, labels = c("Value-Guided", "Directed")),
    task = "Restless"
  ) %>%
  select(-name) %>%
  rename(cor = value)

tbl_recovery_tasks <- rbind(tbl_recovery_horizon, tbl_recovery_2ab, tbl_recovery_restless)


# replicability (fixed effects) ----------------------------------------------------


fixed <- readRDS("analysis/bandits/allFixed.rds") %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "Value-guided",
                            "info" = "Directed",
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "VTU" = "Random"),
         task = recode(task, "sam" = "Two-armed", "horizon" = "Horizon", "restless" = "Restless"),
         session = recode(session, `1` = "Session 1", `2` = "Session 2"))  %>% 
  subset(!grepl("ntercept", predictor))


fixed$predictor <- factor(fixed$predictor, levels = c("Value-guided", "Directed", "Random"))
fixed$task <- factor(fixed$task, levels = c("Horizon", "Two-armed", "Restless"))

rep <- ggplot(fixed, aes(predictor, Estimate,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free_x")+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(title = "Group-level effects across tasks and sessions",
       x = element_blank(),
       y = "Parameter estimate ± 95% HDI")+
  geom_hline(yintercept = 0, linetype = "dotdash")

rep

save_my_pdf_and_tiff_and_png(rep, str_c(my_dir, "/replicability_default"), 
                             w = 6,
                             h = 5)

# 1.2. Reliability --------------------------------------------------------

tbl_reliability_bandits <- readRDS("analysis/bandits/reliabilities-hybrid.csv")
levels(tbl_reliability_bandits$parameter) = c("Intercept", "Value-Guided", "Directed", "Random", "Regret", "p(optimal)", "p(switch)")
levels(tbl_reliability_bandits$task) <- c("Horizon", "Two-Armed", "Restless")

tbl_reliability_bandits$property <- "Reliability"
tbl_recovery_tasks$property <- "Recovery"
tbl_ceiling <- tbl_recovery_tasks %>% filter(true == recovered)

tbl_rel_rec <- tbl_reliability_bandits %>% select(-property) %>%
  filter(parameter != "Intercept") %>% 
  mutate(parameter = as.character(parameter)) %>% 
  left_join(tbl_ceiling %>% select(-property), by = c("task" = "task", "parameter" = "true"), suffix = c("_rel", "")) %>%
  rename(reliability = value, recoverability = cor) %>%
  pivot_longer(cols = c("reliability", "recoverability")) %>%
  mutate(name_display = "]")


tbl_rel_rec$parameter <- factor(fct_inorder(tbl_rel_rec$parameter), ordered = TRUE)


pl_rel <- ggplot(tbl_rel_rec %>% filter(icc_type == "Consistency" & name == "reliability"), aes(value, fct_rev(parameter))) +
  geom_rect(aes(xmin = 0, xmax = .5, ymin = 0, ymax = 7), fill = "tomato3", alpha = .1) +
  geom_rect(aes(xmin = .5, xmax = .75, ymin = 0, ymax = 7), fill = "orange", alpha = .1) +
  geom_rect(aes(xmin = .75, xmax = .9, ymin = 0, ymax = 7), fill = "lightgreen", alpha = .1) +
  geom_rect(aes(xmin = .9, xmax = 1, ymin = 0, ymax = 7), fill = "darkgreen", alpha = .1) +
  geom_text(aes(label = str_extract(measure, "^[P,T]")), size = 1, color = "black") +
  geom_text(data = tbl_rel_rec %>% filter(name == "recoverability"), aes(value, fct_rev(parameter), label = name_display), size = 7, alpha = .3) +
  geom_point(aes(shape = measure), size = 3, color = "black", fill = "black") +
  theme_bw() +
  facet_wrap(~ task) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Test-Retest Reliability", x = "ICC3(C,1)", y = "") + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, .8, by = .2)) +
  scale_y_discrete(expand = c(0.025, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = .95, vjust = .95),
    text = element_text(size = 18)
  ) + 
  scale_shape_manual(values = c(16, 25), name = "")

pl_rel

save_my_pdf_and_tiff_and_png(
  pl_rel,
  str_c(my_dir, "/reliability-bandits-literature"),
  12, 5
)
pl_rel <- ggarrange(pl_rel, ncol =1, labels = c("A"))


tbl_reliability_bandits %>% filter(parameter != "Intercept")


# reliability wm tasks and questionnaires

tbl_rel_wm_q <- readRDS(file = "analysis/reliabilities-wm-qs.rds")

tbl_rel_wm_q %>% 
  pivot_longer(cols = starts_with("reliability")) %>%
  mutate(
    ICC = factor(name, labels = c("Agreement", "Consistency"))
  ) %>%
  select(-name) %>%
  rename(`Retest Reliability` = value)


# 1.3. Convergent Validity --------------------------------------------------

# load bandit data
horizon <- load_and_prep_bandit_data(1)$horizon
sam <- load_and_prep_bandit_data(1)$sam
restless <- load_and_prep_bandit_data(1)$restless

## task based

relabel <- function(df) {
  relabelled <- df %>% 
    mutate(x = factor(x, levels = c("horizon", "2AB", "restless"),
                      labels = c("Horizon", "Two-armed", "Restless")),
           y = factor(y, levels = c("restless", "2AB", "horizon"),
                      labels = c("Restless", "Two-armed", "Horizon")))
  
  return(relabelled)
}

### P(switch)

horizon$prev <- c(NA, horizon$chosen[1:nrow(horizon)-1])
horizon$switch <- ifelse(horizon$chosen == horizon$prev, 0, 1)
horizon$switch[horizon$trial == 1] <- NA

sam$prev <- c(NA, sam$chosen[1:nrow(sam)-1])
sam$switch <- ifelse(sam$chosen == sam$prev, 0, 1)
sam$switch[sam$trial == 1] <- NA

restless$prev <- c(NA, restless$chosen[1:nrow(restless)-1])
restless$switch <- ifelse(restless$chosen == restless$prev, 0, 1)
restless$switch[restless$trial == 1] <- NA

Hswitch <- plyr::ddply(horizon[horizon$trial > 4, ],~ID,summarise, Pswitch = meann(switch))
Sswitch <- plyr::ddply(sam, ~ID,summarise, Pswitch = meann(switch))
Rswitch <- plyr::ddply(restless, ~ID, summarise, Pswitch = meann(switch))

Pswitch <- list("horizon" = Hswitch, "2AB" = Sswitch, "restless" = Rswitch) %>% 
  bind_rows(.id = "model") %>% 
  pivot_wider(id_cols = ID, names_from = model, values_from = Pswitch) %>% 
  self_cor() %>% 
  relabel()

swi <- heatmap(Pswitch) +
  labs(title = "P(switch)",
       x = element_blank(),
       y = element_blank())+
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

swi

### WM
# here, we really just need the WM data from the first session, so use a different source than 4arlb-overviewAll.rds

tbl_include <- read_csv("data/exclusions1_noPID.csv") %>% 
  left_join(read_csv("data/exclusions2_noPID.csv"), by = "ID", suffix = c("_1", "_2")) %>%
  filter(exclude_1 == 0 & exclude_2 == 0) %>%
  select(ID)
wm <- read_csv("data/wm-performance-1.csv") %>% 
  rename(ID = participant_id) %>%
  inner_join(tbl_include, by = "ID") %>%
  select(c(ID, OS_recall_0, SS_recall_0, WMU_recall_0))
colnames(wm) <- str_remove_all(colnames(wm), "_0")
wm <- wm %>% self_cor() %>% 
  mutate(x = factor(x, levels = c("OS_recall", "SS_recall", "WMU_recall"),
                    labels = c("Oper. span", "Sym. span", "Updating")),
         y = factor(y, levels = c("WMU_recall", "SS_recall", "OS_recall"),
                    labels = c("Updating", "Sym. span", "Oper. span")))

# wm <- readRDS("analysis/4arlb-overviewAll.rds") %>% 
#   subset(session == 1, select = c(ID,OS_recall, SS_recall, WMU_recall)) %>% 
#   mutate(ID = as.numeric(as.character(ID))) %>%
#   inner_join(tbl_include, by = "ID") %>%
#   self_cor() %>% 
#   mutate(x = factor(x, levels = c("OS_recall", "SS_recall", "WMU_recall"),
#                     labels = c("Oper. span", "Sym. span", "Updating")),
#          y = factor(y, levels = c("WMU_recall", "SS_recall", "OS_recall"),
#                     labels = c("Updating", "Symmetry span", "Operation span")))

WM <- heatmap(wm) +
  labs(title = "Working Memory",
       x = element_blank(),
       y = element_blank())+
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

WM

combined_plots <- ggarrange(swi, WM, ncol = 1, labels = c("B", "C")) 
task_based <- combined_plots + plot_spacer() + plot_layout(heights = c(1, .075))
#task_based <- ggarrange(swi, WM, ncol = 1, nrow = 2, labels = c("B", "C"), align = "hv")
task_based

### model parameters


cors <- readRDS("analysis/bandits/allParams.Rds") %>% 
  subset(predictor != "Intercept" & session == 1) %>% 
  mutate(predictor = recode(predictor,
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "VTU" = "Random",
                            "delta_mean" = "Value-guided",
                            "info" = "Directed"),
         task = recode(task, "sam" = "Two-armed",
                       "horizon" = "Horizon",
                       "restless" = "Restless"),
         variable = paste(predictor, task)) %>% 
  pivot_wider(id_cols = "ID", names_from = "variable", values_from = "estimate") %>% 
  self_cor() %>% 
  mutate(x = factor(x, levels = c("Value-guided Horizon", "Value-guided Two-armed", "Value-guided Restless",
                                  "Directed Horizon", "Directed Two-armed", "Directed Restless",
                                  "Random Two-armed"),
                    labels = c("Value-guided Horizon", "Value-guided Two-armed", "Value-guided Restless",
                               "Directed Horizon", "Directed Two-armed", "Directed Restless",
                               "Random Two-armed")),
         y = factor(y, levels = c("Random Two-armed", "Directed Restless", "Directed Two-armed", "Directed Horizon",
                                  "Value-guided Restless", "Value-guided Two-armed", "Value-guided Horizon")))


param_based <- heatmap(cors) + 
  labs(title = "Parameter estimates",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_hline(yintercept = 1.5, color = "white", size = 3)+
  geom_hline(yintercept = 4.5, color = "white", size = 3)+
  geom_vline(xintercept = 6.5, color = "white", size = 3)+
  geom_vline(xintercept = 3.5, color = "white", size = 3)+
  theme(legend.position = "none")

param_based <- ggarrange(param_based, labels = c("D"))
converge <- ggarrange(task_based, param_based, ncol = 2, widths = c(1.65,3))
converge

save_my_pdf_and_tiff_and_png(converge,
                             str_c(my_dir, "/convergence_default"),
                             w = 12,
                             h = 5)

#ggsave("plots/submission1/convergent_validity_parameters.png")


# 1.5. Variable Correlations --------------------------------------------------


load("analysis/external_validity_cors_session2.Rda") 

cors$y <- row.names(cors)
cors <- cors %>% 
  pivot_longer(cols = -y, names_to = "x", values_to = "cor") %>% 
  mutate(x = recode(x,
                    "CEI" = "Exploration",
                    "BIG_5" = "Openness",
                    "STICSAcog" = "Cognitive anxiety",
                    "STICSAsoma" = "Somatic anxiety",
                    "PHQ_9" = "Depression",
                    "PANASneg" = "Negative mood",
                    "PANASpos" = "Positive mood",
                    "OS_recall_1" = "Operation span",
                    "SS_recall_1" = "Symmetry span",
                    "WMU_recall_1" = "Updating"
  ),
  y = recode(y,
             "restless_V" = "Value-guided restless",
             "restless_RU" = "Directed restless",
             "restless_Pswitch" = "P(switch) restless",
             "restless_Poptimal" = "P(optimal) restless",
             "horizon_Pswitch" = "P(switch) horizon",
             "horizon_Poptimal" = "P(optimal) horizon",
             "horizon_info" = "Directed horizon",
             "horizon_delta_mean" = "Value-guided horizon",
             "2AB_VTU" = "Random 2AB",
             "2AB_V" = "Value-guided 2AB",
             "2AB_RU" = "Directed 2AB",
             "2AB_Pswitch" = "P(switch) 2AB",
             "2AB_Poptimal" = "P(optimal) 2AB",
  ))


cors$y <- factor(cors$y, levels = c("Random 2AB", "Directed restless","Directed 2AB","Directed horizon", 
                                    "Value-guided restless", "Value-guided 2AB", "Value-guided horizon", 
                                    "P(switch) restless", "P(switch) 2AB","P(switch) horizon",
                                    "P(optimal) restless", "P(optimal) 2AB", "P(optimal) horizon"))

cors$x  <-  factor(cors$x, levels = c("Exploration", "Openness",
                                      "Cognitive anxiety", "Somatic anxiety", "Depression",
                                      "Negative mood", "Positive mood",
                                      "Operation span", "Symmetry span", "Updating"))

ext_validty <- heatmap(cors) + labs(title = "Correlation of task measures and questionnaire scores",
                                    x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ext_validty

save_my_pdf_and_tiff_and_png(ext_validty,
                             str_c(my_dir, "/external_validity_default"),
                             h= 6,
                             w = 12)

ext_validty <- ext_validty +theme(legend.position = "bottom")
ext_validty <- ggarrange(ext_validty, ncol = 1, labels = c("E"))
ext_validty_centered <- plot_spacer() + ext_validty + plot_spacer() + plot_layout(widths = c(1, 4, 1))

rel_conv_default <- ggarrange(pl_rel, converge, ext_validty_centered, ncol = 1, nrow = 3,
                              heights = c(1,2,3))
rel_conv_default

save_my_pdf_and_tiff_and_png(rel_conv_default,
                             str_c(my_dir, "/reliability_convergence_default"),
                             h = 20,
                             w = 10.5)

########## P(switch) over trials #############

horizon <- load_and_prep_bandit_data(1)$horizon
sam <- load_and_prep_bandit_data(1)$sam
restless <- load_and_prep_bandit_data(1)$restless

h <- horizon %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1),
         Condition = ifelse(info == 0, "equal", "unequal")) %>% 
  subset(trial >4) %>% 
  mutate(group_mean = meann(switch)) %>% 
  group_by(ID) %>% 
  mutate(subject_mean = meann(switch)) %>% 
  group_by(ID, trial, Condition) %>% 
  summarise(Pswitch = meann(switch) - mean(subject_mean) + mean(group_mean)) %>% # adjust subject means for group means so the error bars are within subject
  group_by(Condition, trial) %>% 
  summarise(pswitch = meann(Pswitch),
            se = se(Pswitch))

s <- sam %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1),
         Condition = factor(cond, levels = c("FF", "FS", "SF", "SS"),
                            labels = c("both fluctating",
                                       "fluctuating-stable",
                                       "stable-fluctuating",
                                       "both stable")),
         group_mean = meann(switch)) %>% 
  group_by(ID) %>% 
  mutate(subject_mean = meann(switch)) %>% 
  group_by(ID, trial, Condition) %>% 
  summarise(Pswitch = meann(switch) - mean(subject_mean) + mean(group_mean)) %>% # adjust subject means for group means so the error bars are within subject
  group_by(Condition, trial) %>% 
  summarise(pswitch = meann(Pswitch),
            se = se(Pswitch))

r <- restless %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1)) %>% 
  group_by(trial) %>% 
  summarise(pswitch = meann(switch))


ho <- ggplot(h, aes(trial, pswitch, color = Condition)) +
  geom_point()+
  geom_line()+
  geom_linerange(aes(ymin = pswitch -1.96*se, ymax = pswitch + 1.96*se))+
  scale_color_manual(values = colors[1:2])+
  labs(title = "Horizon task",
       y = "P(switch) ± 95%CI")+
  theme(legend.position = c(1,1),
        legend.justification = c(1.01,1.01))+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))

ho

sa <- ggplot(s, aes(trial, pswitch, color = Condition)) +
  geom_point()+
  geom_line()+
  geom_linerange(aes(ymin = pswitch -1.96*se, ymax = pswitch + 1.96*se))+
  scale_color_manual(values = colors[c(1,2,4,5)])+
  labs(title = "Two-armed bandit",
       y = "P(switch) ± 95%CI")+
  theme(legend.position = c(1,1),
        legend.justification = c(1.01,1.01))+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))

sa

re <- ggplot(r, aes(trial, pswitch)) +
  geom_point()+
  geom_line()+
  scale_color_manual(values = colors[c(1,2,4,5)])+
  labs(title = "Restless bandit",
       y = "P(switch)")+
  scale_x_continuous(n.breaks = 21)+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))+
  coord_cartesian(xlim = c(1,200))

re

ggpubr::ggarrange(ho, sa, re, ncol = 3, widths = c(6,10, 20))






# 2. Improved Models ------------------------------------------------------

# 2.1 Recoverability -------------------

## Horizon task -----------------------
horizon <- load_and_prep_bandit_data(session = 1)$horizon
res_list <- recovery_horizon(horizon[horizon$Horizon == 0.5, ], "Wilson", full = T, it = 8000, save = T, bayesian = T, no_horizon = T, no_intercept = F, use_saved = T)

trueParams <- res_list[[1]]
recoveredParams <- res_list[[2]]

# Calculate correlations
params <- unique(trueParams$predictor)
cors <- expand.grid(true = params, recovered = params)
cors$cor <- mapply(function(t, r) cor(trueParams$estimate[trueParams$predictor == t], recoveredParams$estimate[recoveredParams$predictor == r]), cors$true, cors$recovered)

cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("info", "delta_mean", "Intercept"),
                            labels = c("Directed", "Value-Guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "delta_mean", "info"),
                       labels = c("Intercept", "Value-Guided", "Directed"))) %>% 
  subset(recovered != "Intercept" & true != "Intercept")

p1 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Horizon task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p1
cors_horizon <- cors %>% mutate(task = "Horizon")

#ggsave("plots/submission1/recovery_horizon_only_long.png", p1)


## two-armed bd ------------------------------------
sam <- load_and_prep_bandit_data(session = 1)$sam
load("analysis/bandits/modellingResults/recoverySamSession1UCB_hierarchical_notIterative.Rda")
cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("RU", "V", "Intercept"),
                            labels = c("Directed", "Value-Guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "V", "RU"),
                       labels = c("Intercept", "Value-Guided", "Directed"))) %>% 
  subset(recovered != "Intercept" & true != "Intercept")

p2 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Two-armed bandit task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p2
cors_sam <- cors %>% mutate(task = "Two-Armed")
#ggsave("plots/submission1/recovery_2ab_UCB.png", p1)

recovery <- ggarrange(p1, p2, ncol = 2, labels = "AUTO", common.legend = T, legend = "right")

recovery

save_my_pdf_and_tiff_and_png(recovery,
                             str_c(my_dir, "/recovery_improved"),
                             9,4)

tbl_recovery_tasks_improved <- rbind(cors_horizon, cors_sam) %>% filter(recovered == true)

# Replicability (fixed effects) ------------------------------------------

fixed <- readRDS("analysis/bandits/allFixed_improved.rds") %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "Value-guided",
                            "info" = "Directed",
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "VTU" = "Random"),
         task = recode(task, "sam" = "Two-armed", "horizon" = "Horizon", "restless" = "Restless"),
         session = recode(session, `1` = "Session 1", `2` = "Session 2"))  %>% 
  subset(!grepl("ntercept", predictor) & task != "Restless")


fixed$predictor <- factor(fixed$predictor, levels = c("Value-guided", "Directed", "Random"))
fixed$task <- factor(fixed$task, levels = c("Horizon", "Two-armed"))

rep <- ggplot(fixed, aes(predictor, Estimate,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(title = "Group-level effects across tasks and sessions",
       x = element_blank(),
       y = "Parameter estimate ± 95% CI")+
  geom_hline(yintercept = 0, linetype = "dotdash")

rep

save_my_pdf_and_tiff_and_png(rep, str_c(my_dir, "/replicability_improved"), 
                             w = 6,
                             h = 5)

recov_rep <- ggarrange(p1, p2, rep, ncol = 3,widths = c(0.3, 0.3, 0.4), labels = "AUTO", common.legend = T, legend = "bottom")

recov_rep

save_my_pdf_and_tiff_and_png(recov_rep,
                             str_c(my_dir, "/replicability_recovery_improved"),
                             w = 14,
                             h = 4)

# 2.2. Reliability --------------------------------------------------------

tbl_reliability_bandits <- readRDS("analysis/bandits/reliabilities-ucb.csv")
levels(tbl_reliability_bandits$parameter) = c("Intercept", "Value-Guided", "Directed", "Regret", "p(optimal)", "p(switch)")
levels(tbl_reliability_bandits$task) <- c("Horizon", "Two-Armed", "Restless")

tbl_rel_rec_improved <- tbl_reliability_bandits %>% 
  filter(parameter != "Intercept" & task != "Restless" & measure == "Parameter") %>% 
  mutate(parameter = as.character(parameter)) %>% 
  left_join(tbl_recovery_tasks_improved, by = c("task" = "task", "parameter" = "true")) %>%
  rename(reliability = value, recoverability = cor) %>%
  pivot_longer(cols = c("reliability", "recoverability")) %>%
  mutate(name_display = "]")


tbl_rel_rec_improved$parameter <- factor(fct_inorder(tbl_rel_rec_improved$parameter), ordered = TRUE)

tmp <- tbl_reliability_bandits %>% filter(parameter != "Intercept" & task != "Restless" & measure == "Parameter") %>% arrange(task, icc_type, parameter)
print(tmp, n = 30)


pl_rel <- ggplot(tbl_reliability_bandits %>% filter(parameter != "Intercept" & measure != "Task Measure" & task != "Restless" & icc_type == "Consistency"), aes(value, fct_rev(parameter))) +
  geom_rect(aes(xmin = 0, xmax = .5, ymin = 0, ymax = 2.5), fill = "tomato3", alpha = .2) +
  geom_rect(aes(xmin = .5, xmax = .75, ymin = 0, ymax = 2.5), fill = "orange", alpha = .2) +
  geom_rect(aes(xmin = .75, xmax = .9, ymin = 0, ymax = 2.5), fill = "lightgreen", alpha = .2) +
  geom_rect(aes(xmin = .9, xmax = 1, ymin = 0, ymax = 2.5), fill = "darkgreen", alpha = .2) +
  geom_text(aes(label = str_extract(measure, "^[P,T]")), size = 1, color = "black") +
  geom_text(data = tbl_rel_rec_improved %>% filter(name == "recoverability"), aes(value, fct_rev(parameter), label = name_display), size = 7, alpha = .3) +
  geom_point(aes(shape = measure), size = 3, color = "black", fill = "black") +
  facet_wrap(~ task) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Test-Retest Reliability", x = "ICC3(C,1)", y = "") + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, .8, by = .2)) +
  scale_y_discrete(expand = c(0.025, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = .95, vjust = .95)
  ) + 
  scale_shape_manual(values = c(16, 3), name = "")

pl_rel

save_my_pdf_and_tiff_and_png(
  pl_rel,
  str_c(my_dir, "/reliability-bandits-ucb"),
  10,4
)

# 2.3 Convergent validity ------------------------------------------------

cors <- readRDS("analysis/bandits/allParams_improved.Rds") %>% 
  subset(predictor != "Intercept" & session == 1) %>% 
  mutate(predictor = recode(predictor,
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "delta_mean" = "Value-guided",
                            "info" = "Directed"),
         task = recode(task, "sam" = "Two-armed",
                       "horizon" = "Horizon",
                       "restless" = "Restless"),
         variable = paste(predictor, task)) %>% 
  pivot_wider(id_cols = "ID", names_from = "variable", values_from = "estimate") %>% 
  self_cor() %>% 
  mutate(x = factor(x, levels = c("Value-guided Horizon", "Value-guided Two-armed", "Value-guided Restless",
                                  "Directed Horizon", "Directed Two-armed", "Directed Restless"),
                    labels = c("Value-guided Horizon", "Value-guided Two-armed", "Value-guided Restless",
                               "Directed Horizon", "Directed Two-armed", "Directed Restless")),
         y = factor(y, levels = c("Directed Restless", "Directed Two-armed", "Directed Horizon",
                                  "Value-guided Restless", "Value-guided Two-armed", "Value-guided Horizon")))


converge <- heatmap(cors) + 
  labs(title = "Convergent validity of parameter estimates",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_hline(yintercept = 3.5, color = "white", size = 3)+
  geom_vline(xintercept = 3.5, color = "white", size = 3)

converge

# 2.4 Variable correlations (latent) -----------------------


cors <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(read.csv("analysis/CFA_compound_questionnaire_factors_s2.csv") %>% select(-X), by = "ID") %>% 
  self_cor() %>% 
  mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  "Exp", "posMood", "negMood", "AxDep"),
                    labels = c("Value-guided", "Directed","Working memory", "Self-reported exploration","Positive mood", "Negative mood", "Anxiety/depression")),
         y = factor(y, levels = c("AxDep", "negMood","posMood", "Exp","WMC", "G.Directed", "G.Value.Guided"),
                    labels = c("Anxiety/depression","Negative mood","Positive", "Self-reported exploration","Working memory", "Directed", "Value-guided")))
# mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  "MR1", "MR2", "MR3", "MR4", "MR5", "MR6"),
#                   labels = c("Value-guided", "Directed","Working memory","cog. anxiety/depression","pos. affect", 
#                              "Self-reported exploration", "soma. anxiety 1", "soma. anxiety 2", "unknown")),
#        y = factor(y, levels = c("MR6","MR5","MR4", "MR3","MR2", "MR1","WMC", "G.Directed", "G.Value.Guided"),
#                   labels = c("unknown","soma.anxiety 2","soma. anxiety 1", "Self-reported exploration", "pos. affect",
#                              "cog. anxiety/depression" ,"Working memory", "Directed", "Value-guided")))
# 
# mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  "MR1", "MR2"),
#                   labels = c("Value-guided", "Directed","Working memory", "Self-reported exploration", "Anxiety/depression")),
#        y = factor(y, levels = c("MR2", "MR1","WMC", "G.Directed", "G.Value.Guided"),
#                   labels = c("Anxiety/depression", "Self-reported exploration","Working memory", "Directed", "Value-guided")))


latent <- heatmap(cors) + 
  labs(title = "External validity on the latent level",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_hline(yintercept = 4.5, color = "white", size = 3)+
  geom_vline(xintercept = 3.5, color = "white", size = 3)

latent

conv2 <- ggarrange(converge, latent, ncol = 2, labels = c("B", "C"), common.legend = T,
                   legend = "bottom")


conv2

save_my_pdf_and_tiff_and_png(conv2,
                             str_c(my_dir, "/convergence_criterion_improved"),
                             w = 14,
                             h = 5)


######### do we suddenly have links between behaviour and questionnaires if we account for WM?

df <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(read.csv("analysis/CFA_compound_questionnaire_factors_s2.csv"), by = "ID")

colnames(df)
brm(G.Directed ~ WMC * Exp,# 1 datapoint per participant so no random intercepts or slopes
    df,
    cores = 2,
    chains = 2, 
    iter = 4000, 
    control = list(adapt_delta = 0.9))

brm(G.Value.Guided ~ WMC * Exp,# 1 datapoint per participant so no random intercepts or slopes
    df,
    cores = 2,
    chains = 2, 
    iter = 4000, 
    control = list(adapt_delta = 0.9))

m1 <- brm(G.Value.Guided ~ WMC * AxDep, # 1 datapoint per participant so no random intercepts or slopes
          df,
          cores = 2,
          chains = 2, 
          iter = 4000, 
          control = list(adapt_delta = 0.9))
m1

# get bayes factor for main effect of axdep
m2 <- brm(G.Value.Guided ~ WMC : AxDep + WMC, # 1 datapoint per participant so no random intercepts or slopes
          df,
          cores = 2,
          chains = 2, 
          iter = 4000, 
          control = list(adapt_delta = 0.9))

bayes_factor(m1, m2)

brm(G.Directed ~ WMC * AxDep, # 1 datapoint per participant so no random intercepts or slopes
    df,
    cores = 2,
    chains = 2, 
    iter = 4000, 
    control = list(adapt_delta = 0.9))

# Supplementary Information -----------------------------------------------

pl_rel <- ggarrange(pl_rel, ncol = 1, labels = c("A"))
rel_conv_improved <- ggarrange(pl_rel, conv2, ncol = 1, nrow = 2, heights = c(1.5,3))

rel_conv_improved

save_my_pdf_and_tiff_and_png(rel_conv_improved,
                             str_c(my_dir, "/reliability_convergence_improved"),
                             h = 8,
                             w = 13)


## variable correlations latent level vs compound scores #########

allOfIt <- read.csv("analysis/questionnaireScores.csv") %>% 
  subset(session == 2, -c(session)) %>% 
  pivot_wider(id_cols = ID, names_from = measure, values_from = score) %>% 
  mutate(BIG_5 = as.numeric(BIG_5),
         CEI = as.numeric(CEI))

qs <- c("BIG_5","CEI","PANASneg", "PANASpos", "PHQ_9","STICSAcog", "STICSAsoma")

cors <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(allOfIt, by = "ID") %>% 
  self_cor() 

cors <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(allOfIt %>% subset(is.element(measure, qs)) %>% pivot_wider(id_cols = ID, names_from = measure, values_from = value), by = "ID") %>% 
  self_cor() %>% 
  mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  qs),
                    labels = c("Value-guided", "Directed","Working memory", "Openness", "Exploration",
                               "Negative mood", "Positive mood", "Depression", "cog. anxiety", "soma. anxiety")),
         y = factor(y, levels = c(qs[length(qs):1],"WMC", "G.Directed", "G.Value.Guided"),
                    labels = c("soma. anxiety","cog. anxiety","Depression","Positive mood","Negative mood", 
                               "Exploration", "Openness","Working memory", "Directed", "Value-guided")))
# mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  "MR1", "MR2", "MR3", "MR4", "MR5", "MR6"),
#                   labels = c("Value-guided", "Directed","Working memory","cog. anxiety/depression","pos. affect", 
#                              "Self-reported exploration", "soma. anxiety 1", "soma. anxiety 2", "unknown")),
#        y = factor(y, levels = c("MR6","MR5","MR4", "MR3","MR2", "MR1","WMC", "G.Directed", "G.Value.Guided"),
#                   labels = c("unknown","soma.anxiety 2","soma. anxiety 1", "Self-reported exploration", "pos. affect",
#                              "cog. anxiety/depression" ,"Working memory", "Directed", "Value-guided")))
# 
# mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC",  "MR1", "MR2"),
#                   labels = c("Value-guided", "Directed","Working memory", "Self-reported exploration", "Anxiety/depression")),
#        y = factor(y, levels = c("MR2", "MR1","WMC", "G.Directed", "G.Value.Guided"),
#                   labels = c("Anxiety/depression", "Self-reported exploration","Working memory", "Directed", "Value-guided")))


latent <- heatmap(cors) + 
  labs(title = "External validity on the latent level",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))#+
#geom_hline(yintercept = 2.5, color = "white", size = 3)+
#geom_vline(xintercept = 3.5, color = "white", size = 3)

latent


## Correlations between Indendent Variables -------------------------------

# the following three plots display correlations between the independent variables in the three bandit tasks (i.e., V, VTU, and RU)
tbl_cor_trial_2ab <- readRDS("analysis/bandits/var-cors-2armed.rds")
tbl_cor_avg_2ab <- readRDS("analysis/bandits/var-cors-avg-2armed.rds")
pl_2ab <- ggplot(tbl_cor_trial_2ab, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_2ab, aes(6, .15 * as.numeric(as.factor(name)), label = str_c("avg. r = ", round(value, 2)), color = as.factor(name))) +
  facet_wrap(~cond) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = 3:3:9) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Pearson Correlation", title = "a) Two-Armed Bandit") +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2", name = "")

tbl_cor_trial_horizon1 <- readRDS("analysis/bandits/var-cors-horizon.rds")
tbl_cor_avg_horizon1 <- readRDS("analysis/bandits/var-cors-avg-horizon.rds")

#  %>% filter(!str_detect(name, "VTU"))
pl_horizon <- ggplot(tbl_cor_trial_horizon1 %>% filter(!str_detect(name, "VTU")), aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_horizon1 %>% filter(!str_detect(name, "VTU")), aes(
    7.5, -.2,
    label = str_c("avg. r = ", round(value, 2)), color = as.factor(name)
  )) +
  facet_wrap(~Horizon) +
  theme_bw() +
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Correlation", title = c("b) Horizon Task")) +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2", name = "")

tbl_restless_cor <- readRDS("analysis/bandits/var-cors-restless-constrain.rds")

tbl_restless_cor_avg <- tbl_restless_cor %>% 
  mutate(in_gr = str_extract(var_in, "^[VTU]+"), out_gr = str_extract(var_out, "^[VTUR]+")) %>%
  group_by(in_gr, out_gr) %>%
  summarize(value = mean(value)) %>% ungroup()

pl_restless <- ggplot(tbl_restless_cor, aes(var_in, value)) +
  geom_hline(yintercept = 0, linetype = "dotdash", linewidth = 1) +
  geom_col(
    aes(fill = str_c(str_extract(var_in_out, "^[R,V,T,U]+"), "-", "1-4 ")), 
    position = position_dodge2(width = .9, preserve = "single")
  ) +
  geom_label(
    aes(y = ifelse(value > 0, value - .1, value + .1), label = round(value, 2), alpha = var_in_out), 
    position = position_dodge2(width = .9, preserve = "single")
  ) +
  theme_bw() +
  scale_x_discrete(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_alpha_manual(values = c(1, 1, 1), guide = "none") +
  scale_fill_brewer(palette = "Set2", name = "Variable Out") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Variable In", y = "Correlation", title = "c) Restless Bandit") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )

pl_strat_cor <- arrangeGrob(pl_2ab, pl_horizon, ncol = 1, heights = c(1, .6))
grid.draw(pl_strat_cor)

save_my_pdf_and_tiff_and_png(pl_strat_cor, str_c(my_dir, "/strategies-correlations"), 8, 12)


## 2AB Choice Position Analysis ------------------------------------------

tbl_plot <- readRDS("data/2ab-choice-position.RDS")

tbl_plot$parameter <- factor(tbl_plot$parameter, labels = c("Intercept", "Directed", "Value-Guided"))

pl_choice_position <- ggplot(tbl_plot %>% filter(parameter != "Intercept"), aes(position, random, group = parameter)) +
  geom_vline(xintercept = 5, color = "grey60", linetype = "dotdash", linewidth = 1) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dotdash", linewidth = 1) +
  geom_line(aes(color = parameter)) +
  geom_point(size = 5, color = "white") +
  geom_point(aes(color = parameter)) +
  geom_point(data = tbl_plot %>% filter(parameter != "Intercept" & position == 5), size = 5, shape = 1) +
  geom_label(data = tbl_plot %>% filter(parameter == "Directed" & position == 5), aes(y = random + 1.475, label = "Horizon\nTask")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 9, by = 1), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial", y = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")

grid.draw(pl_choice_position)
save_my_pdf_and_tiff_and_png(pl_choice_position, str_c(my_dir, "/2ab-choice-position"), 6, 4)



## Test-retest reliability of questionnaire items ----------------

qdat1 <- read.csv("data/trial_level_questionnaire_session1.csv") # created in prep_questionnaire_data.R

qdat2 <- read.csv("data/trial_level_questionnaire_session2.csv")

colnames(qdat2)
qdat1 <- qdat1 %>% 
  rename("item" = Q,
         "session1" = response)
qdat2 <- qdat2%>% 
  rename("item" = Q,
         "session2" = response)

qdat <- left_join(qdat1, qdat2 %>% select(ID, item, session2), by = c("ID", "item"))

compound_rel <- qdat %>% 
  group_by(Measure, ID) %>% 
  summarise(score1 = mean(session1),
            score2 = mean(session2))  %>% 
  ungroup() %>% 
  group_by(Measure) %>% 
  summarise(compound_cor = cor(as.numeric(score1), as.numeric(score2), use = "pairwise.complete.obs"))

cors <- qdat %>% 
  #subset(!is.element(item, check_items)) %>% # take out attention check items
  group_by(item, Measure) %>% 
  summarise(cor = cor(as.numeric(session1), as.numeric(session2), use = "pairwise.complete.obs")) %>% 
  #left_join(descr, by = "item") %>% 
  left_join(compound_rel, by = "Measure")


ggplot(cors, aes(cor)) + geom_histogram()+
  geom_vline(aes(xintercept = compound_cor), linewidth = 2, color = "#FC8D62")+
  facet_wrap(vars(Measure)) 


# 2.5 Optimality Analysis -------------------------------------------------

tbl_results <- readRDS("analysis/bandits/optimal-strategy-restless.RDS")
## Restless Bandit
pl_optimal_strategy_2d <- grouped_agg(tbl_results, c(gamma, beta, stim_set), value) %>%
  group_by(stim_set) %>%
  mutate(mean_value_prop = mean_value/max(mean_value)) %>%
  ggplot(aes(gamma, beta)) +
  geom_tile(aes(fill = mean_value_prop)) +
  geom_text(aes(label = round(mean_value_prop, 3)), color = "white") + 
  facet_wrap(~ stim_set) +
  scale_fill_viridis_c(guide = "none") +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = "Value-Guided", y = "Directed") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  )



pd <- position_dodge(width = .15)
tbl_results_agg <- grouped_agg(tbl_results, c(gamma, stim_set), value) %>%
  pivot_longer(gamma) %>%
  rbind(
    grouped_agg(tbl_results, c(beta, stim_set), value) %>%
      pivot_longer(beta)
  ) %>% ungroup()
tbl_results_agg$value <- as.factor(as.numeric(as.character(tbl_results_agg$value)))
tbl_results_agg$name <- factor(tbl_results_agg$name, labels = c("Directed", "Value-Guided"))



file_loc_hc_s1 <- "data/restless-hierarchical-model-posterior-s1.RDS"
tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)

ids_sample <- tibble(
  ID = unique(tbl_restless$ID),
  id_stan = 1:length(unique(tbl_restless$ID))
)

tbl_posterior <- tbl_draws_hc_s1 %>% 
  dplyr::select(starts_with("beta") | starts_with("tau"), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with("beta") | starts_with("tau"), names_to = "parameter", values_to = "value") %>%
  mutate(
    id_stan = as.integer(str_extract(parameter, "[0-9]+")),
    parameter = str_extract(parameter, "^[a-z]+")
  )
tbl_posterior_agg <- tbl_posterior %>%
  group_by(parameter, id_stan) %>% 
  summarize(val_mn = mean(value)) %>% ungroup() %>%
  mutate(
    name = factor(parameter, labels = c("Directed", "Value-Guided"))
  )
tbl_ci <- summary_se(tbl_posterior_agg, "val_mn", "name")


y_lower <- 9500
y_upper <- 12500
tbl_results_agg$value <- as.numeric(as.character(tbl_results_agg$value))

pl_optimal_strategy_1d <- ggplot() +
  geom_segment(data = tbl_ci, aes(y = y_lower + 500, x = val_mn - ci, xend = val_mn + ci), lineend = "round", linewidth = 2, alpha = .5) +
  geom_point(data = tbl_ci, aes(x = val_mn, y = y_lower + 500), size = 4, color = "white") +
  geom_point(data = tbl_ci, aes(x = val_mn, y = y_lower + 500), size = 3, alpha = 1) +
  geom_line(data = tbl_results_agg , aes(value, mean_value, color = stim_set), linewidth = 1) +
  geom_errorbar(data = tbl_results_agg , aes(
    value, mean_value, color = stim_set,
    ymin = mean_value - 1.96 * se_value, 
    ymax = mean_value + 1.96 * se_value
  ), width = .15, linewidth = 1) +
  geom_point(data = tbl_results_agg , aes(value, mean_value), color = "white", size = 3) +
  geom_point(data = tbl_results_agg , aes(value, mean_value, color = stim_set)) +
  facet_wrap(~ name, scales = "free_x", labeller = label_parsed) +
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.01, 0), labels = scales::comma) +
  labs(x = "Parameter Value", y = "Cum. Rewards") + 
  theme(
    text = element_text(size = 22),
    axis.text.y = element_text(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white")
  ) + 
  scale_color_brewer(palette = "Set2", name = "") +
  coord_cartesian(ylim = c(y_lower, y_upper))
  

pl_optimal_stratagy_1d_2d <- arrangeGrob(pl_optimal_strategy_2d, pl_optimal_strategy_1d, nrow = 2)
save_my_pdf_and_tiff_and_png(
  pl_optimal_stratagy_1d_2d,
  str_c(my_dir, "/4arlb-optimal-strategy"), 16, 10
)


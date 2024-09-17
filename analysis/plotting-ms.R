# final plotting for MS

rm(list = ls())
library(grid)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(brms)
library(here)
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

my_dir <- "figures/figures-ms/submission-1"
if (!dir.exists(my_dir)){dir.create(my_dir, recursive = TRUE)}


# how did we arrive at the used models?

## restless bd ------------------------------------------------------------

tbl_prep <- readRDS("data/restless-group-patterns-across-methods-data-individual.RDS")
tbl_prep$name <- factor(tbl_prep$parameter, labels = c("Directed", "Value-Guided"))
pl_restless_group_patterns <- ggplot(tbl_prep, aes(name, mn)) +
  geom_col(aes(fill = name)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin = mn - ci, ymax = mn + ci), width = .2) +
  facet_wrap(Session ~ method) +
  theme_bw() +
  scale_y_continuous(expand = c(0.1, 0)) +
  labs(x = "Parameter", y = "Group Mean") + 
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
  theme_bw() +
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

pl_table_rl_reliab <- ggplot() + annotation_custom(table_rl_reliab)
pl_arrive_restless_model <- arrangeGrob(
  pl_restless_group_patterns, pl_params_across_methods, pl_table_rl_reliab,
  nrow = 1
)


save_my_pdf_and_tiff_and_png(
  pl_arrive_restless_model,
  str_c(my_dir, "/arrive-restless-model"),
  20, 7
)


## horizon task ------------------------------------------------------

load("analysis/bandits/model_selection_horizon.Rda")

lims = c(min(c(params_t1$subject_level, params_t1$hierarchical)), max(c(params_t1$subject_level, params_t1$hierarchical)))

# Plot
ggplot(params_t1, aes(subject_level, hierarchical, color = predictor)) + 
  geom_jitter(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  coord_cartesian(xlim = lims, ylim = lims) + 
  geom_label(aes(x = Inf, y = -Inf, label = sprintf("cor = %.2f", cor)), hjust = "inward", vjust = "inward", color = "black") +
  facet_grid(cols = vars(predictor), rows = vars(horizon))+
  scale_color_manual(values = c("#66C2A5", "#FC8D62"))+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "none") 


ggplot(fixed, aes(predictor, Estimate, fill = horizon)) + geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.9), width = 0.25)+
  facet_wrap(vars(method)) + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +
  theme(strip.background = element_rect(fill = "white"))

cors <- params_all %>%
  group_by(method, predictor, horizon) %>%
  summarize(correlation = cor(session1, session2, use = "pairwise.complete.obs"))

print(cors)
print(log_liks)


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

cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("info", "delta_mean", "Intercept"),
                            labels = c("Directed", "Value-guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "delta_mean", "info"),
                       labels = c("Intercept", "Value-guided", "Directed")))

p1 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Horizon task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p1

#ggsave("plots/submission1/recovery_horizon_default.png", p1)


## two-armed bd ------------------------------------
sam <- load_and_prep_bandit_data(session = 1)$sam
load("analysis/bandits/modellingResults/recoverySamSession1hybrid_hierarchical_notIterative.Rda")
cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("VTU", "RU", "V", "Intercept"),
                            labels = c("Random","Directed", "Value-guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "V", "RU", "VTU"),
                       labels = c("Intercept", "Value-guided", "Directed", "Random")))

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


## for aggregating across models we first need to adjust the models intercept wise: Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson’s method. Tutorials in Quantitative Methods for Psychology, 1(1), 42–45. https://doi.org/10.20982/tqmp.01.1.p042
# that's why the variables of interest are called estimate_corrected etc
fixed$predictor <- factor(fixed$predictor, levels = c("Value-guided", "Directed", "Random"))
fixed$task <- factor(fixed$task, levels = c("Horizon", "Two-armed", "Restless"))

rep <- ggplot(fixed, aes(predictor, estimate_corrected,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = l_corrected, ymax = u_corrected), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(title = "Fixed effects across tasks and sessions",
       x = element_blank(),
       y = "Parameter estimate ± 95% CI")+
  geom_hline(yintercept = 0, linetype = "dotdash")

rep

save_my_pdf_and_tiff_and_png(rep, str_c(my_dir, "/replicability_default"), 
                             w = 10,
                             h = 5)

# 1.2. Reliability --------------------------------------------------------

tbl_reliability_bandits <- readRDS("analysis/bandits/reliabilities-hybrid.csv")
levels(tbl_reliability_bandits$parameter) = c("Intercept", "Value-Guided", "Directed", "Random", "Regret", "p(optimal)", "p(switch)")
levels(tbl_reliability_bandits$task) <- c("Horizon", "Two-Armed", "Restless")


pl_rel <- ggplot(tbl_reliability_bandits %>% filter(parameter != "Interept"), aes(value, fct_rev(parameter))) +
  geom_rect(aes(xmin = 0, xmax = .4, ymin = 0, ymax = 8), fill = "tomato3", alpha = .1) +
  geom_rect(aes(xmin = .4, xmax = .6, ymin = 0, ymax = 8), fill = "orange", alpha = .1) +
  geom_rect(aes(xmin = .6, xmax = .8, ymin = 0, ymax = 8), fill = "lightgreen", alpha = .1) +
  geom_rect(aes(xmin = .8, xmax = 1, ymin = 0, ymax = 8), fill = "darkgreen", alpha = .1) +
  geom_point(aes(shape = measure), size = 3, color = "black") +
  facet_wrap(~ task) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Test-Retest Reliability", x = "ICC3(C,1)", y = "") + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, .8, by = .2)) +
  scale_y_discrete(expand = c(0.025, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = .95, vjust = .95)
  ) + 
  scale_shape_manual(values = c(16, 3), name = "")

pl_rel
save_my_pdf_and_tiff_and_png(
  pl_rel,
  str_c(my_dir, "/reliability-bandits-literature"),
  12, 5
)




# 1.3. Convergent Validity --------------------------------------------------

tbl_bandits_1 <- readRDS("analysis/bandits/bandits-1-hybrid.RDS")
tbl_bandits_2 <- readRDS("analysis/bandits/bandits-2-hybrid.RDS")

is_ucb <- FALSE
colnames(tbl_bandits_1) <- c(
  "ID", "Value-Guided Restless", "Directed Restless", 
  "Intercept Two-Armed", "Value-Guided Two-Armed", "Directed Two-Armed", "Random Two-Armed", 
  "Intercept Horizon", "Value-Guided Horizon", "Directed Horizon", "session"
)
colnames(tbl_bandits_2) <- colnames(tbl_bandits_1)
pl_cors_s1 <- my_corr_plot(cor(tbl_bandits_1 %>% select(-c(ID, session, contains("Intercept")))), "", "", "Convergent Validity Exploration") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))
pl_cors_s2 <- my_corr_plot(cor(tbl_bandits_2 %>% select(-c(ID, session, contains("Intercept")))), "", "", "Convergent Validity Exploration") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))

pl_convergent_hybrid <- arrangeGrob(pl_cors_s1, pl_cors_s2, nrow = 1)
save_my_pdf_and_tiff(pl_convergent_hybrid, str_c(my_dir, "/bandits-convergent-hybrid-s1-s2"), 16.5, 7.25)
save_my_pdf_and_tiff(pl_cors_s1, str_c(my_dir, "/bandits-convergent-hybrid-s1"), 9, 7.25)


tbl_bandits_1 <- readRDS("analysis/bandits/bandits-1-ucb.RDS")
tbl_bandits_2 <- readRDS("analysis/bandits/bandits-2-ucb.RDS")
colnames(tbl_bandits_1) <- c(
  "ID", "Value-Guided Restless", "Directed Restless", 
  "Intercept Two-Armed", "Value-Guided Two-Armed", "Directed Two-Armed", 
  "Intercept Horizon", "Value-Guided Horizon", "Directed Horizon", "session"
)
colnames(tbl_bandits_2) <- colnames(tbl_bandits_1)

is_ucb <- TRUE
pl_cors_s1 <- my_corr_plot(cor(tbl_bandits_1 %>% select(-c(ID, session, contains("Intercept")))), "", "", "Convergent Validity Exploration") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))
pl_cors_s2 <- my_corr_plot(cor(tbl_bandits_2 %>% select(-c(ID, session, contains("Intercept")))), "", "", "Convergent Validity Exploration") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))

pl_convergent_ucb <- arrangeGrob(pl_cors_s1, pl_cors_s2, nrow = 1)
save_my_pdf_and_tiff(pl_convergent_ucb, str_c(my_dir, "/bandits-convergent-ucb"), 15, 7.25)
save_my_pdf_and_tiff(pl_cors_s1, str_c(my_dir, "/bandits-convergent-ucb-s1"), 9, 7.25)



l_tbl_wm <- readRDS("analysis/wm/wm-measures.RDS")
pl_cors_wm_s1 <- my_corr_plot(
  cor(
    l_tbl_wm[[1]] %>% 
      select(-c(ID, session, OS_processing, SS_processing, prop_timeout_os_processing, prop_timeout_ss_processing, rt_os, rt_ss)) %>%
      rename("Operation Span" = "OS_recall", "Symmetry Span" = "SS_recall", "WM Updating" = "WMU_recall")
  ), "", "", "Session 1", type = "wm"
) + theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))
pl_cors_wm_s2 <- my_corr_plot(
  cor(
    l_tbl_wm[[2]] %>% 
      select(-c(ID, session, OS_processing, SS_processing, prop_timeout_os_processing, prop_timeout_ss_processing, rt_os, rt_ss)) %>% 
      rename("Operation Span" = "OS_recall", "Symmetry Span" = "SS_recall", "WM Updating" = "WMU_recall")
  ), "", "", "Session 2", type = "wm"
) + theme(axis.text.x = element_text(angle = 30, hjust = 1.02, vjust = 1.025))
pl_wm_convergent <- arrangeGrob(pl_cors_wm_s1, pl_cors_wm_s2, nrow = 1)

save_my_pdf_and_tiff(
  pl_wm_convergent, str_c(my_dir, "/wm-convergent"), 7.5, 3.5
)


#### Kristin version

load("analysis/bandits/optimal_switch_session1.Rda")

relabel <- function(df) {
  
  relabelled <- df %>% 
    mutate(x = factor(x, levels = c("horizon", "2AB", "restless"),
                      labels = c("Horizon", "Two-armed", "Restless")),
           y = factor(y, levels = c("restless", "2AB", "horizon"),
                      labels = c("Restless", "Two-armed", "Horizon")))
  
  return(relabelled)
}

### P(optim)
optimal <- Poptimal %>% 
  pivot_wider(id_cols = ID, names_from = model, values_from = Poptimal) %>% 
  self_cor() %>% 
  relabel()

optim <- heatmap(optimal) +
  labs(title = "P(optimal)",
       x = element_blank(),
       y = element_blank())

optim
### regret

reg<- regret %>% 
  pivot_wider(id_cols = ID, names_from = model, values_from = regret) %>% 
  self_cor() %>% 
  relabel()

re <- heatmap(reg) +
  labs(title = "Regret",
       x = element_blank(),
       y = element_blank())

re

### P(switch)

Pswitch <- switch %>% 
  pivot_wider(id_cols = ID, names_from = model, values_from = Pswitch) %>% 
  self_cor() %>% 
  relabel()

swi <- heatmap(Pswitch) +
  labs(title = "P(switch)",
       x = element_blank(),
       y = element_blank())

swi

### WM

wm <- readRDS("analysis/4arlb-overviewAll.rds") %>% 
  subset(session == 1, select = c(ID,OS_recall, SS_recall, WMU_recall)) %>% 
  self_cor() %>% 
  mutate(x = factor(x, levels = c("OS_recall", "SS_recall", "WMU_recall"),
                    labels = c("Oper. span", "Sym. span", "Updating")),
         y = factor(y, levels = c("WMU_recall", "SS_recall", "OS_recall"),
                    labels = c("Updating", "Symmetry span", "Operation span")))

WM <- heatmap(wm) +
  labs(title = "Working Memory",
       x = element_blank(),
       y = element_blank())

WM

task_based <- ggarrange(optim, re, swi, WM, ncol = 2, nrow = 2, labels = "AUTO", common.legend = T,
                        legend = "right", align = "hv")
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
  subset(select = -ID) %>% 
  cor(use="pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor") %>% 
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

param_based <- ggarrange(param_based, labels = c("E"))
converge <- ggarrange(task_based, param_based, ncol = 2)
converge

save_my_pdf_and_tiff_and_png(converge,
                             str_c(my_dir, "/convergence_default"),
                             w = 17,
                             h = 5)

#ggsave("plots/submission1/convergent_validity_parameters.png")



# 1.5. Variable Correlations ---------------------------------------------------


# horizon
tbl_cor_trial_horizon1 <- readRDS("analysis/bandits/var-cors-horizon.rds")
pl_cors_horizon <- ggplot(tbl_cor_trial_horizon1, aes(trial, value, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_horizon1, aes(
    7.5, .25 * as.numeric(as.factor(name)), 
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


# 2 armed bd
tbl_cor_trial_sam1 <- readRDS("analysis/bandits/var-cors-2armed.rds")
pl_cors_2armed <- ggplot(tbl_cor_trial_sam1, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_sam1, aes(6, (.25 * as.numeric(as.factor(name))) - .2, label = str_c("avg. r = ", round(value, 2)), color = as.factor(name))) +
  facet_wrap(~ cond) +
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



# restless
tbl_restless_cor <- readRDS("analysis/bandits/var-cors-restless.rds")
change_labels <- function(tbl_df, cn) {
  tbl_df %>%
    mutate(
      "{{cn}}" := str_replace({{cn}}, "v_", "RU "),
      "{{cn}}" := str_replace({{cn}}, "m_", "V "),
      "{{cn}}" := str_replace({{cn}}, "th_", "VTU "),
      "{{cn}}" := str_replace({{cn}}, "_", " ")
    )
}
tbl_restless_cor <- change_labels(tbl_restless_cor, var_in)
tbl_restless_cor <- change_labels(tbl_restless_cor, var_out)

pl_cors_restless <- ggplot(tbl_restless_cor, aes(var_in, var_out)) +
  geom_tile(aes(fill = value)) +
  geom_label(aes(label = round(value, 2), label.size = .2)) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = "Variable In", y = "Variable Out") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  ) + 
  scale_fill_gradient2(name = "", high = "#66C2A5", low = "#FC8D62", mid = "white") +
  guides(fill = "none")

pl_cors_all <- arrangeGrob(pl_cors_horizon, pl_cors_2armed, pl_cors_restless, layout_matrix = matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2), nrow = 2, heights = c(.6, 1))
save_my_pdf_and_tiff(pl_cors_all, str_c(my_dir, "/variable-correlations"), 18, 10)

######### Kristin version


load("analysis/external_validity_cors_session1.Rda") 

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
                    "OS_recall_0" = "Operation span",
                    "SS_recall_0" = "Symmetry span",
                    "WMU_recall_0" = "Updating"
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
                            labels = c("Directed", "Value-guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "delta_mean", "info"),
                       labels = c("Intercept", "Value-guided", "Directed")))

p1 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Horizon task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p1

#ggsave("plots/submission1/recovery_horizon_only_long.png", p1)


## two-armed bd ------------------------------------
sam <- load_and_prep_bandit_data(session = 1)$sam
load("analysis/bandits/modellingResults/recoverySamSession1UCB_hierarchical_notIterative.Rda")
cors <- cors %>% 
  mutate(recovered = factor(recovered, levels = c("RU", "V", "Intercept"),
                            labels = c("Directed", "Value-guided", "Intercept")),
         true = factor(true, levels = c("Intercept", "V", "RU"),
                       labels = c("Intercept", "Value-guided", "Directed")))

p2 <- heatmap(cors, x = cors$true, y = cors$recovered) +
  labs(title = "Two-armed bandit task",
       x = "Fitted parameters",
       y = "Recovered parameters")
p2

#ggsave("plots/submission1/recovery_2ab_UCB.png", p1)

recovery <- ggarrange(p1, p2, ncol = 2, labels = "AUTO", common.legend = T, legend = "right")

recovery

save_my_pdf_and_tiff_and_png(recovery,
                             str_c(my_dir, "/recovery_improved"),
                             9,4)


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


## for aggregating across models we first need to adjust the models intercept wise: Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson’s method. Tutorials in Quantitative Methods for Psychology, 1(1), 42–45. https://doi.org/10.20982/tqmp.01.1.p042
# that's why the variables of interest are called estimate_corrected etc
fixed$predictor <- factor(fixed$predictor, levels = c("Value-guided", "Directed", "Random"))
fixed$task <- factor(fixed$task, levels = c("Horizon", "Two-armed"))

rep <- ggplot(fixed, aes(predictor, estimate_corrected,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = l_corrected, ymax = u_corrected), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(title = "Fixed effects across tasks and sessions",
       x = element_blank(),
       y = "Parameter estimate ± 95% CI")+
  geom_hline(yintercept = 0, linetype = "dotdash")

rep

save_my_pdf_and_tiff_and_png(rep, str_c(my_dir, "/replicability_improved"), 
                             w = 6,
                             h = 5)


# 2.2. Reliability --------------------------------------------------------

tbl_reliability_bandits <- readRDS("analysis/bandits/reliabilities-ucb.csv")
levels(tbl_reliability_bandits$parameter) = c("Intercept", "Value-Guided", "Directed", "Regret", "p(optimal)", "p(switch)")
levels(tbl_reliability_bandits$task) <- c("Horizon", "Two-Armed", "Restless")


pl_rel <- ggplot(tbl_reliability_bandits %>% filter(parameter != "Interept"), aes(value, fct_rev(parameter))) +
  geom_rect(aes(xmin = 0, xmax = .4, ymin = 0, ymax = 7), fill = "tomato3", alpha = .1) +
  geom_rect(aes(xmin = .4, xmax = .6, ymin = 0, ymax = 7), fill = "orange", alpha = .1) +
  geom_rect(aes(xmin = .6, xmax = .8, ymin = 0, ymax = 7), fill = "lightgreen", alpha = .1) +
  geom_rect(aes(xmin = .8, xmax = 1, ymin = 0, ymax = 7), fill = "darkgreen", alpha = .1) +
  geom_point(aes(shape = measure), size = 3, color = "black") +
  facet_wrap(~ task) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Test-Retest Reliability", x = "ICC3(C,1)", y = "") + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, .8, by = .2)) +
  scale_y_discrete(expand = c(0.025, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 40, hjust = .95, vjust = .95)
  ) + 
  scale_shape_manual(values = c(16, 3), name = "")

pl_rel

save_my_pdf_and_tiff_and_png(
  pl_rel,
  str_c(my_dir, "/reliability-bandits-ucb"),
  12, 5
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
  subset(select = -ID) %>% 
  cor(use="pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor") %>% 
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
  left_join(read.csv("analysis/questionnaire_factors_s2.csv") %>% select(-X), by = "ID") %>% 
  select(-c(ID)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -c(x), names_to = "y", values_to = "cor") %>% 
  mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed","WMC", "Exp", "posMood", "negMood", "AxDep"),
                    labels = c("Value-guided", "Directed","Working memory", "Self-reported exploration", "Positive mood", "Negative mood", "Anxiety/depression")),
         y = factor(y, levels = c("AxDep", "negMood", "posMood", "Exp","WMC", "G.Directed", "G.Value.Guided"),
                    labels = c("Anxiety/depression", "Negative mood", "Positive mood", "Self-reported exploration","Working memory", "Directed", "Value-guided")))


latent <- heatmap(cors) + 
  labs(title = "External validity on the latent level",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

latent

conv2 <- ggarrange(converge, latent, ncol = 2, labels = "AUTO", common.legend = T,
          legend = "right")

conv2

save_my_pdf_and_tiff_and_png(conv2,
                             str_c(my_dir, "/convergence_criterion_improved"),
                             w = 14,
                             h = 5)

# Supplmentary Information ------------------------------------------------


# the following three plots display correlations between the independent variables in the three bandit tasks (i.e., V, VTU, and RU)
tbl_cor_trial_2ab <- readRDS("analysis/bandits/var-cors-2armed.rds")
pl_2ab <- ggplot(tbl_cor_trial_sam1, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_sam1, aes(6, .15 * as.numeric(as.factor(name)), label = str_c("avg. r = ", round(value, 2)), color = as.factor(name))) +
  facet_wrap(~cond) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Pearson Correlation") +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2", name = "")

tbl_cor_trial_horizon1 <- readRDS("analysis/bandits/var-cors-horizon.rds")
pl_horizon <- ggplot(tbl_cor_trial_horizon1, aes(trial, value)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  geom_label(data = tbl_cor_avg_horizon1, aes(
    6, .15 * as.numeric(as.factor(name)),
    label = str_c("avg. r = ", round(value, 2)), color = as.factor(name)
  )) +
  facet_wrap(~Horizon) +
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

tbl_restless_cor <- readRDS("analysis/bandits/var-cors-restless.rds")
pl_restless <- ggplot(tbl_restless_cor, aes(var_in, var_out)) +
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

pl_strat_cor <- arrangeGrob(pl_2ab, pl_horizon, pl_restless, nrow = 1)
grid.draw(pl_strat_cor)

save_my_pdf_and_tiff_and_png(pl_strat_cor, str_c(my_dir, "/strategies-correlations"), 23, 8)

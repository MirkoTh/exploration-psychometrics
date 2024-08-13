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

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)


# 1. Which Models? ---------------------------------------------------------

my_dir <- "figures/figures-ms/submission-1"

# how did we arrive at the used models?

## restless bd ------------------------------------------------------------


pl_restless_group_patterns <- readRDS("data/restless-group-patterns-across-methods.Rds")
pl_restless_params_across_methods <- readRDS("data/restless-params-across-methods.RDS")
tbl_restless_reliability_across_methods <- readRDS("data/restless-reliabilities-across-methods.Rds")
tbl_restless_reliability_across_methods <- tbl_restless_reliability_across_methods %>%
  mutate(Reliability = round(Reliability, 2))

table_rl_reliab <- tableGrob(tbl_restless_reliability_across_methods)
pl_table_rl_reliab <- ggplot() + annotation_custom(table_rl_reliab)
pl_arrive_restless_model <- arrangeGrob(
  pl_restless_group_patterns, pl_restless_params_across_methods, pl_table_rl_reliab,
  nrow = 1
)

if (!dir.exists(my_dir)){dir.create(my_dir, recursive = TRUE)}

save_my_pdf_and_tiff(
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






# 2. Recoverability --------------------------------------------------------




## restless bd ------------------------------------------------------------


file_loc_hc_s1 <- "data/restless-hierarchical-model-posterior-s1.RDS"
file_loc_hc_s1_recovery <- "data/restless-hierarchical-model-recovery-posterior-s1.RDS"
tbl_draws_hc_s1 <- readRDS(file_loc_hc_s1)
tbl_draws_hc_s1_recovery <- readRDS(file_loc_hc_s1_recovery)

l_recovery_s1 <- map_cor(tbl_draws_hc_s1, tbl_draws_hc_s1_recovery, c("beta", "tau"))
pl_heatmap_s1 <- recovery_heatmap(l_recovery_s1, "Restless Bandit", c("RU", "V"))

save_my_pdf_and_tiff(
  pl_heatmap_s1,
  str_c(my_dir, "/restless-ucb-recovery"),
  5, 4
)




# 3. Reliability ----------------------------------------------------------

tbl_reliability_bandits <- readRDS("analysis/bandits/reliabilities-hybrid.csv")
pl_rel <- ggplot(tbl_rel_both %>% filter(parameter != "IC"), aes(value, fct_rev(parameter))) +
  geom_vline(xintercept = .5, color = "red", linewidth = 2, alpha = .3) +
  geom_vline(xintercept = .75, color = "lightgreen", linewidth = 2, alpha = .5) +
  geom_vline(xintercept = .9, color = "darkgreen", linewidth = 2, alpha = .3) +
  geom_point(aes(fill = measure), shape = 23, size = 3, color = "black") +
  facet_wrap(~ task) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Test-Retest Reliability", y = "Measure") + 
  theme_bw() +
  scale_x_continuous(expand = c(0.025, 0)) +
  scale_y_discrete(expand = c(0.025, 0)) +
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = .05)
  ) + 
  scale_fill_brewer(palette = "Set2", name = "")

save_my_pdf_and_tiff(
  pl_rel,
  str_c(my_dir, "/reliability-bandits"),
  10, 5
)





# 6. Variable Correlations ---------------------------------------------------


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








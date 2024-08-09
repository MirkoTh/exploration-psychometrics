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










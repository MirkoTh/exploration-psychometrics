# final plotting for MS


library(grid)
library(gridExtra)
library(tidyverse)

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)


# Which Models? -----------------------------------------------------------

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


## horizon task -----------------------------------------------------------





## two-armed bd -----------------------------------------------------------






# Recoverability ----------------------------------------------------------




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









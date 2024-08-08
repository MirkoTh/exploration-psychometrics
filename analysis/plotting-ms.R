# final plotting for MS


library(grid)
library(gridExtra)
library(tidyverse)

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


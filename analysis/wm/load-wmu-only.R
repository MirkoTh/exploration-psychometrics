rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)

path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]
participants_returned <- c(
  "65269fb94006e8c966cb3d85", "5dd25456cc192027096069bb",
  "60da18693053b2c1d420085f", "64f3ad25c93a6f6d60e6acd8"
)

# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2023, 12, 08, 15, tz = "CET"), 
  make_datetime(2023, 12, 08, 17, tz = "CET")
) # 10 prolific pilot participant range
hash_ids(path_data, participants_returned, time_period = time_period, is_considered = c("WMU"))

tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup.csv"))

tbl_WMU_recall <- readRDS(str_c(path_data, "tbl_WMU_recall.RDS")) %>%
  filter(is_practice == 0 & trial_type == "update")



# Recall ------------------------------------------------------------------

tbl_wmu_agg <- agg_by_ss(tbl_WMU_recall, tbl_ids_lookup, "WMU")

# for every set size separately

tbl_wmu_ss_agg_ci <- summary_se(
  tbl_wmu_agg, "prop_correct", groupvars = c("set_size", "task")
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

# marginalize over set sizes
tbl_wmu_participant_agg <- grouped_agg(tbl_wmu_agg, c(participant_id, task), prop_correct)

tbl_recall_performance_participants <- tbl_wmu_participant_agg %>%
  select(participant_id, task, mean_prop_correct) %>% pivot_wider(
    id_cols = participant_id, names_from = task, values_from = mean_prop_correct
  ) %>%
  ungroup()

ggplot(tbl_wmu_ss_agg_ci, aes(set_size, prop_correct, group = 1)) +
  geom_hline(yintercept = 1, linetype = "dotdash", alpha = .2) +
  geom_errorbar(
    data = tbl_wmu_ss_agg_ci, aes(
      x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci
    ), width = .5
  ) +
  geom_point(color = "white", size = 4) +
  geom_point() +
  facet_wrap(~ task) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Set Size", y = "Proportion Correct") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + coord_cartesian(xlim = c(1, 8))



# Both --------------------------------------------------------------------


tbl_prolific <- tbl_recall_performance_participants %>% 
  left_join(tbl_ids_lookup, join_by(participant_id == participant_id_randomized), suffix = c("_random", "_orig")) %>%
  select(-participant_id) %>%
  rename(participant_id = participant_id_orig) %>%
  relocate(participant_id, .before = WMU) %>%
  mutate(
    bonus = round(WMU * 1.5, 1)
  )
tbl_prolific %>% select(-WMU) %>%
  write_csv(file = "data/all-data/bonus-payments/2023-12-08-wmu-only.csv")






rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)

path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]
participants_returned <- list()

# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2023, 12, 08, 15, tz = "CET"), 
  make_datetime(2023, 12, 08, 17, tz = "CET")
) # 10 prolific pilot participant range
hash_ids(path_data, participants_returned, time_period = time_period)

tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup.csv"))

tbl_os_recall <- readRDS(str_c(path_data, "tbl_OS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_OS_recall.csv"))
tbl_os_processing <- read_csv(str_c(path_data, "tbl_OS_processing.csv")) %>%
  filter(is_practice == 0) %>%
  mutate(processing_position = processing_position + 1) %>%
  group_by(participant_id, trial_id_recall) %>%
  mutate(
    set_size = max(processing_position),
    n_correct = sum(accuracy)
  ) %>%
  ungroup()

# this is only for pilot data where processing position was not correctly stored
tbl_os_processing <- tbl_os_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
  mutate(rwn = row_number(rt))
tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] <- tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] + 1
tbl_os_processing <- tbl_os_processing %>% select(-rwn)

tbl_ss_recall <- readRDS(str_c(path_data, "tbl_SS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_SS_recall.csv"))
tbl_ss_processing <- read_csv(str_c(path_data, "tbl_SS_processing.csv")) %>%
  filter(is_practice == 0) %>%
  group_by(participant_id, trial_id_recall) %>%
  mutate(
    processing_position = row_number(processing_position),
    set_size = max(processing_position),
    n_correct = sum(accuracy)
  ) %>% ungroup()

# this is only for pilot data where processing position was not correctly stored
tbl_ss_processing <- tbl_ss_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
  mutate(rwn = row_number(rt))
tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] <- tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] + 1
tbl_ss_processing <- tbl_ss_processing %>% select(-rwn)


tbl_WMU_recall <- read_csv(str_c(path_data, "tbl_WMU_recall.csv")) %>%
  filter(is_practice == 0 & trial_type == "update")



# Recall ------------------------------------------------------------------

tbl_os_agg <- agg_by_ss(tbl_os_recall, tbl_ids_lookup, "OS")
tbl_ss_agg <- agg_by_ss(tbl_ss_recall, tbl_ids_lookup, "SS")
tbl_wmu_agg <- agg_by_ss(tbl_WMU_recall, tbl_ids_lookup, "WMU")

# for every set size separately
tbl_os_ss_agg_ci <- summary_se_within(
  tbl_os_agg, "prop_correct", 
  withinvars = c("set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_ss_ss_agg_ci <- summary_se_within(
  tbl_ss_agg, "prop_correct", 
  withinvars = c("set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_wmu_ss_agg_ci <- summary_se(
  tbl_wmu_agg, "prop_correct", groupvars = c("set_size", "task")
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

# marginalize over set sizes
tbl_os_participant_agg <- grouped_agg(tbl_os_agg, c(participant_id, task), prop_correct)
tbl_ss_participant_agg <- grouped_agg(tbl_ss_agg, c(participant_id, task), prop_correct)
tbl_wmu_participant_agg <- grouped_agg(tbl_wmu_agg, c(participant_id, task), prop_correct)

tbl_recall_performance_participants <- tbl_os_participant_agg %>%
  select(participant_id, task, mean_prop_correct) %>%
  rbind(
    tbl_ss_participant_agg %>% select(participant_id, task, mean_prop_correct)
  ) %>% rbind(
    tbl_wmu_participant_agg %>% select(participant_id, task, mean_prop_correct)
  ) %>% pivot_wider(
    id_cols = participant_id, names_from = task, values_from = mean_prop_correct
  ) %>%
  ungroup()


pl_ss_recall <- plot_pc_against_ss(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci, tbl_wmu_ss_agg_ci) +
  ggtitle("Recall")

# recall rts
os_rts <- grouped_agg(tbl_os_recall, participant_id, rt) %>%
  rename(rt_os = mean_rt)
ss_rts <- grouped_agg(tbl_ss_recall, participant_id, rt) %>%
  rename(rt_ss = mean_rt)


# Processing --------------------------------------------------------------


tbl_os_proc_agg <- agg_by_ss(
  tbl_os_processing %>% filter(processing_position == 1), tbl_ids_lookup, "OS"
)
tbl_ss_proc_agg <- agg_by_ss(
  tbl_ss_processing %>% filter(processing_position == 1), tbl_ids_lookup, "SS"
)

# for every set size separately
tbl_os_proc_ss_agg_ci <- summary_se_within(
  tbl_os_proc_agg, "prop_correct", 
  withinvars = c("set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_ss_proc_ss_agg_ci <- summary_se_within(
  tbl_ss_proc_agg, "prop_correct", 
  withinvars = c("set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

# marginalize over set sizes
tbl_os_proc_participant_agg <- grouped_agg(
  tbl_os_proc_agg, c(participant_id, task), prop_correct
)
tbl_ss_proc_participant_agg <- grouped_agg(
  tbl_ss_proc_agg, c(participant_id, task), prop_correct
)

tbl_os_timeouts <- tbl_os_processing %>% group_by(participant_id) %>% 
  summarize(prop_timeout_os = sum(rt == 6000) / n()) %>% ungroup()
tbl_ss_timeouts <- tbl_ss_processing %>% group_by(participant_id) %>%
  summarize(prop_timeout_ss = sum(rt == 6000) / n()) %>% ungroup()



tbl_proc_performance_participants <- tbl_os_proc_participant_agg %>%
  select(participant_id, task, mean_prop_correct) %>%
  rbind(
    tbl_ss_proc_participant_agg %>% select(participant_id, task, mean_prop_correct)
  ) %>% pivot_wider(
    id_cols = participant_id, names_from = task, values_from = mean_prop_correct
  ) %>%
  ungroup() %>%
  left_join(tbl_os_timeouts, by = "participant_id") %>%
  left_join(tbl_ss_timeouts, by = "participant_id")


pl_ss_processing <- plot_pc_against_ss(
  tbl_os_proc_ss_agg_ci, tbl_ss_proc_ss_agg_ci, tbl_ss_proc_ss_agg_ci, 
  is_recall = FALSE
) +
  ggtitle("Processing")



# Both --------------------------------------------------------------------

tbl_performance_all <- tbl_recall_performance_participants %>%
  left_join(
    tbl_proc_performance_participants, 
    by = "participant_id", suffix = c("_recall", "_processing")
  ) %>%
  left_join(os_rts %>% select(participant_id, rt_os), by = "participant_id") %>%
  left_join(ss_rts %>% select(participant_id, rt_ss), by = "participant_id")

ggplot(tbl_performance_all, aes(WMU)) +
  geom_histogram()


cor(tbl_performance_all %>% select(-participant_id) %>%
      filter(
        !is.na(WMU))
)

grid::grid.draw(gridExtra::arrangeGrob(pl_ss_recall, pl_ss_processing, nrow = 1))

tbl_performance_all %>% 
  left_join(tbl_ids_lookup, join_by(participant_id == participant_id_randomized), suffix = c("_random", "_orig")) %>%
  select(-participant_id) %>%
  rename(participant_id = participant_id_orig) %>%
  relocate(participant_id, .before = OS_recall)


load("data/pilot/qs.Rda")
load("data/pilot/bandits.Rda")





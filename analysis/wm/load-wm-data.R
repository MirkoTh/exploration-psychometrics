rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)

path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]



participants_returned <- c(
  "6525b562b415d2f7ed82196e", 
  "612f689ce22db566d4f2a0bb", 
  "615db68d9d021708b6e64603",
  "60ed9d18c82ef5b5aba49371",
  "616c026cadd0c9c23c911037",
  "611f5fca1c852d6d56e5bf8d",
  "615a2a654f4be71b8bfa94c8",
  "6127547b385c9d9bc30d476f",
  "5f2d4939e1b8742bba60a1e4",
  "656a03afacfc5f79e06bca80",
  "647e147c2a007061f5689965",
  "616a94b54cf4a5cde4d9c015",
  "60cc4cbfd6bdf9ee88cdd2f6",
  "611e480c5772542ccce1a22f",
  "64eb650fbf222d6e2ec9cff9" 
)

# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2024, 1, 16, 17, tz = "CET"), 
  make_datetime(2024, 1, 17, 19, tz = "CET")
) # 10 prolific pilot participant range
hash_ids(path_data, participants_returned, time_period = time_period)

tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup.csv"))

tbl_os_recall <- readRDS(str_c(path_data, "tbl_OS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_OS_recall.csv"))
tbl_os_processing <- read_csv(str_c(path_data, "tbl_OS_processing.csv")) %>%
  filter(is_practice == 0)




# this is only for pilot data where processing position was not correctly stored
# tbl_os_processing <- tbl_os_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
#   mutate(rwn = row_number(rt))
# tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] <- tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] + 1
# tbl_os_processing <- tbl_os_processing %>% select(-rwn)




tbl_ss_recall <- readRDS(str_c(path_data, "tbl_SS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_SS_recall.csv"))
tbl_ss_processing <- read_csv(str_c(path_data, "tbl_SS_processing.csv")) %>%
  filter(is_practice == 0)

# this is only for pilot data where processing position was not correctly stored
# tbl_ss_processing <- tbl_ss_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
#   mutate(rwn = row_number(rt))
# tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] <- tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] + 1
# tbl_ss_processing <- tbl_ss_processing %>% select(-rwn)


tbl_WMU_recall <- readRDS(str_c(path_data, "tbl_WMU_recall.rds")) %>%
  filter(is_practice == 0 & trial_type == "update")


# how many trials were collected from the participants?
how_many_trials <- function(nm, tbl_df) {
  tbl_df %>% 
    group_by(participant_id) %>%
    count() %>% 
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(task = nm)
}

tbl_n_trials <- map2(
  list("os_recall", "os_processing", "ss_recall", "ss_processing", "wmu_recall"),
  list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_WMU_recall),
  how_many_trials
) %>% reduce(rbind)

ggplot(tbl_n_trials, aes(task, participant_id)) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white")

# use first given responses for participants with multiple responses

extract_first_response <- function(tbl_df, g_vars) {
  tbl_df %>% 
  group_by(across({{g_vars}})) %>%
  mutate(rwn = row_number()) %>%
  ungroup() %>% filter(rwn == 1)
}

tbl_os_recall <- extract_first_response(tbl_os_recall, c(participant_id, trial_id_recall))
tbl_os_processing <- extract_first_response(tbl_os_processing, c(participant_id, trial_id_recall, processing_position))
tbl_ss_recall <- extract_first_response(tbl_ss_recall, c(participant_id, trial_id_recall))
tbl_ss_processing <- extract_first_response(tbl_ss_processing, c(participant_id, trial_id_recall, processing_position))
tbl_WMU_recall <- extract_first_response(tbl_WMU_recall, c(participant_id, trial_id))

tbl_n_trials <- map2(
  list("os_recall", "os_processing", "ss_recall", "ss_processing", "wmu_recall"),
  list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_WMU_recall),
  how_many_trials
) %>% reduce(rbind)

ggplot(tbl_n_trials, aes(task, participant_id)) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white")


tbl_os_processing <- tbl_os_processing %>%
  mutate(processing_position = processing_position + 1) %>%
  group_by(participant_id, trial_id_recall) %>%
  mutate(
    set_size = max(processing_position),
    n_correct = sum(accuracy)
  ) %>%
  ungroup()

tbl_ss_processing <- tbl_ss_processing %>%
  mutate(processing_position = processing_position + 1) %>%
  group_by(participant_id, trial_id_recall) %>%
  mutate(
    processing_position = row_number(processing_position),
    set_size = max(processing_position),
    n_correct = sum(accuracy)
  ) %>%
  ungroup()


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
  ggtitle("Recall") + coord_cartesian(ylim = c(.5, 1))

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
  ggtitle("Processing") + coord_cartesian(ylim = c(.5, 1))



# Both --------------------------------------------------------------------

tbl_performance_all <- tbl_recall_performance_participants %>%
  left_join(
    tbl_proc_performance_participants, 
    by = "participant_id", suffix = c("_recall", "_processing")
  ) %>%
  left_join(os_rts %>% select(participant_id, rt_os), by = "participant_id") %>%
  left_join(ss_rts %>% select(participant_id, rt_ss), by = "participant_id")

ggplot(tbl_performance_all %>% pivot_longer(c(OS_recall, SS_recall, WMU)), aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_wrap(~ name) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, 0)) +
  scale_y_continuous(expand = c(.01, 0)) +
  labs(x = "Prop. Correct", y = "Nr Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  )

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





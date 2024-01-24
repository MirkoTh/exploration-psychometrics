rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)

path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]

# these are the nr. trials that were administed in the experiment
tbl_trials_administered <- tibble(
  os_recall = 15,
  ss_recall = 12,
  os_processing = 90,
  ss_processing = 54,
  wmu_recall = 20
) %>% pivot_longer(colnames(.))
colnames(tbl_trials_administered) <- c("task", "n_administered")


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
  "64eb650fbf222d6e2ec9cff9",
  "62d1326fee93c50ea5638e9f",
  "5d20b0157e17470001e1c635",
  "62b182ba39468032223841bb",
  "5d4986318fe73e0001569573",
  "59f1d43d24d7bf00012f17f2",
  "61222038c2048b50449284a1",
  "63d8074daa40629318ada75f" 
)

time_out_exclude <- c(
  "56b6e2fcf2fc70000c32f9d5",
  "571a78f06a1c6300114b8d80"
)

# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2024, 1, 23, 11, tz = "CET"), 
  make_datetime(2024, 1, 23, 20, tz = "CET")
) # 10 prolific pilot participant range
hash_ids(
  path_data, 
  c(participants_returned, time_out_exclude), 
  time_period = time_period
)

tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup.csv"))

# exclude practice trials
tbl_os_recall <- readRDS(str_c(path_data, "tbl_OS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_OS_recall.csv"))
tbl_os_processing <- read_csv(str_c(path_data, "tbl_OS_processing.csv")) %>%
  filter(is_practice == 0)




# this is only for pilot data where processing position was not correctly stored
# tbl_os_processing <- tbl_os_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
#   mutate(rwn = row_number(rt))
# tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] <- tbl_os_processing$processing_position[tbl_os_processing$rwn == 2] + 1
# tbl_os_processing <- tbl_os_processing %>% select(-rwn)



# exclude practice trials and add missing variables
tbl_ss_recall <- readRDS(str_c(path_data, "tbl_SS_recall.RDS")) %>%
  filter(is_practice == 0)#read_csv(str_c(path_data, "tbl_SS_recall.csv"))
tbl_ss_processing <- read_csv(str_c(path_data, "tbl_SS_processing.csv")) %>%
  filter(is_practice == 0)

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

# this is only for pilot data where processing position was not correctly stored
# tbl_ss_processing <- tbl_ss_processing %>% group_by(participant_id, trial_id_recall, processing_position) %>%
#   mutate(rwn = row_number(rt))
# tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] <- tbl_ss_processing$processing_position[tbl_ss_processing$rwn == 2] + 1
# tbl_ss_processing <- tbl_ss_processing %>% select(-rwn)


# exclude practice trials
tbl_WMU_recall <- readRDS(str_c(path_data, "tbl_WMU_recall.rds")) %>%
  filter(is_practice == 0 & trial_type == "update")



# N Trials Collected ------------------------------------------------------


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

ggplot(tbl_n_trials, aes(task, substr(participant_id, 1, 6))) +
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

tmp_n_trials <- map2(
  list("os_recall", "os_processing", "ss_recall", "ss_processing", "wmu_recall"),
  list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_WMU_recall),
  how_many_trials
) %>% reduce(rbind)

tbl_design <- crossing(
  task = unique(tbl_n_trials$task),
  participant_id = unique(tbl_n_trials$participant_id)
)

tbl_n_trials <- tbl_design %>%
  left_join(tmp_n_trials, by = c("participant_id", "task")) %>%
  replace_na(list(n = 0))

tbl_n_trials <- tbl_n_trials %>%
  left_join(tbl_trials_administered, by = "task") %>%
  mutate(
    too_few = n < n_administered
  )

tbl_ids_lookup <- tbl_ids_lookup %>%
  left_join(
    tbl_n_trials %>%
      group_by(participant_id) %>%
      summarize(any_task_too_few = sum(too_few) >= 1)
    , by = c("participant_id_randomized" = "participant_id")
  )

ggplot(tbl_n_trials, aes(task, substr(participant_id, 1, 6))) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white")





# Trials per Participant, Task, and Set Size -------------------------

tbl_trials_overview <- tbl_os_recall %>% 
  group_by(participant_id, set_size) %>% 
  count() %>%
  mutate(task = "OS Recall") %>%
  rbind(
    tbl_ss_recall %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "SS Recall")
  ) %>% rbind(
    tbl_WMU_recall %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "WMU")
  )  %>% rbind(
    tbl_os_processing %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "OS Processing")
  )  %>% rbind(
    tbl_ss_processing %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "SS Processing")
  ) %>%
  ungroup() %>%
  mutate(p_id_short = substr(participant_id, 1, 5)) %>%
  group_by(participant_id) %>%
  mutate(n_total_datapoints = sum(n)) %>%
  ungroup()
tbl_trials_overview2 <- tbl_trials_overview %>%
  group_by(participant_id, p_id_short) %>% summarize(n = max(n_total_datapoints)) %>% ungroup() %>%
  mutate(task = "All Tasks", set_size = 4)
#select(-c(set_size)) %>%
#pivot_wider(names_from = task, values_from = n) %>%
tbl_trials_all <- rbind(tbl_trials_overview %>% select(-c(n_total_datapoints)), tbl_trials_overview2)
ggplot(tbl_trials_all %>% arrange(desc(n)) %>% mutate(p_id_short = fct_inorder(factor(p_id_short))), aes(set_size, p_id_short)) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white") +
  facet_wrap(~ task, nrow = 1) +
  theme_bw() +
  scale_fill_gradient2(midpoint = 100) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Set Size", y = "Participant ID") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Nr. Obs.")



# Exclude Incomplete Datasets ---------------------------------------------


# this criterion can be more lenient than excluding participants
# if there is any task with too few trials (as above)
# if n_thx < 191

n_thx <- 190 # keep data set of participants with only one data point missing
tbl_complete <- tbl_trials_all %>% filter(task == "All Tasks" & n >= n_thx)
tbl_complete_p <- tbl_complete %>% select(participant_id)
tbl_os_recall <- tbl_os_recall %>% inner_join(tbl_complete_p, "participant_id")
tbl_ss_recall <- tbl_ss_recall %>% inner_join(tbl_complete_p, "participant_id")
tbl_os_processing <- tbl_os_processing %>% inner_join(tbl_complete_p, "participant_id")
tbl_ss_processing <- tbl_ss_processing %>% inner_join(tbl_complete_p, "participant_id")
tbl_WMU_recall <- tbl_WMU_recall %>% inner_join(tbl_complete_p, "participant_id")

tbl_ids_lookup <- tbl_ids_lookup %>% 
  left_join(
    tbl_complete_p %>% 
      mutate(all_tasks_too_few = FALSE),
    by = c("participant_id_randomized" = "participant_id")
  ) %>%
  replace_na(list(all_tasks_too_few = TRUE))

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
  ggtitle("Recall") + coord_cartesian(ylim = c(.3, 1))

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

tbl_proc_performance_participants$thx_lo_os <- 
  qbinom(.95, tbl_trials_administered$n_administered[tbl_trials_administered$task == "os_processing"], .5)/
  tbl_trials_administered$n_administered[tbl_trials_administered$task == "os_processing"]
tbl_proc_performance_participants$thx_lo_ss <- 
  qbinom(.95, tbl_trials_administered$n_administered[tbl_trials_administered$task == "ss_processing"], .5)/
  tbl_trials_administered$n_administered[tbl_trials_administered$task == "ss_processing"]
tbl_proc_performance_participants <- tbl_proc_performance_participants %>%
  mutate(
    excl_os = OS < thx_lo_os,
    excl_ss = SS < thx_lo_ss,
    proc_below_thx = excl_os + excl_ss > 0
  )

# add exclusions to overview tbl
tbl_ids_lookup <- tbl_ids_lookup %>%
  left_join(
    tbl_proc_performance_participants[, c("participant_id", "proc_below_thx")], 
    by = c("participant_id_randomized" = "participant_id")
  )



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

tbl_thx <- tibble(
  name = c("OS_recall", "SS_recall", "WMU", "OS_processing", "SS_processing"),
  thx = c(
    0, 0, 0, 
    tbl_proc_performance_participants$thx_lo_os[1],
    tbl_proc_performance_participants$thx_lo_ss[1]
  )
)

tbl_ids_lookup %>% select(-proc_below_thx) %>% 
  left_join(tbl_performance_all, by = c("participant_id_randomized" = "participant_id")) %>%
  replace_na(list(proc_below_thx =  0)) %>%
  rename(prolific_pid = participant_id, participant_id = participant_id_randomized) %>%
  select(participant_id, all_tasks_too_few, proc_below_thx) %>%
  mutate(excl_subject = pmax(all_tasks_too_few, proc_below_thx)) %>%
  saveRDS(file = "analysis/wm/subjects-excl-wm.rds")

ggplot() +
  geom_histogram(
    data = tbl_performance_all %>% 
      pivot_longer(
        c(OS_recall, SS_recall, WMU, OS_processing, SS_processing)
      ), aes(value),
    color = "black", fill = "skyblue2") +
  geom_vline(data = tbl_thx, aes(xintercept = thx), color = "red", linetype = "dotdash", linewidth = 1) +
  facet_wrap(~ name) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, 0)) +
  scale_y_continuous(expand = c(.01, 0)) +
  labs(x = "Prop. Correct", y = "Nr Participants") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )

tbl_cor <- cor(tbl_performance_all %>% select(ends_with("recall"), ends_with("processing"), "WMU") %>%
                 filter(
                   !is.na(WMU))
) %>% as.data.frame() %>%
  mutate(task = c("OS Recall", "SS Recall", "OS Processing", "SS Processing", "WMU")) %>%
  pivot_longer(- task) 
tbl_cor$name <- factor(tbl_cor$name)
levels(tbl_cor$name) <- c("OS Processing", "OS Recall", "SS Processing", "SS Recall", "WMU")
tbl_cor$task <- factor(tbl_cor$task)
levels(tbl_cor$task) <- c("OS Processing", "OS Recall", "SS Processing", "SS Recall", "WMU")


tbl_cor %>%
  ggplot(aes(task, name)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "darkgrey", size = 5) +
  scale_fill_viridis_c(name = "Correlation", guide = "none") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90)
  )

grid::grid.draw(gridExtra::arrangeGrob(pl_ss_recall, pl_ss_processing, nrow = 1))

tbl_performance_all %>% 
  left_join(tbl_ids_lookup, join_by(participant_id == participant_id_randomized), suffix = c("_random", "_orig")) %>%
  select(-participant_id) %>%
  rename(participant_id = participant_id_orig) %>%
  relocate(participant_id, .before = OS_recall)


load("data/pilot/qs.Rda")
load("data/pilot/bandits.Rda")





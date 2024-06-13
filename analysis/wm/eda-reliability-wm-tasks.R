# EDA on WM data
# Part I is done on all data
# Part 2 is done on participants who succeeded after session 2


rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)
library(grid)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)

path_data <- "data/all-data/"


# lookup table from second session contains final ids
tbl_ids_lookup <- read_csv(str_c("data/exclusions2.csv"))
tbl_ids_lookup <- tbl_ids_lookup %>% rename("participant_id" = "PID", "participant_id_randomized" = "ID")
# load data from all wm tasks
l_tbl_wm_data <- load_wm_data("data/all-data/")

list2env(l_tbl_wm_data, environment())

# these are the nr. trials that were administered in the experiment
tbl_trials_administered <- tibble(
  os_recall = 15,
  ss_recall = 12,
  os_processing = 90,
  ss_processing = 50, #54, # would actually be 54, but accept a few missing responses
  wmu_recall = 20
) %>% pivot_longer(colnames(.))
colnames(tbl_trials_administered) <- c("task", "n_administered")



# Part I ------------------------------------------------------------------


## N Trials Collected ------------------------------------------------------


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
  list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_wmu_recall),
  how_many_trials
) %>% reduce(rbind)

# use first given responses for participants with multiple responses

extract_first_response <- function(tbl_df, g_vars) {
  tbl_df %>% 
    group_by(across({{g_vars}})) %>%
    mutate(rwn = row_number()) %>%
    ungroup() %>% filter(rwn == 1)
}

tbl_os_recall <- extract_first_response(tbl_os_recall, c(participant_id, session_id, trial_id_recall))
tbl_os_processing <- extract_first_response(tbl_os_processing, c(participant_id, session_id, trial_id_recall, processing_position))
tbl_ss_recall <- extract_first_response(tbl_ss_recall, c(participant_id, session_id, trial_id_recall))
tbl_ss_processing <- extract_first_response(tbl_ss_processing, c(participant_id, session_id, trial_id_recall, processing_position))
tbl_wmu_recall <- extract_first_response(tbl_wmu_recall, c(participant_id, session_id, trial_id))

tmp_n_trials <- map2(
  list("os_recall", "os_processing", "ss_recall", "ss_processing", "wmu_recall"),
  list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_wmu_recall),
  how_many_trials
) %>% reduce(rbind)

tbl_design <- crossing(
  task = unique(tbl_n_trials$task),
  participant_id = unique(tbl_n_trials$participant_id)
)

tbl_n_trials_task <- tbl_design %>%
  left_join(tmp_n_trials, by = c("participant_id", "task")) %>%
  replace_na(list(n = 0))

tbl_n_trials <- tbl_n_trials_task %>%
  left_join(tbl_trials_administered, by = "task") %>%
  mutate(
    too_few = n < (2*n_administered - 5) # - 5 seems reasonable given distribution of nr responses per task
  ) %>%
  group_by(participant_id) %>%
  summarize(any_task_too_few = sum(too_few) >= 1) %>%
  ungroup()

tbl_n_trials_task %>%
  group_by(task, n_trials = n) %>%
  count() %>%
  mutate(rwn = row_number(n_trials)) %>%
  ggplot(aes(as.factor(n_trials), rwn)) +
  geom_tile(aes(fill = n)) +
  geom_label(aes(label = n)) +
  facet_wrap(~ task, scales = "free_x") +
  scale_fill_viridis_c(guide = "none")

ggplot(
  tbl_n_trials_task %>%
    filter(participant_id %in% sample(tbl_n_trials$participant_id, 20)),
  aes(task, substr(participant_id, 1, 6))) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n), color = "white")





## Trials per Participant, Task, and Set Size -------------------------

tbl_trials_overview <- tbl_os_recall %>% 
  group_by(participant_id, set_size) %>% 
  count() %>%
  mutate(task = "OS Recall") %>%
  rbind(
    tbl_ss_recall %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "SS Recall")
  ) %>% rbind(
    tbl_wmu_recall %>% group_by(participant_id, set_size) %>% count() %>% mutate(task = "WMU")
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
  group_by(participant_id, p_id_short) %>%
  summarize(n = max(n_total_datapoints)) %>%
  ungroup() %>%
  mutate(task = "All Tasks", set_size = 4)

tbl_trials_all <- rbind(tbl_trials_overview %>% select(-c(n_total_datapoints)), tbl_trials_overview2)
ggplot(
  tbl_trials_all %>%
    filter(participant_id %in% sample(tbl_trials_all$participant_id, 20)) %>%
    arrange(desc(n)) %>% 
    mutate(p_id_short = fct_inorder(factor(p_id_short))),
  aes(set_size, p_id_short)) +
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





# Part II -----------------------------------------------------------------


## Exclude Incomplete Datasets ---------------------------------------------


# now only consider participants, who were not excluded after the second session

tbl_os_recall <- tbl_os_recall %>% inner_join(tbl_ids_lookup %>% filter(!exclude), by = c("participant_id" = "participant_id_randomized"))
tbl_ss_recall <- tbl_ss_recall %>% inner_join(tbl_ids_lookup %>% filter(!exclude), by = c("participant_id" = "participant_id_randomized"))
tbl_os_processing <- tbl_os_processing %>% inner_join(tbl_ids_lookup %>% filter(!exclude), by = c("participant_id" = "participant_id_randomized"))
tbl_ss_processing <- tbl_ss_processing %>% inner_join(tbl_ids_lookup %>% filter(!exclude), by = c("participant_id" = "participant_id_randomized"))
tbl_wmu_recall <- tbl_wmu_recall %>% inner_join(tbl_ids_lookup %>% filter(!exclude), by = c("participant_id" = "participant_id_randomized"))


## Recall ------------------------------------------------------------------

tbl_os_agg <- agg_by_ss(tbl_os_recall, "OS")
tbl_ss_agg <- agg_by_ss(tbl_ss_recall, "SS")
tbl_wmu_agg <- agg_by_ss(tbl_wmu_recall, "WMU")

# for every set size separately
tbl_os_ss_agg_ci <- summary_se_within(
  tbl_os_agg, "prop_correct", 
  withinvars = c("session_id", "set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_ss_ss_agg_ci <- summary_se_within(
  tbl_ss_agg, "prop_correct", 
  withinvars = c("session_id", "set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_wmu_ss_agg_ci <- summary_se_within(
  tbl_wmu_agg, "prop_correct",
  withinvars = c("session_id", "set_size", "task"),
  idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

# marginalize over set sizes
tbl_os_participant_agg <- grouped_agg(tbl_os_agg, c(participant_id, session_id, task), prop_correct)
tbl_ss_participant_agg <- grouped_agg(tbl_ss_agg, c(participant_id, session_id, task), prop_correct)
tbl_wmu_participant_agg <- grouped_agg(tbl_wmu_agg, c(participant_id, session_id, task), prop_correct)

tbl_recall_performance_participants <- tbl_os_participant_agg %>%
  select(participant_id, session_id, task, mean_prop_correct) %>%
  rbind(
    tbl_ss_participant_agg %>% select(participant_id, session_id, task, mean_prop_correct)
  ) %>% rbind(
    tbl_wmu_participant_agg %>% select(participant_id, session_id, task, mean_prop_correct)
  ) %>% pivot_wider(
    id_cols = c(participant_id, session_id), names_from = task, values_from = mean_prop_correct
  ) %>% pivot_wider(
    id_cols = participant_id, names_from = session_id, values_from = c(OS, SS, WMU)
  ) %>%
  ungroup()


pl_ss_recall <- plot_pc_against_ss(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci, tbl_wmu_ss_agg_ci) +
  ggtitle("Recall") + coord_cartesian(ylim = c(.3, 1))

# recall rts
os_rts <- grouped_agg(tbl_os_recall, c(session_id, participant_id), rt) %>%
  rename(rt_os = mean_rt) %>%
  pivot_wider(id_cols = participant_id, names_from = session_id, values_from = c(n, nunique_rt, rt_os, se_rt))
ss_rts <- grouped_agg(tbl_ss_recall, c(session_id, participant_id), rt) %>%
  rename(rt_ss = mean_rt) %>%
  pivot_wider(id_cols = participant_id, names_from = session_id, values_from = c(n, nunique_rt, rt_ss, se_rt))


## Processing --------------------------------------------------------------


tbl_os_proc_agg <- agg_by_ss(
  tbl_os_processing %>% filter(processing_position == 1), "OS"
)
tbl_ss_proc_agg <- agg_by_ss(
  tbl_ss_processing %>% filter(processing_position == 1), "SS"
)

# for every set size separately
tbl_os_proc_ss_agg_ci <- summary_se_within(
  tbl_os_proc_agg, "prop_correct", 
  withinvars = c("session_id", "set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_ss_proc_ss_agg_ci <- summary_se_within(
  tbl_ss_proc_agg, "prop_correct", 
  withinvars = c("session_id", "set_size", "task"), idvar = "participant_id"
) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

# marginalize over set sizes
tbl_os_proc_participant_agg <- grouped_agg(
  tbl_os_proc_agg, c(participant_id, session_id, task), prop_correct
)
tbl_ss_proc_participant_agg <- grouped_agg(
  tbl_ss_proc_agg, c(participant_id, session_id, task), prop_correct
)

tbl_os_timeouts <- tbl_os_processing %>% group_by(participant_id, session_id) %>% 
  summarize(prop_timeout_os = sum(rt == 6000) / n()) %>% ungroup()
tbl_ss_timeouts <- tbl_ss_processing %>% group_by(participant_id, session_id) %>%
  summarize(prop_timeout_ss = sum(rt == 6000) / n()) %>% ungroup()



tbl_proc_performance_participants <- tbl_os_proc_participant_agg %>%
  select(participant_id, session_id, task, mean_prop_correct) %>%
  rbind(
    tbl_ss_proc_participant_agg %>% select(participant_id, session_id, task, mean_prop_correct)
  ) %>% pivot_wider(
    id_cols = c(participant_id, session_id), names_from = task, values_from = mean_prop_correct
  ) %>%
  ungroup() %>%
  left_join(tbl_os_timeouts, by = c("participant_id", "session_id")) %>%
  left_join(tbl_ss_timeouts, by = c("participant_id", "session_id")) %>%
  pivot_wider(
    id_cols = participant_id, names_from = session_id, values_from = c(OS, SS, prop_timeout_os, prop_timeout_ss)
  )


tbl_proc_performance_participants$thx_lo_os <- 
  qbinom(.95, tbl_trials_administered$n_administered[tbl_trials_administered$task == "os_processing"], .5)/
  (tbl_trials_administered$n_administered[tbl_trials_administered$task == "os_processing"])
tbl_proc_performance_participants$thx_lo_ss <- 
  qbinom(.95, tbl_trials_administered$n_administered[tbl_trials_administered$task == "ss_processing"], .5)/
  (tbl_trials_administered$n_administered[tbl_trials_administered$task == "ss_processing"])
tbl_proc_performance_participants <- tbl_proc_performance_participants %>%
  mutate(
    excl_os = OS_0 < thx_lo_os | OS_1 < thx_lo_os,
    excl_ss = SS_0 < thx_lo_ss | SS_1 < thx_lo_ss,
    proc_below_thx = excl_os + excl_ss > 0
  )

# add exclusions to overview tbl
tbl_ids_lookup <- tbl_ids_lookup %>% select(-proc_below_thx) %>%
  left_join(
    tbl_proc_performance_participants[, c("participant_id", "proc_below_thx")], 
    by = c("participant_id_randomized" = "participant_id")
  )



pl_ss_processing <- plot_pc_against_ss(
  tbl_os_proc_ss_agg_ci, tbl_ss_proc_ss_agg_ci, tbl_ss_proc_ss_agg_ci, 
  is_recall = FALSE
) +
  ggtitle("Processing") + coord_cartesian(ylim = c(.5, 1))


pl_ss <- gridExtra::arrangeGrob(pl_ss_recall + theme(legend.position = "omit"), pl_ss_processing, nrow = 1, widths = c(1, 1.3))

if (!dir.exists("figures/EDA/")) {
  dir.create("figures/EDA/")
}
save_my_pdf(pl_ss, "figures/EDA/setsize-accuracy-rt.pdf", 11, 4.5)


## Both --------------------------------------------------------------------

add_word_cols <- function(my_tbl, my_word) {
  old_colnames <- colnames(my_tbl)
  s_id <- str_match(colnames(my_tbl), "[0-1]$")[, 1]
  colnames_cut <- str_remove(old_colnames, "_[0-1]$")
  to_consider <- (1:length(old_colnames))[str_detect(old_colnames, "[0-1]$")]
  colnames_cut[to_consider] <- str_c(colnames_cut[to_consider], "_", my_word, "_", s_id[!is.na(s_id)])
  return(colnames_cut)
}
colnames(tbl_recall_performance_participants) <- add_word_cols(tbl_recall_performance_participants, "recall")
colnames(tbl_proc_performance_participants) <- add_word_cols(tbl_proc_performance_participants, "processing")


tbl_performance_all <- tbl_recall_performance_participants %>%
  left_join(
    tbl_proc_performance_participants, 
    by = c("participant_id")
  ) %>%
  left_join(os_rts %>% select(participant_id, rt_os_0, rt_os_1), by = c("participant_id")) %>%
  left_join(ss_rts %>% select(participant_id, rt_ss_0, rt_ss_1), by = c("participant_id"))



tbl_performance_all <- tbl_performance_all %>%
  left_join(tbl_ids_lookup[, c("participant_id_randomized", "exclude")], by = c("participant_id" = "participant_id_randomized"))


hist_wm_performance <- tbl_performance_all %>% filter(!exclude) %>%
  pivot_longer(cols = contains("recall") | contains("processing") & !contains("timeout")) %>%
  mutate(
    Session = str_extract(name, "[0-1]$"),
    Session = str_c("Wave ", as.numeric(Session) + 1),
    name = str_remove(name, "[0-1]$"),
    name = str_replace_all(name, "_", " ")
  ) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_grid(name ~ Session, scales = "free_y") +
  theme_bw() +
  scale_x_continuous(expand = c(.01, 0)) +
  scale_y_continuous(expand = c(.01, 0)) +
  labs(x = "Prop. Correct", y = "Nr Participants") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )

save_my_pdf(
  hist_wm_performance,
  "figures/EDA/histograms-wm-performance.pdf",
  7, 12
)


tbl_cor <- cor(
  tbl_performance_all %>% 
    filter(!is.na(WMU_recall_0) & !is.na(WMU_recall_1) & !exclude) %>%
    select(contains("recall"), contains("processing"), contains("WMU")) %>%
    select(!contains("timeout"))
) %>% as.data.frame() %>%
  mutate(
    task_in = str_remove(str_remove(colnames(.), "_"), "[0-1]"),
    session_id_in = as.numeric(str_extract(colnames(.), "[0-1]")) + 1
  ) %>%
  pivot_longer(cols = -c(session_id_in, task_in), names_to = "task_out") %>%
  mutate(
    session_id_out = as.numeric(str_extract(task_out, "[0-1]")) + 1,
    task_out = str_replace(task_out, "_[0-1]_", "_"),
    task_out = str_replace(task_out, "_[0-1]$", "")
  )
tbl_cor$task_in <- factor(tbl_cor$task_in)
levels(tbl_cor$task_in) <- c("OS Processing", "OS Recall", "SS Processing", "SS Recall", "WMU")
tbl_cor$task_out <- factor(tbl_cor$task_out)
levels(tbl_cor$task_out) <- c("OS Processing", "OS Recall", "SS Processing", "SS Recall", "WMU")

# between-task correlations per session
pl_within_session_cors <- tbl_cor %>%
  mutate(
    session_id_in = as.numeric(session_id_in),
    session_id_out = as.numeric(session_id_out)
  ) %>%
  filter(session_id_in == session_id_out) %>%
  ggplot(aes(task_in, task_out)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "white", size = 5) +
  scale_fill_viridis_c(name = "Correlation", guide = "none") +
  facet_wrap(~ session_id_in) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "", title = "Within-Session Correlations") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90)
  )
save_my_pdf(pl_within_session_cors, "figures/EDA/cors-within-session.pdf", 10, 6)

# task correlations between sessions with reliabilities highlighted
tbl_between <- tbl_cor %>%
  filter(session_id_in != session_id_out & session_id_in == 1)

pl_between_session_cors <- ggplot(tbl_between, aes(task_in, task_out)) +
  geom_tile(aes(fill = value)) +
  # highlight diagonal
  geom_tile(data = tbl_between %>% filter(task_in == task_out), aes(task_in, task_out), color = "black", alpha = 0, linewidth = 1.5) +
  geom_text(aes(label = round(value, 2)), color = "white", size = 5) +
  scale_fill_viridis_c(name = "Correlation", guide = "none") +
  scale_color_viridis_c(direction = -1) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Session 1", y = "Session 2", 
    title = "Between-Session Correlations",
    caption = "Reliabilities on the diagonal are highlighted"
  ) + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(size = 10)
  )
save_my_pdf(pl_between_session_cors, "figures/EDA/cors-between-session.pdf", 7.25, 7.25)

## Save recall, and processing files of included participants --------------


saveRDS(tbl_performance_all, file = "data/all-data/tbl-performance-wm-all-s2.rds")

tbl_performance_all_clean <- tbl_performance_all %>%
  select(-c(
    thx_lo_os, thx_lo_ss, excl_os, excl_ss, proc_below_thx, exclude
  ))

write_csv(tbl_performance_all_clean, "data/all-data/wm-performance.csv")
write_csv(tbl_cor, "data/all-data/wm-reliabilities-correlations.csv")






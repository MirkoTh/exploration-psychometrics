rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)

path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]
participants_returned <- list()

time_period <- c(make_datetime(2023, 11, 30, 13), make_datetime(2023, 12, 5))


json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}
# check for each participant which file has more data and select that one
hash_ids <- function(path_data, participants_returned, add_gender = FALSE, time_period = NULL) {
  #' hash prolific ids and save data from wm tasks
  #' 
  #' @description loads data from json files and writes to csv with
  #' prolific ids replaced by random ids; writes hash table to csv as well
  #' @param path_data sub-folder with batch data
  #' @param participants_returned list with returned and rejected prolific
  
  #'  
  #' @return nothing, just writes
  #' 
  # read individual performance
  
  # check for each participant which file has more data and select that one
  if (is.null(time_period)){time_period <- c(now() - years(100), now())}
  
  
  # subset time range
  files_dir_OS <- dir(str_c(path_data, "OS"))
  mtime_OS <- map(str_c(path_data, "OS/", files_dir_OS), ~ file.info(.x)$mtime)
  in_range_OS <- map_lgl(mtime_OS, between, time_period[1], time_period[2])
  files_dir_OS <- files_dir_OS[in_range_OS]
  
  fld_OS_recall <- files_dir_OS[str_detect(files_dir_OS, "recall")]
  fld_OS_processing <- files_dir_OS[str_detect(files_dir_OS, "processing")]
  fld_OS_bonus <- files_dir_OS[str_detect(files_dir_OS, "bonus")]
  fld_OS_cc <- files_dir_OS[str_detect(files_dir_OS, "comprehension")]
  
  # subset time range
  files_dir_SS <- dir(str_c(path_data, "SS"))
  mtime_SS <- map(str_c(path_data, "SS/", files_dir_SS), ~ file.info(.x)$mtime)
  in_range_SS <- map_lgl(mtime_SS, between, time_period[1], time_period[2])
  files_dir_SS <- files_dir_SS[in_range_SS]
  
  fld_SS_recall <- files_dir_SS[str_detect(files_dir_SS, "recall")]
  fld_SS_processing <- files_dir_SS[str_detect(files_dir_SS, "processing")]
  fld_SS_bonus <- files_dir_SS[str_detect(files_dir_SS, "bonus")]
  fld_SS_cc <- files_dir_SS[str_detect(files_dir_SS, "comprehension")]
  
  # subset time range
  files_dir_WMU <- dir(str_c(path_data, "WMU"))
  mtime_WMU <- map(str_c(path_data, "WMU/", files_dir_WMU), ~ file.info(.x)$mtime)
  in_range_WMU <- map_lgl(mtime_WMU, between, time_period[1], time_period[2])
  files_dir_WMU <- files_dir_WMU[in_range_WMU]
  
  fld_WMU_recall <- files_dir_WMU[!str_detect(files_dir_WMU, "comprehension") & !str_detect(files_dir_WMU, "bonus")]
  fld_WMU_bonus <- files_dir_WMU[str_detect(files_dir_WMU, "bonus")]
  fld_WMU_cc <- files_dir_WMU[str_detect(files_dir_WMU, "comprehension")]
  
  
  extract_compound_and_individual <- function(fld, task) {
    paths_individual <- str_c(path_data, task, "/", fld[!str_detect(fld, "allinone")])
    paths_compound <- str_c(path_data, task, "/", fld[str_detect(fld, "allinone")])
    return (list(indiv = paths_individual, compound = paths_compound))
  }
  
  flds <- list(
    fld_OS_recall, fld_OS_processing, fld_OS_bonus,
    fld_SS_recall, fld_SS_processing, fld_SS_bonus,
    fld_WMU_recall, fld_WMU_bonus
  )
  tasks <- c("OS", "OS", "OS", "SS", "SS", "SS", "WMU", "WMU")
  l_paths <- map2(flds, tasks, extract_compound_and_individual)
  
  names(l_paths) <- c(
    "OS-recall", "OS-processing", "OS-bonus",
    "SS-recall", "SS-processing", "SS-bonus",
    "WMU-recall", "WMU-bonus"
  )
  
  
  
  inner_map <- safely(function(x) map(x, json_to_tibble))
  l_tbl_WMU_recall_indiv <- map(map(l_paths$`WMU-recall`$indiv, inner_map), "result")
  l_tbl_WMU_recall_compound <- map(map(l_paths$`WMU-recall`$compound, inner_map), "result")
  l_tbl_WMU_bonus <- map(map(l_paths$`WMU_bonus`$indiv, inner_map), "result")
  
  l_tbl_OS_recall_indiv <- map(map(l_paths$`OS-recall`$indiv, inner_map), "result")
  l_tbl_OS_recall_compound <- map(map(l_paths$`OS-recall`$compound, inner_map), "result")
  l_tbl_OS_processing_indiv <- map(map(l_paths$`OS-processing`$indiv, inner_map), "result")
  l_tbl_OS_processing_compound <- map(map(l_paths$`OS-recall`$compound, inner_map), "result")
  l_tbl_OS_bonus <- map(map(l_paths$`OS-bonus`$indiv, inner_map), "result")
  
  l_tbl_SS_recall_indiv <- map(map(l_paths$`SS-recall`$indiv, inner_map), "result")
  l_tbl_SS_recall_compound <- map(map(l_paths$`SS-recall`$compound, inner_map), "result")
  l_tbl_SS_processing_indiv <- map(map(l_paths$`SS-processing`$indiv, inner_map), "result")
  l_tbl_SS_processing_compound <- map(map(l_paths$`SS-recall`$compound, inner_map), "result")
  l_tbl_SS_bonus <- map(map(l_paths$`SS-bonus`$indiv, inner_map), "result")
  
  select_more_rows <- function(a, b) {
    nrows_a <- nrow(a[[1]])
    nrows_b <- nrow(b[[1]])
    if (is_empty(nrows_a)) nrows_a <- 0
    if(nrows_a > nrows_b) {
      out <- a[[1]]
    } else {out <- b[[1]]}
    out$participant_id <- as.character(out$participant_id)
    return(out)
  }
  
  tbl_OS_recall <- map2_df(l_tbl_OS_recall_compound, l_tbl_OS_recall_indiv, select_more_rows)
  tbl_OS_processing <- map2_df(l_tbl_OS_processing_compound, l_tbl_OS_processing_indiv, select_more_rows)
  #tbl_OS_bonus <- reduce(map(l_tbl_OS_bonus, 1), rbind)
  
  tbl_SS_recall <- map2_df(l_tbl_SS_recall_compound, l_tbl_SS_recall_indiv, select_more_rows)
  tbl_SS_processing <- map2_df(l_tbl_SS_processing_compound, l_tbl_SS_processing_indiv, select_more_rows)
  #tbl_SS_bonus <- reduce(map(l_tbl_SS_bonus, 1), rbind)
  
  tbl_WMU_recall <- map2_df(l_tbl_WMU_recall_compound, l_tbl_WMU_recall_indiv, select_more_rows)
  #tbl_WMU_bonus <- reduce(map(l_tbl_WMU_bonus, 1), rbind)
  
  
  
  
  # add gender
  if (add_gender == TRUE) {
    fls_all <- list.files("experiments/2022-07-category-learning-II/data/")
    fl_start <- "prolific_export"
    pth <- str_c("experiments/2022-07-category-learning-II/data/", fls_all[startsWith(fls_all, fl_start)])
    l <- map(pth, read_csv)
    tbl_prolific <- reduce(l, rbind)
    tbl_cr <- tbl_cr %>% 
      left_join(tbl_prolific[, c("Participant id", "Sex")], by = c("participant_id" = "Participant id"))
  }
  
  # create a lookup table mapping prolific ids to random ids
  tbl_ids_lookup <- tibble(participant_id = unique(tbl_WMU_recall$participant_id))
  tbl_ids_lookup <- tbl_ids_lookup %>%
    group_by(participant_id) %>%
    mutate(participant_id_randomized = random_id(1)) %>% ungroup()
  write_csv(tbl_ids_lookup, str_c(path_data, "participant-lookup.csv"))
  
  
  # replace prolific ids with random ids
  replace_prolific_id <- function(my_tbl) {
    my_tbl %>% left_join(tbl_ids_lookup, by = "participant_id") %>%
      select(-participant_id) %>% rename(participant_id = participant_id_randomized)
  }
  
  tbl_OS_recall <- replace_prolific_id(tbl_OS_recall)
  tbl_OS_processing <- replace_prolific_id(tbl_OS_processing)
  #tbl_OS_bonus <- replace_prolific_id(tbl_OS_bonus)
  
  tbl_SS_recall <- replace_prolific_id(tbl_SS_recall)
  tbl_SS_processing <- replace_prolific_id(tbl_SS_processing)
  #tbl_SS_bonus <- replace_prolific_id(tbl_SS_bonus)
  
  tbl_WMU_recall <- replace_prolific_id(tbl_WMU_recall)
  #tbl_WMU_bonus <- replace_prolific_id(tbl_WMU_bonus)
  
  # exclude returned and rejected participants
  tbl_OS_recall <- tbl_OS_recall %>% filter(!(participant_id %in% participants_returned))
  tbl_OS_processing <- tbl_OS_processing %>% filter(!(participant_id %in% participants_returned))
  tbl_SS_recall <- tbl_SS_recall %>% filter(!(participant_id %in% participants_returned))
  tbl_SS_processing <- tbl_SS_processing %>% filter(!(participant_id %in% participants_returned))
  tbl_WMU_recall <- tbl_WMU_recall %>% filter(!(participant_id %in% participants_returned))
  
  write_csv(tbl_OS_recall, str_c(path_data, "tbl_OS_recall.csv"))
  write_csv(tbl_OS_processing, str_c(path_data, "tbl_OS_processing.csv"))
  #write_csv(tbl_OS_bonus, str_c(path_data, "tbl_OS_bonus.csv"))
  
  write_csv(tbl_SS_recall, str_c(path_data, "tbl_SS_recall.csv"))
  write_csv(tbl_SS_processing, str_c(path_data, "tbl_SS_processing.csv"))
  #write_csv(tbl_SS_bonus, str_c(path_data, "tbl_SS_bonus.csv"))
  
  write_csv(tbl_WMU_recall, str_c(path_data, "tbl_WMU_recall.csv"))
  #write_csv(tbl_WMU_bonus, str_c(path_data, "tbl_WMU_bonus.csv"))
  
}


hash_ids(path_data, participants_returned, time_period = time_period)

tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup.csv"))

#tbl_OS_bonus <- read_csv(str_c(path_data, "tbl_OS_bonus.csv"))
tbl_OS_recall <- read_csv(str_c(path_data, "tbl_OS_recall.csv"))
tbl_OS_processing <- read_csv(str_c(path_data, "tbl_OS_processing.csv"))

#tbl_SS_bonus <- read_csv(str_c(path_data, "tbl_SS_bonus.csv"))
tbl_SS_recall <- read_csv(str_c(path_data, "tbl_SS_recall.csv"))
tbl_SS_processing <- read_csv(str_c(path_data, "tbl_SS_processing.csv"))

#tbl_WMU_bonus <- read_csv(str_c(path_data, "tbl_WMU_bonus.csv"))
tbl_WMU_recall <- read_csv(str_c(path_data, "tbl_WMU_recall.csv"))

tbl_os_agg <- tbl_OS_recall %>% 
  left_join(
    tbl_ids_lookup, 
    by = c("participant_id" = "participant_id_randomized"), 
    suffix = c("_rand", "_orig")
  ) %>%
  filter(is_practice == 0) %>%
  mutate(
    prop_correct = n_correct / set_size
  ) %>%
  group_by(participant_id_orig, set_size) %>%
  summarize(
    n_trials = n(),
    n_items = sum(set_size),
    n_correct = sum(n_correct),
    prop_correct_trial = mean(prop_correct)
  ) %>%
  ungroup() %>%
  mutate(
    task = "OS",
    prop_correct_total = n_correct / sum(4:8*3)
  )

tbl_ss_agg <- tbl_SS_recall %>% 
  left_join(
    tbl_ids_lookup, 
    by = c("participant_id" = "participant_id_randomized"), 
    suffix = c("_rand", "_orig")
  ) %>%
  filter(is_practice == 0)  %>%
  mutate(
    prop_correct = n_correct / set_size
  ) %>%
  group_by(participant_id_orig, set_size) %>%
  summarize(
    n_trials = n(),
    n_items = sum(set_size),
    n_correct = sum(n_correct),
    prop_correct_trial = mean(prop_correct)
  ) %>%
  mutate(
    task = "SS",
    prop_correct_total = n_correct / sum(3:6*3)
  ) %>%
  ungroup()

tbl_wmu_agg <- tbl_WMU_recall %>% 
  filter(is_practice == 0) %>%
  mutate(prop_correct = n_correct / set_size) %>%
  left_join(
    tbl_ids_lookup, 
    by = c("participant_id" = "participant_id_randomized"), 
    suffix = c("_rand", "_orig")
  ) %>%
  group_by(participant_id_orig, set_size) %>%
  summarize(
    n_trials = n(),
    n_items = sum(set_size),
    n_correct = sum(n_correct),
    prop_correct_trial = mean(prop_correct)
  ) %>%  mutate(
    task = "WMU",
    prop_correct_total = n_correct / sum(25*4)
  )%>%
  ungroup()


# for every set size separately
tbl_os_ss_agg_ci <- summary_se_within(tbl_os_agg, "prop_correct_trial", withinvars = c("set_size", "task"), idvar = "participant_id_orig") %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_ss_ss_agg_ci <- summary_se_within(tbl_ss_agg, "prop_correct_trial", withinvars = c("set_size", "task"), idvar = "participant_id_orig") %>%
  mutate(set_size = as.numeric(as.character(set_size)))
tbl_wmu_ss_agg_ci <- summary_se(tbl_wmu_agg, "prop_correct_trial", groupvars = c("set_size", "task")) %>%
  mutate(set_size = as.numeric(as.character(set_size)))

tbl_os_participant_agg <- grouped_agg(tbl_os_agg, c(participant_id_orig, task), prop_correct_trial)
tbl_ss_participant_agg <- grouped_agg(tbl_ss_agg, c(participant_id_orig, task), prop_correct_trial)
tbl_wmu_participant_agg <- grouped_agg(tbl_wmu_agg, c(participant_id_orig, task), prop_correct_trial)

tbl_performance_participants <- tbl_os_participant_agg %>%
  select(participant_id_orig, task, mean_prop_correct_trial) %>%
  rbind(
    tbl_ss_participant_agg %>% select(participant_id_orig, task, mean_prop_correct_trial)
  ) %>% rbind(
    tbl_wmu_participant_agg %>% select(participant_id_orig, task, mean_prop_correct_trial)
  ) %>% pivot_wider(id_cols = participant_id_orig, names_from = task, values_from = mean_prop_correct_trial)

cor(tbl_performance_participants[, c("OS", "SS", "WMU")] %>%
      filter(
        !is.na(WMU))
      )

ggplot(rbind(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci), aes(set_size, prop_correct_trial, group = 1)) +
  geom_errorbar(aes(x = set_size, y = prop_correct_trial, ymin = prop_correct_trial - ci, ymax = prop_correct_trial + ci), width = .5) +
  geom_line() +
  geom_point(color = "white", size = 4) +
  geom_point() +
  geom_errorbar(
    data = tbl_wmu_ss_agg_ci, aes(
      x = set_size, y = prop_correct_trial, ymin = prop_correct_trial - ci, ymax = prop_correct_trial + ci
    ), width = .5
  ) +
  geom_point(data = tbl_wmu_ss_agg_ci, aes(set_size, prop_correct_trial, group = 1)) +
  facet_wrap(~ task) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Set Size", y = "Proportion Correct") + 
  theme(strip.background = element_rect(fill = "white"))



tbl_all_agg <- rbind(
  tbl_os_agg,
  tbl_ss_agg,
  tbl_wmu_agg
) %>% group_by(task, participant_id_orig) %>%
  arrange(prop_correct_trial) %>%
  mutate(participant_id_orig = factor(participant_id_orig))


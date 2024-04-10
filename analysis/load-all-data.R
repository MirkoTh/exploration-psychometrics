rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)
library(grid)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)





# Load WM Data ------------------------------------------------------------


path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]

# these are the nr. trials that were administed in the experiment
tbl_trials_administered <- tibble(
  os_recall = 15,
  ss_recall = 12,
  os_processing = 90,
  ss_processing = 50, #54, # would actually be 54, but accept a few missing responses
  wmu_recall = 20
) %>% pivot_longer(colnames(.))
colnames(tbl_trials_administered) <- c("task", "n_administered")

returned_and_timedout <- return_n_timeout()


## Insert Time Range and Session ID ----------------------------------------


# hash ids either randomly or using Kristin's mapping (i.e., 1:n_participants)

# load and save data from time point I
time_period_wave_I <- c(
  make_datetime(2024, 1, 23, 11, tz = "CET"), 
  make_datetime(2024, 1, 24, 23, 30, tz = "CET")
)

hash_ids(
  path_data, 
  c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
  time_period = time_period_wave_I,
  random_hashes = FALSE, session_id = 0
)


# load and save data from time point II
time_period_wave_II <- c(
  make_datetime(2024, 3, 4, 13, tz = "CET"), 
  make_datetime(2024, 3, 11, 4, tz = "CET")
)

hash_ids(
  path_data, 
  c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
  time_period = time_period_wave_II,
  random_hashes = FALSE, session_id = 1
)

# lookup table from first session contains all ids
tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup-0.csv"))
l_tbl_wm_data <- load_wm_data()





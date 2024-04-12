rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)
library(grid)
library(jsonlite)

path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)


first_time <- FALSE # first time script is run? if not, csv can directly be loaded


# Load WM Data ------------------------------------------------------------


path_data <- c("data/2023-11-lab-pilot/", "data/all-data/")[2]

returned_and_timedout <- return_n_timeout()


## Insert Time Range and Session ID ----------------------------------------

time_period_wave_I <- c(
  make_datetime(2024, 1, 23, 11, tz = "CET"), 
  make_datetime(2024, 1, 24, 23, 30, tz = "CET")
)
time_period_wave_II <- c(
  make_datetime(2024, 3, 4, 13, tz = "CET"), 
  make_datetime(2024, 3, 11, 4, tz = "CET")
)

if (first_time) {
  
  # hash ids either randomly or using Kristin's mapping (i.e., 1:n_participants)
  
  # load and save data from time point I
  
  hash_ids(
    path_data, 
    c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
    time_period = time_period_wave_I,
    random_hashes = FALSE, session_id = 0
  )
  
  
  # load and save data from time point II
  
  hash_ids(
    path_data, 
    c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
    time_period = time_period_wave_II,
    random_hashes = FALSE, session_id = 1
  )
  
}

# lookup table from first session contains all ids
tbl_ids_lookup <- read_csv(str_c(path_data, "participant-lookup-0.csv"))

# load all wm data
l_tbl_wm_data <- load_wm_data()

list2env(l_tbl_wm_data, environment())



# Load Bandit & Questionnaire Data ----------------------------------------


if (first_time) {
  load_bandit_and_questionnaire_data(1, time_period_wave_I)
  load_bandit_and_questionnaire_data(2, time_period_wave_II)
}


# bandit 
load("analysis/bandits/banditsWave1full.Rda")
sam1 <- sam
horizon1 <- horizon
restless1 <- restless
load("analysis/bandits/banditsWave2full.Rda")

# questionnaires
load("analysis/qswave1Full.Rda")
qdat1 <- qdat
load("analysis/qswave2Full.Rda")



# apply exclusion criteria ------------------------------------------------


eda_and_exclusion_criteria_bandits(session = 1)
eda_and_exclusion_criteria_bandits(session = 2)

tbl_excl_bandits_1 <- read_csv("data/exclusions1.csv")
tbl_excl_bandits_2 <- read_csv("data/exclusions2.csv")

tbl_excl_wm <- readRDS("analysis/wm/subjects-excl-wm.rds")

tbl_excl_all <- tbl_excl_wm %>% left_join(tbl_excl_bandits_2, by = c("prolific_pid" = "PID"))

# combine data from two sessions
tbl_horizon <- as_tibble(rbind(horizon, horizon1))
tbl_sam <- as_tibble(rbind(sam, sam1))
tbl_restless <- as_tibble(rbind(restless, restless1))
tbl_qdat <- as_tibble(rbind(qdat, qdat1))

# exclude participants below any type of thx
tbl_horizon <- tbl_horizon %>% 
  left_join(tbl_excl_all %>% select(c(ID, excl_subject, exclude))) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude))

tbl_sam <- tbl_sam %>% 
  left_join(tbl_excl_all %>% select(c(ID, excl_subject, exclude))) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude))

tbl_restless <- tbl_restless %>% 
  left_join(tbl_excl_all %>% select(c(ID, excl_subject, exclude))) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude))

tbl_qdat <- qdat %>% 
  left_join(tbl_excl_all %>% select(c(ID, excl_subject, exclude))) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude))

save(tbl_horizon, tbl_sam, tbl_restless, file ="analysis/bandits/both-waves.Rda")
save(tbl_horizon, tbl_sam, tbl_restless, file ="analysis/bandits-both-waves.Rda")
save(tbl_qdat, file = "analysis/qs-both-waves.Rda")


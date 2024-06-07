rm(list = ls())

library(tidyverse)
library(ids)
library(rutils)
library(grid)
library(jsonlite)

tryCatch(
  error = function(cnd) cat("do not care"),
  setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")
  
)
path_utils <- c("utils/analysis-utils.R", "utils/plotting-utils.R")
walk(path_utils, source)


first_time <- FALSE # TRUE # first time script is run? if not, csv can directly be loaded

# create general lookup with ALL prolific IDs and corresponding anonymous IDs
if (first_time){
  
  IDs <- c()
  # grab all IDs 
  for (folder in c("OS", "SS", "WMU", "qs", "bandits")){
    files <- list.files(paste("data/wave1/", folder, sep = ""))
    
    files <- subset(files, apply(file.info(paste("data/wave1/", folder, "/", ))))
    
    ID <- apply(as.array(files), 1, function(x) parse_out_ID(x, folder))
    
    ID <- subset(ID, ID != "test")
    
    IDs <- unique(c(IDs, ID))
    
  }
    
  lookup <- data.frame(PID = IDs,
                       ID = 1:length(IDs))
    
  write.csv(lookup, "data/FINALlookup.csv")
  
  
}





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
    "data/wave1/", 
    c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
    time_period = NULL,
    random_hashes = FALSE, session_id = 0
  )
  
  
  # load and save data from time point II
  
  hash_ids(
    "data/wave2/", 
    c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
    time_period = NULL,
    random_hashes = FALSE, session_id = 1
  )
  
}

# lookup table from first session contains all ids
tbl_ids_lookup <- read_csv(str_c("data/FINALlookup.csv"))

# load all wm data
l_tbl_wm_data <- load_wm_data()

list2env(l_tbl_wm_data, environment())



# Load Bandit & Questionnaire Data ----------------------------------------


if (first_time) {
  load_bandit_and_questionnaire_data(1, time_period_wave_I)
  load_bandit_and_questionnaire_data(2, time_period_wave_II)
}


# bandit 
load("data/wave1/banditsWave1full.Rda")
sam1 <- sam
horizon1 <- horizon
restless1 <- restless
load("data/wave2/banditsWave2full.Rda")

# questionnaires
load("data/wave1/qswave1Full.Rda")
qdat1 <- qdat
load("data/wave2/qswave2Full.Rda")


# combine data from two sessions
tbl_horizon <- as_tibble(rbind(horizon, horizon1))
tbl_sam <- as_tibble(rbind(sam, sam1))
tbl_restless <- as_tibble(rbind(restless, restless1))
tbl_qdat <- as_tibble(rbind(qdat, qdat1))


# apply exclusion criteria ------------------------------------------------

# exclusions.csvs are written in eda_and_ex...

eda_and_exclusion_criteria_bandits(session = 1)
eda_and_exclusion_criteria_bandits(session = 2)










# I don't know what happens here but it seems duplicate to me. @Mirko?


source("analysis/wm/eda-reliability-wm-tasks.R")

tbl_excl_bandits_1 <- read_csv("data/exclusions1.csv")
tbl_excl_bandits_2 <- read_csv("data/exclusions2.csv")

tbl_excl_wm <- readRDS("analysis/wm/subjects-excl-wm.rds")

tbl_excl_all <- tbl_excl_wm %>% 
  left_join(
    tbl_excl_bandits_1 %>% select(PID, exclude), 
    by = c("prolific_pid" = "PID")) %>%
  left_join(
    tbl_excl_bandits_2 %>% select(PID, exclude),
    by = c("prolific_pid" = "PID"), suffix = c("_1", "_2")
    ) %>%
  mutate(
    exclude = exclude_1 + exclude_2 + proc_below_thx + all_tasks_too_few
  ) %>%
  rename(ID = participant_id)
tbl_excl_all %>% count(exclude)

tbl_performance_wm <- readRDS("data/all-data/tbl-performance-wm-all-s2.rds") %>% select(-excl_subject) %>% rename(ID = participant_id)
tbl_performance_wm <- tbl_performance_wm %>% 
  left_join(tbl_excl_all %>% select(c(ID, excl_subject, exclude))) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude))


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

# these are data frames with only included participants
save(tbl_horizon, tbl_sam, tbl_restless, file ="analysis/bandits/both-waves.Rda")
save(tbl_horizon, tbl_sam, tbl_restless, file ="analysis/bandits-both-waves.Rda")
save(tbl_qdat, file = "analysis/qs-both-waves.Rda")
save(tbl_performance_wm, file = "analysis/wm-performance.Rda")


tbl_os_recall %>%
  left_join(
    tbl_excl_all %>% select(c(ID, excl_subject)) %>% filter(!is.na(ID)), 
    by = c("participant_id" = "ID")) %>% 
  filter(excl_subject == 0 & exclude == 0) %>%
  select(-c(excl_subject, exclude)) %>%
  count(participant_id) %>% count()
























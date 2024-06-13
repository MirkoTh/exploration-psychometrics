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
    time_period = time_period_wave_I,
    random_hashes = FALSE, session_id = 0
  )
  
  
  # load and save data from time point II
  
  hash_ids(
    "data/wave2/", 
    c(returned_and_timedout$participants_returned, returned_and_timedout$participants_timeout), 
    time_period = time_period_wave_II,
    random_hashes = FALSE, session_id = 1
  )
  
}

# copy files over to all-data folder
path_data1 <- "data/wave1/"
path_data2 <- "data/wave2/"

move_to_same_folder <- function(path_data) {
  pths <- dir(path_data)
  files_copy_bool <- str_detect(pths, "^tbl_[OS,SS,WMU]")
  pth_origin <- str_c(path_data, pths[files_copy_bool])
  pth_target <- str_c("data/all-data/", pths[files_copy_bool])
  walk2(pth_origin, pth_target, ~ file.copy(.x, .y, overwrite = TRUE))
}

walk(c(path_data1, path_data2), move_to_same_folder)


# lookup table from first session contains all ids
tbl_ids_lookup <- read_csv(str_c("data/FINALlookup.csv"))

# load all wm data
l_tbl_wm_data <- load_wm_data("data/all-data/")

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



exclusion_criteria(session = 1)
exclusion_criteria(session = 2)



##### testing exclusion stuff
criterions <- colnames(lookup)[grep("incomplete", colnames(lookup)):ncol(lookup)]
criterions <- subset(criterions, !grepl("xclude", criterions))
print(criterions)
testing <- data.frame(criterion = criterions,
                      N_ex = NA)

tem <- subset(lookup, totalExclude == 1)
for (i in criterions){
  temp <- tem[ ,colnames(tem) == i]
  testing$N_ex[testing$criterion == i] <- sum(as.numeric(temp), na.rm = T)
}

print(testing)


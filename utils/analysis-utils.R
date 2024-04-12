json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}
# check for each participant which file has more data and select that one
hash_ids <- function(
    path_data, 
    participants_returned, 
    add_gender = FALSE, 
    time_period = NULL, 
    is_considered = c("OS", "SS", "WMU"),
    random_hashes = TRUE,
    session_id = 0
) {
  #' hash prolific ids and save data from wm tasks
  #' 
  #' @description loads data from json files and writes to csv with
  #' prolific ids replaced by random ids; writes hash table to csv as well
  #' @param path_data sub-folder with batch data
  #' @param participants_returned list with returned and rejected prolific
  #' @param add_gender should gender be added from the prolific data, defaults to FALSE and is currently not implemented
  #' @param time_period the time period considered for loading the .json files
  #' @param is_considered the task to load the data from, default to all three tasks
  #' @param random_hashes should prolific ids be replaced by random ids or 
  #' is a file with an already existing mapping from prolific id to another id available?
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
  l_tbl_OS_processing_compound <- map(map(l_paths$`OS-processing`$compound, inner_map), "result")
  l_tbl_OS_bonus <- map(map(l_paths$`OS-bonus`$indiv, inner_map), "result")
  
  l_tbl_SS_recall_indiv <- map(map(l_paths$`SS-recall`$indiv, inner_map), "result")
  l_tbl_SS_recall_compound <- map(map(l_paths$`SS-recall`$compound, inner_map), "result")
  l_tbl_SS_processing_indiv <- map(map(l_paths$`SS-processing`$indiv, inner_map), "result")
  l_tbl_SS_processing_compound <- map(map(l_paths$`SS-processing`$compound, inner_map), "result")
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
  
  session_id_as_num <- function(x) {
    x[[1]]$session_id <- as.numeric(x[[1]]$session_id)
    return(x)
  }
  l_tbl_WMU_recall_indiv <- map(l_tbl_WMU_recall_indiv, session_id_as_num)
  l_tbl_WMU_recall_compound <- map(l_tbl_WMU_recall_compound, session_id_as_num)
  
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
  if (random_hashes) {
    tbl_ids_lookup <- tbl_ids_lookup %>%
      filter(!(participant_id %in% participants_returned)) %>% 
      group_by(participant_id) %>%
      mutate(participant_id_randomized = random_id(1)) %>% ungroup()
  } else {
    tmp <- read_csv("analysis/bandits/exclusions-session-i.csv") %>%
      rename(participant_id = PID, participant_id_randomized = ID) %>%
      select(-"...1")
    tbl_ids_lookup <- tbl_ids_lookup %>% 
      left_join(tmp, by = c("participant_id"))
  }
  write_csv(tbl_ids_lookup, str_c(path_data, "participant-lookup-", session_id, ".csv"))
  
  
  # replace prolific ids with random ids
  replace_prolific_id <- function(my_tbl) {
    my_tbl %>% left_join(tbl_ids_lookup, by = "participant_id") %>%
      select(-participant_id) %>% rename(participant_id = participant_id_randomized)
  }
  
  if ("OS" %in% is_considered) {
    # exclude returned and rejected participants
    tbl_OS_recall <- tbl_OS_recall %>% filter(!(participant_id %in% participants_returned))
    tbl_OS_processing <- tbl_OS_processing %>% filter(!(participant_id %in% participants_returned))
    
    tbl_OS_recall <- replace_prolific_id(tbl_OS_recall)
    tbl_OS_processing <- replace_prolific_id(tbl_OS_processing)
    #tbl_OS_bonus <- replace_prolific_id(tbl_OS_bonus)
    
    
    # save
    write_csv(tbl_OS_recall, str_c(path_data, "tbl_OS_recall_", session_id, ".csv"))
    write_csv(tbl_OS_processing, str_c(path_data, "tbl_OS_processing_", session_id, ".csv"))
    saveRDS(tbl_OS_recall, str_c(path_data, "tbl_OS_recall_", session_id, ".rds"))
    #write_csv(tbl_OS_bonus, str_c(path_data, "tbl_OS_bonus.csv"))
  }
  
  if ("SS" %in% is_considered) {
    # exclude returned and rejected participants
    tbl_SS_recall <- tbl_SS_recall %>% filter(!(participant_id %in% participants_returned))
    tbl_SS_processing <- tbl_SS_processing %>% filter(!(participant_id %in% participants_returned))
    
    tbl_SS_recall <- replace_prolific_id(tbl_SS_recall)
    tbl_SS_processing <- replace_prolific_id(tbl_SS_processing)
    #tbl_SS_bonus <- replace_prolific_id(tbl_SS_bonus)
    
    
    # save
    write_csv(tbl_SS_recall, str_c(path_data, "tbl_SS_recall_", session_id, ".csv"))
    write_csv(tbl_SS_processing, str_c(path_data, "tbl_SS_processing_", session_id, ".csv"))
    saveRDS(tbl_SS_recall, str_c(path_data, "tbl_SS_recall_", session_id, ".RDS"))
    #write_csv(tbl_SS_bonus, str_c(path_data, "tbl_SS_bonus.csv"))
  }
  
  if ("WMU" %in% is_considered) {
    # exclude returned and rejected participants
    tbl_WMU_recall <- tbl_WMU_recall %>% filter(!(participant_id %in% participants_returned))
    
    tbl_WMU_recall <- replace_prolific_id(tbl_WMU_recall)
    #tbl_WMU_bonus <- replace_prolific_id(tbl_WMU_bonus)
    
    # save
    write_csv(tbl_WMU_recall, str_c(path_data, "tbl_WMU_recall_", session_id, ".csv"))
    saveRDS(tbl_WMU_recall, str_c(path_data, "tbl_WMU_recall_", session_id, ".RDS"))
    #write_csv(tbl_WMU_bonus, str_c(path_data, "tbl_WMU_bonus.csv"))
  }
}


agg_by_ss <- function(my_tbl, tbl_ids_lookup, taskname) {
  my_tbl %>% 
    mutate(
      prop_correct = n_correct / set_size
    ) %>%
    group_by(participant_id, session_id, set_size) %>%
    summarize(
      n_trials = n(),
      n_items = sum(set_size),
      n_correct = sum(n_correct),
      prop_correct = mean(prop_correct)
    ) %>%
    ungroup() %>%
    mutate(
      task = taskname
    )
}

return_n_timeout <- function() {
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
    "63d8074daa40629318ada75f",
    "65905ceb2330f2dae2ddca37",
    "65957cbb286733b37b46dfa4",
    "5a538952f6c517000194916e",
    "60455b1578f6f36a3e8ea81c",
    "6310c063bd0cce86b0289d20",
    "5bcfde8c8301810001a33bea",
    "6090755b3aebf491674a5e86",
    "60b0f62eb8e276ae78fd6180",
    "60e10e95d56310858005f994",
    "5e299d8c2e9dc4000be0fde0",
    "5b258c9ba7cee100011d8aad",
    "63ced717cb553d29a3beac43",
    "5fb6b4fee395490d9f90d513",
    "5e9607da6b866106f5666075",
    "61193d8ce8940db66d5d632e",
    "57dd186e6598aa0001992616",
    "60fd666391dc3d42dc1b1827",
    "62fe6312269d0e4ae894297b",
    "63d152b604e8c21933a58a66",
    "62e1581acad06eee3b6509dc",
    "5fe9284c2c2ee63925e41e5d",
    "6361bfe81e0da4a406b33f1e",
    "57619c099fce230007797320",
    "5f276d8f38b9f857f5cebf21",
    "55b2d3f2fdf99b525bc839aa",
    "600db78b56935a5dd8c759b3",
    "5c01d3c509e9c70001500b10",
    "5e06ecd524f9390991997bb7",
    "5d1a69d8f97a18001977e0d7",
    "648af2818c1b1e60ed0ff001",
    "63d7bea144b8a11f63c2a763",
    "6141f62294ff9bf9d89e8897",
    "5e77a65dcce2a2319f9e8849",
    "5c92f797803bff0017fef8dd",
    "6127c5e5db9bf1a49993c300",
    "6173240b98f6e6f48d97880a",
    "5df3c82961b48a2907204173",
    "5eb6d09db2c9c85dd30dc94e",
    "55eb04337480920010aa9e0d",
    "659f2da91adc00ba10f77f55",
    "5eb8c284ab41e18278ccf247",
    "5dfdc45e617219a8e2e05c26",
    "62bb42e2f21cfdb280cc975f",
    "613dd5fb512aabe8d5d32393",
    "6073048f6aaf87a50bfa9787"
    
  )
  time_out_exclude <- c(
    "56b6e2fcf2fc70000c32f9d5",
    "571a78f06a1c6300114b8d80",
    "5f7f18137488a50407d729e6"
  )
  
  return(list(
    participants_returned = participants_returned,
    participants_timeout = time_out_exclude
  ))
  
}

load_wm_data <- function() {
  #' load data from three wm tasks
  #' 
  #' @description loads data from csv files, excludes practice trials, and
  #' adds processing position to processing task file
  # OS
  tbl_os_recall <- readRDS(str_c(path_data, "tbl_OS_recall_0.RDS")) %>%
    filter(is_practice == 0) %>% rbind(
      readRDS(str_c(path_data, "tbl_OS_recall_1.RDS")) %>%
        filter(is_practice == 0)
    ) %>% mutate(session_id = as.numeric(session_id))
  tbl_os_processing <- read_csv(str_c(path_data, "tbl_OS_processing_0.csv")) %>%
    filter(is_practice == 0) %>% rbind(
      read_csv(str_c(path_data, "tbl_OS_processing_1.csv")) %>%
        filter(is_practice == 0)
    )
  
  # SS
  # exclude practice trials and add missing variables
  tbl_ss_recall <- readRDS(str_c(path_data, "tbl_SS_recall_0.RDS")) %>%
    filter(is_practice == 0) %>% rbind(
      readRDS(str_c(path_data, "tbl_SS_recall_1.RDS")) %>%
        filter(is_practice == 0) 
    ) %>% mutate(session_id = as.numeric(session_id))
  tbl_ss_processing <- read_csv(str_c(path_data, "tbl_SS_processing_0.csv")) %>%
    filter(is_practice == 0) %>% rbind(
      read_csv(str_c(path_data, "tbl_SS_processing_1.csv")) %>%
        filter(is_practice == 0)
    )
  
  tbl_os_setsize <- tbl_os_recall %>%
    group_by(trial_id_recall, session_id, set_size) %>%
    dplyr::count() %>% ungroup() %>% select(-n) %>%
    arrange(set_size, session_id)
  
  
  tbl_os_processing <- tbl_os_processing %>%
    left_join(tbl_os_setsize, by = c("trial_id_recall", "session_id")) %>%
    mutate(processing_position = processing_position + 1) %>%
    group_by(participant_id, session_id, trial_id_recall, processing_position) %>%
    mutate(
      rwn = row_number()
    ) %>%
    filter(rwn == 1) %>%
    group_by(participant_id, session_id, trial_id_recall) %>%
    mutate(
      n_correct = sum(accuracy)
    ) %>%
    filter(processing_position <= set_size) %>%
    ungroup()
  
  tbl_ss_setsize <- tbl_ss_recall %>%
    group_by(trial_id_recall, session_id, set_size) %>%
    dplyr::count() %>% ungroup() %>% select(-n)
  
  tbl_ss_processing <- tbl_ss_processing %>%
    left_join(tbl_ss_setsize, by = c("trial_id_recall", "session_id")) %>%
    mutate(processing_position = processing_position + 1) %>%
    group_by(participant_id, session_id, trial_id_recall, processing_position) %>%
    mutate(
      rwn = row_number()
    ) %>%
    filter(rwn == 1) %>%
    group_by(participant_id, session_id, trial_id_recall) %>%
    mutate(
      # fill processing_position according to saving history
      # i.e., take the first entry, if several have been stored
      # (e.g., the person started twice)
      processing_position = row_number(trial_id_recall),
      n_correct = sum(accuracy)
    ) %>%
    filter(processing_position <= set_size) %>%
    ungroup()
  
  tbl_WMU_recall <- readRDS(str_c(path_data, "tbl_WMU_recall_0.rds")) %>%
    filter(is_practice == 0 & trial_type == "update") %>% 
    rbind(
      readRDS(str_c(path_data, "tbl_WMU_recall_1.rds")) %>%
        filter(is_practice == 0 & trial_type == "update")
    )
  
  return(list(
    tbl_os_recall = tbl_os_recall,
    tbl_os_processing = tbl_os_processing,
    tbl_ss_recall = tbl_ss_recall,
    tbl_ss_processing = tbl_ss_processing,
    tbl_wmu_recall = tbl_WMU_recall
  ))
  
}

se <- function(x){sd(x, na.rm = T) / sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

load_bandit_and_questionnaire_data <- function(session, time_period) {
  
  if (!file.exists("data/all-data/BanditLookup1.csv")){
    stop("Please run script first for first session, and only then for second session")
  }
  
  # now load all bandit and qs data given time constraint
  
  
  ### load data ########
  
  nBlocksH = 80
  nTrialsH = 10
  
  nBlocksS = 30
  nTrialsS = 10
  
  nBlocksR = 1
  nTrialsR = 200
  
  path_data_bandits <- "data/all-data/bandits/"
  path_data_qs <- "data/all-data/qs/"
  files_all <- list.files(path = path_data_bandits)
  mtime_bandits_all <- map(str_c(path_data_bandits, "/", files_all), ~ file.info(.x)$mtime)
  idx_not_temp <- !grepl("temp", files_all)
  files <- files_all[idx_not_temp]
  mtime_bandits <- mtime_bandits_all[idx_not_temp]
  idx_intime_bandits <- map_lgl(mtime_bandits, ~ between(.x, time_period[1], time_period[2]))
  files <- files[idx_intime_bandits]
  
  
  ### get IDs of participants that have questionnaire data (and thus seem to have completed the study)
  qfiles = list.files(path = path_data_qs)
  mtime_qs_all <- map(str_c(path_data_qs, "/", qfiles), ~ file.info(.x)$mtime)
  idx_intime_qs <- map_lgl(mtime_qs_all, ~ between(.x, time_period[1], time_period[2]))
  qfiles <- qfiles[idx_intime_qs]
  qPIDs = apply(as.array(qfiles), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))
  
  
  ### get IDs of participants that completed bandits bc some of them might not have questionnaire data for some reason
  bPIDs = apply(as.array(files), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))
  
  # merge both lists
  PIDs = unique(c(qPIDs, bPIDs))
  
  # make lookup table with anonymised IDs
  
  lookup <- data.frame(PID = PIDs,
                       ID = 1:length(PIDs))
  
  
  bonus <- data.frame(ID = rep(NA, length(PIDs)),
                      TotalBonus = NA,
                      Horizon = NA,
                      Sam = NA,
                      Restless = NA)
  
  comprehension <- data.frame(ID = rep(1:length(PIDs), each = 3),
                              task = rep(c("horizon", "sam", "restless"), length(PIDs)),
                              compAttempts = NA,
                              compTime = NA,
                              instTime = NA)
  
  horizon <- data.frame(ID = rep(1:length(PIDs), each = nBlocksH*nTrialsH),
                        block = rep(rep(1:nBlocksH, each = nTrialsH), length(PIDs)),
                        trial = rep(1:nTrialsH, nBlocksH*length(PIDs)),
                        chosen = NA,
                        reward = NA,
                        rt = NA, 
                        session = session)
  
  
  sam <- data.frame(ID = rep(1:length(PIDs), each = nBlocksS*nTrialsS),
                    block = rep(rep(1:nBlocksS, each = nTrialsS), length(PIDs)),
                    trial = rep(1:nTrialsS, nBlocksS*length(PIDs)),
                    chosen = NA,
                    reward = NA,
                    rt = NA, 
                    session = session)
  
  
  restless <- data.frame(ID = rep(1:length(PIDs), each = nTrialsR),
                         trial = 1:nTrialsR,
                         chosen = NA,
                         reward = NA,
                         rt = NA, 
                         session = session)
  
  for (i in 1:nrow(lookup)){
    
    if (i%%20 == 0){print(i)}
    
    pid <- lookup$PID[i]
    file_ind <- grep(pid, files)
    # check if we have the final data for that participant
    if (length(file_ind)>0){ID = i} 
    else { # if not we need to look through temp, easiest to do that manually
      print(pid)
      next
    }
    temp <- fromJSON(paste(path_data_bandits, files[file_ind], sep = ""))
    ### Horizon task
    for (block in 2:(nBlocksH+1)){# bc block 1 is practice
      for (trial in 1:nTrialsH){
        if (length(temp$horizon$choice[[block]]) < trial){next}
        horizon$chosen[horizon$ID == i & horizon$block == block-1 & horizon$trial == trial] <- temp$horizon$choice[[block]][[trial]]
        horizon$reward[horizon$ID == i & horizon$block == block-1 & horizon$trial == trial] <- temp$horizon$reward[[block]][[trial]]
        horizon$rt[horizon$ID == i & horizon$block == block-1 & horizon$trial == trial] <- temp$horizon$time[[block]][[trial]]
      }
      horizon$info[horizon$ID == i & horizon$block == block-1] <- length(horizon$chosen[horizon$ID == i & horizon$block == block-1&horizon$chosen == 0 & horizon$trial < 5]) -
        length(horizon$chosen[horizon$ID == i & horizon$block == block-1 & horizon$chosen == 1& horizon$trial < 5])
      
      
    }
    
    ### Sam's task
    for (block in 2:(nBlocksS+1)){# bc block 1 is practice
      for (trial in 1:nTrialsS){
        test <- try(temp$sam$choice[[block]][[trial]])
        if(is.element("try-error", class(test))){print(paste(pid, trial, block)); next}
        sam$chosen[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$choice[[block]][[trial]]
        sam$reward[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$reward[[block]][[trial]]
        sam$rt[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$time[[block]][[trial]]
      }
      
    }
    
    ## restless
    for (trial in 1:nTrialsR){
      restless$chosen[restless$ID == i & restless$trial == trial] <- temp$restless$choice[[2]][[trial]]
      restless$reward[restless$ID == i & restless$trial == trial] <- temp$restless$reward[[2]][[trial]]
      restless$rt[restless$ID == i & restless$trial == trial] <- temp$restless$time[[2]][[trial]]
    }
    
    
    ### get some info on their comprehension time etc.
    
    if(length(temp$comprehensionAttemptsH)>0){ # if we have this info
      comprehension$compAttempts[comprehension$ID == i & comprehension$task == "horizon"] <- temp$comprehensionAttemptsH
      comprehension$compAttempts[comprehension$ID == i & comprehension$task == "sam"] <- temp$comprehensionAttemptsS
      comprehension$compAttempts[comprehension$ID == i & comprehension$task == "restless"] <- temp$comprehensionAttemptsR
      
      comprehension$compTime[comprehension$ID == i & comprehension$task == "horizon"] <- temp$horizon$comprehensionTime
      comprehension$compTime[comprehension$ID == i & comprehension$task == "sam"] <- temp$sam$comprehensionTime
      comprehension$compTime[comprehension$ID == i & comprehension$task == "restless"] <- temp$restless$comprehensionTime
      
      comprehension$instTime[comprehension$ID == i & comprehension$task == "horizon"] <- temp$horizon$instructionTime
      comprehension$instTime[comprehension$ID == i & comprehension$task == "sam"] <- temp$sam$instructionTime
      comprehension$instTime[comprehension$ID == i & comprehension$task == "restless"] <- temp$restless$instructionTime
      
    }
  }
  
  # info condition should be coded as -1 0 1 but is now coded as -2 0 2 so fix that
  horizon$info <- horizon$info/2
  
  
  ### save lookup
  
  write.csv(
    lookup, file = str_c(
      str_remove(path_data_bandits, "[a-z]*/$"), 
      str_c("BanditLookup", session, ".csv")
    )
  )
  
  ####### make lookup that transforms IDs from session 2 into IDs from session 1
  
  session1 <- read.csv("data/all-data/BanditLookup1.csv")
  if (session == 2) {
    session2 <- read.csv("data/all-data/BanditLookup2.csv")
    
    session2$wave1ID <- session1$ID[match(session2$PID, session1$PID)]
    
    sam$ID <- session2$wave1ID[match(sam$ID, session2$ID)]
    horizon$ID <- session2$wave1ID[match(horizon$ID, session2$ID)]
    restless$ID <- session2$wave1ID[match(restless$ID, session2$ID)]
    comprehension$ID <- session2$wave1ID[match(comprehension$ID, session2$ID)]
  }
  
  save(comprehension, file = str_c(path_data_bandits, "comprehension", session, ".Rda"))
  ############### calculate max rewards #########
  
  ############## Horizon task
  
  ## load the rewards
  
  rewardsH <- fromJSON(paste("task/rewardsHorizon", session, ".json", sep = ""))
  Hrewards <- data.frame(block = rep(1:(nBlocksH+1), each = nTrialsH),
                         trial = rep(1:nTrialsH, nBlocksH+1),
                         rew1 = NA,
                         rew2 = NA)
  
  Horizon <-  fromJSON(paste("task/Horizon", session, ".json", sep = ""))
  
  Hrewards$horizon <- rep(Horizon, each = nTrialsH)
  
  for (block in 1:(nBlocksH+1)){# +1 bc there is the practice round too
    for (trial in 1:nTrialsH){
      if (trial > Hrewards$horizon[Hrewards$block == block & Hrewards$trial == trial]){next}
      Hrewards$rew1[Hrewards$block == block & Hrewards$trial == trial]<-  rewardsH[block,trial,1]
      Hrewards$rew2[Hrewards$block == block & Hrewards$trial == trial]<-  rewardsH[block,trial,2]
    }
  }
  
  # calculate max reward
  
  Hrewards$max <- ifelse(Hrewards$rew1 > Hrewards$rew2, Hrewards$rew1, Hrewards$rew2)
  
  maxHorizon <- sum(Hrewards$max, na.rm = T)
  
  ########### Sam's task
  load(paste("task/rewardsSam", session, ".Rda", sep = ""))
  Srewards <- rewards
  
  # calculate max reward
  
  #Srewards$max <- ifelse(Srewards$rew1 > Srewards$rew2, Srewards$rew1, Srewards$rew2)
  Srewards$max <- ifelse(Srewards$reward1 > Srewards$reward2, Srewards$reward1, Srewards$reward2)
  
  maxSam <- sum(Srewards$max, na.rm = T)
  
  
  ####### Restless
  
  
  rewardsR <- fromJSON(paste("task/rewards4ARB", session, ".json", sep = ""))
  Rrewards <- data.frame(block = rep(1:(nBlocksR+1), each = nTrialsR),
                         trial = rep(1:nTrialsR, nBlocksR+1),
                         rew1 = NA,
                         rew2 = NA,
                         rew3 = NA,
                         rew4 = NA)
  
  
  for (block in 1:(nBlocksR+1)){# +1 bc there is the practice round too
    for (trial in 1:nTrialsR){
      if (block == 1 & trial > 10){next}
      Rrewards$rew1[Rrewards$block == block & Rrewards$trial == trial]<-  rewardsR[[block]][trial,1]
      Rrewards$rew2[Rrewards$block == block & Rrewards$trial == trial]<-  rewardsR[[block]][trial,2]
      Rrewards$rew3[Rrewards$block == block & Rrewards$trial == trial]<-  rewardsR[[block]][trial,3]
      Rrewards$rew4[Rrewards$block == block & Rrewards$trial == trial]<-  rewardsR[[block]][trial, 4]
    }
  }
  
  # calculate max reward
  Rrewards$row <- 1:nrow(Rrewards)
  Rrewards$max <- apply(as.array(Rrewards$row), 1, function(x) max(Rrewards[x,3:6]))
  
  maxRestless <- sum(Rrewards$max, na.rm = T)
  
  
  save(maxHorizon, maxSam, maxRestless, file = paste("task/maxRewards", session, ".Rda", sep = ""))
  
  
  ################# get ground truth variables ###############
  
  ####### Horizon
  
  rewardsH <- fromJSON(paste("task/rewardsHorizon", session, ".json", sep = ""))
  Horizon <- fromJSON(paste("task/Horizon", session, ".json", sep = ""))
  
  horizon$reward1 <- NA
  horizon$reward2 <- NA
  horizon$Horizon <- NA
  
  for (block in 2:(nBlocksH+1)){
    temp <- data.frame(rewardsH[block, ,])
    horizon$reward1[horizon$block == block-1] <- temp$X1
    horizon$reward2[horizon$block == block-1] <- temp$X2
    horizon$Horizon[horizon$block == block-1] <- Horizon[block]
    
  }
  
  ######### Sam
  
  load(paste("task/rewardsSam", session, ".Rda", sep = ""))
  rewardsS <- rewards
  
  sam$reward1 <- rewardsS$reward1[rewardsS$block > 1]
  sam$reward2 <- rewardsS$reward2[rewardsS$block > 1]
  sam$cond <- rewardsS$cond[rewardsS$block > 1]
  
  # for (block in 2:(nBlocksS+1)){
  #   temp <- data.frame(rewardsS[block, ,])
  #   sam$reward1[sam$block == block-1] <- temp$X1
  #   sam$reward2[sam$block == block-1] <- temp$X2
  #   
  #   # have to infer cond bc I am dumb
  #   # cond1 <- ifelse(sd(temp$X1)>1.5, "F", "S")
  #   # cond2 <- ifelse(sd(temp$X2)>1.5, "F", "S")
  #   # sam$cond[sam$block == block-1] <- paste(cond1, cond2, sep = "")
  # }
  
  
  
  ### restless
  restless$reward1 <- NA
  restless$reward2 <- NA
  restless$reward3 <- NA
  restless$reward4 <- NA
  rewardsR <- fromJSON(paste("task/rewards4ARB", session, ".json", sep = ""))
  
  
  
  temp <- data.frame(rewardsR[[2]])
  restless$reward1 <- rep(temp$X1, length(unique(restless$ID)))
  restless$reward2 <- rep(temp$X2, length(unique(restless$ID)))
  restless$reward3 <- rep(temp$X3, length(unique(restless$ID)))
  restless$reward4 <- rep(temp$X4, length(unique(restless$ID)))
  
  save(horizon, sam, restless, file = str_c(str_remove(path_data_bandits, "[a-z]*/$"),  "bandits.Rda"))
  save(horizon, sam, restless, file = str_c(str_remove(path_data_bandits, "[a-z]*/$"),  str_c("bandits", session, ".Rda")))
  save(horizon, sam, restless, file = paste("analysis/bandits/banditsWave", session, "full.Rda", sep = ""))
  
  #################### questionnaires ###############
  
  
  
  files = list.files(path = path_data_qs)
  
  for (i in 1:nrow(lookup)){
    
    pid <- lookup$PID[i]
    # check if we have the final data for that participant
    if (mean(grepl(pid, files))>0){ID = i} 
    else { # if not we need to look through temp, easiest to do that manually
      print(pid)
      next
    }
    all_paths <- files[grep(pid, files)]
    q_path <- all_paths[str_detect(all_paths, str_c("session_", session - 1))]
    
    if (length(q_path) > 0) {
      tmp <- read_file(paste(path_data_qs, q_path, sep = ""))
      if (str_count(tmp, "motiv_mem_0") > 1) {
        l_txt <- str_match_all(tmp, "(\\\n.*)\\\n.*$")
        my_txt <- l_txt[[1]][,2]
      } else {
        my_txt <- paste(path_data_qs, q_path, sep = "")
      }
      temp <- fromJSON(my_txt)
      
      if (i == 1){# if this is the first one
        qdat <- temp
        qdat <- as.data.frame(qdat)
        qdat$ID <- ID
      } else {
        temp <- as.data.frame(temp)
        temp$ID <- ID
        qdat <- rbind(qdat, temp)
      }
    }
  }
  
  ## recode IDs to be based on wave1 IDs
  if (session == 2) qdat$ID <- session2$wave1ID[match(qdat$ID, session2$ID)]
  
  ## save it
  
  save(qdat, file = str_c(str_remove(path_data_qs, "[a-z]*/$"),  str_c("qsFull", session, ".Rda")))
}



eda_and_exclusion_criteria_bandits <- function(session) {
  
  load(sprintf("analysis/bandits/banditsWave%ifull.Rda", session))
  load(sprintf("analysis/qswave%iFull.Rda", session))
  load(sprintf("analysis/comprehensionWave%i.Rda", session))
  
  ##### WM
  session1 <- read.csv("data/all-data/BanditLookup1.csv")
  session2 <- read.csv("data/all-data/BanditLookup2.csv")
  
  if (session == 2) {
    lookup <- session2
    session2$wave1ID <- session1$ID[match(session2$PID, session1$PID)]
    sam$ID <- session2$wave1ID[match(sam$ID, session2$ID)]
    horizon$ID <- session2$wave1ID[match(horizon$ID, session2$ID)]
    restless$ID <- session2$wave1ID[match(restless$ID, session2$ID)]
    comprehension$ID <- session2$wave1ID[match(comprehension$ID, session2$ID)]
  } else if (session == 1) {
    lookup = session1
  }
  
  #load what mirko did here
  # wm <- readRDS("analysis/subjects-excl-wm.rds")
  # wm$prolific_pid <- wm$participant_id
  # wm <- subset(wm, is.element(prolific_pid, lookup$PID))
  # lookup$perfWM <- NA
  # lookup$perfWM <- wm$excl_subject[match(lookup$PID, wm$prolific_pid)]
  
  #### used external aids
  lookup$WMaid <- NA
  lookup$slotaid <- NA
  lookup$WMaid[match(qdat$ID, lookup$ID)] <- qdat$mem_aid_0
  lookup$slotaid[match(qdat$ID, lookup$ID)] <- qdat$slot_aid_0
  
  #### Horizon performance
  n <- length(na.omit(horizon$chosen[horizon$ID == 1])) - 80*4 # subtract the free choices
  # for which number of best arm choices is there a 95% probability that this is by chance?
  qbinom(0.95, n, 0.5) # 154
  
  # proportion of optimal choices you get by chance
  pchance <- 154/n
  
  horizon$optimal <- ifelse(horizon$reward1 > horizon$reward2, 0, 1)
  horizon$chooseBest <- ifelse(horizon$chosen == horizon$optimal, 1, 0)
  
  # how many subjects are on average at chance performance?
  
  overall <- plyr::ddply(horizon[horizon$trial > 4, ], ~ID, summarise, optimal = meann(chooseBest))
  table(overall$optimal <= pchance) # 6 excluded
  
  # get rid of person w/o data
  overall <- subset(overall, !is.na(optimal))
  
  pl_hist_horizon <- plot_my_chance_hist(overall, "Horizon", pchance)
  
  
  lookup$perfHorizon <- NA
  lookup$perfHorizon <- lookup %>% left_join(overall, by = "ID") %>% mutate(optimal = ifelse(optimal <= pchance, 1, 0)) %>% select(optimal) %>% as_vector()
  #lookup$perfHorizon[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <=pchance, 1, 0)
  
  #### Sam's task performance
  
  sam$optimal <- ifelse(sam$reward1 > sam$reward2, 0, 1)
  sam$chooseBest <- ifelse(sam$chosen == sam$optimal, 1, 0)
  
  overall <- plyr::ddply(sam, ~ID, summarise, optimal = meann(chooseBest))
  
  pchance <- qbinom(0.95, 300, 0.5)/300 # 154
  
  
  # get rid of person w/o data
  overall <- subset(overall, !is.na(optimal))
  
  table(overall$optimal <= pchance)
  
  
  pl_hist_sam <- plot_my_chance_hist(overall, "Two-Armed Fixed", pchance)
  
  
  lookup$perfSam <- NA
  lookup$perfSam <- lookup %>% left_join(overall, by = "ID") %>% mutate(optimal = ifelse(optimal <= pchance, 1, 0)) %>% select(optimal) %>% as_vector()
  #lookup$perfSam[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <= pchance, 1, 0)
  
  
  #### 4arb performance 
  
  data <- restless
  data$optimalR <- rep(apply(as.array(data$trial[data$ID == 1]), 1, function(x) max(c(data$reward1[data$trial == x],
                                                                                      data$reward2[data$trial == x],
                                                                                      data$reward3[data$trial == x],
                                                                                      data$reward4[data$trial == x]))), 
                       length(unique(data$ID)))
  
  data$chooseBest <- ifelse(data$reward == data$optimalR, 1, 0)
  
  n <- 200
  pchance <- qbinom(0.95, n, 0.25)/n
  
  overall <- plyr::ddply(data, ~ID, summarise, optimal = meann(chooseBest))
  
  overall <- subset(overall, !is.na(optimal))
  
  pl_hist_restless <- plot_my_chance_hist(overall, "Four-Armed Restless", pchance, TRUE)
  
  save_my_pdf(
    gridExtra::arrangeGrob(pl_hist_horizon, pl_hist_sam, pl_hist_restless, nrow = 1),
    str_c("figures/EDA/bandit-histograms-session-", session, ".pdf"), 16, 5
  )
  
  
  #lookup$perfRestless[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <= pchance, 1, 0)
  lookup$perfRestless <- lookup %>% left_join(overall, by = "ID") %>% mutate(optimal = ifelse(optimal <= pchance, 1, 0)) %>% select(optimal) %>% as_vector()
  
  
  #### comprehension attempts horizon
  
  mean_sd <- plyr::ddply(comprehension, ~task, summarise, meanComp = mean(compAttempts, na.rm =T), SD = sd(compAttempts, na.rm = T))
  
  mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp
  
  comprehension$excl <- ifelse(comprehension$compAttempts > mean_sd$SD[match(comprehension$task, mean_sd$task)], 1, 0)
  
  table(comprehension$excl)
  
  lookup$compHorizon <- lookup %>% left_join(comprehension %>% filter(task == "horizon"), by = "ID") %>% select(excl) %>% as_vector()
  lookup$compSam <- lookup %>% left_join(comprehension %>% filter(task == "sam"), by = "ID") %>% select(excl) %>% as_vector()
  lookup$compRestless <- lookup %>% left_join(comprehension %>% filter(task == "restless"), by = "ID") %>% select(excl) %>% as_vector()
  
  #lookup$compHorizon[match(comprehension$ID[comprehension$task == "horizon"], lookup$ID)] <- comprehension$excl[comprehension$task == "horizon"]
  
  #### comprehension attempts sam
  
  #lookup$compSam[match(comprehension$ID[comprehension$task == "sam"], lookup$ID)] <- comprehension$excl[comprehension$task == "sam"]
  
  
  #### comprehension attempts 4arb
  
  #lookup$compRestless[match(comprehension$ID[comprehension$task == "restless"], lookup$ID)] <- comprehension$excl[comprehension$task == "restless"]
  
  #### attention checks
  
  lookup$attention <- NA
  lookup$attention <- lookup %>% left_join(qdat, by = "ID") %>% mutate(attention = ifelse(attention1 < 2, 1, 0)) %>% select(attention) %>% as_vector()
  
  #lookup$attention[match(qdat$ID, lookup$ID)] <- ifelse(qdat$attention1 < 2, 1, 0)
  
  table(lookup$attention)
  
  ##### total
  
  lookup$totalExclude <- apply(as.array(1:nrow(lookup)), 1, function(x) sum(as.numeric(unlist(lookup[x, c(grep("perfHorizon", colnames(lookup)): ncol(lookup))])), na.rm = T))
  
  hist(lookup$totalExclude, breaks = max(lookup$totalExclude))
  
  lookup$exclude <- ifelse(lookup$totalExclude == 0, 0 , 1)
  table(lookup$exclude)
  
  
  write_csv(lookup, str_c("data/exclusions", session, ".csv"))
  
  
}


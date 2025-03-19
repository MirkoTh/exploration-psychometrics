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
  
  print(head(tbl_OS_recall))
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
  print(head(tbl_ids_lookup))
  if (random_hashes) {
    tbl_ids_lookup <- tbl_ids_lookup %>%
      filter(!(participant_id %in% participants_returned)) %>% 
      group_by(participant_id) %>%
      mutate(participant_id_randomized = random_id(1)) %>% ungroup()
  } else {
    print(tbl_ids_lookup)
    tmp <- read_csv("data/FINALlookup.csv") %>%
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

parse_out_ID <- function(filename, task){
  
  #' parse prolific ID from a filename
  #' 
  #' @description where to find the prolific ID in a file name differs by task
  #' also some IDs are certainly from our piloting so those get thrown out
  #' @param filname filename to parse ID from
  #' @param task task the data file is from
  
  #' @return id the parsed out prolific ID
  #' 
  
  
  # for wm tasks
  if (is.element(task, c("WMU", "SS", "OS"))){
    
    # Split the string by "_"
    split_string <- strsplit(filename, "_")[[1]]
    
    idpart <- split_string[length(split_string)]
    
    # Get the last element of the split string and remove ".json"
    id <- sub(".json", "", idpart)
    
  } else {
    
    # Split the string by "_"
    split_string <- strsplit(filename, "_")[[1]]
    
    id <- split_string[1]
    
  }
  
  # check whether this was a pilot ID
  # prolific IDs all start with a number while our test ids never do
  if (!grepl("\\d",substr(id, 1,1))){
    id <- "test"
  }
  
  return(id)
}

agg_by_ss <- function(my_tbl, taskname) {
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

load_wm_data <- function(path_data) {
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

json_to_tibble_kristin <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  #js_txt <- str_replace(js_txt, ",,", ",")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}

load_bandit_and_questionnaire_data <- function(session, time_period) {
  
  if (!file.exists("data/FINALlookup.csv")){
    stop("Please first create a lookup file")
  }
  
  # now load all bandit and qs data given time constraint
  
  
  ### load data ########
  
  nBlocksH = 80
  nTrialsH = 10
  
  nBlocksS = 30
  nTrialsS = 10
  
  nBlocksR = 1
  nTrialsR = 200
  
  path_data_bandits <- sprintf("data/wave%i/bandits/", session)
  path_data_qs <- sprintf("data/wave%i/qs/", session)
  files_all <- list.files(path = path_data_bandits)
  #mtime_bandits_all <- map(str_c(path_data_bandits, "/", files_all), ~ file.info(.x)$mtime)
  idx_not_temp <- !grepl("temp", files_all)
  files <- files_all[idx_not_temp]
  #mtime_bandits <- mtime_bandits_all[idx_not_temp]
  #idx_intime_bandits <- map_lgl(mtime_bandits, ~ between(.x, time_period[1], time_period[2]))
  #files <- files[idx_intime_bandits]
  
  
  ### get IDs of participants that have questionnaire data (and thus seem to have completed the study)
  # qfiles = list.files(path = path_data_qs)
  # mtime_qs_all <- map(str_c(path_data_qs, "/", qfiles), ~ file.info(.x)$mtime)
  # idx_intime_qs <- map_lgl(mtime_qs_all, ~ between(.x, time_period[1], time_period[2]))
  # qfiles <- qfiles[idx_intime_qs]
  # qPIDs = apply(as.array(qfiles), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))
  # 
  # 
  # ### get IDs of participants that completed bandits bc some of them might not have questionnaire data for some reason
  # bPIDs = apply(as.array(files), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))
  # 
  # # merge both lists
  # PIDs = unique(c(qPIDs, bPIDs))
  
  # make lookup table with anonymised IDs
  
  lookup <- read.csv("data/FINALlookup.csv")
  
  PIDs <- lookup$PID
  
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
    #for (i in 1:3){
    
    if (i%%20 == 0){print(sprintf("subject %i of %i", i, nrow(lookup)))}
    
    pid <- lookup$PID[i]
    file_ind <- grep(paste0(pid, "_"), files)
    ID <- lookup$ID[i]
    # check if we have the final data for that participant
    if (length(file_ind)>0){
      temp <- fromJSON(paste(path_data_bandits, files[file_ind], sep = ""))
    }
    else { # if not we need to look through temp
      
      # do we even have a temp file in that name?
      if(!file.exists(paste0(path_data_bandits,pid ,"_temp_data_task_session_", session-1,".txt"))){
        print(pid); next} # no we don't
      else{# yes we do
        t <- read_file(paste0(path_data_bandits,pid ,"_temp_data_task_session_", session-1,".txt"))
        t_split <- strsplit(t, "\\}\n\\{")[[1]]
        te <- t_split[length(t_split)]
        #te <- gsub("\\}\n\\{", "},\"next\":{", t)
        if(substr(te, 1,1) != "{"){
          te <- paste0("{", te)
        }
        temp <- fromJSON(te)
        #temp <- tem[[length(tem)]]
      }
      
    }
    
    ### Horizon task
    for (block in 2:(nBlocksH+1)){# bc block 1 is practice
      if (length(temp$horizon$choice) < block){next} # dealing with incomplete data
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
      if(length(temp$sam$choice) < block){next}
      for (trial in 1:nTrialsS){
        if(length(temp$sam$choice[[block]]) < trial){next}
        sam$chosen[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$choice[[block]][[trial]]
        sam$reward[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$reward[[block]][[trial]]
        sam$rt[sam$ID == i & sam$block == block-1 & sam$trial == trial] <- temp$sam$time[[block]][[trial]]
      }
      
    }
    
    ## restless
    for (trial in 1:nTrialsR){
      if(length(temp$restless$choice) < 2) {next} # if there is no data for restless
      if (length(temp$restless$choice[[2]]) < trial){next} # if incomplete data
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
  
  # write.csv(
  #   lookup, file = str_c(
  #     str_remove(path_data_bandits, "[a-z]*/$"),
  #     str_c("BanditLookup", session, ".csv")
  #   )
  # )
  
  ####### make lookup that transforms IDs from session 2 into IDs from session 1
  
  # session1 <- read.csv("data/all-data/BanditLookup1.csv")
  # if (session == 2) {
  #   session2 <- read.csv("data/all-data/BanditLookup2.csv")
  #
  #   session2$wave1ID <- session1$ID[match(session2$PID, session1$PID)]
  #
  #   sam$ID <- session2$wave1ID[match(sam$ID, session2$ID)]
  #   horizon$ID <- session2$wave1ID[match(horizon$ID, session2$ID)]
  #   restless$ID <- session2$wave1ID[match(restless$ID, session2$ID)]
  #   comprehension$ID <- session2$wave1ID[match(comprehension$ID, session2$ID)]
  # }
  
  save(comprehension, file = str_c("data/wave",session,"/comprehension", session, ".Rda"))
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
  
  
  #save(maxHorizon, maxSam, maxRestless, file = paste("task/maxRewards", session, ".Rda", sep = ""))
  
  
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
  
  
  ############ take out subjects who have no data ##############
  remove_na_participants <- function(df) {
    temp <- plyr::ddply(df, ~ID, summarise, ch = mean(chosen, na.rm = T))
    temp <- subset(temp, !is.na(ch))
    df <- subset(df, is.element(ID, temp$ID))
    return(df)
  }
  
  horizon <- remove_na_participants(horizon)
  sam <- remove_na_participants(sam)
  restless <- remove_na_participants(restless)
  
  #save(horizon, sam, restless, file = str_c(str_remove(path_data_bandits, "[a-z]*/$"),  "bandits.Rda"))
  #save(horizon, sam, restless, file = str_c(str_remove(path_data_bandits, "[a-z]*/$"),  str_c("bandits", session, ".Rda")))
  save(horizon, sam, restless, file = sprintf("data/wave%i/banditsWave%ifull.Rda", session, session))
  
  #################### questionnaires ###############
  
  print("done with bandits")
  
  files = list.files(path = path_data_qs)
  
  for (i in 1:nrow(lookup)){
    
    pid <- lookup$PID[i]
    # check if we have the final data for that participant
    if (mean(grepl(pid, files))>0){ID = i}
    else { # we don't have data for that participant
      print(pid)
      next
    }
    all_paths <- files[grep(paste0(pid, "_"), files)]
    q_path <- all_paths[str_detect(all_paths, str_c("session_", session - 1))]
    
    if (length(q_path) > 0) {
      tmp <- read_file(paste(path_data_qs, q_path, sep = ""))
      if (str_count(tmp, "motiv_mem_0") > 1) {# sometimes qdat was saved duplicate
        l_txt <- str_match_all(tmp, "(\\\n.*)\\\n.*$")
        my_txt <- l_txt[[1]][,2]
      } else {
        my_txt <- paste(path_data_qs, q_path, sep = "")
      }
      temp <- fromJSON(my_txt)
      
      if (!exists("qdat")){# if this is the first one
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
  # if (session == 2) qdat$ID <- session2$wave1ID[match(qdat$ID, session2$ID)]
  
  ## save it
  
  save(qdat, file = sprintf("data/wave%i/qsWave%ifull.Rda", session, session))
}



exclusion_criteria <- function(session) {
  
  load(sprintf("data/wave%i/banditsWave%ifull.Rda", session, session))
  load(sprintf("data/wave%i/qswave%iFull.Rda", session, session))
  load(sprintf("data/wave%i/comprehension%i.Rda", session, session))
  
  ##### WM
  # session1 <- read.csv("data/all-data/BanditLookup1.csv")
  # session2 <- read.csv("data/all-data/BanditLookup2.csv")
  # 
  # if (session == 2) {
  #   lookup <- session2
  #   session2$wave1ID <- session1$ID[match(session2$PID, session1$PID)]
  #   sam$ID <- session2$wave1ID[match(sam$ID, session2$ID)]
  #   horizon$ID <- session2$wave1ID[match(horizon$ID, session2$ID)]
  #   restless$ID <- session2$wave1ID[match(restless$ID, session2$ID)]
  #   comprehension$ID <- session2$wave1ID[match(comprehension$ID, session2$ID)]
  # } else if (session == 1) {
  #   lookup = session1
  # }
  
  lookup <- read.csv("data/FINALlookup.csv")
  
  #load what mirko did here
  # wm <- readRDS("analysis/subjects-excl-wm.rds")
  # wm$prolific_pid <- wm$participant_id
  # wm <- subset(wm, is.element(prolific_pid, lookup$PID))
  # lookup$perfWM <- NA
  # lookup$perfWM <- wm$excl_subject[match(lookup$PID, wm$prolific_pid)]
  
  ####### incomplete data
  
  complete_subs <- intersect(unique(qdat$ID), unique(horizon$ID))
  complete_subs <- intersect(complete_subs, unique(sam$ID))
  complete_subs <- intersect(complete_subs, unique(restless$ID))
  
  lookup$incomplete_data <- ifelse(is.element(lookup$ID, complete_subs), 0, 1)
  
  #### used external aids
  lookup$WMaid <- NA
  lookup$slotaid <- NA
  lookup$WMaid[match(qdat$ID, lookup$ID)] <- qdat$mem_aid_0
  lookup$slotaid[match(qdat$ID, lookup$ID)] <- qdat$slot_aid_0
  
  
  #### bandit performance
  
  exclude_based_on_performance <- function(df){
    
    n_arms <- length(grep("reward", colnames(df)))-1 # -1 bc 1 col is the reward they actually got
    
    n <- length(na.omit(df$chosen[df$ID == df$ID[1]]))
    print(n)
    chance_level <- 1/n_arms
    best_by_chance <- qbinom(0.95, n, chance_level)
    
    # proportion of optimal choices you get by chance
    pchance <- best_by_chance/n
    print(pchance)
    df$row <- 1:nrow(df)
    df$optimalR <- rep(apply(as.array(df$row[df$ID == df$ID[1]]), 1, function(x) max(df[x, grepl("reward", colnames(df))])), 
                       length(unique(df$ID)))
    
    df$chooseBest <- ifelse(df$reward == df$optimalR, 1, 0)
    
    overall <- plyr::ddply(df, ~ID, summarise, optimal = meann(chooseBest))
    overall$excl <- ifelse(overall$optimal <= pchance, 1, 0)
    
    return(overall)
    
  }
  
  excl_h <- exclude_based_on_performance(horizon[horizon$trial > 4, ])
  excl_s <- exclude_based_on_performance(sam)
  excl_r <- exclude_based_on_performance(restless)
  
  # # get rid of person w/o data
  # overall <- subset(overall, !is.na(optimal))
  # 
  # pl_hist_horizon <- plot_my_chance_hist(overall, "Horizon", pchance)
  
  
  lookup$perfHorizon <- NA
  lookup$perfHorizon[match(excl_h$ID, lookup$ID)] <- excl_h$excl
  lookup$perfSam <- NA
  lookup$perfSam[match(excl_s$ID, lookup$ID)] <- excl_s$excl
  lookup$perfRestless <- NA
  lookup$perfRestless[match(excl_r$ID, lookup$ID)] <- excl_r$excl
  
  # testing the matchup
  print("tests. The below needs to be all TRUE")
  print(table(na.omit(lookup$ID[lookup$perfHorizon == 1]) == excl_h$ID[excl_h$excl == 1]))
  print(table(na.omit(lookup$ID[lookup$perfSam == 1]) == excl_s$ID[excl_s$excl == 1]))
  print(table(na.omit(lookup$ID[lookup$perfRestless == 1]) == excl_r$ID[excl_r$excl == 1]))
  
  #lookup$perfHorizon <- lookup %>% left_join(overall, by = "ID") %>% mutate(optimal = ifelse(optimal <= pchance, 1, 0)) %>% select(optimal) %>% as_vector()
  #lookup$perfHorizon[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <=pchance, 1, 0)
  
  # save_my_pdf(
  #   gridExtra::arrangeGrob(pl_hist_horizon, pl_hist_sam, pl_hist_restless, nrow = 1),
  #   str_c("figures/EDA/bandit-histograms-session-", session, ".pdf"), 16, 5
  # )
  
  
  #### comprehension attempts  (this we only do for session 1 bc in session 2 we assume that those who were re-invited understood the tasks)
  
  mean_sd <- plyr::ddply(comprehension, ~task, summarise, meanComp = mean(compAttempts, na.rm =T), SD = sd(compAttempts, na.rm = T))
  
  mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp
  
  mean_SD <- meann(comprehension$compAttempts) + 2*sd(comprehension$compAttempts, na.rm = T)
  
  comprehension$excl <- ifelse(comprehension$compAttempts > mean_sd$SD[match(comprehension$task, mean_sd$task)], 1, 0)
  #comprehension$excl <- ifelse(comprehension$compAttempts > mean_SD, 1, 0)
  
  table(comprehension$excl)
  
  lookup$compHorizon <- lookup %>% left_join(comprehension %>% filter(task == "horizon"), by = "ID") %>% select(excl) %>% as_vector()
  lookup$compSam <- lookup %>% left_join(comprehension %>% filter(task == "sam"), by = "ID") %>% select(excl) %>% as_vector()
  lookup$compRestless <- lookup %>% left_join(comprehension %>% filter(task == "restless"), by = "ID") %>% select(excl) %>% as_vector()
  
  
  
  #### attention checks
  
  lookup$attention <- NA
  lookup$attention <- lookup %>% left_join(qdat, by = "ID") %>% mutate(attention = ifelse(attention1 < 2, 1, 0)) %>% select(attention) %>% as_vector()
  #test:
  print(table(na.omit(lookup$ID[lookup$attention == 1]) == qdat$ID[qdat$attention1 < 2]))
  #lookup$attention[match(qdat$ID, lookup$ID)] <- ifelse(qdat$attention1 < 2, 1, 0)
  
  table(lookup$attention)
  
  
  #### wm
  tbl_lookup_wm <- exclusion_criteria_wm_tasks(session)
  tbl_lookup_wm$any_task_too_few <- as.numeric(tbl_lookup_wm$any_task_too_few)
  tbl_lookup_wm$proc_below_thx <- as.numeric(tbl_lookup_wm$proc_below_thx)
 
  
  ##### total
 
  lookup <- lookup %>% 
    left_join(tbl_lookup_wm, by = c("ID" = "participant_id")) %>%
    as_tibble()
  lookup$any_task_too_few[is.na(lookup$any_task_too_few)] <- 1
  lookup$proc_below_thx[is.na(lookup$proc_below_thx)] <- 1
  
  
  lookup$totalExclude <- apply(as.array(1:nrow(lookup)), 1, function(x) sum(as.numeric(unlist(lookup[x, c(grep("incomplete_data", colnames(lookup)): ncol(lookup))])), na.rm = T))
  
  
  hist(lookup$totalExclude, breaks = max(lookup$totalExclude))
  
  lookup$exclude <- ifelse(lookup$totalExclude == 0, 0, 1)
  print(table(lookup$exclude))
  
  write_csv(lookup, str_c("data/exclusions", session, ".csv"))
  
  excl_ID <- lookup$ID[lookup$exclude == 1]
  horizon <- subset(horizon, !is.element(ID, excl_ID) )
  sam <- subset(sam, !is.element(ID, excl_ID))
  restless <- subset(restless, !is.element(ID, excl_ID))
  qdat <- subset(qdat, !is.element(ID, excl_ID))
  
  save(horizon, sam, restless, file = sprintf("analysis/bandits/banditsWave%i.Rda", session))
  save(qdat, file = sprintf("analysis/qswave%i.Rda", session))
  write.csv(horizon, file = sprintf("data/finalHorizonSession%i.csv", session))
  write.csv(sam, file = sprintf("data/final2armedBanditSession%i.csv", session))
  write.csv(restless, file = sprintf("data/finalRestlessSession%i.csv", session))
  write.csv(qdat, file = sprintf("data/finalQuestionnaireDataSession%i.csv", session))
  
}

exclusion_criteria_wm_tasks <- function(s_id) {
  #' create an overview table with applied exclusion criteria
  #' 
  #' @description create an overview table with applied exclusion criteria, 
  #' and write a file with the aggregate working-memory data 
  #' @param s_id session id (1 or 2)
  
  path_data <- "data/all-data/"
  
  # load data from all wm tasks
  l_tbl_wm_data <- load_wm_data(path_data)
  l_tbl_wm_data <- map(l_tbl_wm_data, ~ .x %>% filter(session_id == (s_id - 1)))
  
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
    list(tbl_os_recall, tbl_os_processing, tbl_ss_recall, tbl_ss_processing, tbl_wmu_recall),
    how_many_trials
  ) %>% reduce(rbind)
  
  
  # use first given responses for participants with multiple responses for a given trial
  
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
      too_few = n < (n_administered - 5) # - 5 seems reasonable given distribution of nr responses per task
    ) %>%
    group_by(participant_id) %>%
    summarize(any_task_too_few = sum(too_few) >= 1) %>%
    ungroup()
  
  # Trials per Participant, Task, and Set Size -------------------------
  
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
    group_by(participant_id) %>%
    mutate(n_total_datapoints = sum(n)) %>%
    ungroup()
  tbl_trials_overview2 <- tbl_trials_overview %>%
    group_by(participant_id) %>%
    summarize(n = max(n_total_datapoints)) %>%
    ungroup() %>%
    mutate(task = "All Tasks", set_size = 4)
  
  tbl_trials_all <- rbind(tbl_trials_overview %>% select(-c(n_total_datapoints)), tbl_trials_overview2)
  
  # Exclude Incomplete Datasets ---------------------------------------------
  
  
  tbl_complete_p <- tbl_n_trials %>% filter(!any_task_too_few) %>% select(participant_id)
  tbl_os_recall <- tbl_os_recall %>% inner_join(tbl_complete_p, "participant_id")
  tbl_ss_recall <- tbl_ss_recall %>% inner_join(tbl_complete_p, "participant_id")
  tbl_os_processing <- tbl_os_processing %>% inner_join(tbl_complete_p, "participant_id")
  tbl_ss_processing <- tbl_ss_processing %>% inner_join(tbl_complete_p, "participant_id")
  tbl_wmu_recall <- tbl_wmu_recall %>% inner_join(tbl_complete_p, "participant_id")
  
  
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
  
  
  # Processing --------------------------------------------------------------
  
  tbl_os_proc_agg <- agg_by_ss(
    tbl_os_processing %>% filter(processing_position == 1), "OS"
  )
  tbl_ss_proc_agg <- agg_by_ss(
    tbl_ss_processing %>% filter(processing_position == 1), "SS"
  )
  
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
    mutate(session_id = 9999) %>%
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
      excl_os = OS_9999 < thx_lo_os,
      excl_ss = SS_9999< thx_lo_ss,
      proc_below_thx = excl_os + excl_ss > 0
    )
  
  # add exclusions to overview tbl
  tbl_ids_lookup <- tbl_n_trials %>%
    left_join(
      tbl_proc_performance_participants[, c("participant_id", "proc_below_thx")], 
      by = "participant_id"
    )
  
  # save aggregate wm data
  
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
  colnames(tbl_proc_performance_participants) <- str_replace_all(colnames(tbl_proc_performance_participants), "9999", as.character(s_id - 1))
  
  
  tbl_performance_all <- tbl_recall_performance_participants %>%
    left_join(
      tbl_proc_performance_participants, 
      by = c("participant_id")
    )
  write_csv(tbl_performance_all, str_c("data/wm-performance-", s_id, ".csv"))
  
  return(tbl_ids_lookup)
  
}


reliability_task_measures <- function() {
  #' calculate reliability of task measures from bandit tasks
  #' 
  #' @description loads data from files and calculates reliability of task
  #' measures after applying the exclusion criteria

  # exclusion criteria
  tbl_exclude2 <- read_csv(file = "data/exclusions2_noPID.csv")
  tbl_exclude1 <- read_csv(file = "data/exclusions1_noPID.csv")
  
  # seems like two people were invited to session 2, who should not have been
  tbl_exclude <- tbl_exclude1 %>% select(ID, exclude) %>%
    left_join(tbl_exclude2 %>% select(ID, exclude), by = "ID", suffix = c("_1", "_2")) %>%
    filter(exclude_2 == 0 & exclude_1 == 0)
  
  load(file = "analysis/bandits/banditsWave1.Rda")
  restless1 <- restless
  horizon1 <- horizon
  sam1 <- sam
  load(file = "analysis/bandits/banditsWave2.Rda")
  restless2 <- restless
  horizon2 <- horizon
  sam2 <- sam
  
  # combine data from session 1 and session 2
  tbl_restless <- rbind(restless1, restless2) %>%
    inner_join(tbl_exclude, by = "ID") %>%
    arrange(ID, session, trial) %>%
    group_by(ID, session) %>%
    mutate(
      chosen_prev = lag(chosen, 1),
      is_repetition = chosen == chosen_prev
    ) %>%
    group_by(ID, trial, session) %>%
    mutate(
      max_reward_trial = pmax(reward1, reward2, reward3, reward4),
      is_max = reward == max_reward_trial,
      chosen = chosen + 1
    ) %>% ungroup() %>%
    rename(
      choices = chosen,
      rewards = reward
    )
  
  tbl_sam <- as_tibble(rbind(sam1, sam2)) %>%
    inner_join(tbl_exclude %>% select(ID), by = "ID") %>%
    arrange(ID, session, block, trial) %>%
    group_by(block) %>%
    mutate(
      prev_chosen = lag(chosen, 1),
      is_repetition = chosen == prev_chosen) %>%
    rowwise() %>%
    mutate(
      is_optimal = reward == max(c(reward1, reward2)),
      regret = max(c(reward1, reward2)) - reward
    )
  
  tbl_horizon <-  as_tibble(rbind(horizon1, horizon2 %>% select(colnames(horizon1)))) %>%
    inner_join(tbl_exclude %>% select(ID), by = "ID") %>%
    arrange(ID, session, block, trial) %>%
    group_by(block) %>%
    mutate(
      prev_chosen = lag(chosen, 1),
      is_repetition = chosen == prev_chosen
    ) %>%
    rowwise() %>%
    mutate(
      is_optimal = reward == max(c(reward1, reward2)),
      regret = max(c(reward1, reward2)) - reward
    )
  tbl_horizon$is_repetition[tbl_horizon$trial == 5] <- NA
  
  
  tbl_reliability_rl <- tbl_restless %>%
    rowwise() %>%
    mutate(
      is_optimal = rewards == max(c(reward1, reward2, reward3, reward4)),
      regret = max(c(reward1, reward2, reward3, reward4)) - rewards
    ) %>%
    group_by(ID, session) %>%
    summarize(
      p_switch = mean(is_repetition, na.rm = TRUE),
      p_optimal = mean(is_optimal),
      regret = sum(regret)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
    mutate(task = "Restless")
  tbl_reliability_sam <- tbl_sam %>%
    group_by(ID, session) %>%
    summarize(
      p_switch = mean(is_repetition, na.rm = TRUE),
      p_optimal = mean(is_optimal, na.rm = TRUE),
      regret = sum(regret, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
    mutate(task = "Sam")
  tbl_reliability_horizon <- tbl_horizon %>%
    group_by(ID, session) %>%
    summarize(
      p_switch = mean(is_repetition, na.rm = TRUE),
      p_optimal = mean(is_optimal, na.rm = TRUE),
      regret = sum(regret, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = session, values_from = c(p_switch, p_optimal, regret)) %>%
    mutate(task = "Horizon")
  
  tbl_all_three <- rbind(tbl_reliability_horizon, tbl_reliability_sam, tbl_reliability_rl)
  
  tbl_reliability_measures <- tbl_all_three %>%
    group_by(task) %>%
    summarize(
      reliability_switching_c = calc_icc_3_1(p_switch_1, p_switch_2, t = "consistency"),
      reliability_optimal_c = calc_icc_3_1(p_optimal_1, p_optimal_2, t = "consistency"),
      reliability_regret_c = calc_icc_3_1(regret_1, regret_2, t = "consistency"),
      reliability_switching_a = calc_icc_3_1(p_switch_1, p_switch_2, t = "agreement"),
      reliability_optimal_a = calc_icc_3_1(p_optimal_1, p_optimal_2, t = "agreement"),
      reliability_regret_a = calc_icc_3_1(regret_1, regret_2, t = "agreement")
    ) %>% 
    pivot_longer(-task) %>%
    mutate(
      icc_type = factor(str_extract(name, "[a,c]$"), labels = c("Agreement", "Consistency")), 
      parameter = str_remove(str_remove(name, "reliability_"), "_[a,c]$")
    )
  tbl_reliability_measures$parameter <- factor(tbl_reliability_measures$parameter, labels = c("p(optimal)", "regret", "p(switch)"))
  tbl_reliability_measures <- tbl_reliability_measures %>% select(-name)
  write_csv(tbl_reliability_measures, file = "analysis/reliability-task-measures.csv")
  write_csv(tbl_all_three, file = "analysis/bandits-task-measures.csv")
  return(tbl_reliability_measures)
}

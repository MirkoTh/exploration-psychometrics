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
    count() %>% ungroup() %>% select(-n) %>%
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
    count() %>% ungroup() %>% select(-n)
  
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
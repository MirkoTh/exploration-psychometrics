library(tidyverse)

path_data <- "data/2023-11-lab-pilot/"
participants_returned <- list()

path_file <- "data/2023-11-lab-pilot/WMU/WMU_5.json"

json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  tbl_cr <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(tbl_cr)
}
# check for each participant which file has more data and select that one
hash_ids_e1_e2 <- function(path_data, participants_returned, add_gender = FALSE) {
  #' save continuous reproduction ("cr") and category learning ("cat") data
  #' with prolific ids replaced by random identifiers
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
  
  files_dir_OS <- dir(str_c(path_data, "OS"))
  fld_OS_recall <- files_dir_OS[str_detect(files_dir_OS, "recall")]
  fld_OS_processing <- files_dir_OS[str_detect(files_dir_OS, "processing")]
  fld_OS_bonus <- files_dir_OS[str_detect(files_dir_OS, "bonus")]
  fld_OS_cc <- files_dir_OS[str_detect(files_dir_OS, "comprehension")]
  
  files_dir_SS <- dir(str_c(path_data, "SS"))
  fld_SS_recall <- files_dir_SS[str_detect(files_dir_SS, "recall")]
  fld_SS_processing <- files_dir_SS[str_detect(files_dir_SS, "processing")]
  fld_SS_bonus <- files_dir_SS[str_detect(files_dir_SS, "bonus")]
  fld_SS_cc <- files_dir_SS[str_detect(files_dir_SS, "comprehension")]
  
  files_dir_WMU <- dir(str_c(path_data, "WMU"))
  fld_WMU_recall <- files_dir_WMU[!str_detect(files_dir_WMU, "comprehension") & !str_detect(files_dir_WMU, "bonus")]
  fld_WMU_bonus <- files_dir_WMU[str_detect(files_dir_WMU, "bonus")]
  fld_WMU_cc <- files_dir_WMU[str_detect(files_dir_WMU, "comprehension")]
  
  
  extract_compound_and_individual <- function(fld, task) {
    paths_individual <- str_c(path_data, task, "/", fld[!str_detect(fld, "allinone")])
    paths_compound <- str_c(path_data, task, "/", fld[str_detect(fld, "allinone")])
    return (list(indiv = paths_individual, compound = paths_compound))
  }
  
  flds <- list(fld_OS_recall, fld_OS_processing, fld_SS_recall, fld_SS_processing, fld_WMU_recall)
  tasks <- c("OS", "OS", "SS", "SS", "WMU")
  l_paths <- map2(flds, tasks, extract_compound_and_individual)
  
  names(l_paths) <- c("OS-recall", "OS-processing", "SS-recall", "SS-processing", "WMU-recall")
  
  
  
  inner_map <- safely(function(x) map(x, json_to_tibble))
  l_tbl_all <- map(l_paths, inner_map)
  l_tbl_all <- map(l_tbl_all, "result")
  l_mask <- map_lgl(l_tbl_all, ~!(is.null(.x)))
  l_tbl_all <- l_tbl_all[l_mask]
  inner_map <- function(a, b) map(
    a, function(x) c(participant_id = x$participant_id[1], ntrials = nrow(x))
  ) %>% reduce(rbind) %>% rbind() %>% as_tibble() %>% mutate(savemethod = b)
  tbl_ntrials <- map2(l_tbl_all, names(l_tbl_all), inner_map) %>% reduce(rbind)
  tbl_ntrials$task <- factor(str_detect(tbl_ntrials$savemethod, "cr"), labels = c("cat", "cr"))
  files_select <- tbl_ntrials %>% group_by(participant_id, task) %>%
    mutate(rwn_max = row_number(desc(ntrials))) %>% 
    filter(rwn_max == 1)
  l_files_select <- split(files_select, files_select$task)
  c_paths <- function(x) str_c(path_data, x$savemethod, "-participant-", x$participant_id, ".json")
  l_paths <- map(l_files_select, c_paths)
  
  tbl_cr <- reduce(map(l_paths[["cr"]], json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_cat <- reduce(map(l_paths[["cat"]], json_to_tibble), rbind)
  
  
  # add gender
  if (add_gender = TRUE) {
    fls_all <- list.files("experiments/2022-07-category-learning-II/data/")
    fl_start <- "prolific_export"
    pth <- str_c("experiments/2022-07-category-learning-II/data/", fls_all[startsWith(fls_all, fl_start)])
    l <- map(pth, read_csv)
    tbl_prolific <- reduce(l, rbind)
    tbl_cr <- tbl_cr %>% 
      left_join(tbl_prolific[, c("Participant id", "Sex")], by = c("participant_id" = "Participant id"))
  }
  
  # create a lookup table mapping prolific ids to random ids
  tbl_ids_lookup <- tibble(participant_id = unique(tbl_cr$participant_id))
  tbl_ids_lookup <- tbl_ids_lookup %>%
    group_by(participant_id) %>%
    mutate(participant_id_randomized = random_id(1)) %>% ungroup()
  write_csv(tbl_ids_lookup, str_c(path_data, "participant-lookup.csv"))
  # replace prolific ids with random ids
  tbl_cr <- tbl_cr %>% left_join(tbl_ids_lookup, by = "participant_id") %>%
    select(-participant_id) %>% rename(participant_id = participant_id_randomized)
  tbl_cat <- tbl_cat %>% left_join(tbl_ids_lookup, by = "participant_id") %>%
    select(-participant_id) %>% rename(participant_id = participant_id_randomized)
  # exclude returned and rejected participants
  tbl_cr <- tbl_cr %>% filter(!(participant_id %in% participants_returned))
  tbl_cat <- tbl_cat %>% filter(!(participant_id %in% participants_returned))
  
  write_csv(tbl_cr, str_c(path_data, "tbl_cr.csv"))
  write_csv(tbl_cat, str_c(path_data, "tbl_cat.csv"))
  
}

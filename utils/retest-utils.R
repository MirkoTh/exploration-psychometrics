
overview <- function(tbl_df) {
  #' @description summarizes how many participants completed how many sessions
  #' @param tbl_df the tbl with the by-trial data
  #' @return a list with
  #' a) a summary of participants with one or two sessions
  #' b) the details about who completed which session
  #' c) which participants to keep and which to drop for test-retest analysis
  #' d) the filtered original data set
  tbl_design_retest <- tbl_df %>%
    count(subjectID) %>% select(-n) %>%
    crossing(sessionNum = c(1, 2))
  
  tbl_retest_ntrials <- tbl_df %>% 
    count(subjectID, subjectNumber, sessionNum)
  
  tbl_overview <- tbl_design_retest %>% 
    left_join(tbl_retest_ntrials, by = c("subjectID", "sessionNum"))
  
  tbl_keep <- tbl_overview %>% filter(!is.na(subjectNumber)) %>%
    group_by(subjectID) %>%
    summarize(n_sessions = length(subjectNumber))
  
  tbl_nsessions <- tbl_retest_ntrials %>% 
    arrange(subjectID, subjectNumber, sessionNum) %>%
    count(subjectID) %>% count(n)
  
  tbl_full <- tbl_keep %>% filter(n_sessions == 2) %>%
    select(-n_sessions) %>%
    left_join(tbl_df, by = "subjectID")
  
  return(list(
    tbl_nsessions = tbl_nsessions, 
    tbl_overview = tbl_overview,
    tbl_keep = tbl_keep,
    tbl_full = tbl_full
  ))
}
time_elapsed_retest <- function(tbl_full) {
  #' @description extract time elapsed between test and retest
  #' @param tbl_full tbl with by-trial data of kept participants
  #' @return a tbl the elapsed time per participant with two sessions
  tbl_full %>% group_by(subjectID) %>%
    mutate(
      date_session_prep1 = str_extract(file_name, ".*_.*_"),
      date_session_prep2 = str_match(date_session_prep1, ".*_(.*)_")[,2],
      session_day = as.numeric(str_extract(date_session_prep2, "^[0-9]+")),
      session_day = as.character(ifelse(session_day <= 9, str_c("0", session_day), session_day)),
      session_month = str_match(date_session_prep2, "[0-9]+([a-zA-Z]*)")[,2],
      session_year = as.numeric(str_extract(date_session_prep2, "[0-9]*$")),
      session_date = str_c(session_day, "-", session_month, "-", session_year),
      session_date2 = as.Date(session_date, format = "%d-%b-%Y")
    ) %>%
    count(subjectID, session_date2) %>% group_by(subjectID) %>%
    mutate(rwn = row_number(session_date2)) %>%
    ungroup() %>%
    pivot_wider(names_from = rwn, values_from = session_date2, names_prefix = "session_") %>%
    mutate(time_duration = session_2 - session_1)
}
time_elapsed_vaccine <- function(tbl_full) {
  #' @description extract time elapsed between test and retest
  #' @param tbl_full tbl with by-trial data of kept participants
  #' @return a tbl the elapsed time per participant with two sessions
  tbl_full %>% group_by(subjectID) %>%
    mutate(
      date_session_prep = str_match(file_name, ".*_(.*)_")[,2],
      session_date = as.Date(date_session_prep, format = "%Y%m%d")
    ) %>% 
    count(subjectID, session_date) %>% group_by(subjectID) %>%
    mutate(rwn = row_number(session_date)) %>%
    ungroup() %>%
    pivot_wider(names_from = rwn, values_from = session_date, names_prefix = "session_") %>%
    mutate(time_duration = session_2 - session_1)
}


bring_rewards_to_long <- function(l_retest, l_vaccine) {
  #' @description bring by-trial data from one col into several cols
  #' @param l_retest list with preprocessed retest data
  #' @param l_vaccine list with preprocessed vaccine data
  #' @return a tbl with the data from all studies merged in long format
  #' 
  tbl_both_full <- l_retest$tbl_full %>% 
    select(-c(file_name, subjectNumber)) %>% 
    rbind(
    l_vaccine$tbl_full %>% 
      select(-c(file_name, vaccineSession, subjectNumber, repeatNumber))
  )
  
  r_long <- tbl_both_full %>%
    pivot_longer(cols = str_c("r", 1:10), names_to = "trial", values_to = "reward") %>%
    select(-c(str_c("c", 1:10), str_c("rt", 1:10))) %>%
    mutate(trial = str_extract(trial, "[0-9]+"))
  c_long <- tbl_both_full %>%
    pivot_longer(cols = str_c("c", 1:10), names_to = "trial", values_to = "choice") %>%
    select(-c(str_c("r", 1:10), str_c("rt", 1:10))) %>%
    mutate(trial = str_extract(trial, "[0-9]+"))
  rt_long <- tbl_both_full %>%
    pivot_longer(cols = str_c("rt", 1:10), names_to = "trial", values_to = "rt") %>%
    select(-c(str_c("r", 1:10), str_c("c", 1:10))) %>%
    mutate(trial = str_extract(trial, "[0-9]+"))
  tbl_both_full_long <- r_long %>%
    left_join(
      c_long, by = c(
        "subjectID", "expt_name", "sessionNum", "block",
        "game", "gameLength", "uc", "m1", "m2", "gID", "trial"
      )
    ) %>% left_join(
      rt_long, by = c(
        "subjectID", "expt_name", "sessionNum", "block",
        "game", "gameLength", "uc", "m1", "m2", "gID", "trial"
      )
    )
  # remove trials 6-10 in short horizon games
  tbl_both_full_long <- tbl_both_full_long %>% filter(choice != "NaN")
  return(tbl_both_full_long)
}

running_means <- function(tbl_both_full_long) {
  tbl_both_full_long <- tbl_both_full_long %>%
    group_by(expt_name, subjectID, sessionNum, block, game, gameLength, uc, m1, m2, gID) %>%
    mutate(
      running_mean_1 = NA, 
      running_mean_2 = NA,
      reward_1 = NA,
      reward_2 = NA
    )
  tbl_both_full_long$reward_1[tbl_both_full_long$choice == 1] <- tbl_both_full_long$reward[tbl_both_full_long$choice == 1]
  tbl_both_full_long$reward_2[tbl_both_full_long$choice == 2] <- tbl_both_full_long$reward[tbl_both_full_long$choice == 2]
  
  rm_1 <- tbl_both_full_long %>% 
    filter(!is.na(reward_1)) %>% 
    mutate(
      running_mean_1 = cummean(reward_1),
      n_choices_1 = cumsum(!is.na(reward_1))
      )
  
  rm_2 <- tbl_both_full_long %>%
    filter(!is.na(reward_2)) %>% 
    mutate(
      running_mean_2 = cummean(reward_2),
      n_choices_2 = cumsum(!is.na(reward_2))
    )
  
  tmp <- tbl_both_full_long %>% 
    select(-c(running_mean_1, running_mean_2)) %>% 
    left_join(
      rm_1 %>% select(-c(reward_1, reward_2, choice, rt, reward, running_mean_2)), by = c(
      "expt_name", "subjectID", "sessionNum", "block", "game", "gameLength", "uc", "m1", "m2", "gID", "trial"
    )) %>% 
    left_join(
      rm_2 %>% select(-c(reward_1, reward_2, choice, rt, reward, running_mean_1)), by = c(
      "expt_name", "subjectID", "sessionNum", "block", "game", "gameLength", "uc", "m1", "m2", "gID", "trial"
    ))
  tbl_out <- tmp %>% fill(
    running_mean_1, .direction = "down"
  ) %>% fill(
    running_mean_2, .direction = "down"
  )  %>% fill(
    n_choices_1, .direction = "down"
  )  %>% fill(
    n_choices_2, .direction = "down"
  ) %>% mutate(
    running_mean_1 = lag(running_mean_1),
    running_mean_2 = lag(running_mean_2),
    n_choices_1 = lag(n_choices_1),
    n_choices_2 = lag(n_choices_2),
    trial = as.numeric(trial)
    ) %>% ungroup()
  
  tbl_out <- tbl_out %>%
    mutate(
      is_lower_mean_2 = running_mean_2 < running_mean_1,
      is_higher_info_2 = n_choices_2 < n_choices_1
    )
  return(tbl_out)
}

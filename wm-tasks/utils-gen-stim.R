generate_restless_bandits <- function(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials) {
  #' 
  #' @description generate random walk data on bandits with independent means
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq noise variance
  #' @param mu1 initial means of different bandits
  #' @param lambda decay parameter to keep bandit means closer to 0
  #' @param nr_trials number of trials to generate
  #' @return a tbl with values for all trials and all bandits
  nr_bandits <- length(mu1)
  mus <- matrix(nrow = nr_trials, ncol = nr_bandits)
  mus[1, ] <- mu1
  for (t in 2:nr_trials) {
    mus[t, ] <- lambda * mus[t-1, ] + rnorm(nr_bandits, 0, sqrt(sigma_xi_sq))
  }
  noise <- matrix(
    rnorm(nr_trials * nr_bandits, 0, sqrt(sigma_epsilon_sq)),
    nrow = nr_trials, ncol = nr_bandits
  )
  as_tibble(as.data.frame(mus + noise)) %>% 
    mutate(trial_id = 1:nr_trials) %>%
    rename("Arm 1" = V1, "Arm 2" = V2, "Arm 3" = V3, "Arm 4" = V4)
}


generate_rl_bandits_as_required <- function(as_required, session_id) {
  #seed_counter <- 1
  while(as_required == FALSE) {
    
    #seeds <- round(runif(100, 0, 10000000), 0)
    # 
    seed <- c(2386688, 6719044)[session_id]
    #seed <- seeds[seed_counter]
    #cat(str_c(seed, "\n"))
    set.seed(seed)
    #seed_counter <- seed_counter + 1

    mu1 <- c(0, 0, 0, 0)
    #mu1 <- c(-60, -20, 20, 60)
    nr_trials <- 200
    sigma_xi_sq <- 36
    sigma_epsilon_sq <- 16
    lambda <- .925#.9836
    tbl_4a_rlb <- generate_restless_bandits(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials)
    
    # difference between largest and second-largest reward
    tbl_4a_rlb$max_reward <- pmap_dbl(tbl_4a_rlb[, c("Arm 1", "Arm 2", "Arm 3", "Arm 4")], max)
    differences_to_max <- pmap(
      tbl_4a_rlb[, c("Arm 1", "Arm 2", "Arm 3", "Arm 4", "max_reward")], 
      ~ ..5 - c(..1, ..2, ..3, ..4)
    ) %>% reduce(rbind) %>% as.matrix()
    differences_to_max[differences_to_max == 0] <- 1000
    tbl_differences_to_max <- as_tibble(differences_to_max)
    tbl_4a_rlb$min_diff_to_max <- pmap_dbl(tbl_differences_to_max, min)
    
    
    differences_all <- pmap(
      tbl_4a_rlb, 
      ~ abs(c(..1 - ..2, ..1 - ..3, ..1 - ..4, ..2 - ..3, ..2 - ..3, ..3 - ..4))
    ) %>% reduce(rbind) %>% as.matrix()
    tbl_4a_rlb$avg_difference <- rowMeans(differences_all)
    
    bins_avg <- c(-.1, 10, 20, 30, 40, 1000)
    bins_max <- c(-.1, 4, 8, 10, 12, 16, 20, 1000)
    
    tbl_4a_rlb$min_diff_to_max_cut <- cut(tbl_4a_rlb$min_diff_to_max, bins_max)
    tbl_4a_rlb$avg_difference_cut <- cut(tbl_4a_rlb$avg_difference, bins_avg)
    
    tbl_avg_props <- tbl_4a_rlb %>%
      group_by(avg_difference_cut) %>% 
      count() %>% ungroup() %>% 
      mutate(n_total = sum(n), prop = n/n_total)
    
    tbl_max_props <- tbl_4a_rlb %>%
      group_by(min_diff_to_max_cut) %>% 
      count() %>% ungroup() %>% 
      mutate(n_total = sum(n), prop = n/n_total)
    
    nr_ones <- tbl_4a_rlb %>%
      select(trial_id, `Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`) %>%
      pivot_longer(cols = -trial_id) %>% 
      group_by(trial_id) %>% 
      filter(value == max(value)) %>% 
      group_by(name) %>% count() %>% ungroup() %>%
      mutate(prop_1 = n / sum(n))
    
    thx_max <- .25
    thx_avg <- .05
    thx_nr1 <- 25
    
    flag_max <- tbl_max_props$prop[tbl_max_props$min_diff_to_max_cut == "(20,1e+03]"] > thx_max
    flag_avg <- tbl_avg_props$prop[tbl_avg_props$avg_difference_cut == "(-0.1,10]"] > thx_avg
    flag_nr1 <- sum(nr_ones$n < thx_nr1)
    
    # if (is_empty(flag_max)) {flag_max <- FALSE}
    # if (is_empty(flag_avg)) {flag_avg <- FALSE}
    # 
    # if (flag_max + flag_avg + flag_nr1 == 0) {as_required <- TRUE}
    if (flag_nr1 == 0) {as_required <- TRUE}
    #as_required <- TRUE
    
  }
  return(tbl_4a_rlb)
}


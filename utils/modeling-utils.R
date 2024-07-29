make_conditions_and_rewards <- function(
    opts, mn_spacing, vars, n_trials) {
  #' create tbl with info about conditions and rewards per trial
  #'
  #' @description creates a tbl with cols condition_id, n_options
  #' and list cols reward_stats (mns and sds) and rewards (by-trial rewards)
  #' @param opts vector with number of response options across conditions
  #' @param mn_spacing constant spacing between means
  #' @param vars variances of the data
  #' @param n_trials nr trials of the experiment
  #' @return the tbl with all information
  
  tbl_conditions <- crossing(opts, mn_spacing, vars)
  colnames(tbl_conditions) <- c("n_options", "mn_spacing", "var")
  tbl_conditions$reward_stats <- pmap(
    tbl_conditions, ~ list(
      reward_mn = seq(..2, ..1 * ..2, by = ..2),
      reward_var = rep(..3, ..1)
    )
  )
  
  my_rnorm <- function(mn, sd, n) {
    rnorm(n, mn, sd)
  }
  
  l_out <- map(1:nrow(tbl_conditions), ~ pmap(list(
    tbl_conditions$reward_stats[[.x]]$reward_mn,
    tbl_conditions$reward_stats[[.x]]$reward_var
  ), ~ my_rnorm(n = n_trials, mn = ..1, sd = sqrt(..2))))
  
  # rewards per trial as matrix
  tbl_conditions$rewards <- l_out %>%
    map(~ unlist(.x) %>% matrix(nrow = length(.x), byrow = TRUE)) %>%
    map(~ t(.x))
  
  return(tbl_conditions)
}


update_kalman_filter <- function(var_prev, var_innov, var_error) {
  kg <- (var_prev + var_innov) / (var_prev + var_innov + var_error)
  var_new <- (1 - kg) * (var_prev + var_innov)
  l_params_updated <- list(kg = kg, var_new = var_new)
  return(l_params_updated)
}


rl_softmax_sim <- function(rewards, sigma_epsilon_sq, m0, params, choices = NULL) {
  nt <- nrow(rewards) # number of time points
  no <- ncol(rewards) # number of options
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(sigma_epsilon_sq, ncol = no, nrow = nt + 1) # to hold the posterior variances
  choice <- rep(0, nt) # to hold the choices of the RL agent
  reward <- rep(0.0, nt) # to hold the obtained rewards by the RL agent
  # loop over all time points
  for (t in 1:nt) {
    if (is.null(choices)) {
      # use the prior means and compute the probability of choosing each option
      tb_exp <- params$gamma * m[t, ]
      p <- exp(ifelse(tb_exp > 700, 700, tb_exp))
      p <- p / sum(p)
      # choose an option according to these probabilities
      choice[t] <- sample(1:no, size = 1, prob = p)
      # get the reward of the choice
    } else {
      choice <- choices
    }
    
    reward[t] <- rewards[t, choice[t]]
    
    if (params$model == "Kalman") {
      # set the Kalman gain for not chosen options
      kt <- rep(0, no)
      # set the Kalman gain for the chosen option
      kt[choice[t]] <- (v[t, choice[t]]) / (v[t, choice[t]] + sigma_epsilon_sq)
      # compute the posterior means
      m[t + 1, ] <- m[t, ] + kt * (reward[t] - m[t, ])
      # compute the posterior variances
      v[t + 1, ] <- (1 - kt) * (v[t, ])
    } else if (params$model == "Decay") {
      m[t + 1, ] <- params$eta * m[t, ]
      m[t + 1, choice[t]] <- m[t + 1, choice[t]] + reward[t]
      v[t + 1, choice[t]] <- 0
    }
  }
  # return everything of interest
  return(list(m = m, v = v, choice = choice, reward = reward))
}



total_rewards <- function(model, gamma, eta) {
  #' helper function to compute total rewards for given gamma
  #' @return the tbl with all information
  
  l_params <- list(model = model, gamma = gamma, eta = eta)
  l_l_m <- pmap(
    list(tbl_conditions$rewards, as.list(tbl_conditions$var)),
    rl_softmax_sim,
    m0 = 0, params = l_params
  )
  map_dbl(l_l_m, ~ sum(.x[["reward"]]))
}


iterate_once <- function(x) {
  #' iterate over gamma values for each condition
  #'
  #' @description iterate once over a sequence of gamma values and return total_rewards
  #' once for each condition in tbl_conditions
  #' @param x dummy param to iterate over
  #' @return the results tbl
  
  tbl_params <- params_grid()
  tbl_conditions <- make_conditions_and_rewards(opts, mn_spacing, vars, n_trials)
  l_results <- pmap(tbl_params, total_rewards)
  tbl_results <- l_results %>%
    reduce(rbind) %>%
    as_tibble(.name_repair = "unique")
  colnames(tbl_results) <- interaction(tbl_conditions$n_options, tbl_conditions$var)
  tbl_results <- cbind(tbl_results, tbl_params)
  tbl_results <- tbl_results %>%
    pivot_longer(cols = -colnames(tbl_params), names_to = "noxvar", values_to = "reward_tot") %>%
    mutate(
      n_options = str_extract(noxvar, "^([0-9]+)"),
      var = str_extract(noxvar, "([0-9]+)$")
    )
  return(tbl_results)
}


params_grid <- function() {
  #' create grid of parameter combinations to iterate over
  #'
  #' @description crosses different different parameters for each model
  #' and combines them in one tbl
  #' @return tbl with parameter combinations
  
  gamma <- seq(.1, 5, by = .1) # temperature of softmax
  eta <- seq(.2, 1, by = .2) # decay rate in decay rule
  model <- "Decay"
  tbl_params_decay <- crossing(model, gamma, eta)
  eta <- 0
  model <- "Kalman"
  tbl_params_kalman <- crossing(model, gamma, eta)
  tbl_params <- rbind(tbl_params_decay, tbl_params_kalman)
  return(tbl_params)
}


make_condition_trials <- function(n_options, params_fixed, condition_distinct_ii) {
  #' make forced-choice trials for all conditions
  #'
  #' @description makes a tbl for the forced-choice trials
  #' using desired parameter settings
  #' @param n_options number of response options
  #' @param params_fixed list with simulation parameters
  #' @param condition_distinct_ii vector with most distinctive response option 1
  #' @return tbl with separate row for each forced-choice trial
  #'
  my_rnorm <- function(m, sd) rnorm(params_fixed$n_trials, m, sd)
  l_rewards <- pmap(list(m = params_fixed$v_means, sd = params_fixed$v_sd), my_rnorm)
  tbl_prep <- tibble(
    trial_id = rep(
      seq(1, params_fixed$n_trials, by = 1), length(params_fixed$conditions)
    ),
    condition = rep(params_fixed$conditions, each = params_fixed$n_trials),
    setsize = n_options,
    option_selected = factor(c(
      rep(1:n_options, each = params_fixed$n_trials / n_options),
      rep(1:n_options, params_fixed$n_trials / n_options),
      condition_distinct_ii
    )),
    value_sampled = NA,
    # value_sampled = rnorm(params_fixed$v_means[option_selected], params_fixed$v_sd),
    distance_t = abs(params_fixed$n_trials - trial_id) + params_fixed$ri,
    distance_t_log = log(distance_t)
  )
  extract_nested_sample <- function(idx_l, idx_pos) {
    l_rewards[[idx_l]][idx_pos]
  }
  tbl_prep$value_sampled <- pmap(
    tbl_prep[, c("option_selected", "trial_id")], ~ extract_nested_sample(.x, .y)
  ) %>% unlist()
  return(list(tbl_prep, l_rewards))
}

calc_discriminability <- function(tb, c, alpha) {
  #' SIMPLE discriminability
  #'
  #' @description calculates discriminability of presented items
  #' given temporal structure using SIMPLE formulae
  #' @param tbl for one of the three conditions with by-trial info
  #' @param c SIMPLE parameter: scaling of exponential function
  #' @param alpha SIMPLE parameter: 1 = exponential, 2 = Gaussian
  #' @return list with all discriminabilities
  #'
  distances_all <- tb$distance_t_log
  # map over all distances besides n
  all_positions <- seq(1, length(distances_all), by = 1)
  f_distinct <- function(n) 1 / sum(map_dbl(distances_all, ~ exp(-c * abs(distances_all[n] - .x)^alpha)))
  map_dbl(all_positions, f_distinct)
}

kalman_forced_choice <- function(params_fixed, l_tbl_rewards) {
  #' update Kalman filter using forced-choices
  #'
  #' @description update Kalman filter over n trials using forced choices
  #' from tbl with by-trial info
  #' @param params_fixed list with simulation parameters
  #' @l_tbl_rewards nested list with reward matrix
  #' and tbl with by-trial info about choices and rewards
  #' @return nested list containing list with Kalman
  #' results (i.e., ms, sds, choices, rewards)
  #'
  
  # reward matrix
  rewards <- l_tbl_rewards[[2]]
  # tbl with by-trial info
  tbl_2 <- l_tbl_rewards[[1]]
  m_rewards_2 <- matrix(
    unlist(rewards)[1:(2 * params_fixed$n_trials)],
    nrow = params_fixed$n_trials, ncol = 2, byrow = FALSE
  )
  l_options_selected <- list(
    tbl_2$option_selected[seq(1, params_fixed$n_trials, by = 1)],
    tbl_2$option_selected[seq(params_fixed$n_trials + 1, 2 * params_fixed$n_trials, by = 1)],
    tbl_2$option_selected[seq(params_fixed$n_trials * 2 + 1, 3 * params_fixed$n_trials, by = 1)]
  )
  
  rl_softmax_sim_wrap <- function(choices, m_rew, sd, m, pars) {
    rl_softmax_sim(m_rew, sd, m, pars, choices)
  }
  
  map(
    l_options_selected, rl_softmax_sim_wrap,
    m_rewards_2, params_fixed$v_sd, 0, params_fixed
  )
}


# two helper functions to calculate density above zero after each choice
cum_density_above_zero <- function(i, l) {
  #' @description calculate density of difference of posterior distributions above zero
  #'
  
  m <- l$m[i, 1] + l$m[i, 2]
  sd <- sqrt(l$v[i, 1] + l$v[i, 2])
  pnorm(0, m, sd)
}

wrap_all_densities <- function(l) {
  map_dbl(1:nrow(l$m), cum_density_above_zero, l = l)
}


agg_runs <- function(condition_idx, l_results, params_fixed) {
  #' aggregate simulation runs by condition and trial
  #'
  #' @description calculate average proportion people should
  #' choose the option with the higher mean
  #' @param condition_idx index stating which condition should be aggregated
  #' @param l_results list with simulation results
  #' @param params_fixed list with simulation parameters
  #' @return tbl grouped by condition and trial
  
  map(l_results, condition_idx) %>%
    unlist() %>%
    matrix(nrow = 13, ncol = n_runs, byrow = FALSE) %>%
    as.data.frame() %>%
    mutate(
      trial_id = seq(1, nrow(.)),
      condition = params_fixed$conditions[condition_idx],
    ) %>%
    pivot_longer(cols = starts_with("V")) %>%
    # group_by(condition, trial_id) %>%
    # summarize(
    #   val_mean = mean(value),
    #   val_sd = sd(value)
    # ) %>%
    ungroup()
}



sample_y <- function(subj, mu_t1, mu_t2, var, n) {
  #' sample t1 and t2 values for one subject given means and error variance
  #'
  #' @description sample t1 and t2 values from normal distributions
  #' for one subject given means and error variance; assemble everything in tibble
  #' @param subj subject id
  #' @param mu_t1 mean at t1
  #' @param mu_t2 mean at t2
  #' @param var error variance
  #' @param n number of samples
  #' @return the tbl with columns subject, val t1, and val t2
  
  y_t1 <- rnorm(n, mu_t1, sqrt(var))
  y_t2 <- rnorm(n, mu_t2, sqrt(var))
  tibble(
    subject = subj,
    t1 = y_t1,
    t2 = y_t2
  )
}

reliability_pipeline <- function(n_subjects, n_trials, reliability) {
  #' run reliability pipeline of Bayesian model once
  #'
  #' @description create a data set with given number of subjects, trials,
  #' and reliability and run Bayesian model on that data set
  #' @param n_subjects number of subjects in simulated data set
  #' @param n_trials number of subjects in simulated data set
  #' @param reliability reliability in simulated data set
  #' @return a summary tbl with means and ses of posterior distributions
  
  # set up data set ---------------------------------------------------------
  
  d_r <- 1.5
  a_r <- c(0 - d_r / 2, 0 + d_r / 2) # fixed effect over two time points
  
  sig_sq_0 <- .5 # within variance
  var_subj_t1 <- 1 # between-subjects variance t1
  var_subj_t2 <- 1 # between-subjects variance t2
  cov_t1_t2 <- reliability * sqrt(var_subj_t1) * sqrt(var_subj_t2)
  var_error <- .5 # error variance
  
  mu_subj <- c(0, 0)
  R_bold <- matrix(c(var_subj_t1, cov_t1_t2, cov_t1_t2, var_subj_t2), nrow = 2) # vcov matrix of subject level variability
  
  tau_s <- MASS::mvrnorm(n_subjects, mu_subj, R_bold)
  
  mu_rs <- matrix(rep(a_r, each = n_subjects), ncol = 2) + tau_s
  tbl_sample <- tibble(
    subj = 1:nrow(mu_rs),
    mu_t1 = mu_rs[, 1],
    mu_t2 = mu_rs[, 2]
  )
  
  tbl_sim <- pmap(tbl_sample, sample_y, var = sig_sq_0, n = n_trials) %>% reduce(rbind)
  # add independent random error to both time points
  tbl_sim$t1 <- tbl_sim$t1 + rnorm(nrow(tbl_sim), 0, sqrt(var_error))
  tbl_sim$t2 <- tbl_sim$t2 + rnorm(nrow(tbl_sim), 0, sqrt(var_error))
  
  tbl_sim_long <- pivot_longer(tbl_sim, c(t1, t2), names_to = "timepoint", values_to = "y")
  
  
  # fit Bayesian reliability model ------------------------------------------
  
  
  stan_normal_rel <- stan_normal_reliability()
  mod_normal_rel <- cmdstan_model(stan_normal_rel)
  
  x <- tbl_sim_long$timepoint %>%
    as.factor() %>%
    as.numeric()
  
  l_data <- list(
    n_data = nrow(tbl_sim_long),
    n_subj = length(unique(tbl_sim_long$subject)),
    subj = as.numeric(factor(
      tbl_sim_long$subject,
      labels = 1:length(unique(tbl_sim_long$subject))
    )),
    x = x,
    response = tbl_sim_long$y
  )
  
  fit_normal_rel <- mod_normal_rel$sample(
    data = l_data, iter_sampling = 2000, iter_warmup = 1000, chains = 1
  )
  
  pars_interest <- c("mu_ic", "mu_time", "Sigma")
  tbl_draws <- fit_normal_rel$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_normal_rel$summary(variables = pars_interest)
  
  tbl_posterior <- tbl_draws %>%
    dplyr::select(starts_with(c("mu", "Sigma[2,1]")), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(c("mu", "Sigma[2,1]")), names_to = "parameter", values_to = "value") %>%
    mutate(parameter = factor(parameter, labels = c("Intercept", "Time", "Reliability")))
  
  
  tbl_descriptive <- grouped_agg(tbl_posterior, parameter, value)
  
  return(tbl_descriptive)
}


repeat_tibble <- function(tbl_df, n_reps) {
  #' concatenate the same tibble several times
  #'
  #' @description copy a tibble n_reps times and rbind it to the original tibble
  #' @param tbl_df the tbl to be repeated
  #' @param n_reps the number of times the tbl should be repeated
  #' @return the new larger tibble
  
  i <- 1
  tbl_df_new <- tbl_df
  while (i < n_reps) {
    tbl_df_new <- rbind(tbl_df_new, tbl_df)
    i <- i + 1
  }
  return(tbl_df_new)
}


kalman_learning <- function(tbl_df, no, sigma_xi_sq, sigma_epsilon_sq, m0 = NULL, v0 = NULL, lambda = .9836, decay_center = 0) {
  #' Kalman filter without choice model given chosen options by participants
  #'
  #' @description applies Kalman filter equations for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance
  #' @return a tbl with by-trial posterior means and variances for all bandits
  rewards <- tbl_df$rewards
  choices <- tbl_df$choices
  
  nt <- length(rewards) # number of time points
  if (is.null(m0)) {
    m0 <- 0
  }
  if (is.null(v0)) {
    v0 <- 1000
  }
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(v0, ncol = no, nrow = nt + 1) # to hold the posterior variances
  
  for (t in 1:nt) {
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    kt[choices[t]] <- (v[t, choices[t]] + sigma_xi_sq) / (v[t, choices[t]] + sigma_epsilon_sq + sigma_xi_sq)
    # compute the posterior means
    m[t + 1, ] <- m[t, ] + kt * (rewards[t] - m[t, ])
    # compute the posterior variances
    v[t + 1, ] <- (1 - kt) * (v[t, ] + sigma_xi_sq)
    # decay towards decay center
    m[t + 1, ] <- lambda * m[t + 1, ] + (1 - lambda) * decay_center
    v[t + 1, ] <- lambda^2 * v[t + 1, ] + sigma_xi_sq
  }
  
  tbl_m <- as.data.frame(m)
  # prevent v from becoming too small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- str_c("m_", 1:no)
  colnames(tbl_v) <- str_c("v_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, tbl_v))
  
  return(tbl_return)
}


kalman_learning_choose <- function(tbl_df, tbl_rewards, no, sigma_xi_sq, sigma_epsilon_sq, params_choice, m0 = NULL, v0 = NULL) {
  #' Kalman filter with choice model
  #'
  #' @description applies Kalman filter equations for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance
  #' @return a tbl with by-trial posterior means and variances for all bandits
  
  nt <- nrow(tbl_rewards) # number of time points
  if (is.null(m0)) {
    m0 <- 0
  }
  if (is.null(v0)) {
    v0 <- 1000
  }
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(v0, ncol = no, nrow = nt + 1) # to hold the posterior variances
  c_probs <- matrix(nrow = nt, ncol = no)
  choices <- rep(0, nt)
  rewards <- rep(0, nt)
  
  for (t in 1:nt) {
    if (params_choice$choicemodel == "softmax") {
      c_probs[t, ] <- softmax_choice_prob(matrix(m[t, ], ncol = no), params_choice$gamma)
    } else if (params_choice$choicemodel == "thompson") {
      c_probs[t, ] <- thompson_choice_prob_map(matrix(m[t, ], ncol = no), matrix(v[t, ], ncol = no), no)
    } else if (params_choice$choicemodel == "ucb") {
      c_probs[t, ] <- ucb_choice_prob(matrix(m[t, ], ncol = no), matrix(v[t, ], ncol = no), sigma_xi_sq, params_choice$gamma, params_choice$beta)
    }
    choices[t] <- sample(1:no, 1, prob = c_probs[t, ])
    rewards[t] <- as_vector(tbl_rewards[t, choices[t]])
    
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    kt[choices[t]] <- (v[t, choices[t]] + sigma_xi_sq) / (v[t, choices[t]] + sigma_epsilon_sq + sigma_xi_sq)
    # compute the posterior means
    m[t + 1, ] <- m[t, ] + kt * (rewards[t] - m[t, ])
    # compute the posterior variances
    v[t + 1, ] <- (1 - kt) * (v[t, ] + sigma_xi_sq)
  }
  tbl_m <- as.data.frame(m)
  # constrain v from becoming to small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- str_c("m_", 1:no)
  colnames(tbl_v) <- str_c("v_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, tbl_v))
  
  return(list(tbl_learned = tbl_return, c_probs = c_probs))
}


delta_learning <- function(tbl_df, no, delta, m0 = NULL, is_decay = FALSE) {
  #' delta learning without choice model given chosen options by participants
  #'
  #' @description applies delta rule learning for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param delta learning rate
  #' @return a tbl with by-trial estimated means for all bandits
  
  rewards <- tbl_df$rewards
  choices <- tbl_df$choices
  
  nt <- length(rewards) # number of time points
  if (is.null(m0)) {
    m0 <- 0
  }
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  
  if (!is_decay) {
    f_update <- function() {
      lr <- rep(0, no)
      # set the learning rate for the chosen option
      lr[choices[t]] <- delta
      # compute the estimated means
      out <- m[t, ] + lr * (rewards[t] - m[t, ])
      return(out)
    }
  } else if (is_decay) {
    f_update <- function() {
      lr <- rep(delta, no)
      choices_oh <- rep(0, no)
      # set the decay rate for all options
      choices_oh[choices[t]] <- 1
      # compute the estimated means
      out <- m[t, ] * lr + (rewards[t] * choices_oh)
      return(out)
    }
  }
  
  for (t in 1:nt) {
    m[t + 1, ] <- f_update()
  }
  tbl_m <- as.data.frame(m)
  colnames(tbl_m) <- str_c("m_", 1:no)
  tbl_return <- tibble(tbl_m)
  
  return(tbl_return)
}




normprobs <- function(A_current, mt, vt) {
  #' @description computes probabilities of current bandit being larger than other bandits
  #' @param A_current matrix with pairwise comparisons for current bandit
  #' @param mt prior predictive means on trial t
  #' @param vt prior predictive variances on trial t
  
  newM <- as.vector(A_current %*% mt)
  # newV is the covariance matrix of the difference scores
  newV <- A_current %*% diag(vt) %*% t(A_current)
  # calculate the (inverse) cumulative probability with the Miwa algorithm. Note: this is slow!
  prob <- pmvnorm(lower = rep(0, nrow(A_current)), mean = newM, sigma = newV, algorithm = Miwa(steps = 128))
  # If there are any probabilities below 0 due to numerical issues, set these to 0
  prob[prob < 0] <- 0
  return(prob)
}


choice_probs_rb <- function(l_results_by_id) {
  #' @description wrapper around Thompson sampling for RB
  ms_rb <- l_results_by_id[, c("m_1", "m_2", "m_3", "m_4")] %>% as.matrix()
  vs_rb <- l_results_by_id[, c("v_1", "v_2", "v_3", "v_4")] %>% as.matrix()
  tbl_choice_probs <- thompson_choice_prob_map(ms_rb, vs_rb, 4) %>%
    as.data.frame() %>%
    as_tibble()
  return(tbl_choice_probs)
}


choice_probs_e2 <- function(l_results_by_id) {
  #' @description wrapper around Thompson sampling for E2
  ms_e2 <- l_results_by_id[, c("m_1", "m_2")] %>% as.matrix()
  vs_e2 <- l_results_by_id[, c("v_1", "v_2")] %>% as.matrix()
  tbl_choice_probs <- thompson_choice_prob_map(ms_e2, vs_e2, 2) %>%
    as.data.frame() %>%
    as_tibble()
  return(tbl_choice_probs)
}


simulate_kalman <- function(
    sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, lambda, nr_trials,
    params_decision, simulate_data, seed, tbl_rewards, mu_init = NULL, decay_center = 0,
    choices_made = NULL
) {
  #'
  #' @description simulate choices from a Kalman filter with some choice model
  #' @param sigma_prior prior variance
  #' @param mu_prior prior mean
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance
  #' @param lambda decay parameter of random walk
  #' @param nr_trials number of choices to simulate
  #' @param params_decision parameters of decision model
  #' @param simulate_data should new data be generated for every participant
  #' @param seed seed value of iteration
  #' @param tbl_rewards if data are not simulated, take this tbl instead
  #' @param decay_center decay_center of the random walk
  #' @return a tbl with by-trial posterior means and variances for the chosen bandits
  #'
  set.seed(seed)
  if (simulate_data) {
    if (is.null(mu_init)) {
      tbl_rewards <- generate_restless_bandits(
        sigma_xi_sq, sigma_epsilon_sq, c(-60, -20, 20, 60), lambda, nr_trials
      ) %>% select(-trial_id)
    } else {
      mu_init <- rnorm(4, 50, 3)
      tbl_rewards <- generate_restless_bandits(
        sigma_xi_sq, sigma_epsilon_sq, mu_init, lambda, nr_trials, center_decay = 50
      ) %>% select(-trial_id)
    }
  }
  
  nt <- nrow(tbl_rewards) # number of time points
  no <- ncol(tbl_rewards) # number of options
  m <- matrix(mu_prior, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(sigma_prior, ncol = no, nrow = nt + 1) # to hold the posterior variances
  choices <- rep(0, nt + 1) # to hold the choices of the RL agent
  rewards <- rep(0.0, nt + 1) # to hold the obtained rewards by the RL agent
  
  if (is.null(choices_made)){
    # here choices are generated/predicted, and learning is based on these choices
    for (t in 1:nt) {
      p <- choice_prob(matrix(m[t, ], 1, ncol(m)), matrix(v[t, ], 1, ncol(v)), sigma_xi_sq, params_decision)
      # choose an option according to these probabilities
      choices[t] <- sample(1:4, size = 1, prob = p)
      # get the reward of the choice
      rewards[t] <- tbl_rewards[t, choices[t]] %>% as_vector()
      kt <- rep(0, no)
      # set the Kalman gain for the chosen option
      kt[choices[t]] <- (v[t, choices[t]] + sigma_xi_sq) / (v[t, choices[t]] + sigma_epsilon_sq + sigma_xi_sq)
      # compute the posterior means
      m[t + 1, ] <- m[t, ] + kt * (tbl_rewards[t, ] - m[t, ]) %>% as_vector()
      # compute the posterior variances
      v[t + 1, ] <- (1 - kt) * (v[t, ] + sigma_xi_sq)
      
      # decay towards decay center
      m[t + 1, ] <- lambda * m[t + 1, ] + (1 - lambda) * decay_center
      v[t + 1, ] <- lambda^2 * v[t + 1, ] + sigma_xi_sq
    }
  } else if (!is.null(choices_made)) {
    # here choices are generated/predicted, but learning is fixed (given the made choices by the participants)
    for (t in 1:nt) {
      p <- choice_prob(matrix(m[t, ], 1, ncol(m)), matrix(v[t, ], 1, ncol(v)), sigma_xi_sq, params_decision)
      # choose an option according to these probabilities
      choices[t] <- sample(1:4, size = 1, prob = p)
      # get the reward of the choice
      rewards[t] <- tbl_rewards[t, choices[t]] %>% as_vector()
      kt <- rep(0, no)
      # set the Kalman gain for the chosen option
      kt[choices_made[t]] <- (v[t, choices_made[t]] + sigma_xi_sq) / (v[t, choices_made[t]] + sigma_epsilon_sq + sigma_xi_sq)
      # compute the posterior means
      m[t + 1, ] <- m[t, ] + kt * (tbl_rewards[t, ] - m[t, ]) %>% as_vector()
      # compute the posterior variances
      v[t + 1, ] <- (1 - kt) * (v[t, ] + sigma_xi_sq)
      
      # decay towards decay center
      m[t + 1, ] <- lambda * m[t + 1, ] + (1 - lambda) * decay_center
      v[t + 1, ] <- lambda^2 * v[t + 1, ] + sigma_xi_sq
    }
  }
  
  
  tbl_m <- as.data.frame(m)
  # prevent v from becoming too small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- str_c("m_", 1:no)
  colnames(tbl_v) <- str_c("v_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, tbl_v, choices, rewards))
  
  return(list(tbl_return = tbl_return, tbl_rewards = tbl_rewards))
}


simulate_delta <- function(
    delta, lambda, nr_trials, params_decision,
    simulate_data, seed, tbl_rewards, mu_prior, is_decay = FALSE) {
  #'
  #' @description simulate choices from a Kalman filter with some choice model
  #' @param delta learning rate
  #' @param lambda decay parameter of random walk
  #' @param nr_trials number of choices to simulate
  #' @param params_decision parameters of decision model
  #' @param simulate_data should new data be generated for every participant
  #' @param seed seed value of iteration
  #' @param tbl_rewards if data are not simulated, take this tbl instead
  #' @param mu_prior expected reward without further knowledge
  #' @return a tbl with by-trial posterior means and variances for the chosen bandits
  #'
  if (params_decision$choicemodel %in% c("thompson", "ucb")) stop("only softmax implemented currently")
  set.seed(seed)
  if (simulate_data) {
    tbl_rewards <- generate_restless_bandits(
      sigma_xi_sq, sigma_epsilon_sq, c(-60, -20, 20, 60), lambda, nr_trials
    ) %>% select(-trial_id)
  }
  
  nt <- nrow(tbl_rewards) # number of time points
  no <- ncol(tbl_rewards) # number of options
  m <- matrix(mu_prior, ncol = no, nrow = nt + 1) # to hold the learned means
  choices <- rep(0, nt + 1) # to hold the choices of the RL agent
  rewards <- rep(0.0, nt + 1) # to hold the obtained rewards by the RL agent
  
  if (!is_decay) {
    f_update <- function() {
      # set the learning rate only for the chosen option
      lr[choices[t]] <- delta
      choices_oh[choices[t]] <- 1
      as_vector(m[t, ] + lr * (tbl_rewards[t, ] - m[t, ]))
    }
  } else if (is_decay) {
    f_update <- function() {
      # set the decay rate for all options
      lr[rep(TRUE, length(lr))] <- delta
      choices_oh[choices[t]] <- 1
      as_vector(m[t, ] * lr + (choices_oh * tbl_rewards[t, ]))
    }
  }
  
  for (t in 1:nt) {
    # variance components are just set to NA
    p <- choice_prob(matrix(m[t, ], 1, ncol(m)), matrix(NA, 1, ncol(m)), NA, params_decision)
    # choose an option according to these probabilities
    choices[t] <- sample(1:4, size = 1, prob = p)
    # get the reward of the choice
    rewards[t] <- tbl_rewards[t, choices[t]] %>% as_vector()
    choices_oh <- lr <- rep(0, no)
    
    # compute the posterior means
    m[t + 1, ] <- f_update()
  }
  
  tbl_m <- as.data.frame(m)
  colnames(tbl_m) <- str_c("m_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, choices, rewards))
  
  return(list(tbl_return = tbl_return, tbl_rewards = tbl_rewards))
}


choice_prob <- function(m, v, sigma_xi_sq, pars) {
  #'
  #' @description choose choice model
  #' @return matrix with choice probabilities
  #'
  switch(pars$choicemodel,
         softmax = softmax_choice_prob(m, pars$gamma),
         thompson = thompson_choice_prob_map(m, v, pars$no),
         ucb = ucb_choice_prob(m, v, sigma_xi_sq, pars$gamma, pars$beta),
         ru_thompson = ru_and_thompson_choice_prob(
           m, v, sigma_xi_sq, pars$gamma, pars$beta, pars$w_mix, ncol(m)
         ),
         ucb_thompson = ucb_and_thompson_choice_prob(
           m, v, sigma_xi_sq, pars$gamma, pars$beta, pars$w_mix, ncol(m)
         ),
         stop("Invalid choicemodel")
  )
}


softmax_choice_prob <- function(ms, gamma) {
  #'
  #' @description soft max choice rule
  #' @param ms posterior means of the bandits
  #' @param gamma inverse temperature parameter
  #' @return a tbl with by-trial choice probabilities
  # prevent exponent to become to large
  max_vals <- matrix(rep(708, ncol(ms) * nrow(ms)), nrow = nrow(ms))
  prob <- exp(pmin(max_vals, as.matrix(gamma * ms)))
  prob <- prob / rowSums(prob)
  return(prob)
}


thompson_choice_prob_map <- function(m, v, no) {
  #' Thompson sampling implemented with pmvnorm
  #'
  #' @description takes prior means and variances and computes p(highest)
  #' given all pairwise comparisons between response options
  #' @param m matrix with prior predictive means for each bandit in a column
  #' and every time point in a row
  #' @param v matrix with prior predictive variance for each bandit in a columns
  #' and every time point in a row
  #' @param no number of response options
  #' @return a tbl with by-trial choice probabilities
  
  # construct the transformation matrix for the difference scores for the first option
  A <- list()
  if (no == 4) {
    A1 <- matrix(c(1, -1, 0, 0, 1, 0, -1, 0, 1, 0, 0, -1), nrow = 3, byrow = TRUE)
    # construct an array to contain the transformation matrices for all options
    A[[1]] <- A1
    # transformation of each other option is just a shuffle of the one for option 1
    A[[2]] <- A1[, c(2, 1, 3, 4)]
    A[[3]] <- A1[, c(2, 3, 1, 4)]
    A[[4]] <- A1[, c(2, 3, 4, 1)]
  } else if (no == 2) {
    A[[1]] <- matrix(c(1, -1), nrow = 1)
    A[[2]] <- matrix(c(-1, 1), nrow = 1)
  }
  
  # initialize a matrix for the choice probabilities
  prob <- matrix(0.0, ncol = ncol(m), nrow = nrow(m))
  # loop through all trials
  for (t in 1:nrow(m)) {
    # iterate over all options
    prob[t, ] <- map_dbl(A, normprobs, mt = m[t, ], vt = v[t, ])
  }
  return(prob)
}


ucb_choice_prob <- function(ms, vs, sigma_xi_sq, gamma, beta) {
  #'
  #' @description soft max choice rule with exploration bonus
  #' @param ms posterior means of the bandits
  #' @param vs posterior variances of the bandits
  #' @param gamma inverse temperature parameter
  #' @param beta ucb parameter
  #' @return a tbl with by-trial choice probabilities
  prob <- exp(gamma * (ms + beta * sqrt(vs + sigma_xi_sq)))
  prob <- prob / rowSums(prob)
  return(prob)
}


ru_and_thompson_choice_prob <- function(ms, vs, sigma_xi_sq, gamma, beta, w_mix, nr_options) {
  #'
  #' @description mixture between relative uncertainty and Thompson sampling
  #' @param ms posterior means of the bandits
  #' @param vs posterior variances of the bandits
  #' @param gamma inverse temperature parameter
  #' @param beta ucb parameter
  #' @param w_mix mixture parameter
  #' @param nr_options number of arms/response options
  #' @return a tbl with by-trial choice probabilities
  tbl_zeromeans <- as_tibble(as.data.frame(matrix(
    reduce(map(1:nr_options, ~ rep(0, nrow(ms))), c),
    ncol = nr_options
  )))
  colnames(tbl_zeromeans) <- str_c("m_", 1:nr_options)
  p_choices_ucb <- ucb_choice_prob(tbl_zeromeans, vs, sigma_xi_sq, gamma, beta)
  p_choices_thompson <- thompson_choice_prob_map(ms, vs, nr_options)
  p_choices_mix <- w_mix * p_choices_thompson + (1 - w_mix) * p_choices_ucb
  return(p_choices_mix)
}


ucb_and_thompson_choice_prob <- function(ms, vs, sigma_xi_sq, gamma, beta, w_mix, nr_options) {
  #'
  #' @description mixture between ucb and Thompson sampling
  #' @param ms posterior means of the bandits
  #' @param vs posterior variances of the bandits
  #' @param gamma inverse temperature parameter
  #' @param beta ucb parameter
  #' @param w_mix mixture parameter
  #' @param nr_options number of arms/response options
  #' @return a tbl with by-trial choice probabilities
  tbl_means <- as_tibble(as.data.frame(ms))
  colnames(tbl_means) <- str_c("m_", 1:nr_options)
  p_choices_ucb <- ucb_choice_prob(tbl_means, vs, sigma_xi_sq, gamma, beta)
  p_choices_thompson <- thompson_choice_prob_map(ms, vs, nr_options)
  p_choices_mix <- w_mix * p_choices_thompson + (1 - w_mix) * p_choices_ucb
  return(p_choices_mix)
}


fit_kalman_softmax <- function(x, tbl_results, nr_options) {
  #'
  #' @description Kalman soft max fitting wrapper
  #' conditioned on ground truth responses
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- upper_and_lower_bounds_revert(x[[2]], 0, 30)
  gamma <- upper_and_lower_bounds_revert(x[[3]], 0, 3)
  tbl_learned <- kalman_learning(tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq)
  p_choices <- softmax_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")),
    gamma
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}

fit_kalman_softmax_choose <- function(x, tbl_results, tbl_rewards, nr_options) {
  #'
  #' @description Kalman soft max fitting wrapper
  #' choosing according to learned choice probabilities
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- upper_and_lower_bounds_revert(x[[2]], 0, 30)
  gamma <- upper_and_lower_bounds_revert(x[[3]], 0, 3)
  params_choice <- list(gamma = gamma, choicemodel = "softmax")
  l_learned <- kalman_learning_choose(tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, params_choice)
  p_choices <- as.data.frame(l_learned$c_probs)
  
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_softmax_no_variance <- function(
    x, tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
    sigma_prior = 1000, mu_prior = 0, bds, lambda = .9836, decay_center = 0
) {
  #'
  #' @description Kalman soft max fitting wrapper, do not optimize any of
  #' the kalman variances, fix all to the true values
  #'
  gamma <- upper_and_lower_bounds_revert(x[[1]], bds$gamma$lo, bds$gamma$hi)
  
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
    cat("\nuse pre-learned means and variances")
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
    cat("\napply kalman filter")
    
  }
  
  p_choices <- softmax_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")),
    gamma
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_softmax_no_variance_choose <- function(x, tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, bds) {
  #'
  #' @description Kalman soft max fitting wrapper, only optimize one of the
  #' two available variances, fix the other to the true value
  #'
  sigma_xi_sq <- 16
  sigma_epsilon_sq <- 16
  gamma <- upper_and_lower_bounds_revert(x[[1]], 0, 3)
  params_choice <- list(gamma = gamma, choicemodel = "softmax")
  l_learned <- kalman_learning_choose(tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, params_choice)
  p_choices <- as.data.frame(l_learned$c_probs)
  
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_softmax_xi_variance <- function(x, tbl_results, nr_options) {
  #'
  #' @description kalman softmax fitting wrapper, only optimize one of the
  #' two available variances, fix the other to the true value
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- 16
  gamma <- upper_and_lower_bounds_revert(x[[2]], 0, 3)
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
  }
  p_choices <- softmax_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")),
    gamma
  )
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_softmax_xi_variance_choose <- function(x, tbl_results, tbl_rewards, nr_options) {
  #'
  #' @description kalman softmax fitting wrapper, only optimize one of the
  #' two available variances, fix the other to the true value
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- 16
  gamma <- upper_and_lower_bounds_revert(x[[2]], 0, 3)
  params_choice <- list(gamma = gamma, choicemodel = "softmax")
  l_learned <- kalman_learning_choose(tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, params_choice)
  p_choices <- as.data.frame(l_learned$c_probs)
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_thompson <- function(x, tbl_results, nr_options) {
  #'
  #' @description kalman thompson fitting wrapper
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- upper_and_lower_bounds_revert(x[[2]], 0, 30)
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
  }
  p_choices <- thompson_choice_prob_map(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")) %>% as.matrix(),
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("v_")) %>% as.matrix(),
    nr_options
  ) %>%
    as.data.frame() %>%
    as_tibble()
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  lik <- pmax(lik, .0000001)
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_thompson_choose <- function(x, tbl_results, tbl_rewards, nr_options) {
  #'
  #' @description kalman thompson fitting wrapper
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- upper_and_lower_bounds_revert(x[[2]], 0, 30)
  params_choice <- list(choicemodel = "thompson")
  l_learned <- kalman_learning_choose(tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, params_choice)
  p_choices <- as.data.frame(l_learned$c_probs)
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  lik <- pmax(lik, .0000001)
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_thompson_xi_variance <- function(x, tbl_results, nr_options) {
  #'
  #' @description kalman thompson fitting wrapper
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- 16
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
  }
  p_choices <- thompson_choice_prob_map(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")) %>% as.matrix(),
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("v_")) %>% as.matrix(),
    nr_options
  ) %>%
    as.data.frame() %>%
    as_tibble()
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  lik <- pmax(lik, .0000001)
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_thompson_xi_variance_choose <- function(x, tbl_results, tbl_rewards, nr_options) {
  #'
  #' @description kalman thompson fitting wrapper
  #'
  sigma_xi_sq <- upper_and_lower_bounds_revert(x[[1]], 0, 30)
  sigma_epsilon_sq <- 16
  params_choice <- list(choicemodel = "thompson")
  l_learned <- kalman_learning_choose(tbl_results, tbl_rewards, nr_options, sigma_xi_sq, sigma_epsilon_sq, params_choice)
  p_choices <- as.data.frame(l_learned$c_probs)
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  lik <- pmax(lik, .0000001)
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}

fit_kalman_ucb_no_variance <- function(
    x, tbl_results, nr_options, sigma_xi_sq = 16, sigma_epsilon_sq = 16, 
    sigma_prior = 1000, mu_prior = 0, bds, lambda = .9836, decay_center = 0
) {
  #'
  #' @description Kalman soft max with ucb fitting wrapper,
  #' fix Kalman variances to the true value
  #'
  gamma <- upper_and_lower_bounds_revert(x[[1]], bds$gamma$lo, bds$gamma$hi)
  beta <- upper_and_lower_bounds_revert(x[[2]], bds$beta$lo, bds$beta$hi)
  
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
    cat("\nuse pre-learned means and variances")
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
    cat("\napply kalman filter")
    
  }
  
  p_choices <- ucb_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")),
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("v_")),
    sigma_xi_sq,
    gamma,
    beta
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_ru_thompson_no_variance <- function(
    x, tbl_results, nr_options, sigma_xi_sq = 16, sigma_epsilon_sq = 16, 
    sigma_prior = 1000, mu_prior = 0, bds, lambda = .9836, decay_center = 0
) {
  #'
  #' @description Kalman ru and thompson mixture fitting wrapper,
  #' fix Kalman variances to the true value
  #'
  
  gamma <- upper_and_lower_bounds_revert(x[[1]], bds$gamma$lo, bds$gamma$hi)
  beta <- upper_and_lower_bounds_revert(x[[2]], bds$beta$lo, bds$beta$hi)
  w_mix <- upper_and_lower_bounds_revert(x[[3]], bds$w_mix$lo, bds$w_mix$hi)
  
  
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
    cat("\nuse pre-learned means and variances")
    
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
    cat("\napply kalman filter")
    
  }
  
  p_choices_mix <- ru_and_thompson_choice_prob(
    as.matrix(tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_"))),
    as.matrix(tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("v_"))),
    sigma_xi_sq, gamma, beta, w_mix, nr_options
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices_mix, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_kalman_ucb_thompson_no_variance <- function(
    x, tbl_results, nr_options, sigma_xi_sq = 16, sigma_epsilon_sq = 16, 
    sigma_prior = 1000, mu_prior = 0, bds, lambda = .9836, decay_center = 0
) {
  
  #'
  #' @description Kalman ucb and thompson mixture fitting wrapper,
  #' fix Kalman variances to the true value
  #'
  gamma <- upper_and_lower_bounds_revert(x[[1]], bds$gamma$lo, bds$gamma$hi)
  beta <- upper_and_lower_bounds_revert(x[[2]], bds$beta$lo, bds$beta$hi)
  w_mix <- upper_and_lower_bounds_revert(x[[3]], bds$w_mix$lo, bds$w_mix$hi)
  
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4", "v_1", "v_2", "v_3", "v_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 8){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
    cat("\nuse pre-learned means and variances")
    
  } else {
    tbl_learned <- kalman_learning(
      tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq, 
      m0 = mu_prior, v0 = sigma_prior, lambda = lambda, decay_center = decay_center
    )
    cat("\napply kalman filter")
    
  }
  
  p_choices_mix <- ucb_and_thompson_choice_prob(
    as.matrix(tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_"))),
    as.matrix(tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("v_"))),
    sigma_xi_sq, gamma, beta, w_mix, nr_options
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices_mix, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_delta_softmax <- function(x, tbl_results, nr_options, mu_prior = 0, s_decay = FALSE) {
  #'
  #' @description delta rule soft max fitting wrapper
  #' conditioned on ground truth responses
  #' can be used for delta rule or decay rule
  #' @param is_decay flags what learning rule is used (i.e., delta or decay)
  #'
  delta <- upper_and_lower_bounds_revert(x[[1]], 0, 1)
  gamma <- upper_and_lower_bounds_revert(x[[2]], 0, 3)
  
  # if the means and variances already have been learned, take those
  colnames_learned <- c("m_1", "m_2", "m_3", "m_4")
  if (sum(colnames_learned %in% colnames(tbl_results)) == 4){
    tbl_learned <- tibble(
      cbind(tbl_results[, colnames_learned], choices = tbl_results$choices, rewards = tbl_results$rewards)
    )
    cat("\nuse pre-learned means and variances")
    
  } else {
    tbl_learned <- delta_learning(
      tbl_results, nr_options, delta, m0 = mu_prior, is_decay
    )
    cat("\napply kalman filter")
    
  }
  p_choices <- softmax_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")),
    gamma
  )
  lik <- pmap_dbl(as.data.frame(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-2 * sllik)
}


fit_thompson_wrapper <- function(tbl_results, tbl_rewards, condition_on_observed_choices) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(upper_and_lower_bounds(15, 0, 30), upper_and_lower_bounds(15, 0, 30))
  if (condition_on_observed_choices) {
    result_optim <- optim(params_init, fit_kalman_thompson, tbl_results = tbl_results, nr_options = 4)
    r <- c(
      upper_and_lower_bounds_revert(result_optim$par[1:2], 0, 30),
      result_optim$value
    )
  } else if (!condition_on_observed_choices) {
    result_optim <- DEoptim(
      fit_kalman_thompson_choose,
      lower = c(-19.51929, -17.21671),
      upper = c(19.51929, 17.21671),
      control = DEoptim.control(trace = 10),
      tbl_results = tbl_results, tbl_rewards = tbl_rewards, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[1:2], 0, 30),
      result_optim$optim$bestval
    )
  }
  return(r)
}


fit_thompson_one_variance_wrapper <- function(tbl_results, tbl_rewards, condition_on_observed_choices) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(upper_and_lower_bounds(15, 0, 30))
  if (condition_on_observed_choices) {
    result_optim <- optimize(
      fit_kalman_thompson_xi_variance, c(-10.308, 12.611),
      tbl_results = tbl_results, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$minimum, 0, 30),
      result_optim$objective
    )
  } else if (!condition_on_observed_choices) {
    result_optim <- DEoptim(
      fit_kalman_thompson_xi_variance_choose,
      lower = c(-19.51929),
      upper = c(19.51929),
      control = DEoptim.control(trace = 10),
      tbl_results = tbl_results, tbl_rewards = tbl_rewards, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[1], 0, 30),
      result_optim$optim$bestval
    )
  }
  return(r)
}


fit_softmax_wrapper <- function(tbl_results, tbl_rewards, condition_on_observed_choices) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(
    upper_and_lower_bounds(15, 0, 30),
    upper_and_lower_bounds(15, 0, 30),
    upper_and_lower_bounds(.2, 0, 3)
  )
  if (condition_on_observed_choices) {
    result_optim <- optim(
      params_init, fit_kalman_softmax,
      tbl_results = tbl_results, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$par[1:2], 0, 30),
      upper_and_lower_bounds_revert(result_optim$par[3], 0, 3),
      result_optim$value
    )
  } else if (!condition_on_observed_choices) {
    result_optim <- DEoptim(
      fit_kalman_softmax_choose,
      lower = c(-19.51929, -19.51929, -17.21671),
      upper = c(19.51929, 19.51929, 17.21671),
      control = DEoptim.control(trace = 10),
      tbl_results = tbl_results, tbl_rewards = tbl_rewards, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[1:2], 0, 30),
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[3], 0, 3),
      result_optim$optim$bestval
    )
  }
  
  return(r)
}


fit_softmax_one_variance_wrapper <- function(tbl_results, tbl_rewards, condition_on_observed_choices) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(
    upper_and_lower_bounds(15, 0, 30),
    upper_and_lower_bounds(.2, 0, 3)
  )
  if (condition_on_observed_choices) {
    result_optim <- optim(
      params_init, fit_kalman_softmax_xi_variance,
      tbl_results = tbl_results, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$par[1], 0, 30),
      upper_and_lower_bounds_revert(result_optim$par[2], 0, 3),
      result_optim$value
    )
  } else if (!condition_on_observed_choices) {
    result_optim <- DEoptim(
      fit_kalman_softmax_xi_variance_choose,
      lower = c(-19.51929, -17.21671),
      upper = c(19.51929, 17.21671),
      tbl_results = tbl_results, tbl_rewards = tbl_rewards, nr_options = 4
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[1], 0, 30),
      upper_and_lower_bounds_revert(result_optim$optim$bestmem[2], 0, 3),
      result_optim$optim$bestval
    )
  }
  
  return(r)
}


fit_softmax_no_variance_wrapper <- function(
    tbl_results, tbl_rewards, condition_on_observed_choices, 
    sigma_xi_sq = 16, sigma_epsilon_sq = 16, sigma_prior = 1000,
    mu_prior = 0, bds, params_init = NULL, lambda = .9836, decay_center = 0
) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  
  if (is.null(params_init)) {
    params_init <- c(.25)
  }
  
  params_init_tf <- pmap_dbl(
    list(params_init, map_dbl(bds, "lo"), map_dbl(bds, "hi")),
    upper_and_lower_bounds
  )
  param_edges <- pmap_dbl(
    list(c(.0001, .4999), map_dbl(bds, "lo"), map_dbl(bds, "hi")),
    upper_and_lower_bounds
  )
  
  if (condition_on_observed_choices) {
    result_optim <- optimize(
      fit_kalman_softmax_no_variance, param_edges,
      tbl_results = tbl_results, nr_options = 4,
      sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq,
      sigma_prior = sigma_prior, mu_prior = mu_prior,
      bds = bds, lambda = lambda, decay_center = decay_center
    )
    r <- c(
      pmap_dbl(
        list(result_optim$minimum, map_dbl(bds, "lo"), map_dbl(bds, "hi")),
        upper_and_lower_bounds_revert
      ),
      result_optim$objective
    )
  } else if (!condition_on_observed_choices) {
    result_optim <- DEoptim(
      fit_kalman_softmax_no_variance_choose,
      lower = param_edges[1],
      upper = param_edges[2],
      tbl_results = tbl_results, tbl_rewards = tbl_rewards, nr_options = 4
    )
    
    r <- c(
      upper_and_lower_bounds_revert(result_optim$optim$bestmem, 0, 3),
      result_optim$optim$bestval
    )
  }
  
  
  return(r)
}


fit_ucb_no_variance_wrapper <- function(
    tbl_results, tbl_rewards, condition_on_observed_choices, 
    sigma_xi_sq = 16, sigma_epsilon_sq = 16, sigma_prior = 1000, mu_prior = 0, bds, params_init = NULL, lambda = .9836, decay_center = 0
) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  
  if (is.null(params_init)) {
    params_init <- c(.25, .25)
  }
  params_init_tf <- pmap_dbl(
    list(params_init, map_dbl(bds, "lo"), map_dbl(bds, "hi")),
    upper_and_lower_bounds
  )
  
  if (condition_on_observed_choices) {
    result_optim <- optim(
      params_init_tf, fit_kalman_ucb_no_variance,
      tbl_results = tbl_results, nr_options = 4,
      sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq,
      sigma_prior = sigma_prior, mu_prior = mu_prior,
      bds = bds, lambda = lambda, decay_center = decay_center
    )
    
    r <- c(
      pmap_dbl(list(
        result_optim$par, 
        map_dbl(bds, "lo"), map_dbl(bds, "hi")
      ), upper_and_lower_bounds_revert
      ), result_optim$value
    )
    
  } else if (!condition_on_observed_choices) {
    stop("code not yet developed")
  }
  
  return(r)
}


# fit_ucb_no_variance_wrapper <- function(
    #     tbl_results, tbl_rewards, condition_on_observed_choices,
#     sigma_xi_sq = 16, sigma_epsilon_sq = 16,
#     los = 0, his = 3
# ) {
#   tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
#   params_init <- pmap_dbl(
#     list(c(.5, .2), los, his), 
#     ~ upper_and_lower_bounds(..1, ..2, ..3)
#   )
#   if (condition_on_observed_choices) {
#     result_optim <- optim(
#       params_init, fit_kalman_ucb_no_variance,
#       tbl_results = tbl_results, nr_options = 4,
#       sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq
#     )
#     
#     r <- c(
#       pmap_dbl(
#         list(result_optim$par, los, his), 
#         ~ upper_and_lower_bounds_revert(..1, ..2, ..3)
#       ),
#       result_optim$value
#     )
#   } else if (!condition_on_observed_choices) {
#     stop("code not yet developed")
#   }
#   
#   return(r)
# }


fit_mixture_no_variance_wrapper <- function(
    tbl_results, tbl_rewards, condition_on_observed_choices, f_fit,
    sigma_xi_sq = 16, sigma_epsilon_sq = 16, sigma_prior = 1000, mu_prior = 0,
    bds, params_init = NULL, lambda = .9836, decay_center = 0
) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  
  if (is.null(params_init)) {
    params_init <- c(.25, 0.001, .5)
  }
  
  params_init_tf <- pmap_dbl(
    list(params_init, map_dbl(bds, "lo"), map_dbl(bds, "hi")),
    upper_and_lower_bounds
  )
  
  
  if (condition_on_observed_choices) {
    
    result_optim <- optim(
      params_init_tf, f_fit,
      tbl_results = tbl_results, nr_options = 4,
      sigma_xi_sq = sigma_xi_sq, sigma_epsilon_sq = sigma_epsilon_sq,
      sigma_prior = sigma_prior, mu_prior = mu_prior,
      bds = bds, lambda = lambda, decay_center = decay_center
    )
    
    r <- c(
      pmap_dbl(list(
        result_optim$par, 
        map_dbl(bds, "lo"), map_dbl(bds, "hi")
      ), upper_and_lower_bounds_revert
      ), result_optim$value
    )
    
  } else if (!condition_on_observed_choices) {
    stop("code not yet developed")
  }
  
  return(r)
}




fit_delta_softmax_wrapper <- function(tbl_results, tbl_rewards, is_decay, condition_on_observed_choices, mu_prior = 0) {
  #'
  #' @description wrapper around delta rule soft max fitting wrapper
  #' conditioned on ground truth responses
  #'
  
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(
    upper_and_lower_bounds(.5, 0, 1),
    upper_and_lower_bounds(.2, 0, 3)
  )
  if (condition_on_observed_choices) {
    result_optim <- optim(
      params_init, fit_delta_softmax,
      tbl_results = tbl_results, nr_options = 4,
      mu_prior = mu_prior,
      is_decay = is_decay
    )
    r <- c(
      upper_and_lower_bounds_revert(result_optim$par[1], 0, 1),
      upper_and_lower_bounds_revert(result_optim$par[2], 0, 3),
      result_optim$value
    )
  } else if (!condition_on_observed_choices) {
    stop("not developed without conditioning on given responses")
  }
  
  return(r)
}



upper_and_lower_bounds <- function(par, lo, hi) {
  log(((par - lo) / (hi - lo)) / (1 - (par - lo) / (hi - lo)))
}


upper_and_lower_bounds_revert <- function(par, lo, hi) {
  lo + ((hi - lo) / (1 + exp(-par)))
}


kalman_softmax_experiment <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  # create a tbl with by-participant simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  tbl_params_participants <- create_participant_sample_softmax(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars
  )
  
  tbl_results_kalman_softmax <- simulate_and_fit_softmax(tbl_params_participants, nr_vars, cond_on_choices, nr_trials)
  
  progress_msg <- str_c(
    "finished iteration: gamma mn = ", gamma_mn, ", gamma sd = ", gamma_sd, ",
    simulate data = ", simulate_data, ", nr participants = ", nr_participants,
    " nr trials = ", nr_trials, "\n"
  )
  cat(progress_msg)
  
  return(tbl_results_kalman_softmax)
}

simulate_and_fit_softmax <- function(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials, bds, tbl_rewards = NULL, decay_center = NULL
) {
  
  if (is.null(tbl_rewards)) {
    # simulate data
    tbl_rewards <- generate_restless_bandits(
      sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
    ) %>%
      select(-trial_id)
  }
  list2env(
    tbl_params_participants[1, c("sigma_prior", "mu_prior", "sigma_xi_sq", "sigma_epsilon_sq")],
    environment()
  )
  
  plan(multisession, workers = availableCores() - 2)
  l_choices_simulated <- future_pmap(
    tbl_params_participants,
    simulate_kalman,
    tbl_rewards = tbl_rewards,
    decay_center = decay_center,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  
  # fit data
  if (nr_vars == 0) {
    my_current_wrapper <- fit_softmax_no_variance_wrapper
  } else if (nr_vars == 1) {
    my_current_wrapper <- fit_softmax_one_variance_wrapper
  } else if (nr_vars == 2) {
    my_current_wrapper <- fit_softmax_wrapper
  }
  
  plan(multisession, workers = availableCores() - 2)
  l_softmax <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(my_current_wrapper),
    condition_on_observed_choices = cond_on_choices,
    sigma_xi_sq = sigma_xi_sq,
    sigma_epsilon_sq = sigma_epsilon_sq,
    sigma_prior = sigma_prior,
    mu_prior = mu_prior,
    bds = bds, decay_center = decay_center,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  # replace empty results with NAs
  l_results <- map(l_softmax, "result")
  idx <- 1
  for (p in l_results) {
    if (is.null(p)) {
      l_results[[idx]] <- rep(NA, (nr_vars + 2))
    }
    idx <- idx + 1
  }
  
  tbl_results_softmax <- as.data.frame(reduce(l_results, rbind)) %>% as_tibble()
  
  if (nr_vars == 0) {
    colnames(tbl_results_softmax) <- c("gamma_ml", "neg_ll")
  } else if (nr_vars == 1) {
    colnames(tbl_results_softmax) <- c("sigma_xi_sq_ml", "gamma_ml", "neg_ll")
  } else if (nr_vars == 2) {
    colnames(tbl_results_softmax) <- c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml", "neg_ll")
  }
  
  tbl_results_softmax <- as_tibble(cbind(tbl_params_participants, tbl_results_softmax)) %>%
    mutate(participant_id = 1:nrow(tbl_results_softmax))
  
  return(tbl_results_softmax)
}



kalman_thompson_experiment <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  # create a tbl with by-participant simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  tbl_params_participants <- create_participant_sample_thompson(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars
  )
  
  tbl_results_kalman_thompson <- simulate_and_fit_thompson(tbl_params_participants, nr_vars, cond_on_choices, nr_trials)
  
  progress_msg <- str_c(
    "finished iteration: gamma mn = ", gamma_mn, ", gamma sd = ", gamma_sd, ",
    simulate data = ", simulate_data, ", nr participants = ", nr_participants,
    " nr trials = ", nr_trials, "\n"
  )
  cat(progress_msg)
  
  return(tbl_results_kalman_thompson)
}



simulate_and_fit_thompson <- function(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials) {
  # create a tbl with simulation & model parameters
  
  
  # simulate data
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  plan(multisession, workers = availableCores() / 2)
  l_choices_simulated <- future_pmap(
    tbl_params_participants,
    simulate_kalman,
    tbl_rewards = tbl_rewards,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  # fit data
  if (nr_vars == 1) {
    my_current_wrapper <- fit_thompson_one_variance_wrapper
  } else if (nr_vars == 2) {
    my_current_wrapper <- fit_thompson_wrapper
  }
  
  plan(multisession, workers = availableCores() / 2)
  l_thompson <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(my_current_wrapper),
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  # replace empty results with NAs
  l_results <- map(l_thompson, "result")
  idx <- 1
  for (p in l_results) {
    if (is.null(p)) {
      l_results[[idx]] <- c(NA, NA, NA)
    }
    idx <- idx + 1
  }
  
  tbl_results_thompson <- as.data.frame(reduce(l_results, rbind)) %>% as_tibble()
  
  if (nr_vars == 1) {
    colnames(tbl_results_thompson) <- c("sigma_xi_sq_ml", "neg_ll")
  } else if (nr_vars == 2) {
    colnames(tbl_results_thompson) <- c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "neg_ll")
  }
  
  tbl_results_thompson <- as_tibble(cbind(tbl_params_participants, tbl_results_thompson)) %>%
    mutate(participant_id = 1:nrow(tbl_results_thompson))
  
  return(tbl_results_thompson)
}


kalman_ucb_experiment <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  # create a tbl with by-participant simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  tbl_params_participants <- create_participant_sample_ucb(
    gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data,
    nr_participants, nr_trials, lambda, nr_vars
  )
  
  tbl_results_kalman_ucb <- simulate_and_fit_ucb(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials
  )
  
  progress_msg <- str_c(
    "finished iteration: gamma mn = ", gamma_mn, ", gamma sd = ", gamma_sd,
    " beta_mn = ", beta_mn, ", beta sd = ", beta_sd,
    ", simulate data = ", simulate_data, ", nr participants = ", nr_participants,
    " nr trials = ", nr_trials, "\n"
  )
  cat(progress_msg)
  
  return(tbl_results_kalman_ucb)
}

simulate_and_fit_ucb <- function(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials, bds, tbl_rewards = NULL
) {
  list2env(
    tbl_params_participants[1, c("sigma_prior", "mu_prior", "sigma_xi_sq", "sigma_epsilon_sq")],
    environment()
  )
  if (is.null(tbl_rewards)) {
    # simulate fixed data set
    tbl_rewards <- generate_restless_bandits(
      sigma_xi_sq, sigma_epsilon_sq, mu_prior, lambda, nr_trials
    ) %>%
      select(-trial_id)
  }
  
  # simulate
  plan(multisession, workers = availableCores() - 2)
  l_choices_simulated <- future_pmap(
    tbl_params_participants,
    simulate_kalman,
    tbl_rewards = tbl_rewards,
    decay_center = mu_prior,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  # fit
  plan(multisession, workers = availableCores() - 2)
  l_ucb <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_ucb_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    sigma_xi_sq, sigma_epsilon_sq, sigma_prior, mu_prior,
    bds,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  # replace empty results with NAs
  l_results <- map(l_ucb, "result")
  idx <- 1
  for (p in l_results) {
    if (is.null(p)) {
      l_results[[idx]] <- c(NA, NA, NA)
    }
    idx <- idx + 1
  }
  
  tbl_results_ucb <- as.data.frame(reduce(l_results, rbind)) %>% as_tibble()
  
  if (nr_vars == 0) {
    colnames(tbl_results_ucb) <- c("gamma_ml", "beta_ml", "neg_ll")
  } else if (nr_vars == 1) {
    colnames(tbl_results_ucb) <- c("sigma_xi_sq_ml", "gamma_ml", "beta_ml", "neg_ll")
  } else if (nr_vars == 2) {
    colnames(tbl_results_ucb) <- c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml", "beta_ml", "neg_ll")
  }
  
  
  tbl_results_ucb <- as_tibble(cbind(tbl_params_participants, tbl_results_ucb)) %>%
    mutate(participant_id = 1:nrow(tbl_results_ucb))
  
  
  return(tbl_results_ucb)
}


simulate_and_fit_mixture <- function(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials, bds, tbl_rewards = NULL
) {
  
  list2env(
    tbl_params_participants[1, c("sigma_prior", "mu_prior", "sigma_xi_sq", "sigma_epsilon_sq")],
    environment()
  )
  
  if (is.null(tbl_rewards)) {
    # simulate fixed data set
    tbl_rewards <- generate_restless_bandits(
      sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
    ) %>%
      select(-trial_id)
  }
  
  # simulate
  plan(multisession, workers = availableCores() - 2)
  l_choices_simulated <- future_pmap(
    tbl_params_participants,
    simulate_kalman,
    tbl_rewards = tbl_rewards,
    decay_center = mu_prior,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  # fit
  mixturetype <- tbl_params_participants$params_decision[[1]][["choicemodel"]]
  if (mixturetype == "ucb_thompson") f_fit <- fit_kalman_ucb_thompson_no_variance
  if (mixturetype == "ru_thompson") f_fit <- fit_kalman_ru_thompson_no_variance
  plan(multisession, workers = availableCores() - 2)
  l_mixture <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_mixture_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    f_fit = f_fit,
    sigma_xi_sq, sigma_epsilon_sq, sigma_prior, mu_prior,
    bds,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  plan("sequential")
  
  # replace empty results with NAs
  l_results <- map(l_mixture, "result")
  idx <- 1
  for (p in l_results) {
    if (is.null(p)) {
      l_results[[idx]] <- c(NA, NA, NA)
    }
    idx <- idx + 1
  }
  
  tbl_results_mixture <- as.data.frame(reduce(l_results, rbind)) %>% as_tibble()
  
  if (nr_vars == 0) {
    colnames(tbl_results_mixture) <- c("gamma_ml", "beta_ml", "w_mix_ml", "neg_ll")
  } else if (nr_vars == 1) {
    colnames(tbl_results_mixture) <- c("sigma_xi_sq_ml", "gamma_ml", "beta_ml", "w_mix_ml", "neg_ll")
  } else if (nr_vars == 2) {
    colnames(tbl_results_mixture) <- c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml", "beta_ml", "w_mix_ml", "neg_ll")
  }
  
  
  tbl_results_mixture <- as_tibble(cbind(tbl_params_participants, tbl_results_mixture)) %>%
    mutate(participant_id = 1:nrow(tbl_results_mixture))
  
  
  return(tbl_results_mixture)
}


kalman_mixture_experiment <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, w_mix_mn, w_mix_sd,
    simulate_data, nr_participants, nr_trials, cond_on_choices, lambda, nr_vars, mixturetype) {
  # create a tbl with by-participant simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  tbl_params_participants <- create_participant_sample_mixture(
    gamma_mn, gamma_sd, beta_mn, beta_sd, w_mix_mn, w_mix_sd, simulate_data,
    nr_participants, nr_trials, lambda, nr_vars, mixturetype
  )
  
  tbl_results_kalman_mixture <- simulate_and_fit_mixture(
    tbl_params_participants, nr_vars, cond_on_choices, nr_trials
  )
  
  progress_msg <- str_c(
    "finished iteration: gamma mn = ", gamma_mn, ", gamma sd = ", gamma_sd,
    " w_mix_mn = ", w_mix_mn, ", w_mix sd = ", w_mix_sd,
    ", simulate data = ", simulate_data, ", nr participants = ", nr_participants,
    " nr trials = ", nr_trials, "\n"
  )
  cat(progress_msg)
  
  return(tbl_results_kalman_mixture)
}


delta_experiment <- function(
    gamma_mn, gamma_sd, delta_mn, delta_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, is_decay, lambda) {
  # create a tbl with by-participant simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  tbl_params_participants <- create_participant_sample_delta(
    gamma_mn, gamma_sd, delta_mn, delta_sd, simulate_data, nr_participants,
    nr_trials, is_decay, lambda
  )
  
  tbl_results_delta <- simulate_and_fit_delta(tbl_params_participants, is_decay, cond_on_choices, nr_trials)
  
  progress_msg <- str_c(
    "\nfinished iteration: gamma mn = ", gamma_mn, ", gamma sd = ", gamma_sd, ",
    delta_mn = ", delta_mn, ", delta_sd = ", delta_sd, "
    simulate data = ", simulate_data, ", nr participants = ", nr_participants,
    " nr trials = ", nr_trials, " is decay = ", is_decay, "\n"
  )
  cat(progress_msg)
  
  return(tbl_results_delta)
}


simulate_and_fit_delta <- function(
    tbl_params_participants, is_decay, cond_on_choices, nr_trials) {
  
  list2env(
    tbl_params_participants[1, c("mu_prior")],
    environment()
  )
  
  # simulate fixed data set
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  plan(multisession, workers = availableCores() / 2)
  l_choices_simulated <- future_pmap(
    tbl_params_participants,
    simulate_delta,
    tbl_rewards = tbl_rewards,
    mu_prior = mu_prior,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  plan(multisession, workers = availableCores() / 2)
  l_softmax <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_delta_softmax_wrapper),
    is_decay = is_decay,
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  # replace empty results with NAs
  l_results <- map(l_softmax, "result")
  idx <- 1
  for (p in l_results) {
    if (is.null(p)) {
      l_results[[idx]] <- rep(NA, (nr_vars + 2))
    }
    idx <- idx + 1
  }
  
  tbl_results_delta <- as.data.frame(reduce(l_results, rbind)) %>% as_tibble()
  colnames(tbl_results_delta) <- c("delta_ml", "gamma_ml", "neg_ll")
  
  tbl_results_delta <- as_tibble(cbind(tbl_params_participants, tbl_results_delta)) %>%
    mutate(participant_id = 1:nrow(tbl_results_delta))
  
  
  return(tbl_results_delta)
}

generate_restless_bandits <- function(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials, center_decay = 0) {
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
    mus[t, ] <- lambda * mus[t-1, ] + (1 - lambda) * center_decay + rnorm(nr_bandits, 0, sqrt(sigma_xi_sq))
  }
  noise <- matrix(
    rnorm(nr_trials * nr_bandits, 0, sqrt(sigma_epsilon_sq)),
    nrow = nr_trials, ncol = nr_bandits
  )
  as_tibble(as.data.frame(mus + noise)) %>% 
    mutate(trial_id = 1:nr_trials) %>%
    rename("Arm 1" = V1, "Arm 2" = V2, "Arm 3" = V3, "Arm 4" = V4)
}

#' generate_restless_bandits <- function(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials) {
#'   #'
#'   #' @description generate random walk data on bandits with independent means
#'   #' @param sigma_xi_sq innovation variance
#'   #' @param sigma_epsilon_sq noise variance
#'   #' @param mu1 initial means of different bandits
#'   #' @param lambda decay parameter to keep bandit means closer to 0
#'   #' @param nr_trials number of trials to generate
#'   #' @return a tbl with values for all trials and all bandits
#'   nr_bandits <- length(mu1)
#'   mus <- matrix(nrow = nr_trials, ncol = nr_bandits)
#'   mus[1, ] <- mu1
#'   for (t in 2:nr_trials) {
#'     mus[t, ] <- lambda * mus[t - 1, ] + rnorm(nr_bandits, 0, sqrt(sigma_xi_sq))
#'   }
#'   noise <- matrix(
#'     rnorm(nr_trials * nr_bandits, 0, sqrt(sigma_epsilon_sq)),
#'     nrow = nr_trials, ncol = nr_bandits
#'   )
#'   as_tibble(as.data.frame(mus + noise)) %>%
#'     mutate(trial_id = 1:nr_trials) %>%
#'     rename("Arm 1" = V1, "Arm 2" = V2, "Arm 3" = V3, "Arm 4" = V4)
#' }



create_participant_sample_softmax <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars) {
  #'
  #' @description create pool of participants deciding according to soft max rule
  #' with individual parameters fixed or sampled from normal distribution
  #' @param gamma_mn average inverse temperature
  #' @param gamma_sd population standard deviation of inverse temperature
  #' @param simulate_data should by-trial rewards be generated once or by participant?
  #' @param nr_participants number of participants
  #' @param nr_trials number of choices in the restless bandit task
  #' @param lambda decay parameter of random walk
  #' @param nr_vars how many variances of the kalman filter are varied and fit
  #' @return a tbl with by-participant parameters
  
  sigma_xi_sq <- rep(16, nr_participants)
  sigma_epsilon_sq <- rep(16, nr_participants)
  if (nr_vars == 1) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
  } else if (nr_vars == 2) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
    sigma_epsilon_sq <- rnorm(nr_participants, 16, 3)
  }
  
  s_gamma <- -1
  while (s_gamma < 0) {
    gamma <- rnorm(nr_participants, gamma_mn, gamma_sd)
    s_gamma <- min(gamma)
  }
  s_seeds <- -1
  while (s_seeds < nr_participants) {
    seed <- round(rnorm(nr_participants, 100000, 10000), 0)
    s_seeds <- length(unique(seed))
  }
  tbl_params_softmax <- tibble(
    sigma_prior = rep(1000, nr_participants),
    mu_prior = rep(0, nr_participants),
    sigma_xi_sq,
    sigma_epsilon_sq,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = map(
      gamma, ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
    ),
    simulate_data = simulate_data,
    seed
  )
  
  return(tbl_params_softmax)
}


create_participant_sample_thompson <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars) {
  #'
  #' @description create pool of participants deciding according to thompson sampling (MAP)
  #' with individual parameters fixed or sampled from normal distribution
  #' @param gamma_mn average inverse temperature
  #' @param gamma_sd population standard deviation of inverse temperature
  #' @param simulate_data should by-trial rewards be generated once or by participant?
  #' @param nr_participants number of participants
  #' @param nr_trials number of choices in the restless bandit task
  #' @param lambda decay parameter of random walk
  #' @param nr_vars how many variances of the kalman filter are varied and fit
  #' @return a tbl with by-participant parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  sigma_xi_sq <- rep(16, nr_participants)
  sigma_epsilon_sq <- rep(16, nr_participants)
  if (nr_vars == 1) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
  } else if (nr_vars == 2) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
    sigma_epsilon_sq <- rnorm(nr_participants, 16, 3)
  }
  
  
  s_seeds <- -1
  while (s_seeds < nr_participants) {
    seed <- round(rnorm(nr_participants, 100000, 10000), 0)
    s_seeds <- length(unique(seed))
  }
  tbl_params_thompson <- tibble(
    sigma_prior = rep(1000, nr_participants),
    mu_prior = rep(0, nr_participants),
    sigma_xi_sq,
    sigma_epsilon_sq,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = map(
      seed, ~ list(choicemodel = "thompson", no = 4)
    ),
    simulate_data = simulate_data,
    seed
  )
  
  return(tbl_params_thompson)
}


create_participant_sample_ucb <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars) {
  # create a tbl with simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  sigma_xi_sq <- rep(16, nr_participants)
  sigma_epsilon_sq <- rep(16, nr_participants)
  if (nr_vars == 1) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
  } else if (nr_vars == 2) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
    sigma_epsilon_sq <- rnorm(nr_participants, 16, 3)
  }
  
  s_gamma <- -1
  while (s_gamma < 0) {
    gamma <- rnorm(nr_participants, gamma_mn, gamma_sd)
    s_gamma <- min(gamma)
  }
  s_beta <- -1
  while (s_beta < 0) {
    beta <- rnorm(nr_participants, beta_mn, beta_sd)
    s_beta <- min(beta)
  }
  s_seeds <- -1
  while (s_seeds < nr_participants) {
    seed <- round(rnorm(nr_participants, 100000, 10000), 0)
    s_seeds <- length(unique(seed))
  }
  
  tbl_params_ucb <- tibble(
    sigma_prior = rep(1000, nr_participants),
    mu_prior = rep(0, nr_participants),
    sigma_xi_sq,
    sigma_epsilon_sq,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = map2(
      gamma, beta,
      ~ list(gamma = ..1, beta = ..2, choicemodel = "ucb", no = 4)
    ),
    simulate_data = simulate_data,
    seed = seed
  )
  
  return(tbl_params_ucb)
}


create_participant_sample_mixture <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, w_mix_mn, w_mix_sd,
    simulate_data, nr_participants, nr_trials, lambda, nr_vars,
    mixturetype) {
  # create a tbl with simulation & model parameters
  # if nr_vars == 0, same values on sig_xi and sig_eps for all participants
  sigma_xi_sq <- rep(16, nr_participants)
  sigma_epsilon_sq <- rep(16, nr_participants)
  if (nr_vars == 1) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
  } else if (nr_vars == 2) {
    sigma_xi_sq <- rnorm(nr_participants, 16, 3)
    sigma_epsilon_sq <- rnorm(nr_participants, 16, 3)
  }
  
  max_w_mix <- 1.1
  min_w_mix <- -1
  while (max_w_mix > 1 | min_w_mix < 0) {
    w_mix <- rnorm(nr_participants, w_mix_mn, w_mix_sd)
    max_w_mix <- max(w_mix)
    min_w_mix <- min(w_mix)
  }
  s_beta <- -1
  while (s_beta < 0) {
    beta <- rnorm(nr_participants, beta_mn, beta_sd)
    s_beta <- min(beta)
  }
  s_gamma <- -1
  while (s_gamma < 0) {
    gamma <- rnorm(nr_participants, gamma_mn, gamma_sd)
    s_gamma <- min(gamma)
  }
  
  s_seeds <- -1
  while (s_seeds < nr_participants) {
    seed <- round(rnorm(nr_participants, 100000, 10000), 0)
    s_seeds <- length(unique(seed))
  }
  
  tbl_params_mixture <- tibble(
    sigma_prior = rep(1000, nr_participants),
    mu_prior = rep(0, nr_participants),
    sigma_xi_sq,
    sigma_epsilon_sq,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = pmap(
      list(gamma, beta, w_mix),
      ~ list(gamma = ..1, beta = ..2, w_mix = ..3, choicemodel = mixturetype, no = 4)
    ),
    simulate_data = simulate_data,
    seed = seed
  )
  
  return(tbl_params_mixture)
}


create_participant_sample_delta <- function(
    gamma_mn, gamma_sd, delta_mn, delta_sd, simulate_data, nr_participants,
    nr_trials, is_decay, lambda) {
  #'
  #' @description create pool of participants learning with delta rule and
  #' deciding according to soft max rule
  #' with individual parameters fixed or sampled from normal distribution
  #' @param gamma_mn average inverse temperature
  #' @param gamma_sd population standard deviation of inverse temperature
  #' @param delta_mn average learning rate
  #' @param delta_sd sd of learning rate
  #' @param simulate_data should by-trial rewards be generated once or by participant?
  #' @param nr_participants number of participants
  #' @param nr_trials number of choices in the restless bandit task
  #' @param is_decay delta rule or decay rule learning (decay == TRUE)
  #' @param lambda decay parameter of random walk
  #' @return a tbl with by-participant parameters
  
  min_delta <- -1
  max_delta <- 1.1
  while (min_delta < 0 | max_delta > 1) {
    delta <- rnorm(nr_participants, delta_mn, delta_sd)
    min_delta <- min(delta)
    max_delta <- max(delta)
  }
  s_gamma <- -1
  while (s_gamma < 0) {
    gamma <- rnorm(nr_participants, gamma_mn, gamma_sd)
    s_gamma <- min(gamma)
  }
  s_seeds <- -1
  while (s_seeds < nr_participants) {
    seed <- round(rnorm(nr_participants, 100000, 10000), 0)
    s_seeds <- length(unique(seed))
  }
  
  tbl_params_delta <- tibble(
    delta = delta,
    is_decay = is_decay,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = map(
      gamma,
      ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
    ),
    simulate_data = simulate_data,
    seed = seed
  )
  
  return(tbl_params_delta)
}


recover_softmax <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  #'
  #' @description generate choices according to soft max
  #' and fit them with soft max, thompson, and ucb
  
  tbl_params_participants <- create_participant_sample_softmax(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, nr_vars
  )
  
  # simulate one fixed data set in case needed
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq[1], sigma_epsilon_sq[1], mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  l_models_fit <- simulate_and_fit_models(
    tbl_params_participants, tbl_rewards, cond_on_choices,
    family = "kalman"
  )
  
  l_goodness <- read_out_lls_and_ics(l_models_fit, nr_participants)
  
  return(l_goodness)
}


recover_thompson <- function(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  #'
  #' @description generate choices according to soft max
  #' and fit them with soft max, thompson, and ucb
  
  tbl_params_participants <- create_participant_sample_thompson(
    gamma_mn, gamma_sd, simulate_data, nr_participants,
    nr_trials, lambda, 1
  )
  
  # simulate one fixed data set in case needed
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq[1], sigma_epsilon_sq[1], mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  l_models_fit <- simulate_and_fit_models(
    tbl_params_participants, tbl_rewards, cond_on_choices,
    family = "kalman"
  )
  
  l_goodness <- read_out_lls_and_ics(l_models_fit, nr_participants)
  
  return(l_goodness)
}


recover_ucb <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, lambda, nr_vars) {
  #'
  #' @description generate choices according to soft max
  #' and fit them with soft max, thompson, and ucb
  
  tbl_params_participants <- create_participant_sample_ucb(
    gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data, nr_participants,
    nr_trials, lambda, 1
  )
  
  # simulate one fixed data set in case needed
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq[1], sigma_epsilon_sq[1], mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  l_models_fit <- simulate_and_fit_models(
    tbl_params_participants, tbl_rewards, cond_on_choices,
    family = "kalman"
  )
  
  l_goodness <- read_out_lls_and_ics(l_models_fit, nr_participants)
  
  return(l_goodness)
}


recover_mixture <- function(
    gamma_mn, gamma_sd, beta_mn, beta_sd, w_mix_mn, w_mix_sd,
    simulate_data, nr_participants, nr_trials, cond_on_choices, mixturetype,
    lambda, nr_vars) {
  #'
  #' @description generate choices according to soft max
  #' and fit them with soft max, thompson, and ucb
  
  tbl_params_participants <- create_participant_sample_mixture(
    gamma_mn, gamma_sd, beta_mn, beta_sd, w_mix_mn, w_mix_sd, simulate_data,
    nr_participants, nr_trials, lambda, nr_vars, mixturetype
  )
  
  # simulate one fixed data set in case needed
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  l_models_fit <- simulate_and_fit_models(
    tbl_params_participants, tbl_rewards, cond_on_choices,
    family = "kalman"
  )
  
  l_goodness <- read_out_lls_and_ics(l_models_fit, nr_participants)
  
  return(l_goodness)
}


recover_delta <- function(
    gamma_mn, gamma_sd, delta_mn, delta_sd, simulate_data, nr_participants,
    nr_trials, cond_on_choices, is_decay, lambda) {
  #'
  #' @description generate choices according to soft max
  #' and fit them with soft max, thompson, and ucb
  
  tbl_params_participants <- create_participant_sample_delta(
    gamma_mn, gamma_sd, delta_mn, delta_sd, simulate_data, nr_participants,
    nr_trials, is_decay, lambda
  ) %>% select(-is_decay)
  
  # simulate one fixed data set in case needed
  tbl_rewards <- generate_restless_bandits(
    sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
  ) %>%
    select(-trial_id)
  
  l_models_fit <- simulate_and_fit_models(
    tbl_params_participants, tbl_rewards, cond_on_choices,
    family = "delta", is_decay = is_decay
  )
  
  l_goodness <- read_out_lls_and_ics(l_models_fit, nr_participants)
  
  return(l_goodness)
}


simulate_and_fit_models <- function(tbl_params_simulate, tbl_rewards, cond_on_choices, family, is_decay = NULL) {
  #'
  #' @description simulate choices with specifications from tbl_params_simulate
  #' and fit all models afterwards
  
  # simulate choices given soft max choice model
  plan(multisession, workers = availableCores() / 2)
  # plan(multisession, workers = 2)
  cat("\nsimulating data")
  if (family == "kalman") {
    l_choices_simulated <- future_pmap(
      tbl_params_simulate,
      simulate_kalman,
      tbl_rewards = tbl_rewards,
      .progress = TRUE,
      .options = furrr_options(seed = NULL)
    )
  } else if (family == "delta") {
    l_choices_simulated <- future_pmap(
      tbl_params_simulate,
      simulate_delta,
      tbl_rewards = tbl_rewards,
      is_decay = is_decay,
      .progress = TRUE,
      .options = furrr_options(seed = NULL)
    )
  }
  
  cat("\nfitting kalman softmax")
  # fit candidate models on generated data
  l_softmax <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_softmax_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting kalman thompson")
  l_thompson <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_thompson_one_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting kalman ucb")
  l_ucb <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_ucb_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting kalman ucb & thompson")
  l_ucb_thompson <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_mixture_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    f_fit = fit_kalman_ucb_thompson_no_variance,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting kalman ru & thompson")
  l_ru_thompson <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_mixture_no_variance_wrapper),
    condition_on_observed_choices = cond_on_choices,
    f_fit = fit_kalman_ru_thompson_no_variance,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting delta")
  l_delta <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_delta_softmax_wrapper),
    is_decay = FALSE,
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  cat("\nfitting decay")
  l_decay <- future_map2(
    map(l_choices_simulated, "tbl_return"),
    map(l_choices_simulated, "tbl_rewards"),
    safely(fit_delta_softmax_wrapper),
    is_decay = TRUE,
    condition_on_observed_choices = cond_on_choices,
    .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
  
  return(list(
    softmax = l_softmax,
    thompson = l_thompson,
    ucb = l_ucb,
    ucb_thompson = l_ucb_thompson,
    ru_thompson = l_ru_thompson,
    delta = l_delta,
    decay = l_decay
  ))
}


read_out_lls_and_ics <- function(l_models_fit, nr_participants) {
  #'
  #' @description read out by-participant log likelihoods for each model
  #' and summarize these results
  
  # read out neg lls
  neg2ll_softmax <- map_dbl(map(l_models_fit[["softmax"]], "result"), 2)
  neg2ll_thompson <- map_dbl(map(l_models_fit[["thompson"]], "result"), 2)
  neg2ll_ucb <- map_dbl(map(l_models_fit[["ucb"]], "result"), 3)
  neg2ll_ucb_thompson <- map_dbl(map(l_models_fit[["ucb_thompson"]], "result"), 4)
  neg2ll_ru_thompson <- map_dbl(map(l_models_fit[["ru_thompson"]], "result"), 4)
  neg2ll_delta <- map_dbl(map(l_models_fit[["delta"]], "result"), 3)
  neg2ll_decay <- map_dbl(map(l_models_fit[["decay"]], "result"), 3)
  
  tbl_lls <- tibble(
    participant_id = 1:nr_participants,
    
    # bic
    bic_softmax = log(nr_participants) + neg2ll_softmax,
    bic_thompson = log(nr_participants) + neg2ll_thompson,
    bic_ucb = 2 * log(nr_participants) + neg2ll_ucb,
    bic_ucb_thompson = 3 * log(nr_participants) + neg2ll_ucb_thompson,
    bic_ru_thompson = 3 * log(nr_participants) + neg2ll_ru_thompson,
    bic_delta = 2 * log(nr_participants) + neg2ll_delta,
    bic_decay = 2 * log(nr_participants) + neg2ll_decay,
    
    # aic
    aic_softmax = 2 + neg2ll_softmax,
    aic_thompson = 2 + neg2ll_thompson,
    aic_ucb = 4 + neg2ll_ucb,
    aic_ucb_thompson = 6 + neg2ll_ucb_thompson,
    aic_ru_thompson = 6 + neg2ll_ru_thompson,
    aic_delta = 4 + neg2ll_delta,
    aic_decay = 4 + neg2ll_decay
  )
  
  tbl_recovered_bic <- summarize_model_recovery(tbl_lls, "bic")
  tbl_recovered_aic <- summarize_model_recovery(tbl_lls, "aic")
  
  return(list(
    tbl_lls = tbl_lls,
    tbl_recovered_bic = tbl_recovered_bic,
    tbl_recovered_aic = tbl_recovered_aic
  ))
}

summarize_model_recovery <- function(tbl_lls, ic) {
  #'
  #' @description summarize by-participant log likelihoods
  #'
  tbl_models <- tibble(
    model = str_c(
      ic, c("_softmax", "_thompson", "_ucb", "_ucb_thompson", "_ru_thompson", "_delta", "_decay")
    )
  )
  
  tbl_recovered <- tbl_lls %>%
    pivot_longer(cols = starts_with(ic), names_to = "model") %>%
    group_by(participant_id) %>%
    mutate(min_ic = min(value)) %>%
    ungroup() %>%
    filter(value == min_ic) %>%
    count(model)
  
  tbl_recovered <- left_join(tbl_models, tbl_recovered, by = "model") %>%
    replace_na(list(n = 0))
  
  return(tbl_recovered)
}


ucb_stan <- function() {
  m_txt <- write_stan_file("
    data {
      int<lower=1> nSubjects;
      int<lower=1> nTrials;               
      array[nSubjects, nTrials] int choice;     
      matrix[nSubjects, nTrials] reward; 
    }
    transformed data {
      real<lower=0, upper=100> m_prior;
      real<lower=0> var_mean_prior;
      real<lower=0> var_epsilon;
      real<lower=0> var_xi;
      real<lower=0,upper=1> decay;
      real<lower=0, upper=100> decay_center;
      
      m_prior = 50.0;
      var_mean_prior = 1000.0; // prior variance of the mean
      var_epsilon = 16.0; // error variance
      var_xi = 7.84; // innovation variance
      decay = 0.9836;
      decay_center = 50;
    }
    parameters {
      vector<lower=0,upper=3>[nSubjects] tau; 
      vector[nSubjects] beta;
    }
    model {
      for (s in 1:nSubjects) {
        vector[4] m;   // mean of the mean
        vector[4] var_mean; // variance of the mean
        vector[4] eb;  // exploration bonus
        real pe;       // prediction error
        real Kgain;    // Kalman gain
        m = rep_vector(m_prior, 4);
        var_mean = rep_vector(var_mean_prior, 4);
        for (t in 1:nTrials) {        
        
          if (choice[s,t] != 0) {
            
            // choice model
            eb = beta[s] * sqrt(var_mean + var_xi);
            choice[s,t] ~ categorical_logit(tau[s] * (m + eb));
            
            // learning model
            pe = reward[s,t] - m[choice[s,t]];  // prediction error 
            Kgain = (var_mean[choice[s,t]] + var_xi) / (var_mean[choice[s,t]] + var_epsilon + var_xi); // Kalman gain
            m[choice[s,t]] = m[choice[s,t]] + Kgain * pe;  // value/mu updating (learning)
            var_mean[choice[s,t]] = (1-Kgain) * (var_mean[choice[s,t]] + var_xi);
          }
          
          m = decay * m + (1 - decay) * decay_center;  
          for (j in 1:4) {
            var_mean[j] = decay^2 * var_mean[j] + var_xi;
          }
        }  
      }
    }
    generated quantities{
      vector [nSubjects] log_lik;
      array [nSubjects, nTrials] int choice_pred;
      for (s in 1:nSubjects) {
        vector[4] m;   // value (mu)
        vector[4] var_mean; // sigma
        vector[4] eb;  // exploration bonus
        real pe;       // prediction error
        real Kgain;    // Kalman gain
        m = rep_vector(m_prior, 4);
        var_mean = rep_vector(var_mean_prior, 4);
        
        log_lik[s] = 0;    
        for (t in 1:nTrials) { 
          
          if (choice[s,t] != 0) {
            
            // choice model
            eb = beta[s] * sqrt(var_mean + var_xi);
            log_lik[s] = log_lik[s] + categorical_logit_lpmf(choice[s,t] | tau[s] * (m + eb));
            choice_pred[s, t] = categorical_logit_rng(tau[s] * (m + eb));
            
            // learning model
            pe = reward[s,t] - m[choice[s,t]];  // prediction error 
            Kgain = (var_mean[choice[s,t]] + var_xi) / (var_mean[choice[s,t]] + var_epsilon + var_xi); // Kalman gain
            m[choice[s,t]] = m[choice[s,t]] + Kgain * pe;
            var_mean[choice[s,t]] = (1-Kgain) * (var_mean[choice[s,t]] + var_xi);
          }
          
          m = decay * m + (1 - decay) * decay_center;  
          for (j in 1:4) {
            var_mean[j] = decay^2 * var_mean[j] + var_xi;
          }  
        }
      }
    }
  ")
  return(m_txt)
}


ucb_stan_hierarchical_generate <- function() {
  m_txt <- write_stan_file("
    data {
      int<lower=1> nSubjects;
      int<lower=1> nTrials;               
      array[nSubjects, nTrials] int choice;     
      matrix[nSubjects, nTrials] reward; 
    }
    transformed data {
      real<lower=0, upper=100> m_prior;
      real<lower=0> var_mean_prior;
      real<lower=0> var_epsilon;
      real<lower=0> var_xi;
      real<lower=0,upper=1> decay;
      real<lower=0, upper=100> decay_center;
      
      m_prior = 50.0;
      var_mean_prior = 1000.0; // prior variance of the mean
      var_epsilon = 16.0; // error variance
      var_xi = 7.84; // innovation variance
      decay = 0.9836;
      decay_center = 50;
    }
    parameters {
      vector<lower=0,upper=3>[nSubjects] tau; 
      vector[nSubjects] beta;
      real <lower=0> sigma_tau;
      real <lower=0> sigma_beta;
      real mu_tau;
      real mu_beta;

    }
    model {
      for (s in 1:nSubjects) {
        tau[s] ~ normal(mu_tau, sigma_tau);
        beta[s] ~ normal(mu_beta, sigma_beta);
      }
      
      sigma_tau ~ uniform(0.001, 10);
      sigma_beta ~ uniform(0.001, 10);
      mu_tau ~ normal(0, 1);
      mu_beta ~ student_t(1, 0, 1);
    
      for (s in 1:nSubjects) {
        vector[4] m;   // mean of the mean
        vector[4] var_mean; // variance of the mean
        vector[4] eb;  // exploration bonus
        real pe;       // prediction error
        real Kgain;    // Kalman gain
        m = rep_vector(m_prior, 4);
        var_mean = rep_vector(var_mean_prior, 4);
        for (t in 1:nTrials) {        
        
          if (choice[s,t] != 0) {
            
            // choice model
            eb = beta[s] * sqrt(var_mean + var_xi);
            choice[s,t] ~ categorical_logit(tau[s] * (m + eb));
            
            // learning model
            pe = reward[s,t] - m[choice[s,t]];  // prediction error 
            Kgain = (var_mean[choice[s,t]] + var_xi) / (var_mean[choice[s,t]] + var_epsilon + var_xi); // Kalman gain
            m[choice[s,t]] = m[choice[s,t]] + Kgain * pe;  // value/mu updating (learning)
            var_mean[choice[s,t]] = (1-Kgain) * (var_mean[choice[s,t]] + var_xi);
          }
          
          m = decay * m + (1 - decay) * decay_center;  
          for (j in 1:4) {
            var_mean[j] = decay^2 * var_mean[j] + var_xi;
          }
        }  
      }
    }
    generated quantities{
      vector [nSubjects] log_lik;
      array [nSubjects, nTrials] int choice_pred;

      for (s in 1:nSubjects) {
        vector[4] m;   // value (mu)
        vector[4] var_mean; // sigma
        vector[4] eb;  // exploration bonus
        real pe;       // prediction error
        real Kgain;    // Kalman gain
        m = rep_vector(m_prior, 4);
        var_mean = rep_vector(var_mean_prior, 4);
        
        log_lik[s] = 0;    
        for (t in 1:nTrials) {   
          
          if (choice[s,t] != 0) {
            
            // choice model
            eb = beta[s] * sqrt(var_mean + var_xi);
            log_lik[s] = log_lik[s] + categorical_logit_lpmf(choice[s,t] | tau[s] * (m + eb));
            choice_pred[s, t] = categorical_logit_rng(tau[s] * (m + eb));
            
            // learning model
            pe = reward[s,t] - m[choice[s,t]];  // prediction error 
            Kgain = (var_mean[choice[s,t]] + var_xi) / (var_mean[choice[s,t]] + var_epsilon + var_xi); // Kalman gain
            m[choice[s,t]] = m[choice[s,t]] + Kgain * pe;
            var_mean[choice[s,t]] = (1-Kgain) * (var_mean[choice[s,t]] + var_xi);
          }
          
          m = decay * m + (1 - decay) * decay_center;  
          for (j in 1:4) {
            var_mean[j] = decay^2 * var_mean[j] + var_xi;
          }  
        }
      }
    }
  ")
  return(m_txt)
}


ucb_stan_hierarchical_fit_fixed_learning <- function() {
  m_txt <- write_stan_file("
    data {
      int<lower=1> nSubjects;
      int<lower=1> nTrials;               
      array[nSubjects, nTrials] int choice;     
      matrix[nSubjects, nTrials] reward;
      array[nSubjects, nTrials] int choice_gen;     
    }
    transformed data {
      real<lower=0, upper=100> m_prior;
      real<lower=0> var_mean_prior;
      real<lower=0> var_epsilon;
      real<lower=0> var_xi;
      real<lower=0,upper=1> decay;
      real<lower=0, upper=100> decay_center;
      
      m_prior = 50.0;
      var_mean_prior = 1000.0; // prior variance of the mean
      var_epsilon = 16.0; // error variance
      var_xi = 7.84; // innovation variance
      decay = 0.9836;
      decay_center = 50;
    }
    parameters {
      vector<lower=0,upper=3>[nSubjects] tau; 
      vector[nSubjects] beta;
      real <lower=0> sigma_tau;
      real <lower=0> sigma_beta;
      real mu_tau;
      real mu_beta;

    }
    model {
      for (s in 1:nSubjects) {
        tau[s] ~ normal(mu_tau, sigma_tau);
        beta[s] ~ normal(mu_beta, sigma_beta);
      }
      
      sigma_tau ~ uniform(0.001, 10);
      sigma_beta ~ uniform(0.001, 10);
      mu_tau ~ normal(0, 1);
      mu_beta ~ student_t(1, 0, 1);
    
      for (s in 1:nSubjects) {
        vector[4] m;   // mean of the mean
        vector[4] var_mean; // variance of the mean
        vector[4] eb;  // exploration bonus
        real pe;       // prediction error
        real Kgain;    // Kalman gain
        m = rep_vector(m_prior, 4);
        var_mean = rep_vector(var_mean_prior, 4);
        for (t in 1:nTrials) {        
        
          if (choice[s,t] != 0) {
            
            // choice model
            eb = beta[s] * sqrt(var_mean + var_xi);
            // here, we just want to know, how likely the generated choice (aka backcast) is
            // with leaving the learning as was by participants
            choice_gen[s,t] ~ categorical_logit(tau[s] * (m + eb));
            
            // learning model
            pe = reward[s,t] - m[choice[s,t]];  // prediction error 
            Kgain = (var_mean[choice[s,t]] + var_xi) / (var_mean[choice[s,t]] + var_epsilon + var_xi); // Kalman gain
            m[choice[s,t]] = m[choice[s,t]] + Kgain * pe;  // value/mu updating (learning)
            var_mean[choice[s,t]] = (1-Kgain) * (var_mean[choice[s,t]] + var_xi);
          }
          
          m = decay * m + (1 - decay) * decay_center;  
          for (j in 1:4) {
            var_mean[j] = decay^2 * var_mean[j] + var_xi;
          }
        }  
      }
    }
  ")
  return(m_txt)
}



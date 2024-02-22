############# parameter recovery functions #######################

library(brms)
library(docstring)
library(ggplot2)
theme_set(theme_classic(base_size = 14))
set.seed(123)


getV <- function(m1, m2){
  return(m1-m2)
}

getRU <- function(s1, s2){
  
  return(sqrt(s1) - sqrt(s2))
}

kalman_learning <- function(tbl_df, no, sigma_xi_sq, sigma_epsilon_sq) {
  #' Kalman filter without choice model given chosen options by participants
  #' 
  #' @description applies Kalman filter equations for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance (prior variance)
  #' @return a tbl with by-trial posterior means and variances for all bandits
  rewards <- tbl_df$reward
  choices <- tbl_df$chosen
  m0 <- 50
  v0 <- 100
  nt <- length(rewards) # number of time points
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(v0, ncol = no, nrow = nt + 1) # to hold the posterior variances
  
  for(t in 1:nt) {
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt[choices[t]+1] <- (v[t,choices[t]+1] + sigma_xi_sq[choices[t]+1])/(v[t,choices[t]+1] + sigma_epsilon_sq + sigma_xi_sq[choices[t]+1])
    # compute the posterior means
    m[t+1,] <- m[t,] + kt*(rewards[t] - m[t,])
    # compute the posterior variances
    v[t+1,] <- (1-kt)*(v[t,]) + sigma_xi_sq
  }
  tbl_m <- as.data.frame(m)
  # constrain v from becoming to small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- paste("m_", 1:no, sep = "")
  colnames(tbl_v) <- paste("v_", 1:no, sep = "")
  tbl_return <- tibble(cbind(tbl_m, tbl_v))
  
  return(tbl_return)
}


bayIncrAtOnce <- function(x, horizon){
  #' Bayesian integration
  #' 
  #' @description uses bayesian integration to calculate posterior mean and variance for different options
  #' @param x current trial in task
  #' @param horizon dataframe task data
  #' @return list of means for both arms and variances for both arms
  #' @references taken from section 3 of: https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
  
  
  left <- horizon$reward[horizon$ID == horizon$ID[x]&
                           horizon$block == horizon$block[x] &
                           horizon$chosen == 0& 
                           horizon$trial < 5]
  right <- horizon$reward[horizon$ID == horizon$ID[x]&
                            horizon$block == horizon$block[x] &
                            horizon$chosen == 1& 
                            horizon$trial < 5]
  
  # initialise prior
  muLeft <- 50
  muRight <- 50
  alphaLeft <- 0.001 # where did this come from?
  alphaRight <- 0.001
  betaLeft <- 200
  betaRight <- 200
  nzero <-  0.01 # weighting of prior vs posterior, if same number as number of observations -> equal
  
  # updating left
  
  
  alphaLeft <- alphaLeft+ length(left)/2 # increased precision bc more info
  betaLeft <- betaLeft + 0.5*ifelse(length(left)>1, var(left),0) + (length(left)*nzero/2*(length(left)+nzero)) *  (mean(left) - muLeft)^2 # decreased precision bc info has variance
  tauLeft <- length(left) * qgamma(.5, alphaLeft, betaLeft) + nzero * qgamma(.5, alphaLeft, betaLeft)
  muLeft <- ((length(left)*tauLeft)/((length(left)*tauLeft) + nzero*tauLeft)) * mean(left) + ((nzero * tauLeft) / ((length(left)*tauLeft) + nzero* tauLeft)) * muLeft
  
  # updating right
  alphaRight <- alphaRight+ length(right)/2
  betaRight <- betaRight + 0.5*ifelse(length(right)>1, var(right),0) + (length(right)*nzero/2*(length(right)+nzero)) *  (mean(right) - muRight)^2
  tauRight <- length(right) * qgamma(.5, alphaRight, betaRight) + nzero * qgamma(.5, alphaRight, betaRight)
  muRight <- ((length(right)*tauRight)/((length(right)*tauRight) + nzero*tauRight)) * mean(right) + ((nzero * tauRight) / ((length(right)*tauRight) + nzero* tauRight)) * muRight
  
  
  
  return(c(muLeft,muRight,sqrt(1/tauLeft), sqrt(1/tauRight)))
}

sim_data_sam <- function(data, trueModel, i, bootstrapped = F, hierarchical = F) {
  
  #' simulate data for sam's task from whatever model
  #' 
  #' @description simulates choices for a particular subject in a trial-by-trial fashion
  #' @param data original data from that entire task
  #' @param trueModel Model fit to true choices of that subject
  #' @param i index of subject to simulate data for
  #' @param bootstrapped boolean, if T, trueModel is simply the parameter estimates to build an equation from
  #' @return data.frame with simulated data for that subject
  
  id <- ifelse(bootstrapped, 1, i)
  simdat <- subset(data, ID == id, -c(chosen, V, RU, KLM0, KLM1, KLV0, KLV1, reward, reward1, reward2))
  
  
  blocks <- max(simdat$block)
  trials <- max(simdat$trial)
  
  ## create rewards
  simdat$reward1[simdat$trial == 1] <- sample(data$reward1, blocks, replace = T)
  simdat$reward2[simdat$trial == 1] <- sample(data$reward2, blocks, replace = T)
  
  # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  # stable condition:
  # sample a value for each first trial and repeat it for the rest of the trials
  simdat$reward1[simdat$cond == "SS" | simdat$cond == "SF"] <- rep(sample(data$reward1, nrow(simdat[(simdat$cond == "SS" | simdat$cond == "SF") & simdat$trial == 1, ]), replace = T), each = trials)
  simdat$reward2[ simdat$cond == "SS" | simdat$cond == "FS"] <- rep(sample(data$reward2, nrow(simdat[(simdat$cond == "FS" | simdat$cond == "SS")& simdat$trial == 1, ]), replace = T), each = trials)
  
  # random walk
  for (j in 2:trials){
    simdat$reward1[simdat$trial == j & (simdat$cond == "FS" | simdat$cond == "FF")] <- rnorm(1,simdat$reward1[simdat$trial == j-1 & (simdat$cond == "FS" | simdat$cond == "FF")],4)
    simdat$reward2[simdat$trial == j & (simdat$cond == "SF" | simdat$cond == "FF")] <- rnorm(1,simdat$reward2[simdat$trial == j-1 & (simdat$cond == "SF" | simdat$cond == "FF")],4)
  }
  # add noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward1 <- simdat$reward1 + noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward2 <- simdat$reward2 + noise
  
  
  ## iteratively make choices
  # learning part initialisations
  m0 <- 0
  v0 <- 100
  no <- 2
  sigma_epsilon_sq <- 1
  # sigma xi sq depends on the condition 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  sigma_xi_sq <- matrix(NA, ncol = 2, nrow = blocks)
  sigma_xi_sq[ ,1] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == "FS" | x == "FF", 4, 0))
  sigma_xi_sq[ ,2] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == "SF" | x == "FF", 4, 0))
  
  # get initial posterior mean and variance for each option
  m <- matrix(0, ncol = no, nrow = nrow(simdat)) # to hold the posterior means
  v <- matrix(100, ncol = no, nrow = nrow(simdat)) # to hold the posterior variances
  # get initial V and RU
  simdat$V[simdat$trial == 1] <- 0
  simdat$RU[simdat$trial == 1] <- 0
  simdat$VTU[simdat$trial == 1] <- 0
  
  for(t in 1:trials) {
    # get choice
    if (bootstrapped) {
      C <- trueModel$V[i] * simdat$V[t] + trueModel$RU[i] * simdat$RU[t]
    } else {
      if (hierarchical){
        library(brms)
        C <- predict(trueModel, newdata = simdat[simdat$trial == t, ])[ ,1]
      } else{
        C <- predict(trueModel, newdata = simdat[simdat$trial == t, ] , type = "response")
      }
      
    }
    
    C <- ifelse(runif(length(C)) < C, 1, 0)
    simdat$C[simdat$trial == t] <- C
    # get reward
    reward <- ifelse(C == 0, simdat$reward1[simdat$trial == t], simdat$reward2[simdat$trial == t])
    
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt <- matrix(0, ncol = no, nrow = blocks)
    # indexing doesn't really work if not consistently same column so the _chosen thing is my akward workaround
    v_chosen <- ifelse(C == 0, v[simdat$trial == t, 1], v[simdat$trial == t, 2])
    sigma_xi_chosen <-  ifelse(C == 0, sigma_xi_sq[ , 1], sigma_xi_sq[ ,2])
    kt_chosen <- (v_chosen + sigma_xi_chosen)/(v_chosen + sigma_epsilon_sq + sigma_xi_chosen)
    kt[ ,1] <- ifelse(C == 0, kt_chosen, 0)
    kt[ ,2] <- ifelse(C == 1, kt_chosen, 0)
    # compute the posterior means
    m[simdat$trial == (t+1),] <- m[simdat$trial == t,] + kt*(reward - m[simdat$trial == t,])
    # compute the posterior variances
    v[simdat$trial == (t+1), ] <- (1-kt)*(v[simdat$trial == t,]) + sigma_xi_sq
    
    # compute V and RU
    simdat$V[simdat$trial == (t+1)] <- getV(m[simdat$trial == (t+1),1], m[simdat$trial == (t+1),2])
    simdat$RU[simdat$trial == (t+1)] <- getRU(v[simdat$trial == (t+1),1], v[simdat$trial == (t+1),2])
    simdat$VTU[simdat$trial == (t+1)] <- simdat$V[simdat$trial == (t+1)]/(sqrt(v[simdat$trial == (t+1),1] + v[simdat$trial == (t+1),2]))
    
    simdat$chosen <- simdat$C
    
  }
  
  
  simdat$C <- as.integer(simdat$C)
  
  return(simdat)
}

fit_model_sam <- function(data, model, hierarchical, it = 2000){
  #' fit model to data from Sam's task
  #' 
  #' @description fits the type of model described by input to function to the data given to function
  #' @param data data.frame containing all the task data
  #' @param model string: UCB vs hybrid
  #' @param hierarchical boolean; bayesian fitting or subject-level glm
  #' @param it number of iterations, optional, only relevant if hierarchical = T
  #' @return list containing model object and if hierarchical == F also a data.frame with coefficients
  
  if (hierarchical){
    if (model == "hybrid"){
      
      trueModel <- brm(chosen ~ V+ RU + VTU + (V+ RU + VTU| ID), 
                      family = "bernoulli", 
                      data = data,
                      chains = 2,
                      cores = 2,
                      iter = it)
      
    } else if (model == "UCB") {
      
      trueModel <- brm(chosen ~ V+ RU + (V+ RU | ID), 
                       family = "bernoulli", # has to be bernoulli, crashes otherwise
                       data = data,
                       chains = 2,
                       cores = 2,
                       iter = it)
    }
    
    ## get posterior estimates of subject-level parameters
    
    trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(trueModel))))
    trueParams$predictor <- NA
    trueParams$predictor[grepl("RU", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "RU"
    trueParams$predictor[grepl("V", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "V"
    if (model == "hybrid"){
      trueParams$predictor[grepl("VTU", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "VTU"
    }
    trueParams <- subset(trueParams, !is.na(predictor)& !grepl("ID__", rownames(trueParams)))
    
  } else {
    if (model == "hybrid"){
      trueModel <- glm(chosen ~ V+ RU + VTU,
                       data = data,
                       family = binomial(link = "probit"))
      
    } else if (model == "UCB") {
      trueModel <- glm(chosen ~ V+ RU,
                       data = data,
                       family = binomial(link = "probit"))
    }
    
    # save coefficients themselves if it is the subject-level model
    trueParams <- data.frame(ID = unique(data$ID),
                             V = trueModel$coefficients[2],
                             RU = trueModel$coefficients[3],
                             converged = trueModel$converged)
    
    if (model == "hybrid"){
      trueParams$VTU[trueParams$ID == i] <- trueModel$coefficients[4]
    
  }
    

  
  }
  output <- list(trueModel, trueParams)
  return(output)
  
}

get_KL_into_df <- function(data){
  ## add mean and variance from the Kalman Filter to the data frame
  data$KLM0 <- NA
  data$KLM1 <- NA
  data$KLV0 <- NA
  data$KLV1 <- NA
  
  ind = 0
  ## get output of Kalman Filter into dataframe
  for (i in unique(paste(data$ID, data$block))){ 
    # progress bar type thing
    ind = ind+1
    if (ind %% 30 == 0){print(sprintf("adding output of KF to subject %i of %i", data$ID[paste(data$ID, data$block) == i], data$ID[length(data$ID)]))} 
    
    dat <- subset(data, paste(data$ID, data$block) == i)
    
    # stable or fluctuating arms? -> get innovation variance based on this
    
    if (dat$cond[1] == "FS") {xi <- c(4,0)
    } else if (dat$cond[1] == "SF") {xi <- c(0,4)
    } else if (dat$cond[1] == "FF") {xi <- c(4,4)
    } else if (dat$cond[1] == "SS") {xi <- c(0,0)}
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    data$KLM0[paste(data$ID, data$block) == i] <- posterior$m_1[1:10]
    data$KLM1[paste(data$ID, data$block) == i] <- posterior$m_2[1:10]
    data$KLV0[paste(data$ID, data$block) == i] <- posterior$v_1[1:10]
    data$KLV1[paste(data$ID, data$block) == i] <- posterior$v_2[1:10]
    
  }
  
  data$V <- data$KLM0 - data$KLM1
  data$RU <- getRU(data$KLV0, data$KLV1)
  data$VTU <- data$V/(sqrt(data$KLV0 + data$KLV1))
  
  return(data)
  
}


recovery_sam <- function(data, model, hierarchical, it = 2000){
  #' parameter recovery for data from Sam's task
  #' 
  #' @description fits model to data; simulates data based on subjects' estimates; re-fits that data
  #' @param data data.frame containing all the task data
  #' @param model UCB, hybrid
  #' @param hierarchical boolean; whether data are fit using brms or subject-level glms
  #' @param it iterations, option, only relevant if hierarchical = T
  #' @return a list containing a data.frame with subject-level estimates fitted to the observed data, a data.frame with the recovered estimates, a ggplot element plotting the recovery
  
  
   
    
    # prep dataframe
    trueParams <- data.frame(ID = unique(data$ID),
                             V = rep(NA, length(unique(data$ID))),
                             RU = rep(NA, length(unique(data$ID))),
                             converged = rep(NA, length(unique(data$ID))))
    if (model == "hybrid"){
      trueParams$VTU <- NA
    }
    
    simParams <- trueParams
    
    
    blocks <- max(data$block)
    trials <- max(data$trial)
    nsubs <- 1 # bc we do 1 subject at a time
    
    
    ### fit model
    if (hierarchical){
      out <- fit_model_sam(data, model, T, it)
      trueModel <- out[[1]]
      trueParams <- out[[2]]
    }
    
    simdatCollect <- data.frame()# for hierarchical model bc that one needs the simdat of all subjects at once
    
    ## iterate through subjects to simulate for each subject
    for (i in unique(data$ID)){
      if (i %% 10 == 0) {print(paste("subject", i, "of", max(data$ID)))}
      
      ### fit model
      if (!hierarchical){
        out <- fit_model_sam(data[data$ID == i, ], model, F)
        
        trueParams[trueParams$ID == i, ] <- out[[2]]
        trueModel <- out[[1]]
      }
      
      
      ### simulate data
      
      # create data
      simdat <- sim_data_sam(data, trueModel, i, hierarchical= hierarchical)
      
      if (!hierarchical) { # if it's not hierarchical then we do this for every subject separately, otherwise only in end
        simParams[simParams$ID == i, ] <- fit_model_sam(simdat, model, F)[[2]]
      } else {simdatCollect <- rbind(simdatCollect, simdat)} # collect simdat for later for hierarchical model
      

    }
    
    params <- c("RU", "V")
    if (model == "hybrid"){
      params[3] <- "VTU"
    }
    
    ## extract parameters for hierarchical
    if (hierarchical){
      simParams <- fit_model_sam(simdatCollect, model, T, it)[[2]]
      
      ### get correlations for hierarchical
      
      
      cors <- data.frame(true = rep(params, length(params)),
                         recovered =  rep(params, each = length(params)),
                         cor = NA)
      
      cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$`colMeans(as.data.frame(posterior_samples(trueModel)))`[trueParams$predictor == cors$true[x]],
                                                                   simParams$`colMeans(as.data.frame(posterior_samples(trueModel)))`[simParams$predictor == cors$recovered[x]]))
      
    } else {
      
      ## get correlations for subject-level
      simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
      
      trueParams$bothConverged <- simParams$bothConverged
      
      cors <- data.frame(true = rep(params, length(params)),
                         recovered =  rep(params, each = length(params)),
                         cor = NA)
      
      cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                                   simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))
        
    }
    
    
    # plot them
    
    p <- ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
      geom_text(aes(label = round(cor, digits = 2))) + ggtitle(paste("Recovery of Sam's task using ", model))
    
    
    return(list(trueParams, simParams, p))
  
  
}

fit_model_horizon <- function(data, model, full = T, it = 2000){
  #' parameter recovery for data from Horizon task
  #' 
  #' @description fits model to data
  #' @usage model = fit_model_horizon(data, model, full)
  #' @param data data.frame containing all the task data
  #' @param model UCB, Wilson
  #' @param full boolean; T for full random effects; F for reduced random effects
  #' @param it iterations of brms
  #' @return a brms model object
  #' 
  
  ### Wilson model
  if (model == "Wilson"){
    data$mean_L <- NA
    data$mean_R <- NA
    
    data$row <- 1:nrow(data)
    data$mean_L[data$trial == 5] <- apply(as.array(data$row[data$trial == 5]), 1, function(x) meann(data$reward[data$ID == data$ID[x]&
                                                                                                                  data$block == data$block[x] &
                                                                                                                  data$chosen == 0 & 
                                                                                                                  data$trial < 5]))
    data$mean_R[data$trial == 5] <- apply(as.array(data$row[data$trial == 5]), 1, function(x) meann(data$reward[data$ID == data$ID[x]&
                                                                                                                  data$block == data$block[x] &
                                                                                                                  data$chosen == 1& 
                                                                                                                  data$trial < 5]))
    ## calculate deltas
    data$delta_mean <- scale(data$mean_L - data$mean_R)
    
      
      if (full == T){
        
        baymodel <- brm(chosen ~ delta_mean*Horizon + info*Horizon + (info*Horizon+ delta_mean*Horizon| ID), family = "bernoulli", 
                        data = data[data$trial == 5, ],
                        chains = 2,
                        cores = 2,
                        iter = it)
        
      } else {
        baymodel <- brm(chosen ~ delta_mean*Horizon + info*Horizon + (info:Horizon + delta_mean:Horizon| ID), family = "bernoulli",
                        data = data[data$trial == 5, ],
                        chains = 2,
                        cores = 2,
                        iter = it)
        
      }
      
      return(baymodel)
      
  } else if (model == "UCB"){
      
    if (full == T){
      baymodelUCB <- brm(chosen ~ V*Horizon + RU*Horizon + (RU*Horizon + V*Horizon| ID), family = "bernoulli",
                         data = data[data$trial == 5, ],
                         chains = 2,
                         cores = 2,
                         iter = 1000)
    } else {
      baymodelUCB <- brm(chosen ~ V*Horizon + RU*Horizon + (RU:Horizon + V:Horizon| ID), family = "bernoulli",
                         data = data[data$trial == 5, ],
                         chains = 2,
                         cores = 2,
                         iter = 1000)
    }
    
    
    }
  
  
}
  
  
recovery_horizon <- function(data, model, full = T, bayesian = T, it = 2000){
  #' parameter recovery for data from Horizon task
  #' 
  #' @description fits model to data; simulates data based on subjects' estimates; re-fits that data
  #' @usage res_list = recovery_horizon(data, model, full = T, bayesian = T)
  #' @param data data.frame containing all the task data
  #' @param model UCB, Wilson
  #' @param full boolean; T for full random effects; F for reduced random effects; irrelevant if bayesian == F
  #' @param bayesian boolean; T for brms implementation, F for subject-level glm implementation
  #' @param it iterations of brms, only relevant if bayesian = T
  #' @return a list containing a data.frame with subject-level estimates fitted to the observed data, a data.frame with the recovered estimates, a ggplot element plotting the recovery
  
  
  nTrials = max(data$block)
  
  ### Wilson model
  if (model == "Wilson"){
    
    
    if (bayesian == T){
      
     baymodel <- fit_model_horizon(data = data, model = model, full = full, it = it)
      
      # simulate data
      simdat <- subset(data, trial == 5, -chosen)
      simdat$chosen <- predict(baymodel)[ ,1]
      simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
      
    
      recovModel <- fit_model_horizon(data, model, full, it)
      
      # get posterior estimates from both models
      
      trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(baymodel))))
      trueParams$predictor <- NA
      if (full == T){
        trueParams$predictor[grepl("info", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "info"
        trueParams$predictor[grepl("delta_mean", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "delta_mean"
        trueParams$predictor[grepl("Horizon", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "Horizon"
      }
      trueParams$predictor[grepl("Intercept", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "Intercept"
      trueParams$predictor[grepl("info:Horizon", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "info*Horizon"
      trueParams$predictor[grepl("Horizon:delta_mean", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "delta_mean*Horizon"
      trueParams <- subset(trueParams, !is.na(predictor)& !grepl("ID__", rownames(trueParams)))
      
      
      recoveredParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(recovModel))))
      recoveredParams$predictor <- NA
      if (full == T){
        recoveredParams$predictor[grepl("info", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "info"
        recoveredParams$predictor[grepl("delta_mean", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "delta_mean"
        recoveredParams$predictor[grepl("Horizon", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "Horizon"
      }
      recoveredParams$predictor[grepl("Intercept", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "Intercept"
      recoveredParams$predictor[grepl("info:Horizon", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "info*Horizon"
      recoveredParams$predictor[grepl("Horizon:delta_mean", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "delta_mean*Horizon"
      recoveredParams <- subset(recoveredParams, !is.na(predictor)& !grepl("ID__", rownames(recoveredParams)))
      
      # get correlations
      
      params <- c("Intercept", "info*Horizon", "delta_mean*Horizon")
      if (full == T){
        params <- c(params, "info", "delta_mean", "Horizon")
      }
      
      
      cors <- data.frame(true = rep(params, length(params)),
                         recovered =  rep(params, each = length(params)),
                         cor = NA)
      
      cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$`colMeans(as.data.frame(posterior_samples(baymodel)))`[trueParams$predictor == cors$true[x]],
                                                                   recoveredParams$`colMeans(as.data.frame(posterior_samples(recovModel)))`[recoveredParams$predictor == cors$recovered[x]]))
      
      
    } else {warning("A non-bayesian version of the wilson model has not yet been implemented.")}
    
    
  ### UCB  
  } else if (model == "UCB"){
    
    # add bay means if they are not already there (this takes a while)
    if (!is.element("bayMeanL", colnames(data))) {
      
      print("calculating posterior means and variances, this might take a while... consider doing this before and saving it in the variable")
      data$bayMeanL <- NA
      data$bayMeanR <- NA
      data$bayVarL <- NA
      data$bayVarR <- NA
      data$row <- 1:nrow(data)
      
      for (i in data$row[data$trial == 5]){
        data[data$row == i, grep("bay", colnames(data))] <- bayIncrAtOnce(i, data)
      }
    }
    

    
    data$V <- scale(getV(data$bayMeanL, data$bayMeanR))
    data$RU <- scale(getRU(data$bayVarL, data$bayVarR))
    
    ## GLM implementation
    if (bayesian == F){
      
      
      trueParams <- data.frame(ID = unique(data$ID),
                               V = NA,
                               RU = NA,
                               Horizon = NA,
                               VH = NA,
                               RUH = NA,
                               converged = NA)
      
      simParams <- data.frame(ID = unique(data$ID),
                              V = NA,
                              RU = NA,
                              Horizon = NA,
                              VH = NA,
                              RUH =NA,
                              converged = NA)

      
      for (i in unique(data$ID)){
        
        trueModel <- glm(chosen ~ V*Horizon + RU*Horizon ,
                          data = data[data$trial ==5 & data$ID == i, ],
                          family = binomial(link = "probit"))
        # save coefficients
        trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
        trueParams$RU[trueParams$ID == i] <- trueModel$coefficients[4]
        trueParams$Horizon[trueParams$ID == i] <- trueModel$coefficients[3]
        trueParams$VH[trueParams$ID == i] <- trueModel$coefficients[5]
        trueParams$RUH[trueParams$ID == i] <- trueModel$coefficients[6]
        trueParams$converged[trueParams$ID == i] <- trueModel$converged
        
        # simulate data
        simdat <- subset(data, trial == 5 & ID == i, -chosen)
        simdat$chosen <- predict(trueModel, type = "response")
        simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
        
        simModel <- glm(chosen ~ V*Horizon + RU*Horizon ,
                        data = simdat,
                        family = binomial(link = "probit"))
        
        
        simParams$V[simParams$ID == i] <- simModel$coefficients[2]
        simParams$RU[simParams$ID == i] <- simModel$coefficients[4]
        simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
        simParams$VH[simParams$ID == i] <- simModel$coefficients[5]
        simParams$RUH[simParams$ID == i] <- simModel$coefficients[6]
        simParams$converged[simParams$ID == i] <- simModel$converged
        
      }
      
      simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
     
      trueParams$bothConverged <- simParams$bothConverged
      
      
      # get correlations
      cors <- data.frame(true = rep(c("RU", "V", "Horizon", "RUH", "VH"), 5),
                         recovered =  rep(c("RU", "V", "Horizon", "RUH", "VH"), each = 5),
                         cor = NA)
      
      cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                                   simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))
      
      
      recoveredParams <- simParams
      
      
      
      ##### bayesian implementation
    } else {

     baymodelUCB <- fit_model_horizon(data, model, full, it)
      
      # simulate data
      simdat <- subset(data, trial == 5, -chosen)
      simdat$chosen <- predict(baymodelUCB)[ ,1]
      simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
      
      recovModelUCB <- fit_model_horizon(data, model, full, it)
      
      # get posterior estimates from both models
      
      trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(baymodelUCB))))
      trueParams$predictor <- NA
      if (full == T){
        trueParams$predictor[grepl("RU", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "RU"
        trueParams$predictor[grepl("V", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "V"
        trueParams$predictor[grepl("Horizon", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "Horizon"
      }
      trueParams$predictor[grepl("Intercept", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "Intercept"
      trueParams$predictor[grepl("RU:Horizon", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "RU*Horizon"
      trueParams$predictor[grepl("Horizon:V", rownames(trueParams))& grepl("r_ID", rownames(trueParams))] <- "V*Horizon"
      trueParams <- subset(trueParams, !is.na(predictor)& !grepl("ID__", rownames(trueParams)))
      
      
      recoveredParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(recovModelUCB))))
      recoveredParams$predictor <- NA
      if (full == T){
        recoveredParams$predictor[grepl("RU", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "RU"
        recoveredParams$predictor[grepl("V", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "V"
        recoveredParams$predictor[grepl("Horizon", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "Horizon"
      }
      recoveredParams$predictor[grepl("Intercept", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "Intercept"
      recoveredParams$predictor[grepl("RU:Horizon", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "RU*Horizon"
      recoveredParams$predictor[grepl("Horizon:V", rownames(recoveredParams))& grepl("r_ID", rownames(recoveredParams))] <- "V*Horizon"
      recoveredParams <- subset(recoveredParams, !is.na(predictor)& !grepl("ID__", rownames(recoveredParams)))
      
      # get correlations
      params <- c("Intercept", "RU*Horizon", "V*Horizon")
      if (full == T){
        params <- c(params, "RU", "V", "Horizon")
      }
      
      cors <- data.frame(true = rep(params, length(params)),
                         recovered =  rep(params, each = length(params)),
                         cor = NA)
      
      cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$`colMeans(as.data.frame(posterior_samples(baymodelUCB)))`[trueParams$predictor == cors$true[x]],
                                                                   recoveredParams$`colMeans(as.data.frame(posterior_samples(recovModelUCB)))`[recoveredParams$predictor == cors$recovered[x]]))
      
      
      
    }
    
    
  }
  
  
  # plotting and packaging it all for the return
  
  p <-ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
    geom_text(aes(label = round(cor, digits = 2))) + ggtitle(paste("Recovery of Horizon task using ", model))
  
  
  
  return(list(trueParams, recoveredParams, p))
  
  
  
  
  
  
}
  

recover_bootstrapped_estimates_glm <- function(N, trueParams, model, task, data){
  
  #' parameter recovery using bootstrapped estimates for any desired sample size
  #' only works for the non-bayesian (subject-level) implementations of the models
  #' 
  #' @description create bootstrapped estimates, simulate data from them, re-fit that data
  #' @usage res_list = recover_bootstrapped_estimates_glm(N, trueParams, model, task, data)
  #' @param data data.frame containing all the task data
  #' @param model UCB, Wilson, Sam
  #' @param N number of subjects to generate data for
  #' @param trueParams parameter estimates to bootstrap from
  #' @return a list containing a data.frame with bootstrapped subject-level estimates fitted to the observed data, a data.frame with the recovered estimates, a ggplot element plotting the recovery
  
  
  # take out outliers
  if (task == "Horizon"){
    trueParams <- subset(trueParams, V > mean(V)-1*sd(V) &  V < mean(V)+1*sd(V))
  }
  
  # List of column names
  column_names <- colnames(trueParams)
  column_names <- column_names[!grepl("onverged", column_names)]
  
  # Create an empty data frame with specific column names
  bootstrappedParams <- data.frame(matrix(nrow = N, ncol = length(column_names)))
  colnames(bootstrappedParams) <- column_names
  
  bootstrappedParams$ID <- 1:N
  
  simParams <- bootstrappedParams
  
  for (i in 2:length(column_names)){
    bootstrappedParams[ ,i] <- rnorm(N, mean = mean(trueParams[ ,colnames(trueParams)== column_names[i]]), sd = sd(trueParams[ ,colnames(trueParams)== column_names[i]]) )
    
    
  }
  
  if (model == "UCB"){
    
    if (task == "Horizon") {
      data$bayMeanL <- NA
      data$bayMeanR <- NA
      data$bayVarL <- NA
      data$bayVarR <- NA
      data$row <- 1:nrow(data)
      
      for (i in data$row[data$trial == 5]){
        data[data$row == i, grep("bay", colnames(data))] <- bayIncrAtOnce(i, data)
      }
      
      data$V <- scale(getV(data$bayMeanL, data$bayMeanR))
      data$RU <- scale(getRU(data$bayVarL, data$bayVarR))
      
    } else if (task == "Sam"){
      ## add mean and variance from the Kalman Filter to the data frame
      data$KLM0 <- NA
      data$KLM1 <- NA
      data$KLV0 <- NA
      data$KLV1 <- NA
      
      
      for (i in unique(paste(data$ID, data$block))){ 
        
        dat <- subset(data, paste(data$ID, data$block) == i)
        
        # stable or fluctuating arms? -> get innovation variance based on this
        
        if (dat$cond[1] == "FS") {xi <- c(4,0)
        } else if (dat$cond[1] == "SF") {xi <- c(0,4)
        } else if (dat$cond[1] == "FF") {xi <- c(4,4)
        } else if (dat$cond[1] == "SS") {xi <- c(0,0)}
        
        posterior <- kalman_learning(dat, 2, xi, 1)
        
        data$KLM0[paste(data$ID, data$block) == i] <- posterior$m_1[1:10]
        data$KLM1[paste(data$ID, data$block) == i] <- posterior$m_2[1:10]
        data$KLV0[paste(data$ID, data$block) == i] <- posterior$v_1[1:10]
        data$KLV1[paste(data$ID, data$block) == i] <- posterior$v_2[1:10]
        
      }
      
      data$V <- data$KLM0 - data$KLM1
      data$RU <- getRU(data$KLV0, data$KLV1)
    }

    for (i in bootstrappedParams$ID){
      
      # simulate data
      if(task == "Horizon"){
        simdat <- subset(data, trial == 5 & ID == 1, -chosen) # ID does not matter here, everyone observed the same fixed choices anyway
        simdat$chosen <- bootstrappedParams$V[i] * simdat$V + bootstrappedParams$RU[i] * simdat$RU + bootstrappedParams$Horizon[i] * simdat$Horizon +
          bootstrappedParams$VH[i] * (simdat$V * simdat$Horizon) + bootstrappedParams$RUH[i] * (simdat$RU * simdat$Horizon) # setting intercept to 0 bc used scaled params so should approximately be ok
        simdat$chosen <- ifelse(simdat$chosen < runif(nrow(simdat)), 0, 1)
        
        simModel <- glm(chosen ~ V*Horizon + RU*Horizon ,
                        data = simdat,
                        family = binomial(link = "probit"))
        
        
        simParams$V[simParams$ID == i] <- simModel$coefficients[2]
        simParams$RU[simParams$ID == i] <- simModel$coefficients[4]
        simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
        simParams$VH[simParams$ID == i] <- simModel$coefficients[5]
        simParams$RUH[simParams$ID == i] <- simModel$coefficients[6]
        simParams$converged[simParams$ID == i] <- simModel$converged
      } else if (task == "Sam") {
        
        simdat <- sim_data_sam(data, bootstrappedParams, i, bootstrapped = T)
        
        simModel <- glm(C ~ V + RU,
                        data = simdat,
                        family = binomial(link = "probit"))
        
        
        simParams$V[simParams$ID == i] <- simModel$coefficients[2]
        simParams$RU[simParams$ID == i] <- simModel$coefficients[3]
        simParams$converged[simParams$ID == i] <- simModel$converged
      }
    

      
    }
    
    
    # get correlations
    cors <- data.frame(true = rep(column_names[-c(1)], length(column_names)-1),
                       recovered =  rep(column_names[-c(1)], each = length(column_names)-1),
                       cor = NA)
    
    
    
    cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(bootstrappedParams[simParams$converged,grep(cors$true[x], colnames(bootstrappedParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                                 simParams[simParams$converged, grep(cors$recovered[x], colnames(simParams))[1]]))
    
    
    recoveredParams <- simParams
  } else if (task == "sam"){
    
    
    
    
    
  } else {warning("this model is not implemented yet.")}
  
  # plotting and packaging it all for the return
  
  p <- ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
    geom_text(aes(label = round(cor, digits = 2))) 
  
  
  
  return(list(bootstrappedParams, recoveredParams, p))
  
  
}








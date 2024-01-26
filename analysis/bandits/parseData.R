########### analysing bandit data #######

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")


# select required directory by setting data_idx index
data_idx <- 4
rel_dir_data_bandits <- c("data/pilot/bandits/", "data/2023-11-lab-pilot/bandits/", "data/pilot4arb/", "data/wave1/bandits/")[data_idx]
rel_dir_data_qs <- c("data/pilot/qs/", "data/2023-11-lab-pilot/qs/", "_", "data/wave1/qs/")[data_idx]

json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}
### load data ########

session = 1

nBlocksH = 80
nTrialsH = 10

nBlocksS = 30
nTrialsS = 10

nBlocksR = 1
nTrialsR = 200

files_all = list.files(path = rel_dir_data_bandits)
files <- files_all[!grepl("temp", files_all)]


### get IDs of participants that have questionnaire data (and thus seem to have completed the study)
qfiles = list.files(path = rel_dir_data_qs)
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
  temp <- fromJSON(paste(rel_dir_data_bandits, files[file_ind], sep = ""))
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

#write.csv(lookup, file = "/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/BanditLookup.csv")
write.csv(lookup, file = str_c(str_remove(rel_dir_data_bandits, "[a-z]*/$"),  "BanditLookup.csv"))


save(comprehension, file = paste(rel_dir_data_bandits, "comprehension.Rda", sep = ""))
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

save(horizon, sam, restless, file = str_c(str_remove(rel_dir_data_bandits, "[a-z]*/$"),  "bandits.Rda"))


#################### questionnaires ###############



files = list.files(path = rel_dir_data_qs)

for (i in 1:nrow(lookup)){

  pid <- lookup$PID[i]
  # check if we have the final data for that participant
  if (mean(grepl(pid, files))>0){ID = i} 
  else { # if not we need to look through temp, easiest to do that manually
    print(pid)
    next
  }
  temp <- fromJSON(paste(rel_dir_data_qs, files[grep(pid, files)], sep = ""))
  
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

## save it

save(qdat, file = str_c(str_remove(rel_dir_data_qs, "[a-z]*/$"),  "qs.Rda"))

########## make table of exclusions #############

##### WM

#load what mirko did here
wm <- readRDS("data/wave1/subjects-excl-wm.rds")
wm <- subset(wm, is.element(prolific_pid, lookup$PID))
lookup$perfWM <- NA
lookup$perfWM[match(wm$prolific_pid, lookup$PID)] <- wm$excl_subject

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

overall <- ddply(horizon[horizon$trial > 4, ], ~ID, summarise, optimal = meann(chooseBest))
table(overall$optimal <= pchance) # 6 excluded

# get rid of person w/o data
overall <- subset(overall, !is.na(optimal))

ggplot(overall, aes(optimal)) + geom_histogram(alpha = 0.5) + geom_vline(aes(xintercept = pchance))+
  ggtitle("proportion of optimal choices by subject", subtitle = "vertical line indicates 95 percentile of chance performance")

lookup$perfHorizon <- NA
lookup$perfHorizon[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <=pchance, 1, 0)

#### Sam's task performance

sam$optimal <- ifelse(sam$reward1 > sam$reward2, 0, 1)
sam$chooseBest <- ifelse(sam$chosen == sam$optimal, 1, 0)

overall <- ddply(sam, ~ID, summarise, optimal = meann(chooseBest))

# get rid of person w/o data
overall <- subset(overall, !is.na(optimal))

table(overall$optimal <= pchance)

ggplot(overall, aes(optimal)) + geom_histogram(alpha = 0.5) + geom_vline(aes(xintercept = pchance))+
  ggtitle("proportion of optimal choices by subject", subtitle = "vertical line indicates 95 percentile of chance performance")

lookup$perfSam <- NA
lookup$perfSam[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <= pchance, 1, 0)


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

overall <- ddply(data, ~ID, summarise, optimal = meann(chooseBest))

overall <- subset(overall, !is.na(optimal))

ggplot(overall, aes(optimal)) + geom_histogram(alpha = 0.5) + geom_vline(aes(xintercept = pchance))+
  ggtitle("proportion of optimal choices by subject", subtitle = "vertical line indicates 95 percentile of chance performance")

lookup$perfRestless[match(overall$ID, lookup$ID)] <- ifelse(overall$optimal <= pchance, 1, 0)

#### comprehension attempts horizon

mean_sd <- ddply(comprehension, ~task, summarise, meanComp = mean(compAttempts, na.rm =T), SD = sd(compAttempts, na.rm = T))

mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp

comprehension$excl <- ifelse(comprehension$compAttempts > mean_sd$SD[match(comprehension$task, mean_sd$task)], 1, 0)

table(comprehension$excl)
lookup$compHorizon[match(comprehension$ID[comprehension$task == "horizon"], lookup$ID)] <- comprehension$excl[comprehension$task == "horizon"]

#### comprehension attempts sam

lookup$compSam[match(comprehension$ID[comprehension$task == "sam"], lookup$ID)] <- comprehension$excl[comprehension$task == "sam"]


#### comprehension attempts 4arb

lookup$compRestless[match(comprehension$ID[comprehension$task == "restless"], lookup$ID)] <- comprehension$excl[comprehension$task == "restless"]

#### attention checks

lookup$attention <- NA
lookup$attention[match(qdat$ID, lookup$ID)] <- ifelse(qdat$attention1 < 2, 1, 0)

table(lookup$attention)

##### total

lookup$totalExclude <- apply(as.array(1:nrow(lookup)), 1, function(x) sum(as.numeric(unlist(lookup[x, -c(1:2)])), na.rm = T))

hist(lookup$totalExclude, breaks = max(lookup$totalExclude))

lookup$exclude <- ifelse(lookup$totalExclude == 0, 0 , 1)
table(lookup$exclude)/nrow(lookup)

write.csv(lookup, "data/wave1/exclusions.csv")

########### analysing bandit data #######

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")

### load data ########

session = 1

nBlocksH = 80
nTrialsH = 10

nBlocksS = 30
nTrialsS = 10

nBlocksR = 1
nTrialsR = 200

files = list.files(path = "data/pilot/bandits")
files <- files[!grepl("temp", files)]

lookup <- data.frame(PID = rep(NA, length(files)),
                     ID = 1:length(files))

bonus <- data.frame(ID = rep(NA, length(files)),
                    TotalBonus = NA,
                    Horizon = NA,
                    Sam = NA,
                    Restless = NA)

horizon <- data.frame(ID = rep(1:length(files), each = nBlocksH*nTrialsH),
                      block = rep(rep(1:nBlocksH, each = nTrialsH), length(files)),
                      trial = rep(1:nTrialsH, nBlocksH*length(files)),
                      chosen = NA,
                      reward = NA,
                      rt = NA, 
                      session = session)


sam <- data.frame(ID = rep(1:length(files), each = nBlocksS*nTrialsS),
                      block = rep(rep(1:nBlocksS, each = nTrialsS), length(files)),
                      trial = rep(1:nTrialsS, nBlocksS*length(files)),
                      chosen = NA,
                      reward = NA,
                      rt = NA, 
                      session = session)


restless <- data.frame(ID = rep(1:length(files), each = nTrialsR),
                      trial = 1:nTrialsR,
                      chosen = NA,
                      reward = NA,
                      rt = NA, 
                      session = session)

for (i in 1:length(files)){
  temp <- fromJSON(paste("data/pilot/bandits/",files[i], sep = ""))
  
  # add PID into lookup
  lookup$PID[i] <- temp$subjectID
  
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
    

  
}

# info condition should be coded as -1 0 1 but is now coded as -2 0 2 so fix that
horizon$info <- horizon$info/2


### save lookup

write.csv(lookup, file = "/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/BanditLookup.csv")


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

Srewards$max <- ifelse(Srewards$rew1 > Srewards$rew2, Srewards$rew1, Srewards$rew2)

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


save(maxHorizon, maxSam, maxRestless, file = "task/maxRewards.Rda")


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
  
save(horizon, sam, restless, file = "data/pilot/bandits.Rda")



#################### questionnaires ###############

files = list.files(path = "data/pilot/qs")

for (i in files){
  if (i == files[1]){# if this is the first one
    qdat <- fromJSON(paste("data/pilot/qs/", i, sep =""))
    pid <- substr(i, 1, gregexpr("_", i)[[1]][1]-1)# cut filename after prolific ID which ends with _ but there are other _ in the filename
    qdat$ID <- lookup$ID[lookup$PID == pid]
  } else {
    temp <- fromJSON(paste("data/pilot/qs/", i, sep =""))
    pid <- substr(i, 1, gregexpr("_", i)[[1]][1]-1)# cut filename after prolific ID which ends with _ but there are other _ in the filename
    temp$ID <- lookup$ID[lookup$PID == pid]
    qdat <- rbind(qdat, temp)
  }
}


## save it

save(qdat, file = "data/pilot/qs.Rda")



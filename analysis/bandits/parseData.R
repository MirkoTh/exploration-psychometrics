########### analysing bandit data #######

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")


# select required directory by setting data_idx index
data_idx <- 3
rel_dir_data_bandits <- c("data/pilot/bandits/", "data/2023-11-lab-pilot/bandits/", "data/pilot4arb/")[data_idx]
rel_dir_data_qs <- c("data/pilot/qs/", "data/2023-11-lab-pilot/qs/")[data_idx]

### load data ########

session = 1

nBlocksH = 80
nTrialsH = 10

nBlocksS = 30
nTrialsS = 10

nBlocksR = 2
nTrialsR = 200

files = list.files(path = rel_dir_data_bandits)
files <- files[!grepl("temp", files)]

lookup <- data.frame(PID = rep(NA, length(files)),
                     ID = 1:length(files))

bonus <- data.frame(ID = rep(NA, length(files)),
                    TotalBonus = NA,
                    Horizon = NA,
                    Sam = NA,
                    Restless = NA)


restless <- data.frame(ID = rep(1:length(files), each = nTrialsR*nBlocksR),
                       block = rep(rep(1:nBlocksR, each = nTrialsR), length(files)),
                       trial = 1:nTrialsR,
                       chosen = NA,
                       reward = NA,
                       rt = NA, 
                       session = session)

for (i in 1:length(files)){
  temp <- fromJSON(paste(rel_dir_data_bandits,files[i], sep = ""))
  
  # add PID into lookup
  lookup$PID[i] <- temp$subjectID

  ## restless
  for (block in 1:nBlocksR){
    for (trial in 1:nTrialsR){
      restless$chosen[restless$ID == i & restless$trial == trial & restless$block == block] <- temp$restless$choice[[block+1]][[trial]] # +1 bc of practice
      restless$reward[restless$ID == i & restless$trial == trial& restless$block == block] <- temp$restless$reward[[block+1]][[trial]]
      restless$rt[restless$ID == i & restless$trial == trial& restless$block == block] <- temp$restless$time[[block+1]][[trial]]
    }
  }
  bonus$ID[i] <- temp$subjectID
  bonus$Restless[i] <- sum(restless$reward[restless$ID == i])

}


### save lookup

#write.csv(lookup, file = "/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/BanditLookup.csv")
write.csv(lookup, file = str_c(str_remove(rel_dir_data_bandits, "[a-z]*/$"),  "BanditLookup.csv"))


############### calculate max rewards #########


####### Restless


rewardsR <- fromJSON(paste("pilot4arb/rewards4ARB", session, ".json", sep = ""))
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

bonus$Restless <- bonus$Restless /maxRestless



save(maxRestless, file = "pilot4arb/maxRewards.Rda")


################# get ground truth variables ###############

### restless
restless$reward1 <- NA
restless$reward2 <- NA
restless$reward3 <- NA
restless$reward4 <- NA
rewardsR <- fromJSON(paste("pilot4arb/rewards4ARB", session, ".json", sep = ""))



temp <- data.frame(rewardsR[[2]])
temp <- rbind(temp, data.frame(rewardsR[[3]]))
restless$reward1 <- rep(temp$X1, length(unique(restless$ID)))
restless$reward2 <- rep(temp$X2, length(unique(restless$ID)))
restless$reward3 <- rep(temp$X3, length(unique(restless$ID)))
restless$reward4 <- rep(temp$X4, length(unique(restless$ID)))

save(restless, file = str_c(str_remove(rel_dir_data_bandits, "[a-z]*/$"),  "bandits.Rda"))


#################### questionnaires ###############



files = list.files(path = rel_dir_data_qs)

for (i in files){
  if (i == files[1]){# if this is the first one
    qdat <- fromJSON(paste(rel_dir_data_qs, i, sep =""))
    pid <- substr(i, 1, gregexpr("_", i)[[1]][1]-1)# cut filename after prolific ID which ends with _ but there are other _ in the filename
    qdat$ID <- lookup$ID[lookup$PID == pid]
  } else {
    temp <- fromJSON(paste(rel_dir_data_qs, i, sep =""))
    pid <- substr(i, 1, gregexpr("_", i)[[1]][1]-1)# cut filename after prolific ID which ends with _ but there are other _ in the filename
    temp$ID <- lookup$ID[lookup$PID == pid]
    qdat <- rbind(qdat, temp)
  }
}


## save it

save(qdat, file = str_c(str_remove(rel_dir_data_qs, "[a-z]*/$"),  "qs.Rda"))



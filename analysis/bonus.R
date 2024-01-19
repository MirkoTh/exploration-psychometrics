################ get all boni for the exploration study ###################

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")
session = 1

load(paste("task/maxRewards", session, ".Rda", sep = ""))

#######################################

json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}



files = list.files(path = "data/wave1/bandits")
files <- files[!grepl("temp", files)]


bonus <- data.frame(ID = rep(NA, length(files)),
                    TotalBonus = NA,
                    Horizon = NA,
                    Sam = NA,
                    Restless = NA,
                    OS = NA,
                    WMU = NA,
                    SS = NA,
                    attention = NA)


############# get proportion correct for all tasks #######


for (i in 1:length(files)){
  
  ### bandits
  temp <- fromJSON(paste("data/wave1/bandits/",files[i], sep = ""))
  bonus$ID[i] <- temp$subjectID
  horizonPoints <- temp$horizon$taskReward/maxHorizon
  bonus$Horizon[i] <- horizonPoints
  samPoints <- temp$sam$taskReward/maxSam
  bonus$Sam[i] <- samPoints
  restlessPoints <- temp$restless$taskReward/maxRestless
  bonus$Restless[i] <- restlessPoints
  
  ### OS
  
  OS <- json_to_tibble(paste("data/wave1/OS/OS_recall_", bonus$ID[i], ".json", sep = ""))
  maxOS <- mean(OS$set_size[OS$is_practice == 0])
  bonus$OS[i] <- mean(OS$n_correct[OS$is_practice == 0])/maxOS
  
  ### WMU
  
  WMU <- json_to_tibble(paste("data/wave1/WMU/WMU_", bonus$ID[i], ".json", sep = ""))
  maxWMU <- 4 # set size is always 4 in our study
  bonus$WMU[i] <- mean(WMU$n_correct[WMU$is_practice == 0])/maxWMU
  
  ## SS
  SS <- json_to_tibble(paste("data/wave1/SS/SS_recall_", bonus$ID[i], ".json", sep = ""))
  maxSS <- mean(SS$set_size[SS$is_practice == 0])
  bonus$SS[i] <- mean(SS$n_correct[SS$is_practice == 0])/maxSS
  
}


######### attention ##########

files = list.files(path = "data/wave1/qs")

for (i in 1:length(files)){
  temp <- fromJSON(paste("data/wave1/qs/",files[i], sep = ""))
  bonus$attention[i] <- temp$attention1
  
}

########## total bonus ########

bonus$TotalBonus <- round(rowMeans(bonus[ ,3:8])*6, digits = 2)
bonus$TotalBonus[bonus$attention < 1] <- 0



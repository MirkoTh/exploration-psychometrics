################ get all boni for the exploration study ###################

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")
session = 1
s = session-1
load(paste("task/maxRewards", session, "_old220124.Rda", sep = ""))

dir = "data/wave1/failedFirstBatch/"

#######################################

json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}



files = list.files(path = paste(dir, "qs/", sep = ""))
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
  bonus$ID[i] <- substr(files[i], 1, gregexpr("_", files[i])[[1]][1]-1)
  ### bandits
  temp <- try(fromJSON(paste(dir, "bandits/",bonus$ID[i],"_data_task_bonus_undefined_session_", s,".txt", sep = "")))
  if(is.element("try-error", class(temp))){print(bonus$ID[i])}
  else{
    horizonPoints <- temp$horizon$taskReward/maxHorizon
    bonus$Horizon[i] <- horizonPoints
    samPoints <- temp$sam$taskReward/maxSam
    bonus$Sam[i] <- samPoints
    restlessPoints <- temp$restless$taskReward/maxRestless
    bonus$Restless[i] <- restlessPoints
  }

  
  ### OS
  
  OS <- try(json_to_tibble(paste(dir, "OS/OS_recall_",s,"_", bonus$ID[i], ".json", sep = "")))
  if(is.element("try-error", class(OS))){print(bonus$ID[i])}
  else {
    maxOS <- mean(OS$set_size[OS$is_practice == 0])
    bonus$OS[i] <- mean(OS$n_correct[OS$is_practice == 0])/maxOS
  }

  
  ### WMU
  
  WMU <- try(json_to_tibble(paste(dir, "WMU/WMU_",s,"_", bonus$ID[i], ".json", sep = "")))
  if(is.element("try-error", class(WMU))){print(bonus$ID[i])}
  else {
  maxWMU <- 5 # set size is always 4 in our study
  bonus$WMU[i] <- mean(WMU$n_correct[WMU$is_practice == 0])/maxWMU
  }
  ## SS
  SS <- try(json_to_tibble(paste(dir, "SS/SS_recall_",s,"_", bonus$ID[i], ".json", sep = "")))
  if(is.element("try-error", class(SS))){print(bonus$ID[i]); next}
  maxSS <- mean(SS$set_size[SS$is_practice == 0])
  bonus$SS[i] <- mean(SS$n_correct[SS$is_practice == 0])/maxSS
  
}


######### attention ##########

files = list.files(path = paste(dir, "qs/", sep = ""))

for (i in 1:length(files)){
  temp <- fromJSON(paste(dir, "qs/",files[i], sep = ""))
  bonus$attention[i] <- temp$attention1
  
}

########## total bonus ########

bonus$TotalBonus <- round(rowMeans(bonus[ ,3:8], na.rm = T), digits = 2)
min(bonus$TotalBonus)
max(bonus$TotalBonus)
bonus$TotalBonus <-(bonus$TotalBonus - min(bonus$TotalBonus)) / (max(bonus$TotalBonus) - min(bonus$TotalBonus)) * max(bonus$TotalBonus) 
# min-max normalisation but then rescaled by max value so that best person does not get max bonus but insteady their proportion
bonus$TotalBonus <- bonus$TotalBonus *6
bonus$TotalBonus[bonus$attention < 1] <- 0

hist(bonus$TotalBonus, breaks = 100)

write.csv(bonus, file = paste(dir, "bonus.csv", sep = ""))

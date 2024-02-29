################ get all boni for the exploration study ###################

library(tidyverse)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")
session = 1
s = session-1
load(paste("task/maxRewards", session,".Rda", sep = ""))

dir = "data/wave1/"

load("data/wave1/bandits.Rda")

#######################################

json_to_tibble <- function(path_file) {
  js_txt <- read_file(path_file)
  js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
  js_txt <- str_replace(js_txt, ",\n]", "]")
  my_tbl <- jsonlite::fromJSON(js_txt) %>% as_tibble()
  return(my_tbl)
}



files_all = list.files(path = paste(dir, "bandits/", sep = ""))
files <- files_all[!grepl("temp", files_all)]


### get IDs of participants that have questionnaire data (and thus seem to have completed the study)
qfiles = list.files(path = paste(dir, "qs/", sep = ""))
qPIDs = apply(as.array(qfiles), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))

### get IDs of participants that completed bandits bc some of them might not have questionnaire data for some reason
bPIDs = apply(as.array(files), 1, function(x) substr(x, 1, gregexpr("_", x)[[1]][1]-1))

# merge both lists
PIDs = unique(c(qPIDs, bPIDs))

bonus <- data.frame(ID = 1:length(PIDs),
                    PID = PIDs,
                    TotalBonus = NA,
                    Horizon = NA,
                    Sam = NA,
                    Restless = NA,
                    OS = NA,
                    WMU = NA,
                    SS = NA,
                    attention = NA)


############# get proportion correct for all tasks #######


for (i in 1:length(PIDs)){
  
  if (i %% 20 == 0){print(paste(i, "of", length(PIDs)))}
  
  ### OS
  
  OS <- try(json_to_tibble(paste(dir, "OS/OS_recall_",s,"_", PIDs[i], ".json", sep = "")))
  if(is.element("try-error", class(OS))){print(PIDs[i])}
  else {
    maxOS <- mean(OS$set_size[OS$is_practice == 0])
    bonus$OS[bonus$PID == PIDs[i]] <- mean(OS$n_correct[OS$is_practice == 0])/maxOS
  }

  
  ### WMU
  
  WMU <- try(json_to_tibble(paste(dir, "WMU/WMU_",s,"_", PIDs[i], ".json", sep = "")))
  if(is.element("try-error", class(WMU))){print(PIDs[i])}
  else {
  maxWMU <- 5 # set size is always 4 in our study
  bonus$WMU[bonus$PID == PIDs[i]] <- mean(WMU$n_correct[WMU$is_practice == 0])/maxWMU
  }
  
  ## SS
  SS <- try(json_to_tibble(paste(dir, "SS/SS_recall_",s,"_", PIDs[i], ".json", sep = "")))
  if(is.element("try-error", class(SS))){print(PIDs[i]); next}
  maxSS <- mean(SS$set_size[SS$is_practice == 0])
  bonus$SS[bonus$PID == PIDs[i]] <- mean(SS$n_correct[SS$is_practice == 0])/maxSS
  
}


######### attention ##########

files = list.files(path = paste(dir, "qs/", sep = ""))

for (i in 1:length(files)){
  temp <- fromJSON(paste(dir, "qs/",files[i], sep = ""))
  ID <- substr(files[i], 1, gregexpr("_", files[i])[[1]][1]-1)
  bonus$attention[bonus$PID == ID] <- temp$attention1
  
}

###### bandits #########

horizonrew <- ddply(horizon, ~ID,summarise, reward = sum(reward, na.rm = T))

horizonrew$reward[horizonrew$reward == 0] <- NA# somehow NA turned into 0 so fix that
samrew <- ddply(sam, ~ID, summarise, reward = sum(reward, na.rm = T))
samrew$reward[samrew$reward == 0] <- NA
restlessrew <- ddply(restless, ~ID, summarise, reward = sum(reward, na.rm = T))
restlessrew$reward[restlessrew$reward == 0] <- NA
bonus$Horizon[match(horizonrew$ID, bonus$ID)] <- horizonrew$reward/maxHorizon
bonus$Sam[match(samrew$ID, bonus$ID)] <- samrew$reward/maxSam
bonus$Restless[match(restlessrew$ID, bonus$ID)] <- restlessrew$reward/maxRestless

########## total bonus ########

bonus$TotalBonus <- round(rowMeans(bonus[ ,4:9], na.rm = T), digits = 2)
min(bonus$TotalBonus)
max(bonus$TotalBonus)
bonus$TotalBonus <-(bonus$TotalBonus - min(bonus$TotalBonus)) / (max(bonus$TotalBonus) - min(bonus$TotalBonus)) * max(bonus$TotalBonus) 
# min-max normalisation but then rescaled by max value so that best person does not get max bonus but instead their proportion
bonus$TotalBonus <- bonus$TotalBonus *6
min(bonus$TotalBonus)
max(bonus$TotalBonus)
bonus$TotalBonus[bonus$attention < 1] <- 0

hist(bonus$TotalBonus, breaks = 100)

############## add exclusions #############

exclusions <- read.csv("data/wave1/exclusions.csv", stringsAsFactors = F, quote = "") # the file got messed up when saving but redoing takes forever

# remove excess quotation marks on the variables where it matters
exclusions$X..exclude... <- as.numeric(substr(exclusions$X..exclude..., start = 1, stop = 1))
exclusions$X..PID.. <- substr(exclusions$X..PID.., start = 3, stop = nchar(exclusions$X..PID..)-2)

bonus$exclude[match(exclusions$X..PID.., bonus$PID)] <- exclusions$X..exclude...

write.csv(bonus, paste(dir, "bonus.csv", sep = ""))

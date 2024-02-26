############# relating bandit task behaviour to questionnaire scores ##############


library(tidyverse)
library(ggplot2)
library(jsonlite)
library(brms)
library(ggridges)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/bandits/banditsWave1.Rda")
source("analysis/recovery_utils.R")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


########## prep questionnaire data #######

load("analysis/qsWave1.Rda")

qs <- read.csv("task/questionnaires.csv", sep = ";")

qs$Qnum <- NA
for (i in unique(qs$Measure)){
  
  qs$Qnum[qs$Measure == i] <- 0:(nrow(qs[qs$Measure == i, ])-1)
  
}

qs$Q <- paste(qs$Measure, qs$Qnum, sep = "_")



responses <- pivot_longer(qdat, cols = c(10:71), names_to = "Q", values_to = "response")

responses$attention_check <- qs$Attention.check[match(responses$Q, qs$Q)]
responses$reversed <- qs$Reverse.coded[match(responses$Q, qs$Q)]

responses <- subset(responses, attention_check == 0)

# distinguish panas positive and negative
PANASpos <- c(1,3,5,9,10,12,14,16,17,19)
PANASneg <- c(2,4,6,7,8,11,13,15,18,20)

qs$Measure[qs$Measure == "PANAS" & is.element((qs$Qnum+1), PANASpos)] <- "PANASpos"
qs$Measure[qs$Measure == "PANAS" & is.element((qs$Qnum+1), PANASneg)] <- "PANASneg"

responses$measure <- qs$Measure[match(responses$Q, qs$Q)]

library(plyr)

maxval <- ddply(responses, ~measure, summarise, max = max(response) )

responses$max <- maxval$max[match(responses$measure, maxval$measure)]
responses$response <- ifelse(responses$reversed == 1, as.numeric(responses$max) - as.numeric(responses$response), as.numeric(responses$response))

avg <- ddply(responses, ~ID+measure, summarise, score = mean(response))

########### prep bandit behaviour - proportion of high variance arm choices ##########

######## Horizon task
horizon$highVar <- ifelse(horizon$bayVarL > horizon$bayVarR, 0, 1)
horizon$chooseVar <- ifelse(horizon$chosen == horizon$highVar, 1, 0)

bestH <- ddply(horizon[horizon$trial > 4, ], ~ID, summarise, highVar = mean(chooseVar, na.rm = T))

##### plot this behaviour over trials (code recycled from performance plots)

best <- ddply(horizon[horizon$trial > 4, ], ~ID+Horizon+trial, summarise, optimal = mean(chooseVar, na.rm = T))
head(best)

# dist plot

ggplot(best, aes(x = optimal, fill = as.factor(Horizon)))+ geom_density( alpha = 0.6) +
  #geom_jitter(aes(y = as.factor(Horizon)), alpha = 0.5, height = 0.1)+
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5))+
  labs(title = "Proportion of exploratory choices on the Horizon task over trials",
       x = "Proportion of high variance arm choices")+
  scale_fill_discrete(name = "Horizon", labels = c(5, 10))

########### sam's task

sam$highVar <- ifelse(sam$KLV0 > sam$KLV1, 0, 1)
sam$chooseVar <- ifelse(sam$chosen == sam$highVar, 1, 0)

bestS <- ddply(sam, ~ID, summarise, highVar = meann(chooseVar))

best <- ddply(sam, ~ID+trial+cond, summarise, optimal = meann(chooseVar))
head(best)

# dist plot

ggplot(best, aes(x = optimal))+ geom_density_ridges(aes(y = as.factor(trial)), alpha = 0.6) +
  facet_wrap(vars(cond))+
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5))+
  labs(title = "Proportion of exploratory choices on Sam's task over trials",
       x = "Proportion of high variance arm choices")




#### restless bandit

data <- restless
data$optimalR <- rep(apply(as.array(paste(data$trial[data$ID == 1], data$block[data$ID == 1])), 1, function(x) max(c(data$reward1[paste(data$trial, data$block) == x],
                                                                                                                     data$reward2[paste(data$trial, data$block) == x],
                                                                                                                     data$reward3[paste(data$trial, data$block) == x],
                                                                                                                     data$reward4[paste(data$trial, data$block) == x]))), 
                     length(unique(data$ID)))

data$regret <- data$optimalR - data$reward


data$chooseBest <- ifelse(data$reward == data$optimalR, 1, 0)

best <- ddply(data, ~ID, summarise, optimal = meann(chooseBest))
head(best)

ggplot(best, aes(optimal)) + geom_density(alpha = 0.6, fill = "grey") + xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.25))+
  labs(title = "Proportion of best arm choices on restless 4 armed bandit task over trials",
       x = "Proportion of best arm choices")

## add mean and variance from the Kalman Filter to the data frame of restless
data <- restless

data$KLM0 <- NA
data$KLM1 <- NA
data$KLM2 <- NA
data$KLM3 <- NA
data$KLV0 <- NA
data$KLV1 <- NA
data$KLV2 <- NA
data$KLV3 <- NA


ind = 0
## get output of Kalman Filter into dataframe
for (i in unique(data$ID)){ 
  # progress bar type thing
  ind = ind+1
  if (ind %% 10 == 0){print(sprintf("adding output of KF to subject %i of %i", unique(data$ID[data$ID == i]), data$ID[length(data$ID)]))} 
  
  dat <- subset(data, data$ID == i)
  
  xi <- rep(7.84, 4)
  epsilon <- 16
  
  
  
  posterior <- kalman_learning(dat, 4, xi, epsilon)
  
  data$KLM0[data$ID == i] <- posterior$m_1[1:200]
  data$KLM1[data$ID == i] <- posterior$m_2[1:200]
  data$KLM2[data$ID == i] <- posterior$m_3[1:200]
  data$KLM3[data$ID == i] <- posterior$m_4[1:200]
  data$KLV0[data$ID == i] <- posterior$v_1[1:200]
  data$KLV1[data$ID == i] <- posterior$v_2[1:200]
  data$KLV2[data$ID == i] <- posterior$v_3[1:200]
  data$KLV3[data$ID == i] <- posterior$v_4[1:200]
  
  
}


data$highVar <- apply(as.array(1:nrow(data)), 1, function(x) max(c(data$KLV0[ x],
                                                                                  data$KLV1[x],
                                                                                   data$KLV2[x],
                                                                                  data$KLV3[x])))

data$high <- apply(as.array(1:nrow(data)), 1, function(x) which(data[x,grep("KLV", colnames(data))] == data$highVar[x]))


data$chooseVar <- apply(as.array(1:nrow(data)), 1, function(x) ifelse(is.element((data$chosen[x]+1), unlist(data$high[x])), 1, 0))



bestR <- ddply(data, ~ID, summarise, highVar = meann(chooseVar))

ggplot(bestR, aes(highVar)) + geom_density(alpha = 0.6, fill = "grey")+ geom_vline(aes(xintercept = 0.25)) + labs(title = "Proportion of exploratory choices on the restless bandit", x = "proportion of high variance arm choices")

bestH$task <- "Horizon"
bestS$task <- "Sam"
bestR$task <- "restless"

bandits <- rbind(bestH, bestS, bestR)



########## get to comparing #########

questionnaires <- unique(qs$Measure)
tasks <- unique(bandits$task)

### compare tasks to each other

cors <- data.frame(x = rep(tasks, length(tasks)),
                   y = rep(tasks, each = length(tasks)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(bandits$highVar[bandits$task == cors$x[x]],
                                                             bandits$highVar[bandits$task == cors$y[x]]))




# plot them

ggplot(cors, aes(x = x, y = y, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of proportion of high variance choices between different tasks",
                                                        x = element_blank(), y = element_blank())



### correlate task with questionnaires
# some subjects don't have data in both so get intersection

IDs <- intersect(unique(avg$ID), unique(bandits$ID))

cors <- data.frame(task = rep(tasks, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(tasks)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(bandits$highVar[bandits$task == cors$task[x] & is.element(bandits$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = task, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of proportion of high variance choices between different tasks",
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



########## correlate with working memory performance 

import <- readRDS("analysis/4arlb-overview.rds")

wm <- pivot_longer(import, cols = c(10:12), names_to = "task", values_to = "score")

wm_tasks <- unique(wm$task)


IDs <- intersect(unique(wm$ID), unique(bandits$ID))

cors <- data.frame(task = rep(tasks, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(tasks)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(bandits$highVar[bandits$task == cors$task[x] & is.element(bandits$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = task, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of proportion of high variance choices and WM performance",
                                                        x = element_blank(), y = element_blank())




###################### relationship with bandit task parameters #############

####### horizon

load("analysis/bandits/recovHorizonFull.Rda")

trueParams <- res_list[[1]]
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodelUCB)))`

params <- unique(trueParams$predictor)

ids <- 1:334
ids <- ids[-c(168)]

trueParams$ID <- ids

IDs <- intersect(unique(avg$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of proportion of high variance choices between different tasks",
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



##### wm

IDs <- intersect(unique(wm$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())



######### sams task


load("analysis/bandits/recovSam.Rda")

trueParams <- res_list1[[1]]
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(trueModel)))`

params <- unique(trueParams$predictor)

ids <- 1:334
ids <- ids[-c(168)]

trueParams$ID <- ids

IDs <- intersect(unique(avg$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters with questionnaire scores",
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



##### wm

IDs <- intersect(unique(wm$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())



############### restless bandit

restlessParams <- readRDS("analysis/4arlb-overview.rds")

trueParams <- pivot_longer(restlessParams, cols = (5:6), names_to = "predictor", values_to = "estimate")


params <- unique(trueParams$predictor)

trueParams <- subset(trueParams, !is.na(estimate))


IDs <- intersect(unique(avg$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters with questionnaire scores",
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



##### wm

IDs <- intersect(unique(wm$ID), unique(trueParams$ID))

cors <- data.frame(parameter = rep(params, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$estimate[trueParams$predictor == cors$parameter[x] & is.element(trueParams$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())



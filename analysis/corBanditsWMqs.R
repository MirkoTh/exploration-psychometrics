############# relating bandit task behaviour to questionnaire scores ##############
rm(list = ls())

library(plyr)
library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(ggridges)
theme_set(theme_classic(base_size = 14))
library(here)

session <- 1

load(sprintf("analysis/bandits/banditsWave%i.Rda", session))
source("analysis/recovery_utils.R")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

### remove the person that has no data

horizon <- subset(horizon,!is.na(info))
sam <- subset(sam, !is.na(chosen))
restless <- subset(restless, !is.na(chosen))


## make sure all bandit datasets are sorted with assending IDs so that there are no downstream issues in the matching of IDs

horizon <- horizon[order(horizon$ID), ]
sam <- sam[order(sam$ID), ]
restless <- restless[order(restless$ID), ]

########## prep questionnaire data #######

load(sprintf("analysis/qswave%i.Rda", session))

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

# split sticsa cognitive and somatic

qs$Measure[qs$Measure == "STICSA" & qs$sticsaSubscale == "s"] <- "STICSAsoma"
qs$Measure[qs$Measure == "STICSA" & qs$sticsaSubscale == "c"] <- "STICSAcog"


responses$measure <- qs$Measure[match(responses$Q, qs$Q)]

library(plyr)

maxval <- ddply(responses, ~measure, summarise, max = max(response) )

responses$max <- maxval$max[match(responses$measure, maxval$measure)]
responses$response <- ifelse(responses$reversed == 1, as.numeric(responses$max) - as.numeric(responses$response), as.numeric(responses$response))

dat_for_efa <- responses %>% 
  select(ID, Q, response) %>% 
  pivot_wider(id_cols = ID, names_from = Q, values_from = response)

write.csv(dat_for_efa, sprintf("analysis/dat_for_efa_s%i.csv", session))

descr_for_efa <- responses %>% 
  select(ID, Q, measure)

write.csv(descr_for_efa, "analysis/descr_for_efa.csv")

avg <- ddply(responses, ~ID+measure, summarise, score = mean(response))

avg <- avg[order(avg$ID), ]


save(avg, file = sprintf("analysis/qdat_session%i.Rda", session))


############# prep regret ########

best <- function(df){
  
  rewards <- subset(df, select = grepl("reward", colnames(df)))
  rewards <- subset(rewards, select = -reward)
  
  best <- apply(as.array(1:nrow(rewards)), 1, function(x) max(rewards[x,]))
  
  return(best)
  
} 


horizon$best <- best(horizon)
sam$best <- best(sam)
restless$best <- best(restless)

horizon$regret <- horizon$best - horizon$reward
sam$regret <- sam$best - sam$reward
restless$regret <- restless$best - restless$reward

Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID,summarise, regret = meann(regret))
Sperf <- ddply(sam, ~ID, summarise, regret = meann(regret))
Rperf <- ddply(restless, ~ID, summarise, regret = meann(regret))

Hperf$model <- "horizon"
Sperf$model <- "2AB"
Rperf$model <- "restless"

regret <- rbind(Hperf, Sperf, Rperf)

###### prep Poptimal ######

horizon$optimal <- ifelse(horizon$regret == 0, 1, 0)
Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID, summarise, Poptimal = meann(optimal))

sam$optimal <- ifelse(sam$regret == 0, 1, 0)
Sperf <- ddply(sam, ~ID, summarise, Poptimal = meann(optimal))

restless$optimal <- ifelse(restless$regret == 0, 1, 0)
Rperf <- ddply(restless, ~ID, summarise, Poptimal = meann(optimal))

Poptimal <- list("horizon" = Hperf, "2AB" = Sperf, "restless" = Rperf) %>% 
  bind_rows(.id = "model")

######### prep P(switch) ##########


horizon$prev <- c(NA, horizon$chosen[1:nrow(horizon)-1])
horizon$switch <- ifelse(horizon$chosen == horizon$prev, 0, 1)
horizon$switch[horizon$trial == 1] <- NA

sam$prev <- c(NA, sam$chosen[1:nrow(sam)-1])
sam$switch <- ifelse(sam$chosen == sam$prev, 0, 1)
sam$switch[sam$trial == 1] <- NA

restless$prev <- c(NA, restless$chosen[1:nrow(restless)-1])
restless$switch <- ifelse(restless$chosen == restless$prev, 0, 1)
restless$switch[restless$trial == 1] <- NA

Hswitch <- ddply(horizon[horizon$trial > 4, ],~ID,summarise, Pswitch = meann(switch))
Sswitch <- ddply(sam, ~ID,summarise, Pswitch = meann(switch))
Rswitch <- ddply(restless, ~ID, summarise, Pswitch = meann(switch))

switch <- list("horizon" = Hswitch, "2AB" = Sswitch, "restless" = Rswitch) %>% 
  bind_rows(.id = "model")


save(Poptimal, switch, regret, file = sprintf("analysis/bandits/optimal_switch_session%i.Rda", session))


###################### relationship with bandit task parameters #############

####### horizon

#load("analysis/bandits/recovHorizonFull.Rda")

load(sprintf("analysis/bandits/modellingResults/fitHorizonSession%iUCBfull.Rda", session))

HParams <- trueParams

params <- unique(HParams$predictor)

HParams$ID <- readr::parse_number(rownames(HParams))

HParams <- HParams[order(HParams$ID), ]

IDs <- intersect(unique(avg$ID), unique(HParams$ID))

questionnaires <- unique(avg$measure)

cors <- data.frame(parameter = rep(params, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[HParams$predictor == cors$parameter[x] & is.element(HParams$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation Horizon task parameters with questionnaire items",
                                                        subtitle = sprintf("Session %i", session),
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



##### wm

IDs <- intersect(unique(wm$ID), unique(HParams$ID))

cors <- data.frame(parameter = rep(params, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[HParams$predictor == cors$parameter[x] & is.element(HParams$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())



######### sams task


#load("analysis/bandits/recovSam.Rda")
load(sprintf("analysis/bandits/modellingResults/fitSamSession%iUCBhierarchical.Rda", session))
Sparams <- trueParams

Sparams$ID <- readr::parse_number(rownames(Sparams))

Sparams <- Sparams[order(Sparams$ID), ]

params <- unique(Sparams$predictor)

IDs <- intersect(unique(avg$ID), unique(Sparams$ID))

cors <- data.frame(parameter = rep(params, length(questionnaires)),
                   questionnaire = rep(questionnaires, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Sparams$estimate[Sparams$predictor == cors$parameter[x] & is.element(Sparams$ID, IDs)],
                                                             avg$score[avg$measure == cors$questionnaire[x] & is.element(avg$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = questionnaire, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters with questionnaire scores",
                                                        x = element_blank(), y = element_blank())+
  scale_y_discrete(labels = c("openness", "exploration", "neg. mood","pos. mood", "depression", "anxiety"))



##### wm

IDs <- intersect(unique(wm$ID), unique(Sparams$ID))

cors <- data.frame(parameter = rep(params, length(wm_tasks)),
                   wm = rep(wm_tasks, each = length(params)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Sparams$estimate[Sparams$predictor == cors$parameter[x] & is.element(Sparams$ID, IDs)],
                                                             wm$score[wm$task == cors$wm[x] & is.element(wm$ID, IDs)]))




# plot them

ggplot(cors, aes(x = parameter, y = wm, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())



############### restless bandit

restlessParams <- import # the wm data file I use also has the restless bandit parameter estimates

trueParams <- pivot_longer(restlessParams, cols = (5:6), names_to = "predictor", values_to = "estimate")


params <- unique(trueParams$predictor)

trueParams <- subset(trueParams, !is.na(estimate))

trueParams$ID <- as.numeric(as.character(trueParams$ID))

trueParams <- trueParams[order(trueParams$ID), ]

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

Rparams <- trueParams


############## that big plot that has wm and questionnaires and model params and model agnostic stuff #####

### make 1 big table with all variables

Poptimal$measure <- paste(Poptimal$model, "Poptimal")
regret$measure <- paste(regret$model, "regret")
switch$measure <- paste(switch$model, "Pswitch")


# start big dataframe with the questionnaire scores
allOfIt <- data.frame(ID = avg$ID,
                      measure = avg$measure,
                      value = avg$score)

# add model agnostic task measures
allOfIt <- rbind(allOfIt, data.frame(ID = Poptimal$ID,
                                     measure = Poptimal$measure,
                                     value = Poptimal$Poptimal))

allOfIt <- rbind(allOfIt, data.frame(ID = regret$ID,
                                     measure = regret$measure,
                                     value = regret$regret))

allOfIt <- rbind(allOfIt, data.frame(ID = switch$ID,
                                     measure = switch$measure,
                                     value = switch$Pswitch))


# add model parameters
allOfIt <- rbind(allOfIt, data.frame(ID = HParams$ID,
                                     measure = paste("horizon", HParams$predictor, sep = "_"),
                                     value = HParams$estimate))

allOfIt <- rbind(allOfIt, data.frame(ID = Sparams$ID,
                                     measure = paste("sam", Sparams$predictor, sep = "_"),
                                     value = Sparams$estimate))


allOfIt <- rbind(allOfIt, data.frame(ID = Rparams$ID,
                                     measure = paste("restless", Rparams$predictor, sep = "_"),
                                     value = Rparams$estimate))


# add wm

allOfIt <- rbind(allOfIt, data.frame(ID = wm$ID,
                                     measure = wm$task,
                                     value = wm$score))


save(allOfIt, file = sprintf("analysis/allOfItSession%i.Rda", session))

### get that big correlation


measures <- unique(allOfIt$measure)

IDs <- intersect(unique(avg$ID), unique(horizon$ID))
IDs <- intersect(IDs, unique(wm$ID))


cors <- data.frame(var1 = rep(measures, length(measures)),
                   var2 = rep(measures, each = length(measures)),
                   cor = NA)


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(allOfIt$value[allOfIt$measure == cors$var1[x] & is.element(allOfIt$ID, IDs)],
                                                             allOfIt$value[allOfIt$measure == cors$var2[x] & is.element(allOfIt$ID, IDs)]))



cors$var1 <- factor(cors$var1, levels = cors$var1, labels = cors$var1)
cors$var2 <- factor(cors$var2, levels = cors$var2, labels = cors$var2)

# plot them

ggplot(cors, aes(x = var1, y = var2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of model parameters and WM performance",
                                                        x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### validity plot for BAP poster #########

#### get model parameters

load(sprintf("analysis/bandits/modellingResults/fitHorizonSession%iUCB_full_horizon10only.Rda", session))

Hparams <- trueParams
Hparams$ID <- parse_number(rownames(Hparams))

load(sprintf("analysis/bandits/modellingResults/fitSamSession%iUCB_hierarchical.Rda", session))
Sparams <- trueParams
Sparams$ID <- parse_number(rownames(Sparams))

restparams <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")
Rparams <- subset(restparams, select = grepl(session, colnames(restparams)) )
Rparams$ID <- restparams$ID


# get all IDs to be the same ones and in the same order

IDs <- intersect(Hparams$ID, Sparams$ID)
IDs <- intersect(IDs, Rparams$ID)
IDs <- intersect(IDs, restless$ID)
IDs <- intersect(IDs, avg$ID)

Hparams <- Hparams[order(Hparams$ID), ]
Sparams <- Sparams[order(Sparams$ID), ]
Rparams <- Rparams[order(Rparams$ID), ]

# for all other data.frames this ordering step is already done

# make dataframe that has all model parameters and task measures and questionnaire scores
all <- data.frame(ID = IDs,
                  measure = rep(c("V", "RU"), each = length(IDs)),
                  value = c(Hparams$estimate[Hparams$predictor == "V" & is.element(Hparams$ID, IDs)],
                            Hparams$estimate[Hparams$predictor == "RU"& is.element(Hparams$ID, IDs)]),
                  task = "Horizon")
# add sams task
all <- rbind(all, data.frame(ID = IDs,
                             measure = rep(c("V", "RU"), each = length(IDs)),
                             value = c(Sparams$estimate[Sparams$predictor == "V"& is.element(Sparams$ID, IDs)],
                                       Sparams$estimate[Sparams$predictor == "RU"& is.element(Sparams$ID, IDs)]),
                             task = "2AB"))
# add restless bandit
all <- rbind(all,data.frame(ID = IDs,
                            measure = rep(c("V", "RU"), each = length(IDs)),
                            value = c(Rparams$rlb_map_1_v[is.element(Rparams$ID, IDs)],
                                      Rparams$rlb_map_1_ru[is.element(Rparams$ID, IDs)]),
                            task = "Restless"))


# add task measures

all <- rbind(all, data.frame(ID = IDs,
                             measure = "regret",
                             value = regret$regret[is.element(regret$ID, IDs)],
                             task = regret$model[is.element(regret$ID, IDs)]))

all <- rbind(all, data.frame(ID = IDs,
                             measure = "p(optimal)",
                             value = Poptimal$Poptimal[is.element(Poptimal$ID, IDs)],
                             task = Poptimal$model[is.element(Poptimal$ID, IDs)]))


all <- rbind(all, data.frame(ID = IDs,
                             measure = "p(switch)",
                             value = switch$Pswitch[is.element(switch$ID, IDs)],
                             task = switch$model[is.element(switch$ID, IDs)]))



# add questionnaires

all <- rbind(all, data.frame(ID = IDs,
                             measure = avg$measure[is.element(avg$ID, IDs)],
                             value = avg$score[is.element(avg$ID, IDs)],
                             task = ""))


all$task[all$task == "sam"] <- "2AB"
all$task[all$task == "horizon"] <- "Horizon"
all$task[all$task == "restless"] <- "Restless"

all$predictor <- paste(all$task, all$measure)
unique(all$predictor)

# V and RU are reverse coded so I need to switch that back 

all$value[all$measure == "V"] <- -1 * all$value[all$measure == "V"]
all$value[all$measure == "RU"] <- -1 * all$value[all$measure == "RU"]

## to have it all in the order I want it is easiest to just hard-code

# x <- c("Horizon V", "2AB V", "Restless V", "Horizon RU", "2AB RU", "Restless RU", 
#        "Horizon regret", "2AB regret", "Restless regret", 
#        "Horizon p(optimal)", "2AB p(optimal)", "Restless p(optimal)",
#        "Horizon p(switch)", "2AB p(switch)", "Restless p(switch)")

x <- c("Horizon V", "2AB V", "Restless V", "Horizon RU", "2AB RU", "Restless RU", 
       "Horizon p(optimal)", "2AB p(optimal)", "Restless p(optimal)",
       "Horizon p(switch)", "2AB p(switch)", "Restless p(switch)")

y <- c(" CEI", " BIG_5", " STICSAcog", " STICSAsoma", " PHQ_9")

cors <- data.frame(x = rep(x, length(y)),
                   y = rep(y, each = length(x)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(all$value[all$predictor == cors$x[x]],
                                                             all$value[all$predictor == cors$y[x]]))


cors$x <- factor(cors$x, levels = cors$x, labels = cors$x)
cors$y <- factor(cors$y, levels = cors$y, labels = cors$y)

ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "correlation of task measures and questionnaire scores",
                                                        x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  scale_x_discrete(labels = as_labeller(c(" CEI" = "exploration", " BIG_5" = "openness", " STICSAcog" = "cog. anxiety", " STICSAsoma" = "soma. anxiety", " PHQ_9" = "depression")))
  

# ########### regressions #############
#

load("analysis/allOfItSession1.Rda")
all1 <- allOfIt
all1$session <- -0.5
load("analysis/allOfItSession2.Rda")
all2 <- allOfIt
all2$session <- 0.5
allOfIt <- rbind(all1, all2)

df <- pivot_wider(allOfIt, id_cols = c("ID", "session"), names_from = "measure")

colnames(df) <- str_replace(colnames(df), ":", "_")
colnames(df) <- str_replace(colnames(df), " ", "_")

df[ ,colnames(df) != "ID" & colnames(df)!= "session"] <- sapply(df[ ,colnames(df) != "ID" & colnames(df)!= "session"], function(df) scale(df))
head(df)


banditParams <- colnames(df)[grepl("horizon", colnames(df)) | grepl("sam", colnames(df)) | grepl("restless", colnames(df))]

qs <- c("BIG_5", "CEI", "PANASneg", "PANASpos", "PHQ_9", "STICSAcog", "STICSAsoma")

## df to collect results of regressions

results <- data.frame(dependentVariable = rep(banditParams, each = length(qs)),
                      predictor = rep(qs, length(banditParams)),
                      mainEffect = NA,
                      mainHDIlower = NA,
                      mainHDIupper = NA,
                      interactionWithSession = NA,
                      interactionHDIlower = NA,
                      interactionHDIupper = NA,
                      session = NA,
                      sessionHDIlower = NA,
                      sessionHDIupper = NA)

for (dv in unique(results$dependentVariable)){# takes about 1-2h
  for(iv in unique(results$predictor)){
    
    print(paste(dv, iv))
    
    formula <- as.formula(paste(dv, "~", iv, "*session + (1|ID)"))
    out <- brm(formula,
        data = df,
        cores = 2,
        chains = 2,
        it = 3000)
    
    fixed <- summary(out)$fixed
    
    results$mainEffect[results$dependentVariable == dv & results$predictor == iv] <- fixed$Estimate[rownames(fixed) == iv]
    results$mainHDIlower[results$dependentVariable == dv & results$predictor == iv] <- fixed$`l-95% CI`[rownames(fixed) == iv]
    results$mainHDIupper[results$dependentVariable == dv & results$predictor == iv] <- fixed$`u-95% CI`[rownames(fixed) == iv]
    results$interactionWithSession[results$dependentVariable == dv & results$predictor == iv] <- fixed$Estimate[rownames(fixed) == paste(iv, ":session", sep = "")]
    results$interactionHDIlower[results$dependentVariable == dv & results$predictor == iv] <- fixed$`l-95% CI`[rownames(fixed) == paste(iv, ":session", sep = "")]
    results$interactionHDIupper[results$dependentVariable == dv & results$predictor == iv] <- fixed$`u-95% CI`[rownames(fixed) == paste(iv, ":session", sep = "")]
    
    results$session[results$dependentVariable == dv & results$predictor == iv] <- fixed$Estimate[rownames(fixed) == "session"]
    results$sessionHDIlower[results$dependentVariable == dv & results$predictor == iv] <- fixed$`l-95% CI`[rownames(fixed) == "session"]
    results$sessionHDIupper[results$dependentVariable == dv & results$predictor == iv] <- fixed$`u-95% CI`[rownames(fixed) == "session"]
    
  }
}

save(results, file = "analysis/bandits/regressionResults_noRandomSlope.Rda")


ggplot(results, aes(predictor, mainEffect)) + geom_col()+
  geom_errorbar(aes(ymin = mainHDIlower, ymax = mainHDIupper))+
  facet_wrap(vars(dependentVariable))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("regression results main effects")

ggplot(results, aes(predictor, interactionWithSession)) + geom_col()+
  geom_errorbar(aes(ymin = interactionHDIlower, ymax = interactionHDIupper))+
  facet_wrap(vars(dependentVariable))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("regression results interaction with session")


## plot session effects exemplary for when BIG5 was predictor

ggplot(results[results$predictor == "BIG_5", ], aes(dependentVariable, session)) + geom_col()+
  geom_errorbar(aes(ymin = sessionHDIlower, ymax = sessionHDIupper), width = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("effects of session when accounting for some questionnaire")


## the effect of session is surprisingly interesting so lets do this again with just session in the regression
sessionResults <- data.frame(dependentVariable = banditParams,
                      mainEffect = NA,
                      mainHDIlower = NA,
                      mainHDIupper = NA)

for (dv in unique(sessionResults$dependentVariable)){
    
    print(paste(dv))
    
    formula <- as.formula(paste(dv, "~ session + (session|ID)"))
    out <- brm(formula,
               data = df,
               cores = 2,
               chains = 2,
               it = 3000)
    
    fixed <- summary(out)$fixed
    
    sessionResults$mainEffect[sessionResults$dependentVariable == dv ] <- fixed$Estimate[rownames(fixed) ==  "session"]
    sessionResults$mainHDIlower[sessionResults$dependentVariable == dv] <- fixed$`l-95% CI`[rownames(fixed) ==  "session"]
    sessionResults$mainHDIupper[sessionResults$dependentVariable == dv] <- fixed$`u-95% CI`[rownames(fixed) == "session"]
  
}

ggplot(sessionResults, aes(dependentVariable, mainEffect)) + geom_col()+
  geom_errorbar(aes(ymin = mainHDIlower, ymax = mainHDIupper), width = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("effects of session")


############## simple linear regressions for the BAP abstract #######

# giving those psychologists what they want...

# cannot have random slope for session bc that is unidentifiable


for (dv in c("horizon_RU_Horizon", "sam_RU", "restless_beta")){
  for(iv in c("BIG_5", "CEI", "STICSAcog", "STICSAsoma", "PHQ_9")){
    
    print(paste(dv, iv))
    
    formula <- as.formula(paste(dv, "~", iv, "*session + (1|ID)"))
    out <- anova(lmerTest::lmer(formula,
                                df))
    
    print(out)
    
  }
}


###################### final correlation matrix #########################
s <- session
params <- readRDS("analysis/bandits/allParams.rds") %>% 
  mutate(task = recode(task,"sam" = "2AB"),
         predictor = paste(task, predictor, sep = "_")) %>% 
  subset(!grepl("ntercept", predictor) & session == s,
         select = c("ID", "predictor", "estimate"))

load(sprintf("analysis/bandits/optimal_switch_session%i.Rda", session))
load(sprintf("analysis/qdat_session%i.Rda", session))

Poptimal <- Poptimal %>% 
  mutate(estimate = Poptimal) %>% 
  mutate(predictor = paste(model, "Poptimal", sep = "_")) %>% 
  subset(select = c("ID", "predictor", "estimate"))
  
switch <- switch %>% 
  mutate(estimate = Pswitch) %>% 
  mutate(predictor = paste(model, "Pswitch", sep = "_")) %>% 
  subset(select = c(ID, predictor, estimate))

avg <- avg %>% 
  mutate(estimate = score,
         predictor = measure) %>% 
  select(ID, estimate, predictor)

wm <- read.csv("data/wm-performance.csv") %>% 
  pivot_longer(cols = -c("participant_id"), names_to = "predictor", values_to = "estimate") %>% 
  mutate(session = parse_number(predictor)+1,
         ID = as.numeric(as.character(participant_id))) %>% 
  subset(grepl("recall", predictor) & session == s)

all <- list(params, Poptimal, switch, wm, avg) %>% 
  bind_rows() %>% 
  pivot_wider(id_cols = "ID", names_from = "predictor", values_from = "estimate")


cors <- all %>% 
  subset(select = -ID) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame()

selected_columns <- colnames(cors) %>%
  str_subset("^(?!.*(2AB|restless|horizon)).*$")

# Select rows that contain "2AB", "restless", or "horizon"
selected_rows <- rownames(cors) %>%
  str_subset("2AB|restless|horizon")

# Filter the correlation matrix
cors <- cors[selected_rows, selected_columns]

save(cors, file = sprintf("analysis/external_validity_cors_session%i.Rda", s))



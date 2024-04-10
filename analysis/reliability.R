################### test retest reliabilities #################

library(plyr)
library(ggplot2)
#library(jsonlite)
library(brms)

#library(ggridges)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/bandits/banditsWave1full.Rda")
horizon1 <- horizon
sam1 <- sam
restless1 <- restless
load("analysis/bandits/banditsWave2.Rda")
horizon2 <- horizon
sam2 <- sam
restless2 <- restless

##### kick out everyone that did not do second part ####
horizon1 <- subset(horizon1, is.element(ID, horizon2$ID))
horizon1$optimal <- NA # need to already add these columns bc apparently horizon2 has them
horizon1$chooseBest <- NA
horizon <- rbind(horizon1, horizon2)

sam1 <- subset(sam1, is.element(ID, sam2$ID))
sam1$optimal <- NA
sam1$chooseBest <- NA
sam <- rbind(sam1, sam2)

restless1 <- subset(restless1, is.element(ID, restless2$ID))
restless <- rbind(restless1, restless2)

#source("analysis/recovery_utils.R")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


############### performance reliability ###########

############# P (optimal)


horizon$optimal <- ifelse(horizon$reward1 > horizon$reward2, 0, 1)
horizon$chooseBest <- ifelse(horizon$chosen == horizon$optimal, 1, 0)

Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID+session,summarise, Poptim = meann(chooseBest))

sam$optimal <- ifelse(sam$reward1 > sam$reward2, 0, 1)
sam$chooseBest <- ifelse(sam$chosen == sam$optimal, 1, 0)


Sperf <- ddply(sam, ~ID + session, summarise, Poptim = meann(chooseBest))


restrewards <- subset(restless, ID == 1, c(reward1, reward2, reward3, reward4, trial, session))
restrewards$best <- apply(as.array(1:nrow(restrewards)), 1, function(x) max(c(restrewards$reward1[x],
                                                                              restrewards$reward2[x],
                                                                              restrewards$reward3[x],
                                                                              restrewards$reward4[x])))
  
  
restless$optimalR <- restrewards$best[match(paste(restless$trial, restless$session), paste(restrewards$trial, restrewards$session))]
restless$chooseBest <- ifelse(restless$reward == restless$optimalR, 1, 0)

Rperf <- ddply(restless, ~ID+session, summarise, Poptim = meann(chooseBest))

Hperf$model <- "horizon"
Sperf$model <- "sam"
Rperf$model <- "restless"

Perfs <- rbind(Hperf, Sperf, Rperf)

models <- c("horizon", "sam", "restless")

cors <- data.frame(session1 = rep(models, length(models)),
                   session2 = rep(models, each = length(models)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$Poptim[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$Poptim[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2"
                                                        )
cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$Poptim[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$Poptim[Perfs$model == cors$session2[x] & Perfs$session == 1], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between bandit tasks session 1")


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$Poptim[Perfs$model == cors$session1[x] & Perfs$session == 2],
                                                             Perfs$Poptim[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between bandit tasks session 2")


############### regret

horizon$maxR <- ifelse(horizon$reward1 > horizon$reward2, horizon$reward1, horizon$reward2)
horizon$regret <- horizon$maxR - horizon$reward

Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID+session,summarise, regret = meann(regret))

sam$maxR <- ifelse(sam$reward1 > sam$reward2, sam$reward1, sam$reward2)
sam$regret <- sam$maxR - sam$reward


Sperf <- ddply(sam, ~ID + session, summarise, regret = meann(regret))

restless$regret <-restless$optimalR - restless$reward

Rperf <- ddply(restless, ~ID+session, summarise, regret = meann(regret))

Hperf$model <- "horizon"
Sperf$model <- "sam"
Rperf$model <- "restless"

Perfs <- rbind(Hperf, Sperf, Rperf)

ggplot(Perfs, aes(regret, fill = as.factor(session))) + geom_histogram(position = "identity", alpha = 0.3) + facet_wrap(vars(model), scales = "free")

models <- c("horizon", "sam", "restless")

cors <- data.frame(session1 = rep(models, length(models)),
                   session2 = rep(models, each = length(models)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$regret[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$regret[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2"
                                                        )
cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$regret[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$regret[Perfs$model == cors$session2[x] & Perfs$session == 1], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between bandit tasks session 1")


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$regret[Perfs$model == cors$session1[x] & Perfs$session == 2],
                                                             Perfs$regret[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between bandit tasks session 2")


##################### model-agnostic measures of exploration ###########

########## P(switch)

horizon$prev <- c(NA, horizon$chosen[1:nrow(horizon)-1])
horizon$switch <- ifelse(horizon$chosen == horizon$prev, 0, 1)
horizon$switch[horizon$trial == 1] <- NA

sam$prev <- c(NA, sam$chosen[1:nrow(sam)-1])
sam$switch <- ifelse(sam$chosen == sam$prev, 0, 1)
sam$switch[sam$trial == 1] <- NA

restless$prev <- c(NA, restless$chosen[1:nrow(restless)-1])
restless$switch <- ifelse(restless$chosen == restless$prev, 0, 1)
restless$switch[restless$trial == 1] <- NA

Hswitch <- ddply(horizon[horizon$trial > 4, ],~ID+session,summarise, Pswitch = meann(switch))
Sswitch <- ddply(sam, ~ID+session,summarise, Pswitch = meann(switch))
Rswitch <- ddply(restless, ~ID+session, summarise, Pswitch = meann(switch))

Hswitch$model <- "horizon"
Sswitch$model <- "sam"
Rswitch$model <- "restless"

switch <- rbind(Hswitch, Sswitch, Rswitch)

ggplot(switch, aes(Pswitch, fill = as.factor(session))) + geom_histogram(position = "identity", alpha = 0.3) + facet_wrap(vars(model), scales = "free")

models <- c("horizon", "sam", "restless")

cors <- data.frame(session1 = rep(models, length(models)),
                   session2 = rep(models, each = length(models)),
                   cor = NA)
cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(switch$Pswitch[switch$model == cors$session1[x] & switch$session == 1],
                                                             switch$Pswitch[switch$model == cors$session2[x] & switch$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of switch probability between session 1 and 2")
                                                        
############ what does the switch probabilitiy actually look like across tasks and time points? 
restless$trialBin <- round(restless$trial/10) *10

Hswitch <- ddply(horizon[horizon$trial > 4, ],~session+trial+ID,summarise, Pswitch = meann(switch))
Sswitch <- ddply(sam, ~session+trial+ID,summarise, Pswitch = meann(switch))
Rswitch <- ddply(restless, ~session+trialBin+ID, summarise, Pswitch = meann(switch))

Hswitch <- ddply(Hswitch,~session+trial,summarise,se = se(Pswitch), Pswitch = meann(Pswitch))
Sswitch <- ddply(Sswitch, ~session+trial,summarise,se= se(Pswitch), Pswitch = meann(Pswitch))
Rswitch <- ddply(Rswitch, ~session+trialBin, summarise,se = se(Pswitch), Pswitch = meann(Pswitch))
colnames(Rswitch) <- colnames(Sswitch)

Hswitch$model <- "horizon"
Sswitch$model <- "sam"
Rswitch$model <- "restless"

switch <- rbind(Hswitch, Sswitch, Rswitch)

ggplot(switch, aes(trial, Pswitch, color = as.factor(session))) + geom_line() +
  geom_point()+
  geom_linerange(aes(ymin = Pswitch -se, ymax = Pswitch +se))+
  facet_wrap(vars(model), scales = "free_x")

################ reliability of model parameters #############

################ Horizon

load("analysis/bandits/modelFitHorizon1.Rda")
load("analysis/bandits/modelFitHorizon2.Rda")

HParams1 <- trueParams1
HParams1$ID <- readr::parse_number(HParams1$X)
Hparams2 <- trueParams
Hparams2$X <- rownames(Hparams2)
Hparams2$ID <- readr::parse_number(Hparams2$X)

HParams1 <- subset(HParams1, is.element(ID, Hparams2$ID))

HParams1$session <- 1
Hparams2$session <- 2
HParams <- rbind(HParams1[ ,!grepl("colMeans", colnames(HParams1))], Hparams2[ ,!grepl("colMeans", colnames(Hparams2))])

table(unique(HParams1$ID) == unique(Hparams2$ID))

params <- unique(HParams$predictor)

cors <- data.frame(session1 = rep(params, length(params)),
                   session2 = rep(params, each = length(params)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[HParams$predictor == cors$session1[x] & HParams$session == 1],
                                                             HParams$estimate[HParams$predictor == cors$session2[x] & HParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")


## within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[HParams$predictor == cors$session1[x] & HParams$session == 2],
                                                             HParams$estimate[HParams$predictor == cors$session2[x] & HParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")


###################### Sam's task



load("analysis/bandits/modelFitSamWave1.Rda")
Sparams1 <- modelfit[[2]]
load("analysis/bandits/modelFitSamWave2.Rda")
Sparams2 <- modelfit[[2]]

Sparams1$ID <- readr::parse_number(rownames(Sparams1))
Sparams2$ID <- readr::parse_number(rownames(Sparams2))

Sparams1 <- subset(Sparams1, is.element(ID, Sparams2$ID))

Sparams1$session <- 1
Sparams2$session <- 2
SParams <- rbind(Sparams1[ ,!grepl("colMeans", colnames(Sparams1))], Sparams2[ ,!grepl("colMeans", colnames(Sparams2))])

table(unique(Sparams1$ID) == unique(Sparams2$ID))

params <- unique(SParams$predictor)

cors <- data.frame(session1 = rep(params, length(params)),
                   session2 = rep(params, each = length(params)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(SParams$estimate[SParams$predictor == cors$session1[x] & SParams$session == 1],
                                                             SParams$estimate[SParams$predictor == cors$session2[x] & SParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")


## within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[SParams$predictor == cors$session1[x] & SParams$session == 2],
                                                             HParams$estimate[SParams$predictor == cors$session2[x] & SParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")





                                                        


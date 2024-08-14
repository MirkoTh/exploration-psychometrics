################### test retest reliabilities #################

library(plyr)
library(ggplot2)
#library(jsonlite)
library(brms)

#library(ggridges)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/bandits/banditsWave1.Rda")
horizon1 <- horizon
sam1 <- sam
restless1 <- restless
load("analysis/bandits/banditsWave2.Rda")
horizon2 <- horizon
sam2 <- sam
restless2 <- restless

### get IDs that are in both sessions

IDs <- intersect(horizon1$ID, horizon2$ID)


horizon1 <- subset(horizon1, is.element(ID, IDs))
horizon2 <- subset(horizon2, is.element(ID, IDs))
horizon <- rbind(horizon1, horizon2)

sam1 <- subset(sam1, is.element(ID, IDs))
sam2 <- subset(sam2, is.element(ID,IDs))
sam <- rbind(sam1, sam2)

restless1 <- subset(restless1, is.element(ID, IDs))
restless2 <- subset(restless2, is.element(ID, IDs))
restless <- rbind(restless1, restless2)

#source("analysis/recovery_utils.R")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


rel_collect <- data.frame(task = c("horizon", "sam", "restless"),
                          Poptimal = NA,
                          regret = NA,
                          Pswitch = NA,
                          V = NA,
                          RU = NA,
                          Intercept = NA) 

############### performance reliability ###########

############# P (optimal)
best <- function(df){
  
  rewards <- subset(df, select = grepl("reward", colnames(df)))
  rewards <- subset(rewards, select = -reward)
  
  best <- apply(as.array(1:nrow(rewards)), 1, function(x) max(rewards[x,]))
  
  return(best)
  
} 


horizon$best <- best(horizon)
sam$best <- best(sam)
restless$best <- best(restless)

horizon$chooseBest <- ifelse(horizon$reward == horizon$best, 1, 0)

Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID+session,summarise, Poptim = meann(chooseBest))

sam$chooseBest <- ifelse(sam$reward == sam$best, 1, 0)
Sperf <- ddply(sam, ~ID + session, summarise, Poptim = meann(chooseBest))


restless$chooseBest <- ifelse(restless$reward == restless$best, 1, 0)

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
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of P(optimal) session 1 and 2"
                                                        )
# save this for the final big plot

rel_collect$Poptimal <- cors$cor[cors$session1 == cors$session2]


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$Poptim[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$Poptim[Perfs$model == cors$session2[x] & Perfs$session == 1], use = "pairwise.complete.obs"))

p1 <- ggplot(cors, aes(session1, session2, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "P(optimal)",
                                                                         x = element_blank(), y = element_blank())+
  scale_x_discrete(labels = as_labeller(c("horizon" = "Horizon", "restless" = "Restless", "sam" = "2AB")))+
  scale_y_discrete(labels = as_labeller(c("horizon" = "Horizon", "restless" = "Restless", "sam" = "2AB")))

p1

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$Poptim[Perfs$model == cors$session1[x] & Perfs$session == 2],
                                                             Perfs$Poptim[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of P(optimal) between bandit tasks session 2")


############### regret

horizon$regret <- horizon$best - horizon$reward

Hperf <- ddply(horizon[horizon$trial > 4, ], ~ID+session,summarise, regret = meann(regret))

sam$regret <- sam$best - sam$reward


Sperf <- ddply(sam, ~ID + session, summarise, regret = meann(regret))

restless$regret <-restless$best - restless$reward

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

# save this for the final big plot

rel_collect$regret <- cors$cor[cors$session1 == cors$session2]

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$regret[Perfs$model == cors$session1[x] & Perfs$session == 1],
                                                             Perfs$regret[Perfs$model == cors$session2[x] & Perfs$session == 1], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of regret between bandit tasks session 1")


cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(Perfs$regret[Perfs$model == cors$session1[x] & Perfs$session == 2],
                                                             Perfs$regret[Perfs$model == cors$session2[x] & Perfs$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of regret between bandit tasks session 2")


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

# save this for the final big plot

rel_collect$Pswitch <- cors$cor[cors$session1 == cors$session2]

### look at agreement within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(switch$Pswitch[switch$model == cors$session1[x] & switch$session == 1],
                                                             switch$Pswitch[switch$model == cors$session2[x] & switch$session == 1], use = "pairwise.complete.obs"))

p2 <- ggplot(cors, aes(session1, session2, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "P(switch)",
                                                                         x = element_blank(), y = element_blank())+
  scale_x_discrete(labels = as_labeller(c("horizon" = "Horizon", "restless" = "Restless", "sam" = "2AB")))+
  scale_y_discrete(labels = as_labeller(c("horizon" = "Horizon", "restless" = "Restless", "sam" = "2AB")))

p2

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(switch$Pswitch[switch$model == cors$session1[x] & switch$session == 2],
                                                             switch$Pswitch[switch$model == cors$session2[x] & switch$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of switch probability within session 2")
                                                        

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


####################### convergent validity plots for BAP poster ##########

#### get model parameters

load("analysis/bandits/modellingResults/fitHorizonSession1UCB_full_horizon10only.Rda")

Hparams <- trueParams
Hparams$ID <- parse_number(rownames(Hparams))

load("analysis/bandits/modellingResults/fitSamSession1UCB_hierarchical.Rda")
Sparams <- trueParams
Sparams$ID <- parse_number(rownames(Sparams))

restparams <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")
Rparams <- subset(restparams, select = grepl(1, colnames(restparams)) )
Rparams$ID <- restparams$ID


# get all IDs to be the same ones and in the same order

IDs <- intersect(Hparams$ID, Sparams$ID)
IDs <- intersect(IDs, Rparams$ID)
IDs <- IDs[order(IDs)]

Hparams <- Hparams[order(Hparams$ID), ]
Sparams <- Sparams[order(Sparams$ID), ]
Rparams <- Rparams[order(Rparams$ID), ]

params <- data.frame(ID = rep(IDs, 3),
                     task = rep(c("Horizon", "Restless", "2AB"), each = length(IDs)),
                     V = c(Hparams$estimate[Hparams$predictor == "V" & is.element(Hparams$ID, IDs)],
                           Sparams$estimate[Sparams$predictor == "V" & is.element(Sparams$ID, IDs)],
                           Rparams$rlb_map_1_v[is.element(Rparams$ID, IDs)]),
                     RU = c(Hparams$estimate[Hparams$predictor == "RU" & is.element(Hparams$ID, IDs)],
                            Sparams$estimate[Sparams$predictor == "RU" & is.element(Sparams$ID, IDs)],
                            Rparams$rlb_map_1_ru[is.element(Rparams$ID, IDs)]))

tasks <- c("Horizon", "Restless", "2AB")

cors <- data.frame(x = rep(tasks, length(tasks)),
                   y = rep(tasks, each = length(tasks)),
                   cor = NA)

cors$x <- factor(cors$x, levels = cors$x, labels = cors$x)
cors$y <- factor(cors$y, levels = cors$y, labels = cors$y)
# V
cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(params$V[params$task == cors$x[x]],
                                                            params$V[params$task == cors$y[x]]))

p3 <- ggplot(cors, aes(x,y, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "V",
                                                                         x = element_blank(), y = element_blank())

p3

# RU

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(params$RU[params$task == cors$x[x]],
                                                             params$RU[params$task == cors$y[x]]))

p4 <- ggplot(cors, aes(x,y, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "RU",
                                                                         x = element_blank(), y = element_blank())

p4

ggpubr::ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, labels = "AUTO", common.legend = T, legend = "right")

################ reliability of model parameters #############

params <- read.csv("analysis/bandits/AllModelParameters.csv")

params <- params %>% 
  subset(!grepl("ntercept", predictor), -c(X))

restparams <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")
restparams <- restparams %>% 
  pivot_longer(cols = -ID, values_to = "estimate", names_to = "predictor")

restparams$session <- parse_number(restparams$predictor)
restparams$horizon <- NA
restparams$task <- "restless"
restparams <- restparams %>% 
  mutate(predictor = ifelse(grepl("ru", predictor), "RU", "V"))

params <- list(params, restparams) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = "session", values_from = "estimate", id_cols = c("ID", "task", "predictor", "horizon"))

cors <- params %>% group_by(task, predictor, horizon) %>% 
  summarise(cor = cor(`1`, `2`, use = "pairwise.complete.obs"))

cors

save(cors, file = "analysis/bandits/reliability_parameters.Rda")

################ Horizon

load("analysis/bandits/modellingResults/fitHorizonSession1UCBfullno_horizon.Rda") 

HParams1 <- trueParams

load("analysis/bandits/modellingResults/fitHorizonSession2UCBfullno_horizon.Rda")
Hparams2 <- trueParams


HParams1$ID <- readr::parse_number(rownames(HParams1))
Hparams2$ID <- readr::parse_number(rownames(Hparams2))

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


rel_collect$V[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "V" & cors$session2 == "V"]
rel_collect$RU[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "RU" & cors$session2 == "RU"]
rel_collect$Intercept[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "Intercept" & cors$session2 == "Intercept"]

## within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[HParams$predictor == cors$session1[x] & HParams$session == 2],
                                                             HParams$estimate[HParams$predictor == cors$session2[x] & HParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")


######### horizon task full model


load("analysis/bandits/modellingResults/fitHorizonSession1UCBfull.Rda") 

HParams1 <- trueParams

load("analysis/bandits/modellingResults/fitHorizonSession2UCBfull.Rda")
Hparams2 <- trueParams


HParams1$ID <- readr::parse_number(rownames(HParams1))
Hparams2$ID <- readr::parse_number(rownames(Hparams2))

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



rel_collect$Horizon <- NA
rel_collect$VH <- NA
rel_collect$RUH <- NA
rel_collect$V[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "V" & cors$session2 == "V"]
rel_collect$RU[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "RU" & cors$session2 == "RU"]
rel_collect$Horizon[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "Horizon" & cors$session2 == "Horizon"]
rel_collect$VH[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "Horizon:V" & cors$session2 == "Horizon:V"]
rel_collect$RUH[rel_collect$task == "horizon"] <- cors$cor[cors$session1 == "RU:Horizon" & cors$session2 == "RU:Horizon"]
###################### Sam's task

load("analysis/bandits/modellingResults/fitSamSession1UCBhierarchical.Rda") 
Sparams1 <- trueParams
load("analysis/bandits/modellingResults/fitSamSession2UCBhierarchical.Rda")
Sparams2 <- trueParams

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

rel_collect$V[rel_collect$task == "sam"] <- cors$cor[cors$session1 == "V" & cors$session2 == "V"]
rel_collect$RU[rel_collect$task == "sam"] <- cors$cor[cors$session1 == "RU" & cors$session2 == "RU"]
rel_collect$Intercept[rel_collect$task == "sam"] <- cors$cor[cors$session1 == "Intercept" & cors$session2 == "Intercept"]

## within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(HParams$estimate[SParams$predictor == cors$session1[x] & SParams$session == 2],
                                                             HParams$estimate[SParams$predictor == cors$session2[x] & SParams$session == 2], use = "pairwise.complete.obs"))


ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of performance between session 1 and 2")


################### reliability of questionnaire scores ############

load("analysis/qsWave1.Rda")
qdat1 <- qdat
load("analysis/qsWave2.Rda")
qdat2 <- qdat

qdat1 <- subset(qdat1, is.element(ID, IDs))
qdat2 <- subset(qdat2, is.element(ID, IDs))
qdat1$session <- 1
qdat2$session <- 2

qdat <- rbind(qdat1, qdat2)

qs <- read.csv("task/questionnaires.csv", sep = ";")

qs$Qnum <- NA
for (i in unique(qs$Measure)){
  
  qs$Qnum[qs$Measure == i] <- 0:(nrow(qs[qs$Measure == i, ])-1)
  
}

qs$Q <- paste(qs$Measure, qs$Qnum, sep = "_")



responses <- tidyr::pivot_longer(qdat, cols = c(10:71), names_to = "Q", values_to = "response")

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

avg <- ddply(responses, ~ID+measure+session, summarise, score = mean(response))

write.csv(avg, "analysis/questionnaireScores.csv")


measures <- unique(avg$measure)

cors <- data.frame(session1 = rep(measures, length(measures)),
                   session2 = rep(measures, each = length(measures)),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(avg$score[avg$measure == cors$session1[x] & avg$session == 1],
                                                             avg$score[avg$measure == cors$session2[x] & avg$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "correlation of questionnaires between session 1 and 2")

save(cors, file = "analysis/reliabilityResults/questionnaires.Rda")

rel_questionnaires <- data.frame(measure = measures,
                                 rel = cors$cor[cors$session1 == cors$session2])

## agreement within a session

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(avg$score[avg$measure == cors$session1[x] & avg$session == 1],
                                                             avg$score[avg$measure == cors$session2[x] & avg$session == 1], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of questionnaires in session 1")

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(avg$score[avg$measure == cors$session1[x] & avg$session == 2],
                                                             avg$score[avg$measure == cors$session2[x] & avg$session == 2], use = "pairwise.complete.obs"))

ggplot(cors, aes(x = session1, y = session2, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) + labs(title = "agreement of questionnaires in session 2")

##################### big reliability plot(s) #################

######### questionnaires

p1 <- ggplot(rel_questionnaires, aes(rel, measure)) + 
  geom_vline(aes(xintercept = 0.6), color = "grey") +
  geom_vline(aes(xintercept = 0.8), color = "grey", linetype = "dotted") +
  geom_point(color = "#B40F20", size = 5) +
  coord_cartesian(xlim = c(0,1))+
  labs(title = "Reliability of the questionnaire measures",
       x = "correlation between session 1 and session2",
       y = element_blank()) +
  scale_y_discrete(labels = c("openness", "exploration", "negative mood", "positive mood", "depression", "cognitive anxiety", "somatic anxiety"))

p1
######### tasks

## for the restless bandit I only have the stats from the plots Mirko sent so I go with this for now
rel_collect$V[rel_collect$task == "restless"] <- 0.4
rel_collect$RU[rel_collect$task == "restless"] <- 0.21


df <- tidyr::pivot_longer(rel_collect, cols = 2:ncol(rel_collect), names_to = "measure", values_to = "reliability")
df$color <- NA
df$color[df$measure == "Poptimal" | df$measure == "regret"] <- "#DD8D29"
df$color[df$measure == "Pswitch"] <- "#E2D200"
df$color[is.na(df$color)] <- "#46ACC8"

df$measure <- factor(df$measure, levels = df$measure, labels = df$measure)

p2 <- ggplot(df, aes(reliability, measure)) + 
  geom_vline(aes(xintercept = 0.6), color = "grey") +
  geom_vline(aes(xintercept = 0.8), color = "grey", linetype = "dotted") +
  geom_point(aes(color = color), size = 5) +
  coord_cartesian(xlim = c(0,1))+
  labs(title = "Reliability of the task measures",
       x = "correlation between session 1 and session2",
       y = element_blank()) +
 scale_color_manual(values = unique(df$color))+
  facet_wrap(vars(task))+
  theme(legend.position = "none")

  
p2
save(rel_collect, file ="analysis/reliabilityResults/reliability_tasks_Horizonfull.Rda")


ggpubr::ggarrange(p1, p2, ncol = 2)



########### convergent validity ############

params <- read.csv("analysis/bandits/AllModelParameters.csv") %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "V", "info" = "RU"),
         horizon = ifelse(is.na(horizon), "long", horizon)) %>% 
  subset(horizon == "long" & !grepl("ntercept", predictor) & session == 1, -c(horizon, X, session))
  

restparams <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")
restparams <- restparams %>% 
  pivot_longer(cols = -ID, values_to = "estimate", names_to = "predictor")

restparams$session <- parse_number(restparams$predictor)
restparams$task <- "restless"
restparams <- restparams %>% 
  mutate(predictor = ifelse(grepl("ru", predictor), "RU", "V")) %>% 
  subset(session == 1, -session)

params <- list(params, restparams) %>% 
  bind_rows()

s







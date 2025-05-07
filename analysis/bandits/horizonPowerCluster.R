########### power analysis on the cluster ##################
#library(lme4)
library(brms)
#setwd("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl")
se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

#data <- read.csv("~/reliability/data.csv")
data <- read.csv("data.csv")

task_id <-  as.numeric(commandArgs(TRUE)[1])
Ngames = as.numeric(commandArgs(TRUE)[2]) # 4 conditions of Ngames each (2 horizons * 2 info conditions; fit horizons separately)
#NSub = as.numeric(commandArgs(TRUE)[3])
horizon = as.numeric(commandArgs(TRUE)[3])
NSum = 99
load(paste("~/reliability/baymodel", horizon, ".Rda", sep = ""))

set.seed(task_id)
########################################


# first I need to calculate the observed means bc here is only latent

data$mean_L <- NA
data$mean_R <- NA

data$row <- 1:nrow(data)
data$mean_L[data$Trial == 5] <- apply(as.array(data$row[data$Trial == 5]), 1, function(x) meann(data$Outcome[data$Subject == data$Subject[x]&
                                                                                                               data$Block == data$Block[x] &
                                                                                                               data$Choice == 1& 
                                                                                                               data$Trial < 5]))
data$mean_R[data$Trial == 5] <- apply(as.array(data$row[data$Trial == 5]), 1, function(x) meann(data$Outcome[data$Subject == data$Subject[x]&
                                                                                                               data$Block == data$Block[x] &
                                                                                                               data$Choice == 0& 
                                                                                                               data$Trial < 5]))

## calculate deltas
data$delta_mean <- data$mean_L - data$mean_R

#load("~/reliability/baymodel.Rda")

data$delta_mean <- scale(data$delta_mean)
data$Info <- data$Info/2

baymodel5 <-  brm(Choice ~ delta_mean+ Info+ (Info+ delta_mean | Subject), family = "bernoulli",
                 data = data[data$Trial ==5 & data$Horizon == 5, ],
                 chains = 2,
                 cores = 2,
                 iter = 10000) # this converged

baymodel10 <-  brm(Choice ~ delta_mean+ Info+ (Info+ delta_mean | Subject), family = "bernoulli",
                  data = data[data$Trial ==5 & data$Horizon == 10, ],
                  chains = 2,
                  cores = 2,
                  iter = 10000) # this converged

################## 99 subjects ######################

## do we have sufficient power with N games per condition ?


df <- data.frame(Subject = rep(sample(unique(data$Subject), NSub, replace = FALSE), each = Ngames*2))


df$delta_mean <- sample(na.omit(data$delta_mean), Ngames*NSub*2, replace = T)
df$Info <- rep(rep(c(0,0,-1, 1), Ngames/2), NSub)
#df$Horizon <- rep(rep(c(5, 10), each = Ngames*2), NSub)

## simulate data

if (exists("baymodel5")){
df$Choice <- predict(baymodel5, newdata = df)[ ,1]
}else {df$Choice <- predict(baymodel10, newdata = df)[ ,1]}
# make the choice binary
df$Choice <- ifelse(df$Choice < runif(NSub*Ngames*2), 0, 1)

## fit it again
res <- data.frame(Nsub = rep(NSub, 2),
                  Ngames = rep(Ngames, 2),
                  Horizon = rep(horizon, 2),
                  effect = c("mean", "Info"),
                  sig = rep(NA, 2),
                  est = rep(NA, 2),
                  range = rep(NA, 2),
                  rhat = rep(NA, 2))



model <- brm(Choice ~ delta_mean + Info + (Info+ delta_mean| Subject), 
             family = "bernoulli", 
             data = df,
             chains = 2,
             cores = 2,
             iter = 6000,
             #file = paste("~/reliability/powHoriz/", NSub,"_", Ngames,"_", task_id,"_", horizon, sep = ""))
)
warn <- names(warnings())
if (mean(grepl("Effective Samples Size", warn)) > 0){ # if there is a warning about lacking effective sample size then rerun with more iterations
  model <- brm(Choice ~ delta_mean + Info + (Info+ delta_mean| Subject), 
               family = "bernoulli", 
               data = df,
               chains = 2,
               cores = 2,
               iter = 8000)# no file bc then it would just load the old model
  
}

res$sig <- ifelse(sign(summary(model)$fixed[2:3, 3]) == sign(summary(model)$fixed[2:3, 4]), 1, 0)
res$est <- summary(model)$fixed[2:3, 1]
res$range <- summary(model)$fixed[2:3, 4] - summary(model)$fixed[2:3, 3]
res$rhat <- summary(model)$fixed[2:3, 5]

save(res, file = paste("~/reliability/powHoriz/", NSub,"_", Ngames,"_", task_id,"_", horizon,".Rda", sep = ""))


recovParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(model))))

save(recovParams, file = paste("~/reliability/powHoriz/params", NSub,"_", Ngames,"_", task_id,"_", horizon,".Rda", sep = ""))



######### eval results ##########

if (exists("estims")){rm(estims)}

for (horizon in c(5,10)){
  for (Ngames in c(10, 20, 40)){
    for (task_id in c(1:10)){
      
      load(sprintf("resPowerHorizon/params99_%i_%i_%i.Rda", Ngames, task_id, horizon))
      recovParams$predictor <- NA
      for (i in c("delta_mean", "Info")){
        recovParams$predictor[grepl(i, rownames(recovParams))& grepl("r_Subject", rownames(recovParams))] <- i
      }
      
      recovParams <- subset(recovParams, !is.na(predictor)& !grepl("Subject__", rownames(recovParams)))
      
      
      temp <- recovParams
      temp$horizon <- horizon
      temp$Ngames <- Ngames
      temp$task_id <- task_id
      temp$row <- rownames(temp)
      if (exists("estims")){
        estims <- rbind(estims, temp)
      }else {
        estims <- temp
      }
      
      
    }
  }
}

load("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl/baymodel5.Rda")
load("/Users/kristinwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl/baymodel10.Rda")

trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(baymodel5))))

for (i in c("delta_mean", "Info")){
  trueParams$predictor[grepl(i, rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- i
}
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodel5)))`
trueParams$row <- rownames(trueParams)
trueParams5 <- subset(trueParams, !is.na(predictor)& !grepl("Subject__", rownames(trueParams)), select = -`colMeans(as.data.frame(posterior_samples(baymodel5)))`)

trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(baymodel10))))

for (i in c("delta_mean", "Info")){
  trueParams$predictor[grepl(i, rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- i
}
trueParams$estimate <- trueParams$`colMeans(as.data.frame(posterior_samples(baymodel10)))`
trueParams$row <- rownames(trueParams)
trueParams10 <- subset(trueParams, !is.na(predictor)& !grepl("Subject__", rownames(trueParams)), select = -`colMeans(as.data.frame(posterior_samples(baymodel10)))`)

trueParams5$horizon <- 5
trueParams10$horizon <- 10

trueParams <- rbind(trueParams5, trueParams10)

estims$orig <- trueParams$estimate[match(paste(estims$horizon, estims$row), paste(trueParams$horizon, trueParams$row))]

library(tidyverse)
cors <- estims %>% 
  group_by(horizon, predictor, Ngames, task_id) %>% 
  summarise(r = cor(`colMeans(as.data.frame(posterior_samples(model)))`, orig))

library(ggplot2)
ggplot(cors, aes(r, fill = as.factor(Ngames))) + 
  geom_histogram(position= "identity", alpha = 0.3)+
  facet_grid(rows = vars(predictor), cols = vars(horizon))

tex <- cors %>% 
  group_by(horizon, predictor, Ngames) %>% 
  summarise(cor = mean(r), l = cor - 1.96*se(r), u = cor + 1.96*se(r)) %>% 
  ungroup() %>% 
  mutate(Horizon = recode(horizon, `5` = "short", `10` = "long"),
         Predictor = recode(predictor, "Info" = "directed", 
                            "delta_mean" = "value-guided"),
         Ngames = Ngames*4,# Ngames was coded as number per 2*2 cell in Horizon*equal vs unequal information
         cor = round(cor, digits = 2),
         l = round(l, digits = 2),
         u = round(u, digits = 2)) %>% 
  select(Horizon, Predictor, Ngames, cor, l, u)
tex

stargazer::stargazer(tex, summary = F, rownames = F)

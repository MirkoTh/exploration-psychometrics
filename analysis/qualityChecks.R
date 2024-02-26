############ data quality checks ##############


library(plyr)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))
library(ggridges)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/bandits/banditsWave1.Rda")
load("analysis/qsWave1.Rda")
load("data/wave1/bandits/comprehension.Rda")

######## plot aspects of comprehension questions ###########

ggplot(comprehension, aes(compAttempts, fill = task)) + geom_histogram(position = "identity", alpha = 0.3)+
  facet_wrap(vars(task))

mean_sd <- ddply(comprehension, ~task, summarise, meanComp = mean(compAttempts), SD = sd(compAttempts))

mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp

ggplot(comprehension, aes(compAttempts, fill = task)) + geom_histogram(position = "identity", alpha = 0.3)+
  facet_wrap(vars(task)) + geom_vline(data = mean_sd, aes(xintercept = meanComp), alpha = 0.8) + 
  geom_vline(data = mean_sd, aes(xintercept = SD), alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,50,2))


comprehension$compTime <- comprehension$compTime/60000 # ms to min
mean_sd <- ddply(comprehension, ~task, summarise, meanComp = mean(compTime), SD = sd(compTime))
mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp
ggplot(comprehension, aes(compTime, fill = task)) + geom_histogram(position = "identity", alpha = 0.3)+
  facet_wrap(vars(task))+
  scale_x_continuous(breaks = seq(0,50,2))+
  labs(x = "time spent at comprehension questions in minutes")+ geom_vline(data = mean_sd, aes(xintercept = meanComp), alpha = 0.8) + 
  geom_vline(data = mean_sd, aes(xintercept = SD), alpha = 0.5)

cor(comprehension$compAttempts, comprehension$compTime)# 0.28


comprehension$instTime <- comprehension$instTime/60000 # ms to min
mean_sd <- ddply(comprehension, ~task, summarise, meanComp = mean(instTime), SD = sd(instTime))
mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp
ggplot(comprehension, aes(instTime, fill = task)) + geom_histogram(position = "identity", alpha = 0.3)+
  facet_wrap(vars(task))+
  scale_x_continuous(breaks = seq(0,50,2))+
  labs(x = "time spent at instructions in minutes")+ geom_vline(data = mean_sd, aes(xintercept = meanComp), alpha = 0.8) + 
  geom_vline(data = mean_sd, aes(xintercept = SD), alpha = 0.5)



cor(pivot_wider(comprehension, id_cols = ID, names_from = task, values_from = instTime))

############## exclusion criteria ############
# as per preregistration

# cheating
table(qdat$mem_aid_0)
table(qdat$slot_aid_0)

# attention
table(qdat$attention1>1)

qdat$exclude <- 0

qdat$exclude[qdat$mem_aid_0 == 1 | qdat$slot_aid_0 == 1 | qdat$attention1 < 2] <- 1

table(qdat$exclude)

# comprehension attempts

mean_sd <- ddply(comprehension, ~task, summarise, meanComp = mean(compAttempts, na.rm =T), SD = sd(compAttempts, na.rm = T))

mean_sd$SD <- 2*mean_sd$SD + mean_sd$meanComp

table(comprehension$compAttempts > mean_sd$SD)

qdat$exclude[match(comprehension$ID[na.omit(comprehension$compAttempts)>mean_sd$SD], qdat$ID)] <- 1

table(qdat$exclude)

# performance on bandit tasks

# horizon

# when responding by chance, for how many of the choices would we pick the best option?
# get number of free choices:
n <- length(na.omit(horizon$chosen[horizon$ID == 1])) - 80*4 # subtract the free choices
# for which number of best arm choices is there a 95% probability that this is by chance?
qbinom(0.95, n, 0.5) # 154

# proportion of optimal choices you get by chance
pchance <- 154/n

horizon$optimal <- ifelse(horizon$reward1 > horizon$reward2, 0, 1)
horizon$chooseBest <- ifelse(horizon$chosen == horizon$optimal, 1, 0)

best <- ddply(horizon[horizon$trial > 4, ], ~ID+Horizon, summarise, optimal = mean(chooseBest, na.rm = T))
head(best)

# dist plot

ggplot(best, aes(x = optimal))+ geom_density_ridges(aes(y = as.factor(Horizon)), alpha = 0.6) +
  geom_jitter(aes(y = as.factor(Horizon)), alpha = 0.5, height = 0.1)+
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5)) +
  geom_vline(aes(xintercept = pchance), color = "red")

# how many subjects are on average at chance performance?

overall <- ddply(horizon[horizon$trial > 4, ], ~ID, summarise, optimal = meann(chooseBest))
table(overall$optimal <= pchance) # 6 excluded

qdat$exclude[match(overall$ID[overall$optimal <= pchance], qdat$ID)] <- 1
table(qdat$exclude) # 3 more than before

## same thing for sam

sam$optimal <- ifelse(sam$reward1 > sam$reward2, 0, 1)
sam$chooseBest <- ifelse(sam$chosen == sam$optimal, 1, 0)

best <- ddply(sam, ~ID+trial+cond, summarise, optimal = meann(chooseBest))
head(best)


# dist plot

ggplot(best, aes(x = optimal))+ geom_density_ridges(aes(y = as.factor(trial)), alpha = 0.6) +
  facet_wrap(vars(cond))+
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5))

overall <- ddply(sam, ~ID, summarise, optimal = meann(chooseBest))

ggplot(overall, aes(optimal)) + geom_histogram(alpha = 0.5) + geom_vline(aes(xintercept = pchance))+
  ggtitle("proportion of optimal choices by subject", subtitle = "vertical line indicates 95 percentile of chance performance")

table(overall$optimal <= pchance) # 7

qdat$exclude[match(overall$ID[overall$optimal <= pchance], qdat$ID)] <- 1

table(qdat$exclude) # 4 more

######## restless bandit

data <- restless
data$optimalR <- rep(apply(as.array(data$trial[data$ID == 1]), 1, function(x) max(c(data$reward1[data$trial == x],
                                                                                    data$reward2[data$trial == x],
                                                                                    data$reward3[data$trial == x],
                                                                                    data$reward4[data$trial == x]))), 
                     length(unique(data$ID)))

data$chooseBest <- ifelse(data$reward == data$optimalR, 1, 0)

n <- 200
pchance <- qbinom(0.95, n, 0.25)/n

overall <- ddply(data, ~ID, summarise, optimal = meann(chooseBest))

ggplot(overall, aes(optimal)) + geom_histogram(alpha = 0.5) + geom_vline(aes(xintercept = pchance))+
  ggtitle("proportion of optimal choices by subject", subtitle = "vertical line indicates 95 percentile of chance performance")

table(overall$optimal <= pchance)

qdat$exclude[match(overall$ID[overall$optimal <= pchance], qdat$ID)] <- 1

table(qdat$exclude) # 3 new









######## do we find the horizon effect? ##########

# let's only use those that would be included

horizon <- subset(horizon, is.element(ID, qdat$ID[qdat$exclude == 0]))


horizon$mean_diff <- ifelse(is.na(horizon$reward), NA, horizon$reward1 - horizon$reward2)

# adjust it to be informative - uninformative

horizon$mean_diff[horizon$info == 1 & horizon$trial == 5] <- horizon$mean_diff[horizon$info == 1 & horizon$trial == 5] * -1

# turn mean diff into binned

horizon$mean_diff <- round(horizon$mean_diff / 5) * 5

horizon$explore <- NA
horizon$explore[horizon$trial == 5] <- ifelse(horizon$info[horizon$trial == 5] != -1, horizon$chosen[horizon$trial == 5], 1-horizon$chosen[horizon$trial == 5])


psych <- ddply(horizon[horizon$info != 0&horizon$trial == 5, ], ~mean_diff+Horizon, summarise, explore = meann(explore))

ggplot(psych, aes(mean_diff, explore, color = as.factor(Horizon))) + geom_point() + geom_line()

library(brms)
summary(brm(chosen ~ Horizon * info + Horizon*mean_diff+ (Horizon*info + Horizon*mean_diff | ID), 
            data = horizon[horizon$trial == 5, ], 
            family = "bernoulli",
            cores = 2,
            chains = 2,
            iter = 2000))



############ performance plots for presentation ############

########## horizon

n <- length(na.omit(horizon$chosen[horizon$ID == 1])) - 80*4 # subtract the free choices
# for which number of best arm choices is there a 95% probability that this is by chance?
qbinom(0.95, n, 0.5) # 154

# proportion of optimal choices you get by chance
pchance <- 154/n

horizon$optimal <- ifelse(horizon$reward1 > horizon$reward2, 0, 1)
horizon$chooseBest <- ifelse(horizon$chosen == horizon$optimal, 1, 0)

best <- ddply(horizon[horizon$trial > 4, ], ~ID+Horizon+trial, summarise, optimal = mean(chooseBest, na.rm = T))
head(best)


# dist plot

ggplot(best, aes(x = optimal,y = trial, fill = as.factor(Horizon)))+ geom_density_ridges(aes(y = as.factor(trial)), alpha = 0.6) +
  #geom_jitter(aes(y = as.factor(Horizon)), alpha = 0.5, height = 0.1)+
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5))+
  labs(title = "Proportion of best arm choices on the Horizon task over trials",
       x = "Proportion of best arm choices")+
  scale_fill_discrete(name = "Horizon", labels = c(5, 10))

#### Sam


sam$optimal <- ifelse(sam$reward1 > sam$reward2, 0, 1)
sam$chooseBest <- ifelse(sam$chosen == sam$optimal, 1, 0)

best <- ddply(sam, ~ID+trial, summarise, optimal = meann(chooseBest))
head(best)

ggplot(best, aes(x = optimal))+ geom_density_ridges(aes(y = as.factor(trial)), alpha = 0.6) +
  xlim(c(0, 1))+
  geom_vline(aes(xintercept = 0.5))+
  labs(title = "Proportion of best arm choices on Sam's task over trials",
       x = "Proportion of best arm choices",
       y = "trial")
  


sam$optimalR <- ifelse(sam$reward1>sam$reward2, sam$reward1, sam$reward2)
sam$regret <- sam$optimalR - sam$reward

regret <- ddply(sam, ~ID+trial+cond, summarise, regret = meann(regret))

# dist plot

ggplot(regret, aes(x = regret))+ geom_density_ridges(aes(y = as.factor(trial)), alpha = 0.6) +
  facet_wrap(vars(cond))


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






############ psychometric curve Horizon task ################

data <- horizon



## get mean difference

data$mean_L <- NA
data$mean_R <- NA

data$row <- 1:nrow(data)
data$mean_L[data$trial == 5] <- apply(as.array(data$row[data$trial == 5]), 1, function(x) meann(data$reward[data$ID == data$ID[x]&
                                                                                                              data$block == data$block[x] &
                                                                                                              data$chosen == 0 & 
                                                                                                              data$trial < 5]))
data$mean_R[data$trial == 5] <- apply(as.array(data$row[data$trial == 5]), 1, function(x) meann(data$reward[data$ID == data$ID[x]&
                                                                                                              data$block == data$block[x] &
                                                                                                              data$chosen == 1& 
                                                                                                              data$trial < 5]))
## calculate deltas
data$delta_mean <- data$mean_L - data$mean_R




# recode choices to be informative vs uninformative and not left vs right (for unequal info cond)
data$chosen[data$info == -1] <- ifelse(data$chosen[data$info == -1] == 0, 1, 0)
data$delta_mean[data$info == 1] <- data$delta_mean[data$info == 1] * -1 # flip sign in direction of informative - uninformative

data$info <- abs(data$info)


# bin mean differences

data$diffBin <- round(data$delta_mean / 10) * 10

unique(data$diffBin)

df <- ddply(data[!is.na(data$diffBin), ], ~diffBin+ Horizon,summarise, chooseUncertain = meann(chosen), n = length(na.omit(chosen))) 

df <- subset(df, n >= 999)
ggplot(df, aes(diffBin, chooseUncertain, fill = as.factor(Horizon), color = as.factor(Horizon))) + geom_point(aes(size = n)) + geom_line()



############# do participants have a general bias towards one side in sam's task? #############

library(lme4)

df <- ddply(sam, ~ID, summarise, chooseRight = meann(chosen))

hist(df$chooseRight, breaks = 50)

t.test(df$chooseRight, mu= 0.5)


library(brms)

brm(chosen ~ V + RU + (V + RU | ID), sam, family = "bernoulli", cores = 2, iter = 500, chains = 2)

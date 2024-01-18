############ data quality checks ##############


library(plyr)
library(ggplot2)
library(jsonlite)
theme_set(theme_classic(base_size = 14))
library(ggridges)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")

load("data/wave1/bandits.Rda")
load("data/wave1/qs.Rda")
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

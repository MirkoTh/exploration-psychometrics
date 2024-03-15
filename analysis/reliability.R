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
restelss2 <- restless

############ translate IDs from session 2 into IDs from session 1 ##########

wave2 <- read.csv("data/wave2/banditLookup.csv")
wave1 <- read.csv("data/wave1/banditLookup.csv")


##### kick out everyone that did not do second part ####


#source("analysis/recovery_utils.R")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}




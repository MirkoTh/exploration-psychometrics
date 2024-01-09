############ visualise results from questionnaire things ###############


library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics")

load("data/pilot/qs.Rda")

dat <- data.frame(qdat)


ggplot(dat, aes(as.numeric(motiv_mem_0))) + geom_histogram() + ggtitle("How motivated were you to do well in the memory games?")+ xlim(c(-0,100.1))


ggplot(dat, aes(as.numeric(motiv_slot_0))) + geom_histogram() + ggtitle("How motivated were you to do well in the slot machine games?")+ xlim(c(-0.1,100.1))

ggplot(dat, aes(as.numeric(age))) + geom_histogram() + ggtitle("age") + xlim(c(18, 35))


ggplot(dat, aes(as.numeric(rt))) + geom_histogram() + ggtitle("time spent on questionnaires in min")



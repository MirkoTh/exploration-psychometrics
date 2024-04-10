############ visualise results from questionnaire things ###############


library(ggplot2)
library(jsonlite)
library(brms)
theme_set(theme_classic(base_size = 14))

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")

load("analysis/qsWave1Full.Rda")
qdat1 <- qdat
load("analysis/qsWave2.Rda")
qdat2 <- qdat

qdat1 <- subset(qdat1, is.element(ID, qdat2$ID))
qdat1$session <- 1
qdat2$session <- 2

qdat <- rbind(qdat1, qdat2)


ggplot(dat, aes(as.numeric(motiv_mem_0))) + geom_histogram() + ggtitle("How motivated were you to do well in the memory games?")+ coord_cartesian(xlim(c(-0,100.1)))


ggplot(dat, aes(as.numeric(motiv_slot_0))) + geom_histogram() + ggtitle("How motivated were you to do well in the slot machine games?")+ xlim(c(-0.1,100.1))

ggplot(dat, aes(as.numeric(age))) + geom_histogram() + ggtitle("age") + xlim(c(18, 35))


ggplot(dat, aes(as.numeric(rt))) + geom_histogram() + ggtitle("time spent on questionnaires in min")



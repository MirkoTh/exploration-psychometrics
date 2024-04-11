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
qdat$session <- factor(qdat$session)

ggplot(qdat, aes(as.numeric(motiv_mem_0), fill = session)) + 
  geom_histogram(position = "identity", alpha = 0.4) + 
  ggtitle("How motivated were you to do well in the memory games?")+
  coord_cartesian(xlim =c(-0,100.1))


ggplot(qdat, aes(as.numeric(motiv_slot_0), fill = session)) + 
  geom_histogram(position = "identity", alpha = 0.4) + 
  ggtitle("How motivated were you to do well in the slot machine games?")+ 
  coord_cartesian(xlim =c(-0,100.1))

ggplot(qdat, aes(age, fill = session)) + 
  geom_histogram(position = "identity", alpha = 0.4) +
  ggtitle("age") 

max(as.numeric(qdat$age))
min(as.numeric(qdat$age))

# someone said sth that is bullshit
qdat$age[as.numeric(qdat$age) > 100] <- NA

ggplot(qdat, aes(as.numeric(age), fill = session)) + 
  geom_histogram(position = "identity", alpha = 0.4) +
  ggtitle("age")

ggplot(qdat, aes(as.numeric(rt), fill = session)) + 
  geom_histogram(position = "identity", alpha = 0.4) + 
  ggtitle("time spent on questionnaires in min")

######## questionnaire score things ##########


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

responses$measure <- qs$Measure[match(responses$Q, qs$Q)]

library(plyr)

maxval <- ddply(responses, ~measure, summarise, max = max(response) )

responses$max <- maxval$max[match(responses$measure, maxval$measure)]
responses$response <- ifelse(responses$reversed == 1, as.numeric(responses$max) - as.numeric(responses$response), as.numeric(responses$response))

avg <- ddply(responses, ~ID+measure+session, summarise, score = mean(response))

ggplot(avg, aes(score, fill = session)) +
  geom_histogram(position = "identity", alpha = 0.4)+
  facet_wrap(vars(measure))

############# prep questionnaire data #################

rm(list = ls())


library(plyr)
library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(brms)
library(ggridges)
theme_set(theme_classic(base_size = 14))
library(here)

session <- 2

source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}


######### prep questionnaire data #############

qdat <- read.csv(sprintf("data/finalQuestionnaireDataSession%i.csv", session))

qs <- read.csv("task/questionnaires.csv", sep = ";")

qs$Q <- paste(qs$Measure, qs$num, sep = "_")

responses <- qdat %>% 
  pivot_longer(cols = colnames(qdat)[is.element(colnames(qdat), qs$Q)], names_to = "Q", values_to = "response") %>% 
  select(ID, response, Q) %>% 
  left_join(qs %>% select(Q, Attention.check, Reverse.coded, sticsaSubscale, num, Measure), by = "Q") %>% 
  subset(Attention.check == 0)


# distinguish panas positive and negative
PANASpos <- c(1,3,5,9,10,12,14,16,17,19)
PANASneg <- c(2,4,6,7,8,11,13,15,18,20)

responses$Measure[grepl("PANAS", responses$Q) & is.element((responses$num+1), PANASpos)] <- "PANASpos"
responses$Measure[grepl("PANAS", responses$Q) & is.element((responses$num+1), PANASneg)] <- "PANASneg"

responses$Measure[responses$sticsaSubscale == "c"] <- "STICSAcog"
responses$Measure[responses$sticsaSubscale == "s"] <- "STICSAsoma"

# reverse back reverse-coded items
maxval <- ddply(responses, ~Measure, summarise, max = max(response) )

responses$max <- maxval$max[match(responses$Measure, maxval$Measure)]
responses$response <- ifelse(responses$Reverse.coded == 1, as.numeric(responses$max) - as.numeric(responses$response), as.numeric(responses$response))

write.csv(responses, sprintf("data/trial_level_questionnaire_session%i.csv", session))

dat_for_efa <- responses %>% 
  select(ID, Q, response) %>% 
  pivot_wider(id_cols = ID, names_from = Q, values_from = response)

write.csv(dat_for_efa, sprintf("analysis/dat_for_efa_s%i.csv", session))

descr_for_efa <- responses %>% 
  select(ID, Q, Measure)

write.csv(descr_for_efa, "analysis/descr_for_efa.csv")

avg <- ddply(responses, ~ID+Measure, summarise, score = mean(response))

avg <- avg[order(avg$ID), ]


save(avg, file = sprintf("analysis/qdat_session%i.Rda", session))

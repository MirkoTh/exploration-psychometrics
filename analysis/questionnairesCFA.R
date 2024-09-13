############## CFA questionnaires #################

rm(list = ls())

library(tidyverse)
library(lavaan)
library(lavaanPlot)


colors <- c( "#66C2A5", "#FC8D62", "white", "#8DA0CB", "#E78AC3")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

######### prep data ############

data <- read.csv("analysis/dat_for_efa_s2.csv")
details <- read.csv("analysis/descr_for_efa.csv")
unique(details$Q[details$measure == "PANASpos"]) # get the positive items which are negatively correlated with anxiety and depression
# we want to flip them such that the whole factor encodes level of negative emotionality and not level of positive emotionality
max(data$PANAS_0)
min(data$PANAS_0)

IDs <- data$ID

data <- data %>% 
  mutate(PANAS_0 = 4 - PANAS_0,
         PANAS_2 = 4 - PANAS_2,
         PANAS_4 = 4 - PANAS_4,
         PANAS_8 = 4 - PANAS_8,
         PANAS_9 = 4 - PANAS_9,
         PANAS_11 = 4 - PANAS_11,
         PANAS_13 = 4 - PANAS_13,
         PANAS_15 = 4 - PANAS_15,
         PANAS_16 = 4 - PANAS_16,
         PANAS_18 = 4 - PANAS_18) %>% 
  select(-c(ID, X))
colnames(data)

# model based on correlation matrix of questionnaires
simple_model <- '
  AxDep =~ PANAS_0 + PANAS_1 + PANAS_2 + PANAS_3 + PANAS_4 + PANAS_5 + PANAS_6 +
  PANAS_7 + PANAS_8 + PANAS_9 + PANAS_10 + PANAS_11 + PANAS_12 + PANAS_13 +
  PANAS_14 + PANAS_15 + PANAS_16 + PANAS_17 + PANAS_18 + PANAS_19 + STICSA_0 +
  STICSA_1 + STICSA_2 + STICSA_3 + STICSA_4 + STICSA_5 + STICSA_6 + STICSA_7 +
  STICSA_8 + STICSA_10 + STICSA_11 + STICSA_12 + STICSA_13 + STICSA_14 + STICSA_15 +
  STICSA_16 + STICSA_17 + STICSA_18 + STICSA_19 + STICSA_20 + STICSA_21 + PHQ_9_0 +
  PHQ_9_1 + PHQ_9_2 + PHQ_9_3 + PHQ_9_4 + PHQ_9_5 + PHQ_9_7 + PHQ_9_8 + PHQ_9_9
  
  Exp =~ BIG_5_0 + BIG_5_1 + BIG_5_2 + BIG_5_3 + BIG_5_4 + BIG_5_5 + CEI_0 +
  CEI_1 + CEI_2 + CEI_3
  
'
  
fit_rug <- sem(simple_model, data = data)
summ <- summary(fit_rug, fit.measures = TRUE, standardized = TRUE)
summ
summ$fit

# plot results
lavaanPlot(
  fit_rug, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)

### medium separtion
medium_model <- '
  mood =~ PANAS_0 + PANAS_1 + PANAS_2 + PANAS_3 + PANAS_4 + PANAS_5 + PANAS_6 +
  PANAS_7 + PANAS_8 + PANAS_9 + PANAS_10 + PANAS_11 + PANAS_12 + PANAS_13 +
  PANAS_14 + PANAS_15 + PANAS_16 + PANAS_17 + PANAS_18 + PANAS_19 
  
  anxDepr =~ STICSA_0 +  STICSA_1 + STICSA_2 + STICSA_3 + STICSA_4 + STICSA_5 + STICSA_6 + STICSA_7 +
  STICSA_8 + STICSA_10 + STICSA_11 + STICSA_12 + STICSA_13 + STICSA_14 + STICSA_15 +
  STICSA_16 + STICSA_17 + STICSA_18 + STICSA_19 + STICSA_20 + STICSA_21 + PHQ_9_0 +
  PHQ_9_1 + PHQ_9_2 + PHQ_9_3 + PHQ_9_4 + PHQ_9_5 + PHQ_9_7 + PHQ_9_8 + PHQ_9_9
  
  Exp =~ BIG_5_0 + BIG_5_1 + BIG_5_2 + BIG_5_3 + BIG_5_4 + BIG_5_5 + CEI_0 +
  CEI_1 + CEI_2 + CEI_3
  
'

fit_rug_mid <- sem(medium_model, data = data)
summ <- summary(fit_rug_mid, fit.measures = TRUE, standardized = TRUE)
#summ
summ$fit

# plot results
lavaanPlot(
  fit_rug_mid, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)


## more complex model

# reload data to have the positive mood items not be reversed anymore
data <- read.csv("analysis/dat_for_efa_s2.csv")

data <- data%>% 
  select(-c(ID, X))

seperate_scales <- '
  AxDep =~ STICSA_0 +
  STICSA_1 + STICSA_2 + STICSA_3 + STICSA_4 + STICSA_5 + STICSA_6 + STICSA_7 +
  STICSA_8 + STICSA_10 + STICSA_11 + STICSA_12 + STICSA_13 + STICSA_14 + STICSA_15 +
  STICSA_16 + STICSA_17 + STICSA_18 + STICSA_19 + STICSA_20 + STICSA_21 + PHQ_9_0 +
  PHQ_9_1 + PHQ_9_2 + PHQ_9_3 + PHQ_9_4 + PHQ_9_5 + PHQ_9_7 + PHQ_9_8 + PHQ_9_9
  
  negMood =~ PANAS_1 + PANAS_3 +  PANAS_5 + PANAS_6 +
  PANAS_7 + PANAS_10 +  PANAS_12 + PANAS_13 +
  PANAS_14 + PANAS_17 +  PANAS_19
  
  posMood =~ PANAS_0 + PANAS_2+  PANAS_4 + PANAS_8 + PANAS_9+  PANAS_11+
  PANAS_13+ PANAS_15+ PANAS_16+ PANAS_18
  
  Exp =~ BIG_5_0 + BIG_5_1 + BIG_5_2 + BIG_5_3 + BIG_5_4 + BIG_5_5 + CEI_0 +
  CEI_1 + CEI_2 + CEI_3
  
'
fit_rug_sep <- sem(seperate_scales, data = data)
summ <- summary(fit_rug_sep, fit.measures = TRUE, standardized = TRUE)

summ$fit

# plot results
lavaanPlot(
  fit_rug_sep, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)






factor_scores <- as_tibble(predict(fit_rug_sep))
factor_scores$ID <- IDs
write.csv(factor_scores, "analysis/questionnaire_factors_s2.csv")



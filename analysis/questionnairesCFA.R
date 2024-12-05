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
  select(-c(ID, X)) %>% 
  scale() %>% 
  as.data.frame()
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
  select(-c(ID, X))%>% 
  scale() %>% 
  as.data.frame()

library(moments)

skewness_values <- sapply(data, skewness, na.rm = TRUE)
skewness_values

data_filtered <- data %>%
  select(which(abs(skewness_values) < 2))

which(abs(skewness_values) > 2)

seperate_scales_filtered <- '
  AxDep =~ STICSA_0 +
  STICSA_1 + STICSA_2 + STICSA_3 + STICSA_4 + STICSA_5 + STICSA_6 + STICSA_7 +
  STICSA_8 + STICSA_10 + STICSA_11 + STICSA_12 + STICSA_13 + STICSA_14 + STICSA_15 +
  STICSA_16 + STICSA_17 + STICSA_18 + STICSA_19 + STICSA_20 + STICSA_21 + PHQ_9_0 +
  PHQ_9_1 + PHQ_9_2 + PHQ_9_3 + PHQ_9_4 + PHQ_9_5 + PHQ_9_7
  
  posMood =~ PANAS_0 + PANAS_2+  PANAS_4 + PANAS_8 + PANAS_9+  PANAS_11+
  PANAS_13+ PANAS_15+ PANAS_16+ PANAS_18
  
  Exp =~ BIG_5_0 + BIG_5_1 + BIG_5_2 + BIG_5_3 + BIG_5_4 + BIG_5_5 + CEI_0 +
  CEI_1 + CEI_2 + CEI_3
  
'
# no point in having negative mood factor bc all items but one are too skewed

seperate_scales <- '
  AxDep =~ STICSA_0 +
  STICSA_1 + STICSA_2 + STICSA_3 + STICSA_4 + STICSA_5 + STICSA_6 + STICSA_7 +
  STICSA_8 + STICSA_10 + STICSA_11 + STICSA_12 + STICSA_13 + STICSA_14 + STICSA_15 +
  STICSA_16 + STICSA_17 + STICSA_18 + STICSA_19 + STICSA_20 + STICSA_21 + PHQ_9_0 +
  PHQ_9_1 + PHQ_9_2 + PHQ_9_3 + PHQ_9_4 + PHQ_9_5 + PHQ_9_7 + PHQ_9_8 + PHQ_9_9
  
  negMood =~ PANAS_1 + PANAS_3 +  PANAS_5 + PANAS_6 +
  PANAS_7 + PANAS_10 +  PANAS_12 +
  PANAS_14 + PANAS_17 +  PANAS_19
  
  posMood =~ PANAS_0 + PANAS_2+  PANAS_4 + PANAS_8 + PANAS_9+  PANAS_11+
  PANAS_13+ PANAS_15+ PANAS_16+ PANAS_18
  
  Exp =~ BIG_5_0 + BIG_5_1 + BIG_5_2 + BIG_5_3 + BIG_5_4 + BIG_5_5 + CEI_0 +
  CEI_1 + CEI_2 + CEI_3
  
'
fit_rug_sep <- sem(seperate_scales_filtered, data = data_filtered)
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

############### these factors are uninterpretable, let's do PCA ###################


### investigating the data
data <- read.csv("analysis/dat_for_efa_s2.csv")
IDs <- data$ID
data <- data%>% 
  select(-c(ID, X)) 

## scale all items to be between 0 and 1
maxes <- data.frame(data[1,])
maxes[ ,grepl("PANAS", colnames(maxes))] <- 4
maxes[ ,grepl("STICSA", colnames(maxes))] <- 3
maxes[ ,grepl("BIG", colnames(maxes))] <-  4
maxes[, grepl("PHQ", colnames(maxes))] <- 3
maxes[, grepl("CEI", colnames(maxes))] <- 5

# append maxes to make sure max of each column is grand max possible on that item

data <- rbind(data, maxes)
data <- data.frame(sapply(data, function(x) x / max(x)))
data <- data[1:(nrow(data)-1), ]# remove maxes again
summary(data)

library(moments)

skewness_values <- sapply(data, skewness, na.rm = TRUE)
skewness_values

data_filtered <- data %>%
  select(which(abs(skewness_values) < 2))

which(abs(skewness_values) > 2)


library(factoextra)

pca_result <- prcomp(data_filtered, center = T, scale. =T)

fviz_eig(pca_result) # Scree plot
fviz_pca_ind(pca_result) # Individuals plot
fviz_pca_var(pca_result) # Variables plot

summary(pca_result)

# Kaiser criterion
eigenvalues <- get_eigenvalue(pca_result)
retained_components <- sum(eigenvalues$eigenvalue > 1)
retained_components


# saveing the result for the sake of comparing it to other solutions but it is actually quite uninterpretable

factor_scores <- predict(pca_result, newdata = data_filtered) %>% 
  as.data.frame() %>% 
  mutate(ID = IDs)

write.csv(factor_scores, file = "analysis/PCA_item_level.csv")

## this is all aweful, what if we get rid off PANAS?
data <- read.csv("analysis/dat_for_efa_s2.csv")
data <- data%>% 
  select(-c(ID, X, grep("PANAS", colnames(.))))%>% 
  scale() %>% 
  as.data.frame()

colnames(data)

pca_result <- prcomp(data, center = T, scale. =T)

fviz_eig(pca_result) # Scree plot
fviz_pca_ind(pca_result) # Individuals plot
fviz_pca_var(pca_result) # Variables plot

summary(pca_result)

# Kaiser criterion
eigenvalues <- get_eigenvalue(pca_result)
retained_components <- sum(eigenvalues$eigenvalue > 1)
retained_components




#### exploring the variances in the data

vars <- data %>% 
  mutate(ind = 1:nrow(.)) %>% 
  pivot_longer(cols = -ind, names_to = "item", values_to = "score") %>% 
  group_by(item) %>% 
  summarise(variance = var(score),
            mean = mean(score))

############## exploratory factor analysis ##################

data <- read.csv("analysis/dat_for_efa_s2.csv")
IDs <- data$ID
data <- data%>% 
  select(-c(ID, X)) %>% 
  scale() %>% 
  as.data.frame()

summary(data)

library(moments)

skewness_values <- sapply(data, skewness, na.rm = TRUE)
skewness_values

data_filtered <- data %>%
  select(which(abs(skewness_values) < 2))

which(abs(skewness_values) > 2)

ev <- eigen(cor(data))
plot(1:length(ev$values), ev$values)

sum(ev$values > 1)

library(psych)
for (nFac in 1:10){
  fa_result <- fa(r = as.matrix(data_filtered), nfactors = nFac, rotate = "oblimin")
  print(sprintf("Factors: %i;RMSEA = %.4f; CFI = %.3f; TLI = %.3f", nFac, fa_result$RMSEA[1], fa_result$CFI, fa_result$TLI))
  
}

## this is not a good fit but we will still save the best fitting version (according to elbow) for the sake of comparison

fa_result <- fa(r = as.matrix(data_filtered), nfactors = 6, rotate = "oblimin")

factor_scores <- as.data.frame(factor.scores(data_filtered, fa_result)$scores) %>% 
  mutate(ID = IDs)

write.csv(factor_scores, "analysis/EFA_factor_scores_item_level_s2.csv")

################## cfa based on compound scores ############

library(lavaan)
library(lavaanPlot)

data <- read.csv("analysis/questionnaireScores.csv") %>% 
  subset(session == 2, -c(session)) %>% 
  pivot_wider(id_cols = ID, values_from = score, names_from = measure) %>% 
  subset(!is.na(BIG_5))



simple_model <- '
  AxDep =~  STICSAcog + STICSAsoma + PHQ_9
  
  posMood =~ PANASpos
  
  negMood =~PANASneg
  
  Exp =~ BIG_5 + CEI
  
'

fit_rug <- sem(simple_model, data = data)
summ <- summary(fit_rug, fit.measures = TRUE, standardized = TRUE)
#summ
summ$fit

# plot results
lavaanPlot(
  fit_rug, coefs = TRUE, covs = TRUE, sig = TRUE, stars = "covs",
  stand = TRUE,
  edge_options = list(color = "grey"), 
  node_options = list(shape = "box", fontname = "Helvetica")
)


factor_scores <- as_tibble(predict(fit_rug))
factor_scores$ID <- data$ID
write.csv(factor_scores, "analysis/CFA_compound_questionnaire_factors_s2.csv")


####### efa based on compound scores ##########
load("analysis/allOfItSession2.Rda")

data <- allOfIt %>% 
  pivot_wider(id_cols = ID, values_from = value, names_from = measure) %>% 
  select(c(BIG_5, CEI, PANASpos, PANASneg, STICSAcog, STICSAsoma, PHQ_9, ID)) %>% 
  subset(!is.na(BIG_5)) %>% 
  scale() %>% 
  as.data.frame()

IDs <- data$ID

data <- data %>% 
  select(-ID)


summary(data)

library(moments)

skewness_values <- sapply(data, skewness, na.rm = TRUE)
skewness_values


ev <- eigen(cor(data))
plot(1:length(ev$values), ev$values)

sum(ev$values > 1)

library(psych)
for (nFac in 1:7){
  fa_result <- fa(r = as.matrix(data), nfactors = nFac, rotate = "oblimin")
  print(sprintf("Factors: %i;RMSEA = %.4f; CFI = %.3f; TLI = %.3f", nFac, fa_result$RMSEA[1], fa_result$CFI, fa_result$TLI))
  
}

# best seems to be 2 factors
fa_result <- fa(r = as.matrix(data), nfactors = 2, rotate = "oblimin")
fa_result

factor_scores <- as.data.frame(factor.scores(data, fa_result)$scores) %>% 
  mutate(ID = IDs)

save(factor_scores, file = "analysis/EFA_factor_scores_questionnaires_s2.Rda")
write.csv(factor_scores, "analysis/EFA_factor_scores_questionnaires_s2.csv")

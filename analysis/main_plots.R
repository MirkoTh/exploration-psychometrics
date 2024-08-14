########### main results plots ############


rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(here)
theme_set(theme_bw(base_size = 14))

colors <- c( "#66C2A5", "#FC8D62", "white")
############ test retest reliability ###############


load("analysis/bandits/reliability_parameters.Rda")

cors <- cors %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "V", "info" = "RU"), 
         horizon = ifelse(is.na(horizon), "long", horizon)) %>% 
  subset(horizon == "long") %>% 
  subset(select = -horizon)



p2 <- ggplot(cors, aes(cor, predictor)) + 
  geom_vline(aes(xintercept = 0.5), color = "grey") +
  geom_vline(aes(xintercept = 0.75), color = "grey", linetype = "dotted") +
  geom_point(, size = 2) +
  coord_cartesian(xlim = c(0,1))+
  labs(title = "Reliability of model parameters",
       x = "correlation between session 1 and session2",
       y = element_blank()) +
  facet_wrap(vars(task))+
  theme(legend.position = "none")

p2


############ fixed effects #########


fixed <- readRDS("analysis/bandits/allFixed.rds") %>% 
  mutate(horizon = ifelse(is.na(horizon), "long", horizon),
         predictor = recode(predictor, "delta_mean" = "V", "info" = "RU"),
         task = recode(task, "sam" = "2AB"))  %>% 
  subset(horizon == "long" & !grepl("ntercept", predictor))


ggplot(fixed, aes(predictor, Estimate,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none")+
  labs(title = "Fixed effects across tasks and sessions",
       x = "Model parameter",
       y = "Parameter estimate ± 95% HDI")



########### heatmap correlations with questionnaires and wm ###########

load("analysis/external_validity_cors.Rda") 

cors$x <- row.names(cors)
cors <- cors %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor")

# cors$y <- factor(cors$y,
#                  labels = c("CEI" = "exploration", 
#                                         "BIG_5" = "openness", 
#                                         "STICSAcog" = "cog. anxiety", 
#                                         "STICSAsoma" = "soma. anxiety", 
#                                         "PHQ_9" = "depression",
#                             "PANASneg" = "neg. mood",
#                             "PANASpos" = "pos. mood",
#                                         "OS_recall_0" = "oper. span",
#                                         "SS_recall_0" = "sym. span",
#                                         "WMU_recall_0" = "updating"))

ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "correlation of task measures and questionnaire scores",
                                                                         x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






########### main results plots ############


rm(list = ls())

library(tidyverse)
library(ggplot2)
#library(jsonlite)
library(here)
theme_set(theme_bw(base_size = 14))

colors <- c( "#66C2A5", "#FC8D62", "white", "#8DA0CB", "#E78AC3")


source("analysis/recovery_utils.R")

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

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
  mutate(predictor = recode(predictor, "delta_mean" = "Value-guided",
                            "info" = "Directed",
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "VTU" = "Random"),
         task = recode(task, "sam" = "Two-Armed", "horizon" = "Horizon", "restless" = "Restless"),
         session = recode(session, `1` = "Session 1", `2` = "Session 2"))  %>% 
  subset(!grepl("ntercept", predictor))


## for aggregating across models we first need to adjust the models intercept wise: Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson’s method. Tutorials in Quantitative Methods for Psychology, 1(1), 42–45. https://doi.org/10.20982/tqmp.01.1.p042

fixed$predictor <- factor(fixed$predictor, levels = c("Value-guided", "Directed", "Random"))
fixed$task <- factor(fixed$task, levels = c("Horizon", "Two-Armed", "Restless"))

ggplot(fixed, aes(predictor, estimate_corrected,fill = predictor)) + geom_col()+
  geom_errorbar(aes(ymin = l_corrected, ymax = u_corrected), width = 0.25)+
  scale_fill_brewer(palette  = "Set2") +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(title = "Fixed effects across tasks and sessions",
       x = element_blank(),
       y = "Parameter estimate ± 95% CI")+
  geom_hline(yintercept = 0, linetype = "dotdash")



########### heatmap correlations with questionnaires and wm ###########

load("analysis/external_validity_cors_2abUCB.Rda") 

cors$x <- row.names(cors)
cors <- cors %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor") %>% 
  mutate(y = recode(y,
    "CEI" = "Exploration",
    "BIG_5" = "Openness",
    "STICSAcog" = "Cognitive anxiety",
    "STICSAsoma" = "Somatic anxiety",
    "PHQ_9" = "Depression",
    "PANASneg" = "Negative mood",
    "PANASpos" = "Positive mood",
    "OS_recall_0" = "Operation span",
    "SS_recall_0" = "Symmetry span",
    "WMU_recall_0" = "Updating"
  ),
  x = recode(x,
    "restless_V" = "Value-guided restless",
    "restless_RU" = "Directed restless",
    "restless_Pswitch" = "P(switch) restless",
    "restless_Poptimal" = "P(optimal) restless",
    "horizon_Pswitch" = "P(switch) horizon",
    "horizon_Poptimal" = "P(optimal) horizon",
    "horizon_info" = "Directed horizon",
    "horizon_delta_mean" = "Value-guided horizon",
    "2AB_VTU" = "Random 2AB",
    "2AB_V" = "Value-guided 2AB",
    "2AB_RU" = "Directed 2AB",
    "2AB_Pswitch" = "P(switch) 2AB",
    "2AB_Poptimal" = "P(optimal) 2AB",
  ))


cors$x <- factor(cors$x, levels = c("Random 2AB", "Directed restless","Directed 2AB","Directed horizon", 
                            "Value-guided restless", "Value-guided 2AB", "Value-guided horizon", 
                            "P(switch) restless", "P(switch) 2AB","P(switch) horizon",
                            "P(optimal) restless", "P(optimal) 2AB", "P(optimal) horizon"))

cors$y  <-  factor(cors$y, levels = c("Exploration", "Openness",
                                      "Cognitive anxiety", "Somatic anxiety", "Depression",
                                      "Negative mood", "Positive mood",
                                      "Operation span", "Symmetry span", "Updating"))
ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "Correlation of task measures and questionnaire scores",
                                                                         x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))




##################### P(switch) over trials #################

horizon <- load_and_prep_bandit_data(1)$horizon
sam <- load_and_prep_bandit_data(1)$sam
restless <- load_and_prep_bandit_data(1)$restless

h <- horizon %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1),
         Condition = ifelse(info == 0, "equal", "unequal")) %>% 
  subset(trial >4) %>% 
  mutate(group_mean = meann(switch)) %>% 
  group_by(ID) %>% 
  mutate(subject_mean = meann(switch)) %>% 
  group_by(ID, trial, Condition) %>% 
  summarise(Pswitch = meann(switch) - mean(subject_mean) + mean(group_mean)) %>% # adjust subject means for group means so the error bars are within subject
  group_by(Condition, trial) %>% 
  summarise(pswitch = meann(Pswitch),
            se = se(Pswitch))

s <- sam %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1),
         Condition = factor(cond, levels = c("FF", "FS", "SF", "SS"),
                            labels = c("both fluctating",
                                       "fluctuating-stable",
                                       "stable-fluctuating",
                                       "both stable")),
         group_mean = meann(switch)) %>% 
  group_by(ID) %>% 
  mutate(subject_mean = meann(switch)) %>% 
  group_by(ID, trial, Condition) %>% 
  summarise(Pswitch = meann(switch) - mean(subject_mean) + mean(group_mean)) %>% # adjust subject means for group means so the error bars are within subject
  group_by(Condition, trial) %>% 
  summarise(pswitch = meann(Pswitch),
            se = se(Pswitch))

r <- restless %>% 
  mutate(prev = c(NA,chosen[1:(nrow(.)-1)]),
         prev = ifelse(trial == 1, NA, prev),
         switch = ifelse(prev == chosen, 0, 1)) %>% 
  group_by(trial) %>% 
  summarise(pswitch = meann(switch))


ho <- ggplot(h, aes(trial, pswitch, color = Condition)) +
  geom_point()+
  geom_line()+
  geom_linerange(aes(ymin = pswitch -1.96*se, ymax = pswitch + 1.96*se))+
  scale_color_manual(values = colors[1:2])+
  labs(title = "Horizon task",
       y = "P(switch) ± 95%CI")+
  theme(legend.position = c(1,1),
        legend.justification = c(1.01,1.01))+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))

ho

sa <- ggplot(s, aes(trial, pswitch, color = Condition)) +
  geom_point()+
  geom_line()+
  geom_linerange(aes(ymin = pswitch -1.96*se, ymax = pswitch + 1.96*se))+
  scale_color_manual(values = colors[c(1,2,4,5)])+
  labs(title = "Two-armed bandit",
       y = "P(switch) ± 95%CI")+
  theme(legend.position = c(1,1),
        legend.justification = c(1.01,1.01))+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))

sa

re <- ggplot(r, aes(trial, pswitch)) +
  geom_point()+
  geom_line()+
  scale_color_manual(values = colors[c(1,2,4,5)])+
  labs(title = "Restless bandit",
       y = "P(switch)")+
  scale_x_continuous(n.breaks = 21)+
  scale_y_continuous(n.breaks = 10, limits = c(0,1))+
  coord_cartesian(xlim = c(1,200))

re

ggpubr::ggarrange(ho, sa, re, ncol = 3, widths = c(6,10, 20))

########### convergent validity #########

cors <- readRDS("analysis/bandits/allParams_2abUCB.Rds") %>% 
  subset(predictor != "Intercept" & session == 1) %>% 
  mutate(predictor = recode(predictor,
                            "V" = "Value-guided",
                            "RU" = "Directed",
                            "VTU" = "Random",
                            "delta_mean" = "Value-guided",
                            "info" = "Directed"),
         task = recode(task, "sam" = "Two-armed",
                       "horizon" = "Horizon",
                       "restless" = "Restless"),
         variable = paste(predictor, task)) %>% 
  pivot_wider(id_cols = "ID", names_from = "variable", values_from = "estimate") %>% 
  subset(select = -ID) %>% 
  cor(use="pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor")
  

ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + 
  scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + 
  labs(title = "Convergent validity of parameter estimates",
        x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_hline(yintercept = 3.5, color = "white", size = 3)+
 # geom_hline(yintercept = 4.5, color = "white", size = 3)+
  geom_vline(xintercept = 3.5, color = "white", size = 3)#+
 # geom_vline(xintercept = 4.5, color = "white", size = 3)


############# correlation of parameters and comprehension attempts ################
load("analysis/comprehensionWave1.Rda")

cors <- readRDS("analysis/bandits/allParams.Rds") %>% 
  subset(predictor != "Intercept" & session == 1) %>% 
  mutate(variable = paste(predictor, task)) %>% 
  pivot_wider(id_cols = c("ID", "task"), names_from = "variable", values_from = "estimate") %>% 
  left_join(comprehension%>% pivot_wider(id_cols = c("ID"),
                                         names_from = "task", values_from = "compAttempts")
            , by = "ID") %>% 
  select(-c(ID,task)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -c(x), names_to = "y", values_to = "cor") %>% 
  subset(is.element(x, c("horizon", "sam", "restless")) & !is.element(y, c("horizon", "sam", "restless")))
  
ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + 
  scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + 
  labs(title = "Correlation of model parameters and comprehension attempts",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

########### correlation of latent factors and comprehension attempts #########
load("analysis/comprehensionWave2.Rda")


cors <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
               72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
               95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
               113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
              133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
              170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
              197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
              213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
              254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
              294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
              320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(comprehension%>% pivot_wider(id_cols = c("ID"),
                                         names_from = "task", values_from = "compAttempts")
            , by = "ID") %>% 
  select(-c(ID)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -c(x), names_to = "y", values_to = "cor") %>% 
  subset(is.element(x, c("horizon", "sam", "restless")) & !is.element(y, c("horizon", "sam", "restless")))



ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + 
  scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + 
  labs(title = "Correlation of model parameters and comprehension attempts",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## investigating the strongest of these correlations


load("analysis/comprehensionWave2.Rda")


df <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(comprehension%>% pivot_wider(id_cols = c("ID"),
                                         names_from = "task", values_from = "compAttempts")
            , by = "ID")

ggplot(df, aes(sam, G.Value.Guided)) + geom_jitter() +
  geom_smooth(method = "lm")
ggplot(df, aes(sam, G.Directed)) + geom_jitter() +
  geom_smooth(method = "lm")

################ external validity using only latents ############

cors <- read.csv("analysis/behavioral-tasks-latents-s2.csv") %>% 
  mutate(ID = c(2, 3, 4, 6, 7 , 10 , 11  ,12 , 13  ,14,  16 , 20  ,21 , 23 , 24,
                25,  26 , 27 , 28 , 29 , 31 , 32 , 33 , 34 , 35 , 36 , 37,  40  ,
                48 , 49,  52  ,55 , 56,  59 , 60 , 61 , 63 , 65 , 69 , 71,
                72 , 73 , 74 , 76 , 77 , 78,  79 , 83  ,84  ,85 , 90 , 93,  94 ,
                95,  96 , 97,  98  ,99 ,100, 101 ,102 ,103 ,105 ,108 ,109, 110,
                113 ,114, 117, 118, 119, 120, 122, 123, 124, 125, 128 ,129, 131, 132,
                133, 134, 135, 137, 142, 149 ,151, 154, 162, 163, 165, 166, 168 ,169,
                170 ,171 ,172, 175 ,176, 180 ,182 ,183, 185, 187 ,188 ,190, 193 ,195,
                197, 198 ,199, 200 ,202 ,203, 204, 205, 206, 207, 208, 209,
                213 ,216 ,219, 220, 222, 229 ,232, 233, 239, 243, 244, 245, 246 ,251,
                254, 261 ,262, 269, 270, 271, 273, 274 ,276 ,279, 284, 288, 290 ,291,
                294 ,297, 298 ,299 ,302 ,303 ,306, 308, 309, 313, 316, 317,
                320, 322, 323, 324, 325, 326, 332, 333, 338 ,343, 346, 347, 349, 351, 352)) %>% 
  left_join(read.csv("analysis/questionnaire_factors_s2.csv") %>% select(-X), by = "ID") %>% 
  select(-c(ID)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% 
  mutate(x = rownames(.)) %>% 
  pivot_longer(cols = -c(x), names_to = "y", values_to = "cor") %>% 
  mutate(x = factor(x, levels = c("G.Value.Guided", "G.Directed", "Exp", "posMood", "negMood", "AxDep", "WMC"),
                    labels = c("Value-guided", "Directed", "Self-reported exploration", "Positive mood", "Negative mood", "Anxiety/depression", "Working memory")),
         y = factor(y, levels = c("WMC", "AxDep", "negMood", "posMood", "Exp", "G.Directed", "G.Value.Guided"),
                    labels = c("Working memory", "Anxiety/depression", "Negative mood", "Positive mood", "Self-reported exploration", "Directed", "Value-guided")))


ggplot(cors, aes(x,y, fill = cor)) + geom_raster() + 
  scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + 
  labs(title = "External validity on the latent level",
       x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


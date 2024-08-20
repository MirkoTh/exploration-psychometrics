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

load("analysis/external_validity_cors.Rda") 

cors$x <- row.names(cors)
cors <- cors %>% 
  pivot_longer(cols = -x, names_to = "y", values_to = "cor") %>% 
  mutate(y = recode(y,
    "CEI" = "Exploration",
    "BIG_5" = "Openness",
    "STICSAcog" = "Cog. anxiety",
    "STICSAsoma" = "Soma. anxiety",
    "PHQ_9" = "Depression",
    "PANASneg" = "Neg. mood",
    "PANASpos" = "Pos. mood",
    "OS_recall_0" = "Oper. span",
    "SS_recall_0" = "Sym. span",
    "WMU_recall_0" = "Updating"
  ),
  x = recode(x,
    "restless_V" = "Value-g. restless",
    "restless_RU" = "Directed restless",
    "restless_Pswitch" = "P(switch) restless",
    "restless_Poptimal" = "P(optimal) restless",
    "horizon_Pswitch" = "P(switch) horizon",
    "horizon_Poptimal" = "P(optimal) horizon",
    "horizon_info" = "Directed horizon",
    "horizon_delta_mean" = "Value-g. horizon",
    "2AB_VTU" = "Random 2AB",
    "2AB_V" = "Value-g. 2AB",
    "2AB_RU" = "Directed 2AB",
    "2AB_Pswitch" = "P(switch) 2AB",
    "2AB_Poptimal" = "P(optimal) 2AB",
  ))


cors$x <- factor(cors$x, levels = c("Random 2AB", "Directed restless","Directed 2AB","Directed horizon", 
                            "Value-g. restless", "Value-g. 2AB", "Value-g. horizon", 
                            "P(switch) restless", "P(switch) 2AB","P(switch) horizon",
                            "P(optimal) restless", "P(optimal) 2AB", "P(optimal) horizon"))

cors$y  <-  factor(cors$y, levels = c("Exploration", "Openness",
                                      "Cog. anxiety", "Soma. anxiety", "Depression",
                                      "Neg. mood", "Pos. mood",
                                      "Oper. span", "Sym. span", "Updating"))
ggplot(cors, aes(y, x, fill = cor)) + geom_raster() + scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = c(-1,1))+
  geom_label(aes(label = round(cor, digits = 2)), fill = "white") + labs(title = "Correlation of task measures and questionnaire scores",
                                                                         x = element_blank(), y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))






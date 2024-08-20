############ fixed effect plots ################

library(here)
library(ggplot2)
se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
theme_set(theme_bw(base_size = 14))

colors <- c( "#66C2A5", "#FC8D62", "white")

########### load fixed effects #########

fixed <- readRDS("analysis/bandits/allFixed.rds") %>% 
  mutate(predictor = recode(predictor, "delta_mean" = "V", "info" = "RU"),
         task = recode(task, "sam" = "2AB"))  %>% 
  subset(!grepl("ntercept", predictor))


ggplot(fixed, aes(predictor, Estimate,fill = predictor)) + geom_col(color = "black")+
  geom_errorbar(aes(ymin = `l-95% CI`, ymax = `u-95% CI`), width = 0.25)+
  scale_fill_manual(values = colors) +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none")+
  labs(title = "Fixed effects across tasks and sessions",
       x = "Model parameter",
       y = "Parameter estimate ± 95% HDI")

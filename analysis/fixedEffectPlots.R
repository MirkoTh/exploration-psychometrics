############ fixed effect plots ################

library(here)
library(ggplot2)
se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
theme_set(theme_bw(base_size = 14))

colors <- c( "#66C2A5", "#FC8D62", "white")

########### load fixed effects #########

fixed <- read.csv("analysis/bandits/allFixedEffects.csv")
#random <- read.csv("analysis/bandits/AllModelParameters.csv")

# flip the sign to have larger values mean more preference
fixed$estim <- -1*fixed$estim
fixed$lower <- -1*fixed$lower
fixed$upper <- -1*fixed$upper

restless <- readRDS("analysis/bandits/4arlb-maps-hierarchical.RDS")

colnames(fixed)
restFixed <- data.frame(X = NA,
                        task = "restless",
                        param = rep(c("RU", "V"), 2),
                        session = rep(1:2, each = 2),
                        estim = NA,
                        lower = NA,
                        upper = NA)


restFixed$estim <- apply(as.array(1:nrow(restFixed)), 1, function(x) colMeans(restless[ ,grepl(restFixed$session[x], colnames(restless)) & grepl(restFixed$param[x], colnames(restless), ignore.case = T)]) )
SE <- apply(as.array(1:nrow(restFixed)), 1, function(x) se(unlist(restless[ ,grepl(restFixed$session[x], colnames(restless)) & grepl(restFixed$param[x], colnames(restless), ignore.case = T)]) ))
restFixed$lower <- restFixed$estim - 1.96*SE
restFixed$upper <- restFixed$estim + 1.96*SE

fixed <- rbind(fixed, restFixed)

fixed$task <- factor(fixed$task, levels = c("horizon_5", "horizon_10", "sam", "restless"), labels = c("Horizon short", "Horizon long", "2AB", "Restless"))

ggplot(fixed, aes(param, estim,fill = param)) + geom_col()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)+
  scale_fill_manual(values = colors[1:2]) +
  facet_grid(rows = vars(task), cols =vars(session), scales = "free")+
  theme(legend.position = "none")+
  labs(title = "Fixed effects across tasks and sessions",
       x = "Model parameter",
       y = "Parameter estimate ± 95% HDI")

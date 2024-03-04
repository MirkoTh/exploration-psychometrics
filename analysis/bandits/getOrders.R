############# get who viewed what order ###############


library(jsonlite)

setwd("/Users/kristinwitte/Documents/GitHub/exploration-psychometrics")


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}
### load data ########

session = 1


files_all = list.files(path = "data/wave1/bandits")
files <- files_all[!grepl("temp", files_all)]

order <- data.frame(PID = rep(NA, length(files)),
                    order = NA)

counter <- 0
for (i in files){
  counter <- counter +1
  temp <- fromJSON(paste("data/wave1/bandits/", i, sep = ""))
  order$PID[counter] <- temp$subjectID
  order$order[counter] <- temp$taskOrder
}


###### test whether really same order in both tp #####

order2$order1 <- order$order[na.omit(match(order2$PID, order$PID))]

#####



# add 1 row for testing

me <- data.frame(PID = "kristin-test",
                 order = 6)

order <- rbind(order,me)

orders <- list(c(order$PID), c(order$order))

json <- toJSON(orders)

write(json, file = "task/orders.json")

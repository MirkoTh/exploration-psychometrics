library(tidyverse)


# set session_id to 1 or 2
# stimulus sets for all tasks are generated for that session
session_id <- 2
my_two_seeds <- c(39737632, 8567389)


#setwd("/Users/kwitte/Documents/GitHub/exploration-psychometrics/wm-tasks")
#source("utils-gen-stim.R")

set.seed(my_two_seeds[session_id])
library(permute)


# Operation Span ----------------------------------------------------------


maxSetSize <- 8
minSetSize <- 4
n_reps <- 3
SetSizes <- seq(minSetSize, maxSetSize, by = 1)
nTrials <- length(SetSizes) * n_reps
SetSizes_shuffled <- sample(rep(SetSizes, n_reps), nTrials, replace = FALSE)

possibleLetters <- c(
  "B", "C", "D", "F", "G", "H", "K", "L", "M", 
  "N", "P", "R", "S", "T", "V", "W", "X", "Z"
)

l_letters <- map(SetSizes_shuffled, ~ sample(possibleLetters, .x, replace = FALSE))

sample_size_sufficient <- (maxSetSize + 1) * nTrials
possibleOperations <- c(" + ", " - ")

eqsCorrect <- sample(c("true", "false"), sample_size_sufficient, replace = TRUE)
eqsCorrect_demo <- sample(c("true", "false"), 100, replace = TRUE)
operations <- sample(possibleOperations, sample_size_sufficient, replace = TRUE)
ansDiffs <- sample(c(1, 2), sample_size_sufficient, replace = TRUE)
coinFlips <- sample(c("true", "false"), sample_size_sufficient, replace = TRUE)

num1plus <- floor(runif(sample_size_sufficient, min = 1, max = 9))
num2plus <- floor(runif(sample_size_sufficient, min = 1, max = 9))
num1minus <- floor(runif(sample_size_sufficient, min = 1, max = 9))
num2minus <- floor(runif(sample_size_sufficient, min = 1, max = num1minus))


eqsCorrect_str <- reduce(c("[", str_c(eqsCorrect, ", ")), str_c)
str_c(str_replace_all(eqsCorrect_str, "(e)(,) $", replacement="\\1"), "]")

eqsCorrect_demo_str <- reduce(c("[", str_c(eqsCorrect_demo, ", ")), str_c)
str_c(str_replace_all(eqsCorrect_demo_str, "(e)(,) $", replacement="\\1"), "]")

operations_str <- reduce(c("[", str_c("'", operations, "', ")), str_c)
str_c(str_replace_all(operations_str, "(,) $", replacement=""), "]")

ansDiffs_str <- reduce(c("[", str_c(ansDiffs, ", ")), str_c)
str_c(str_replace(ansDiffs_str, ", $", replacement=""), "]")

coinFlips_str <- reduce(c("[", str_c(coinFlips, ", ")), str_c)
str_c(str_replace_all(coinFlips_str, "(e)(,) $", replacement="\\1"), "]")

l_str <- map(
  list(num1plus, num2plus, num1minus, num2minus), 
  ~ reduce(c("[", str_c(.x, ", ")), str_c)
)
map(l_str, ~ str_c(str_replace(.x, ", $", replacement=""), "]"))


SetSizes_shuffled_str <- reduce(c("[", str_c(SetSizes_shuffled, ", ")), str_c)
str_c(str_replace_all(SetSizes_shuffled_str, "(,) $", replacement=""), "]")

l_letters_str <- map(l_letters, ~ reduce(c("['", str_c(.x, "', '")), str_c))
df_lists <- map(l_letters_str, ~ str_c(str_replace_all(.x, "(,) $", replacement=""), "]")) %>%
  reduce(rbind)
rownames(df_lists) <- rep("", nrow(df_lists))
l_lists <- str_c("[", reduce(str_c(reduce(df_lists, c), ", "), str_c), "]")
tmp <- str_c(str_replace_all(l_lists, "(, ')(\\])", replacement=, "\\2"), "]")
str_replace(tmp, "(, \\])(\\])$", replacement=, "\\2")



# Symmetry Span -----------------------------------------------------------

minSetSize <- 3 #// starting length of each trial (i.e., min number of letters in a trial)
maxSetSize <- 6 #// ending length of each trial (i.e., max number of letters in a trial)
repSet <- 3

SetSizes <- rep(seq(minSetSize, maxSetSize, by = 1), repSet)
demo_mem <- c(3, 4, 4)
demo_comb <- c(3, 3, 4)
SetSizes <- c(demo_mem, demo_comb, sample(SetSizes, length(SetSizes), replace = FALSE))

matrix_pos <- crossing(row = seq(0, 3, by = 1), col = seq(0, 3, by = 1))
locate <- function(x, mat) {
  l_sample <- sample(1:nrow(mat), x, replace = FALSE)
  d_prep <- mat[l_sample, ]
  indices <- unlist(pmap(d_prep, ~ str_c("[", .x, ",", .y, "],")))
  out <- str_c("[", str_replace(reduce(indices, str_c), ",$", ""), "]")
  return(list(
    d_prep, out
  ))
}
l2_mat_full <- map(SetSizes, locate, mat = matrix_pos)
l_matrix_pos <- map(l2_mat_full, 2)
m_matrix_pos <- map(l2_mat_full, 1)
out <- map(l_matrix_pos, ~ str_c("[", str_replace(reduce(.x, str_c), ",$", ""), "]"))
l_matrix_pos <- str_c("[", reduce(str_c(reduce(l_matrix_pos, c), ", "), str_c), "]")


half_matrix <- crossing(row = seq(0, 7, by = 1), col = seq(0, 3, by = 1))
ns_dark_possible <- c(17, 18, 19)
ns_dark <- sample(ns_dark_possible, sum(SetSizes), replace = TRUE)

l2_half <- map(ns_dark, locate, mat = half_matrix)
l_half_matrix_pos <- map(l2_half, 2)
out <- map(l_half_matrix_pos, ~ str_c("[", str_replace(reduce(.x, str_c), ",$", ""), "]"))
l_half_matrix_pos <- str_c("[", reduce(str_c(reduce(l_half_matrix_pos, c), ", "), str_c), "]")

m_half_matrix_pos <- map(l2_half, 1)
pSymmetric <- sample(c(0, 1), sum(SetSizes), replace = TRUE, prob = c(.5, .5))

m_boxes_available <- map(
  m_half_matrix_pos,
  ~ left_join(half_matrix, .x %>% mutate(avl = 1), by = c("row", "col")) %>%
    filter(is.na(avl)) %>% select(-avl)
)

numberBoxesDifferent <- sample(c(3, 4), sum(SetSizes), replace = TRUE)

ns_remain <- ns_dark - numberBoxesDifferent

l2_remain <- map2(ns_remain, m_half_matrix_pos, ~ locate(.x, .y))
l_boxes_remain <- map(l2_remain, 2)
m_boxes_remain <- map(l2_remain, 1)

l2_change <- map2(numberBoxesDifferent, m_boxes_available, ~ locate(.x, .y))
l_boxes_change <- map(l2_change, 2)
m_boxes_change <- map(l2_change, 1)

m_boxes_modified <- map2(m_boxes_remain, m_boxes_change, ~ rbind(.x, .y))
indices <- map(m_boxes_modified, ~ unlist(pmap(.x, ~ str_c("[", .x, ",", .y, "],"))))
out <- map(indices, ~ str_c("[", str_replace(reduce(.x, str_c), ",$", ""), "]"))
l_boxes_modified <- str_c("[", reduce(str_c(reduce(out, c), ", "), str_c), "]")

# setSizes_symm
SetSizes_str <- reduce(c("[", str_c(SetSizes, ", ")), str_c)
str_c(str_replace_all(SetSizes_str, "(,) $", replacement=""), "]")

# blackBoxNumbers
ns_dark_str <- reduce(c("[", str_c(ns_dark, ", ")), str_c)
str_c(str_replace_all(ns_dark_str, "(,) $", replacement=""), "]")

# blackBoxesAll
str_replace(l_half_matrix_pos, "(, )(\\])$", replacement=, "\\2")
# blackBoxesModified
str_replace(l_boxes_modified, "(, )(\\])$", replacement=, "\\2")

# pSymmetric
pSymmetric_str <- reduce(c("[", str_c(pSymmetric, ", ")), str_c)
str_c(str_replace_all(pSymmetric_str, "(,) $", replacement=""), "]")

# selections_symm
str_replace(l_matrix_pos, "(, )(\\])$", replacement=, "\\2")



# WM Updating -------------------------------------------------------------


set_size <- 5
n_upd_steps <- 7
possibleNumbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
nTrials_update <- 20
nTrials_immediate <- 5
nPracticeTrials <- 2


totalTrials <- nPracticeTrials + nTrials_update + nTrials_immediate

trial_type <- c(
  # two practice trials
  "update", "immediate",
  # shuffled immediate and update trials
  sample(
    c(rep("update", nTrials_update), rep("immediate", nTrials_immediate)), 
    size = nTrials_update + nTrials_immediate)
)

initial_locations <- seq(0, set_size - 1, by = 1)
initial_set <- map(rep(set_size, nTrials_update + 1), ~ sample(possibleNumbers, .x))
locations_update <- map(rep(n_upd_steps, nTrials_update + 1), ~ sample(initial_locations, .x, replace = TRUE))
items_replace <- map(rep(n_upd_steps, nTrials_update + 1), ~ sample(possibleNumbers, .x, replace = TRUE))

final_set <- initial_set
for (i in 1:(nTrials_update + 1)) {
  for (j in 1:n_upd_steps) {
    final_set[[i]][locations_update[[i]][[j]] + 1] <- items_replace[[i]][[j]]
  }
}

immediate_set <- map(rep(set_size, nTrials_immediate + 1), ~ sample(possibleNumbers, .x))

immediate_set_str <- str_c("[", map(immediate_set, ~ paste(str_c(map_chr(.x, ~ str_c(.x, ", "))), collapse = "")), "]")
initial_set_str <- str_c("[", map(initial_set, ~ paste(str_c(map_chr(.x, ~ str_c(.x, ", "))), collapse = "")), "]")
locations_update_str <- str_c("[", map(locations_update, ~ paste(str_c(map_chr(.x, ~ str_c(.x, ", "))), collapse = "")), "]")
items_replace_str <- str_c("[", map(items_replace, ~ paste(str_c(map_chr(.x, ~ str_c(.x, ", "))), collapse = "")), "]")
final_set_str <- str_c("[", map(final_set, ~ paste(str_c(map_chr(.x, ~ str_c(.x, ", "))), collapse = "")), "]")



trial_type_str <- reduce(c("[", str_c("'", trial_type, "', ")), str_c)
str_c(str_replace_all(trial_type_str, "(,) $", replacement=""), "]")

str_c("[", str_replace_all(paste(immediate_set_str, collapse = ","), "(, )(])", replacement = "\\2"), "]")
str_c("[", str_replace_all(paste(initial_set_str, collapse = ","), "(, )(])", replacement = "\\2"), "]")
str_c("[", str_replace_all(paste(locations_update_str, collapse = ","), "(, )(])", replacement = "\\2"), "]")
str_c("[", str_replace_all(paste(items_replace_str, collapse = ","), "(, )(])", replacement = "\\2"), "]")
str_c("[", str_replace_all(paste(final_set_str, collapse = ","), "(, )(])", replacement = "\\2"), "]")




# 4-armed restless bandit (4ARLB) -----------------------------------------



source("wm-tasks/utils-gen-stim.R")

# change seeds for 4arlb
my_two_seeds <- c(997733, 5247222)
set.seed(my_two_seeds[session_id])
mu_init <- rnorm(4, 50, 3)
tbl_4a_rlb <- generate_restless_bandits(
  sigma_xi_sq = 7.84, sigma_epsilon_sq = 16, mu1 = mu_init, 
  lambda = .9836, nr_trials = 200, center_decay = 50
  )

ggplot(tbl_4a_rlb %>% pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")


## make practice trials

tbl_4a_rlb_practice <- tbl_4a_rlb

tbl_4a_rlb$block <- 2
tbl_4a_rlb_practice$block <- 1
tbl_4a_rlb_practice <- subset(tbl_4a_rlb_practice, trial_id >= 100)
tbl_4a_rlb_practice$trial_id <- 1:nrow(tbl_4a_rlb_practice)

rewards <- rbind(tbl_4a_rlb_practice, tbl_4a_rlb)

## save as json
nBlocks <- 2 # 1 for practice
nTrials <- c(10,200)# first is practice

ls = list()
for (i in 1:nBlocks){
  block = list()
  for (j in 1:nTrials[i]){
    re <- c(round(rewards$`Arm 1`[rewards$block == i & rewards$trial_id == j]), 
            round(rewards$`Arm 2`[rewards$block == i & rewards$trial_id == j]),
            round(rewards$`Arm 3`[rewards$block == i & rewards$trial_id == j]),
            round(rewards$`Arm 4`[rewards$block == i & rewards$trial_id == j]))
    block[[j]] <- re

  }
  ls[[i]] <- block
}

# to check practice and test rewards via plotting
# # ls[[1]] and 1:10
# # ls[[2]] and 1:200
# reduce(ls[[2]], rbind) %>% as.data.frame() %>% mutate(trial_id = 1:200) %>%
#   rename("Arm 1" = "V1", "Arm 2" = "V2", "Arm 3" = "V3", "Arm 4" = "V4") %>%
#   pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)) %>%
#   ggplot(aes(trial_id, value, group = name)) +
#   geom_line(aes(color = name), size = .75) +
#   scale_color_viridis_d(name = "") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_bw() +
#   labs(x = "Trial ID", y = "Reward")


library(jsonlite)

json = toJSON(ls)

write(json, paste("../task/rewards4ARB", session_id, ".json", sep = ""))


######### gen rewards for testing the "random" walks

my_two_seeds <- c(997733, 5247222)
set.seed(my_two_seeds[1])
mu_init <- rnorm(4, 50, 3)
tbl_4a_rlb1 <- generate_restless_bandits(
  sigma_xi_sq = 7.84, sigma_epsilon_sq = 16, mu1 = mu_init, 
  lambda = .9836, nr_trials = 200, center_decay = 50
)

ggplot(tbl_4a_rlb1 %>% pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")


# second random walk

set.seed(my_two_seeds[2])
mu_init <- rnorm(4, 50, 3)
tbl_4a_rlb2 <- generate_restless_bandits(
  sigma_xi_sq = 7.84, sigma_epsilon_sq = 16, mu1 = mu_init, 
  lambda = .9836, nr_trials = 200, center_decay = 50
)

ggplot(tbl_4a_rlb2 %>% pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")


## make practice trials

tbl_4a_rlb_practice <- generate_restless_bandits(
  sigma_xi_sq = 7.84, sigma_epsilon_sq = 16, mu1 = mu_init, 
  lambda = .9836, nr_trials = 200, center_decay = 50
)

tbl_4a_rlb1$block <- 2
tbl_4a_rlb2$block <- 3
tbl_4a_rlb_practice$block <- 1
tbl_4a_rlb_practice <- subset(tbl_4a_rlb_practice, trial_id >= 100)
tbl_4a_rlb_practice$trial_id <- 1:nrow(tbl_4a_rlb_practice)

rewards <- rbind(tbl_4a_rlb_practice, tbl_4a_rlb1, tbl_4a_rlb2)

## save as json
nBlocks <- 3 # 1 for practice, 2 walks we are testing
nTrials <- c(10,200, 200)# first is practice

ls = list()
for (i in 1:nBlocks){
  block = list()
  for (j in 1:nTrials[i]){
    re <- c(round(rewards$`Arm 1`[rewards$block == i & rewards$trial_id == j]), 
            round(rewards$`Arm 2`[rewards$block == i & rewards$trial_id == j]),
            round(rewards$`Arm 3`[rewards$block == i & rewards$trial_id == j]),
            round(rewards$`Arm 4`[rewards$block == i & rewards$trial_id == j]))
    block[[j]] <- re
    
  }
  ls[[i]] <- block
}


library(jsonlite)

json = toJSON(ls)

write(json, paste("../pilot4arb/rewards4ARB", session_id, ".json", sep = ""))


# Sam's task -------------------------------------------------------------


my_two_seeds <- c(934742, 445941)# process of arriving at these seeds can be found in the commented out parts below
# 
# found = F
# while (found == F){
#   seed <- round(runif(1, 1, 1000000))
#set.seed(seed)
set.seed(my_two_seeds[session_id])
nBlocks = 31
nTrials = 10
# same for all subjects so nSubs = 1

# 5% mean diff 1&0
# 15% mean diff 2
# 60% mean diff [3-5]
# 15% mean diff [6-8]
# 5% mean diff [9-23]

# create differences in initial reward means

diffs <- data.frame(diff = seq(0,23),
                    prob = c(5,5,15,60,60,60,15,15,15,rep(5,15)))

# normalise probabilities by number of elements in that bin so that it represents the actual probability of that difference being drawn and not probability of a number in that bin being drawn
diffs$prob[1:2] <- diffs$prob[1:2]/2
diffs$prob[4:6] <- diffs$prob[4:6]/3
diffs$prob[7:9] <- diffs$prob[7:9]/3
diffs$prob[10:24] <- diffs$prob[10:24]/15

# make them actual probabilities not percentages
diffs$prob <- diffs$prob/100


# sample start rewards differences

rewards <- data.frame(trial = rep(1:nTrials, nBlocks),
                      block = rep(1:nBlocks, each = nTrials),
                      reward1 = NA,
                      reward2 = NA,
                      cond = NA)

rewards$reward1[rewards$trial == 1] <- runif(nBlocks, 10, 90)

rewards$reward2[rewards$trial == 1] <- ifelse(runif(nBlocks)>0.5, rewards$reward1[rewards$trial == 1] + sample(diffs$diff, nBlocks, replace = T, prob = diffs$prob), # randomise whether 1 or 2 is higher
                                              rewards$reward1[rewards$trial == 1] - sample(diffs$diff, nBlocks, replace = T, prob = diffs$prob))

# make condition random
library(permute)

for (i in seq(11,nTrials*(nBlocks-1), 3*nTrials)){
  conds <- c("FF", "SS", "FS")
  inds <- rep(shuffle(3), each = nTrials)
  rewards$cond[i:(i+3*nTrials-1)] <- conds[inds]
}

# add condition for practice round

rewards$cond[rewards$block == 1] <- "FS"

SF <- sample(unique(rewards$block[rewards$cond == "FS"]), size = 5)

rewards$cond[is.element(rewards$block, SF)] <- "SF"

table(rewards$cond) # should be 100 FF, 100 SS, 60 FS (50 + practice), 50 SF

## create remaining trials

# stable condition:

rewards$reward1[rewards$trial != 1 & (rewards$cond == "SS" | rewards$cond == "SF")] <- rep(rewards$reward1[rewards$trial == 1 & (rewards$cond == "SS" | rewards$cond == "SF")], each = nTrials-1)
rewards$reward2[rewards$trial != 1 & (rewards$cond == "SS" | rewards$cond == "FS")] <- rep(rewards$reward2[rewards$trial == 1& (rewards$cond == "SS" | rewards$cond == "FS")], each = nTrials-1)

# random walk
for (i in 1:nBlocks){
  if (rewards$cond[rewards$block == i & rewards$trial == 1] == "SS"){next} # for stable cond nothing to do
  else if (rewards$cond[rewards$block == i & rewards$trial == 1] == "FF"){# for both fluctuating random walk of both
    done = F
    while (done == F){
      for (j in 2:nTrials){
        rewards$reward1[rewards$trial == j & rewards$block == i] <- rnorm(1,rewards$reward1[rewards$trial == j-1 & rewards$block == i],4)
        rewards$reward2[rewards$trial == j & rewards$block == i] <- rnorm(1,rewards$reward2[rewards$trial == j-1 & rewards$block == i],4)
      }
      # calculate mean distance between rewards
      dist = mean(abs(rewards$reward1[rewards$block == i] - rewards$reward2[rewards$block == i]))
      print(dist)
      if (dist < 15) {done = T} # if average difference in rewards is still below 20, keep it, else, do this again
    }
    
  } else if (rewards$cond[rewards$block == i & rewards$trial == 1] == "SF"){# right fluctuating
    done = F
    while (done == F){
      for (j in 2:nTrials){
        rewards$reward2[rewards$trial == j & rewards$block == i] <- rnorm(1,rewards$reward2[rewards$trial == j-1 & rewards$block == i],4)
      }
      # calculate mean distance between rewards
      dist = mean(abs(rewards$reward1[rewards$block == i] - rewards$reward2[rewards$block == i]))
      print(dist)
      if (dist < 15) {done = T} # if average difference in rewards is still below 20, keep it, else, do this again
    }
  }else if (rewards$cond[rewards$block == i & rewards$trial == 1] == "FS"){# left fluctating
    done = F
    while (done == F){
      for (j in 2:nTrials){
        rewards$reward1[rewards$trial == j & rewards$block == i] <- rnorm(1,rewards$reward1[rewards$trial == j-1 & rewards$block == i],4)
      }
      # calculate mean distance between rewards
      dist = mean(abs(rewards$reward1[rewards$block == i] - rewards$reward2[rewards$block == i]))
      print(dist)
      if (dist < 15) {done = T} # if average difference in rewards is still below 20, keep it, else, do this again
    }
    
  }
  
}

# add noise and round to integers
rewards$reward1 <- round(rewards$reward1 + rnorm(nrow(rewards), 0, 1), 0)
rewards$reward2 <- round(rewards$reward2 + rnorm(nrow(rewards), 0, 1), 0)


######## check whether it is all nice and balanced
rewards$diff <- rewards$reward1 - rewards$reward2

# is 1 arm better on average?

mean(rewards$reward1)
mean(rewards$reward2)

# do the conditions differ on average

ddply(rewards, ~cond, summarise,  rew1 = mean(reward1), rew2 = mean(reward2))

df <- pivot_longer(rewards, cols = c(3:4), names_to = "arm", values_to = "reward")

df$fluc <- NA
df$fluc[df$cond == "SS"] <- "S"
df$fluc[df$cond == "FF"] <- "F"
df$fluc[df$cond == "SF"] <- ifelse(df$arm[df$cond == "SF"] == "reward1", "S", "F")
df$fluc[df$cond == "FS"] <- ifelse(df$arm[df$cond == "FS"] == "reward2", "S", "F")

t <- summary(lmer(reward ~ arm* fluc + (1|block), df))

print(t$coefficients)
# if (mean(t$coefficients[2:4,5]< 0.5) == 0) { found = T; print(seed)}# p of these differences being observed when rewards from same distribution as to be >0.5
# }

# seed1 <- seed
# seed2 <- seed


######## save the rewards

ls = list()
for (i in 1:nBlocks){
  block = list()
  for (j in 1:nTrials){
    re <- c(rewards$reward1[rewards$block == i & rewards$trial == j], rewards$reward2[rewards$block == i & rewards$trial == j])
    block[[j]] <- re
    
  }
  ls[[i]] <- block
}

library(jsonlite)

json = toJSON(ls)

write(json, paste("../task/rewardsSam", session_id, ".json" , sep = ""))

# save the rewards df that also contains condition

save(rewards, file =paste("../task/rewardsSam", session_id, ".Rda" , sep = ""))

################ Horizon task -----------------------------------------------


my_two_seeds <- c(39737632, 8567389)

set.seed(my_two_seeds[session_id])

data <- read.csv("ZallerEtAl.csv")

nBlocks = 81 # 1 extra for practice
nTrials = 10 # just always sample 10 trials even if it is a short horizon and the round is over after 5 trials bc simpler this way

sub <- sample(unique(data$Subject),1)

diffs <- c(-30, -20, -12, -8, -4, 4, 8, 12, 20, 30)

rewards <- data.frame(block = rep(1:nBlocks, each = nTrials),
                      trial = rep(1:nTrials, nBlocks),
                      reward1 = rep(c(40, data$mu_L[data$Subject == sub & data$Trial == 1]), each = nTrials), # their rewards are perfectly balanced, just different order for every subject
                      reward2 = rep(NA, nBlocks*nTrials),
                      infoCond = rep(c(-1, data$Info[data$Subject == sub & data$Trial == 1]), each = nTrials), # stealing infoCond and Horizon from Zaller et al. bc it is a pain to get it as perfectly orthogonal as they did without having a pattern
                      Horizon = rep(c(10, data$Horizon[data$Subject == sub & data$Trial == 1]), each = nTrials))


rewards$diff <- NA
rewards$diff[rewards$Horizon == 5 & rewards$infoCond == 0] <- rep(rep(diffs, each = 2)[shuffle(length(diffs)*2)], each = nTrials)
rewards$diff[rewards$Horizon == 10 & rewards$infoCond == 0] <- rep(rep(diffs, each = 2)[shuffle(length(diffs)*2)], each = nTrials)
rewards$diff[rewards$Horizon == 5 & rewards$infoCond == 1] <- rep(diffs[shuffle(length(diffs))], each = nTrials)
rewards$diff[rewards$Horizon == 5 & rewards$infoCond == -1] <- rep(diffs[shuffle(length(diffs))], each = nTrials)
rewards$diff[rewards$Horizon == 10 & rewards$infoCond == 1] <- rep(diffs[shuffle(length(diffs))], each = nTrials)
rewards$diff[rewards$Horizon == 10 & rewards$infoCond == -1] <- rep(c(20, diffs[shuffle(length(diffs))]), each = nTrials)# prepend large difference for practice
  
rewards$reward2 <- rewards$reward1 + rewards$diff

noise_var <- 17 # variance inferred from dataset by Zaller et al

rewards$reward1 <- round(rewards$reward1 + rnorm(nrow(rewards), 1, sqrt(noise_var)))
rewards$reward2 <- round(rewards$reward2 + rnorm(nrow(rewards), 1, sqrt(noise_var)))


######### test whether this looks good and balanced now

rewards$mean_diff <- NA
rewards$mean_diff[rewards$trial == 5] <- apply(as.array(1:nBlocks), 1, function(x) mean(rewards$reward1[rewards$block == x & rewards$trial < 5] -
                                                                                          rewards$reward2[rewards$block == x & rewards$trial <5]))

table(rewards$diff, rewards$Horizon, rewards$infoCond)

table(round(rewards$mean_diff[rewards$trial == 5]/10)*10, rewards$Horizon[rewards$trial == 5], rewards$infoCond[rewards$trial == 5])

ddply(rewards, ~Horizon+infoCond, summarise, rew1 = mean(reward1), rew2 = mean(reward2))

# test for statistical significance of any potential unwanted patterns
library(lmerTest)
library(tidyr)
df <- pivot_longer(rewards, cols = 3:4, names_to = "arm", values_to = "reward")

# balancing of means between conditions and between left and right
summary(lmer(reward ~ arm * Horizon + arm* infoCond + Horizon*infoCond + (1|block), df))

summary(lm(mean_diff ~Horizon * infoCond , rewards[rewards$trial == 5, ]))# here also intercept should be nowhere near significant



## save rewards

ls = list()
for (i in 1:nBlocks){
  block = list()
  for (j in 1:nTrials){
      re <- c(rewards$reward1[rewards$block == i & rewards$trial == j], rewards$reward2[rewards$block == i&rewards$trial == j])
      block[[j]] <- re
  }
  ls[[i]] <- block
}

library(jsonlite)

json = toJSON(ls)

write(json, paste("../task/rewardsHorizon", session_id,".json", sep = ""))

## save fixed choices

fixed <- list()

library(permute)

for (i in 1:nBlocks){
  if (rewards$infoCond[rewards$block == i & rewards$trial == 1] == -1){ # right more info
    choices <- c(0,1,1,1)
  } else if (rewards$infoCond[rewards$block == i& rewards$trial == 1] == 0){# equal info
    choices <- c(1,1,0,0)
  }else if (rewards$infoCond[rewards$block == i& rewards$trial == 1] == 1){# left more info
    choices <- c(0,0,0,1)
  }
  fixed[[i]] <- choices[shuffle(4)]
}

json = toJSON(fixed)

write(json, paste("../task/fixedChoices", session_id,".json", sep = ""))

## save horizon

horizon <- rewards$Horizon[rewards$trial == 1]

json = toJSON(horizon)
write(json, paste("../task/Horizon", session_id,".json", sep = ""))


save(rewards, file = paste("../task/rewardsHorizon", session_id, ".Rda", sep =""))

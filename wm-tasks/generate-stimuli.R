library(tidyverse)


# set session_id to 1 or 2
# stimulus sets for all tasks are generated for that session
session_id <- 2
my_two_seeds <- c(39737632, 8567389)
set.seed(my_two_seeds[session_id])






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

minSetSize <- 2 #// starting length of each trial (i.e., min number of letters in a trial)
maxSetSize <- 5 #// ending length of each trial (i.e., max number of letters in a trial)
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


set_size <- 4
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



# control for
# distribution of average difference between four arms -> not too close
# distribution of differences of second-best arm - best arm --> not too easy
# proportion of one arm being the best arm across all trials

source("wm-tasks/utils-gen-stim.R")


as_required <- FALSE
tbl_4a_rlb <- generate_rl_bandits_as_required(FALSE, session_id)

ggplot(tbl_4a_rlb %>% pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")

plot(tbl_4a_rlb$min_diff_to_max)
hist(tbl_4a_rlb$min_diff_to_max)

plot(tbl_4a_rlb$avg_difference)
hist(tbl_4a_rlb$avg_difference)

cor(tbl_4a_rlb$min_diff_to_max, tbl_4a_rlb$avg_difference)

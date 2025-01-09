rm(list = ls())

# Load Packages and Data --------------------------------------------------


library(tidyverse)
library(grid)
library(gridExtra)
library(rutils)

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R", "utils/retest-utils.R"
)
walk(dirs_homegrown, source)

# date conversion depends on locale
Sys.setlocale("LC_ALL","English")

tbl_retest <- read_csv("data/bob-wilson/TEST-RETEST-2015_HorizonTask.csv") %>%
  select(-c(repeatNumber, age, gender))
# cols to drop: repeatNumber (always 1), age (range from 11-23), gender
tbl_vaccine <- read_csv("data/bob-wilson/VACCINE-2015_HorizonTask.csv") %>%
  select(-c(age, gender))
# cols to drop: age, gender
# question: what does repeatNumber mean?


l_retest <- overview(tbl_retest)
l_vaccine <- overview(tbl_vaccine)
tbl_retest_elapsed <- time_elapsed_retest(l_retest$tbl_full) %>% mutate(Study = "Retest")
tbl_vaccine_elapsed <- time_elapsed_vaccine(l_vaccine$tbl_full) %>% mutate(Study = "Vaccine")
tbl_elapsed_both <- tbl_retest_elapsed %>% rbind(tbl_vaccine_elapsed)

# plot test-retest times
tbl_elapsed_both %>%
  ggplot(aes(time_duration)) + 
  geom_histogram(fill = "#66C2A5", color = "black", binwidth = 1) +
  geom_label(
    data = tbl_elapsed_both %>% count(Study), 
    aes(x = 12, y = 22, label = str_c("n = ", n))) +
  facet_wrap(~ Study) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 15, by = 2), expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Days Elapsed", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) +
  coord_cartesian(xlim = c(0, 15))

tbl_both_full_long <- bring_rewards_to_long(l_retest, l_vaccine)
tbl_both_full_long <- running_means(tbl_both_full_long)

only_first_frc <- tbl_both_full_long %>% filter(trial == 5)

only_first_frc <- only_first_frc %>% 
  mutate(
    # running calculations
    which_lower_mean = factor(is_lower_mean_2, labels = c(1, 2)),
    which_higher_info = factor(is_higher_info_2, labels = c(1, 2)),
    is_lower_mean = choice == which_lower_mean,
    is_higher_info = choice == which_higher_info,
    # ground truth (mean), and forced-choice info diff (info)
    which_lower_mean_gt = factor(m2 < m1, labels = c(1, 2)),
    which_higher_info_gt = factor(uc, labels = c(1, "equal", 2)),
    is_lower_mean_gt = choice == which_lower_mean_gt,
    is_higher_info_gt = as.character(choice) == as.character(which_higher_info_gt),
    # numeric variables
    mean_diff = running_mean_2 - running_mean_1,
    mean_diff_bin = cut(mean_diff, c(-Inf, -25, seq(-16, 16, by = 4), 25, Inf))
  )

# plot for information is different than for unequal means,
# because unequal info can only be calculated in 1-3 or 3-1 trials
# i.e., not in 2-2 trials

agg_means <- only_first_frc %>% 
  #filter(n_choices_1 == 2 & n_choices_2 == 2) %>%
  group_by(
    expt_name, subjectID, sessionNum, gameLength
  ) %>%
  summarize(
    is_lower_mean_avg = mean(is_lower_mean),
  ) %>% ungroup()

agg_info <- only_first_frc %>% 
  filter(which_higher_info_gt != "equal") %>%
  group_by(
    expt_name, subjectID, sessionNum, gameLength
  ) %>%
  summarize(
    is_higher_info_avg = mean(is_higher_info)
  ) %>% ungroup()

agg_means_info <- left_join(agg_means, agg_info, by = c("expt_name", "subjectID", "sessionNum", "gameLength"))

agg_plot <- agg_means_info %>% pivot_longer(c(is_lower_mean_avg, is_higher_info_avg)) %>%
  pivot_wider(values_from = value, names_from = sessionNum, names_prefix = "session_")
agg_plot$name <- factor(agg_plot$name, labels = c("p (high info)", "p (low mean)"))
agg_plot$gameLength <- factor(agg_plot$gameLength, labels = c("Horizon 1", "Horizon 6"))

ggplot(agg_plot, aes(session_1, session_2)) +
  geom_abline() +
  geom_point(shape = 1, size = 3) +
  geom_smooth(method = "lm") +
  facet_grid(name ~ gameLength) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(.1, .9, by = .2)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Session 1", y = "Session 2") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  )
agg_plot$expt_name <- factor(agg_plot$expt_name, labels = c("Retest", "Vacc."))

rutils::grouped_agg(agg_plot, c(expt_name, gameLength, name), c(session_1, session_2)) %>%
  rename(
    measure = name,
    S1 = mean_session_1,
    S2 = mean_session_2
    ) %>%
  pivot_longer(c("S1", "S2")) %>%
  ggplot(aes(gameLength, value, group = gameLength)) +
  geom_hline(yintercept = .5, color = "grey", linetype = "dotdash", linewidth = 1) +
  geom_col(aes(fill = gameLength)) +
  facet_grid(interaction(expt_name, name, sep = "-")  ~ measure) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0), breaks = c(.1, .5, .9)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Horizon", y = "P (Choice)") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_fill_brewer(palette = "Set2", name = "")

only_first_frc %>%
  group_by(
    expt_name, subjectID, sessionNum, gameLength, mean_diff_bin
  ) %>%
  summarize(
    prop_high_mean = mean(choice == 2),
    n = n()
  ) %>%
  group_by(
    expt_name, sessionNum, gameLength, mean_diff_bin
  ) %>%
  summarize(
    prop_high_mean = mean(prop_high_mean),
    n = sum(n)
  ) %>%
  ggplot(aes(mean_diff_bin, prop_high_mean, group = gameLength)) +
  geom_line(aes(color = gameLength)) +
  geom_point(aes(color = gameLength, size = n)) +
  facet_wrap(expt_name ~ sessionNum)
s

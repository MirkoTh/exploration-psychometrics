library(tidyverse)
library(grid)
library(gridExtra)

dirs_homegrown <- c(
  "utils/analysis-utils.R", "utils/plotting-utils.R", 
  "utils/modeling-utils.R"
)
walk(dirs_homegrown, source)

my_two_seeds <- c(997733, 5247222)
set.seed(my_two_seeds[1])
mu_init <- rnorm(4, 50, 3)
tbl_4a_rlb <- generate_restless_bandits(
  sigma_xi_sq = 7.84, sigma_epsilon_sq = 16, mu1 = mu_init, 
  lambda = .9836, nr_trials = 200, center_decay = 50
)

pl_restless <- ggplot(tbl_4a_rlb %>% pivot_longer(c(`Arm 1`, `Arm 2`, `Arm 3`, `Arm 4`)), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  theme_bw() +
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Reward") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "")


tbl_horizon <- tibble(
  trial_id = rep(1:10, 2),
  arm = rep(c("Arm 1", "Arm 2"), each = 10),
  val = rep(c(41, 57), each = 10)
)

tbl_choices_equal <- tibble(
  trial_id = 1:10,
  val = c(41, 57, 41, 57, 41, 57, 57, 57, 57, 57),
  arm = c("Arm 1", "Arm 2", "Arm 1", "Arm 2", "Arm 1", "Arm 2", "Arm 2", "Arm 2", "Arm 2", "Arm 2"),
  Choice = c(rep("Forced", 4), rep("Free", 6))
)


pl_horizon_equal <- ggplot(tbl_horizon, aes(trial_id, val, group = arm)) +
  geom_line(aes(color = arm), size = .75) +
  geom_vline(xintercept = 4.5, linetype = "dotdash") +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = arm)) +
  geom_point(data = tbl_choices_equal, aes(trial_id, val, color = arm, shape = Choice), size = 5) +
  geom_segment(aes(x=1, y=27, xend=4, yend=27),
               arrow = arrow(length=unit(.45, 'cm'), ends = "both"), lineend = "round", linejoin = "round", size = .5) +
  geom_segment(aes(x = 10, xend = 10, y = 24, yend = 35), size = .5) +
  geom_segment(aes(x = 5, xend = 5, y = 24, yend = 35), size = .5) +
  geom_label(aes(x = 5, y = 35, label = "Short Horizon"), label.size = .5) +
  geom_label(aes(x = 9.5, y = 35, label = "Long Horizon"), label.size = .5) +
  geom_label(aes(x = 2.5, y = 31, label = "Forced"), label.size = .5) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Mean Reward") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "") +
  scale_shape_manual(values = c(7, 1)) +
  coord_cartesian(ylim = c(25, 62), xlim = c(.5, 10.5)) +
  guides(color = "none", shape = "none")

tbl_choices_unequal <- tibble(
  trial_id = 1:10,
  val = c(41, 41, 41, 57, 41, 57, 57, 57, 57, 57),
  arm = c("Arm 1", "Arm 1", "Arm 1", "Arm 2", "Arm 1", "Arm 2", "Arm 2", "Arm 2", "Arm 2", "Arm 2"),
  Choice = c(rep("Forced", 4), rep("Free", 6))
)

pl_horizon_unequal <- ggplot(tbl_horizon, aes(trial_id, val, group = arm)) +
  geom_line(aes(color = arm), size = .75) +
  geom_vline(xintercept = 4.5, linetype = "dotdash") +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = arm)) +
  geom_point(data = tbl_choices_unequal, aes(trial_id, val, color = arm, shape = Choice), size = 5) +
  geom_segment(aes(x=1, y=27, xend=4, yend=27),
               arrow = arrow(length=unit(.45, 'cm'), ends = "both"), lineend = "round", linejoin = "round", size = .5) +
  geom_segment(aes(x = 10, xend = 10, y = 24, yend = 35), size = .5) +
  geom_segment(aes(x = 5, xend = 5, y = 24, yend = 35), size = .5) +
  geom_label(aes(x = 5, y = 35, label = "Short Horizon"), label.size = .5) +
  geom_label(aes(x = 9.5, y = 35, label = "Long Horizon"), label.size = .5) +
  geom_label(aes(x = 2.5, y = 31, label = "Forced"), label.size = .5) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Mean Reward") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "") +
  scale_shape_manual(values = c(7, 1)) +
  coord_cartesian(ylim = c(25, 62), xlim = c(.5, 10.5))

tbl_2armed <- tibble(
  trial_id = rep(1:10, 2),
  arm = rep(c("Arm 1", "Arm 2"), each = 10),
  val = c(rep(39, 10), c(35, 37, 32, 28, 35, 42, 44, 47, 46, 51))
  )

pl_2armed <- ggplot(tbl_2armed, aes(trial_id, val, group = arm)) +
  geom_line(aes(color = arm), size = .75) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = arm)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(1, 9, by = 2)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial ID", y = "Mean Reward") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_brewer(palette = "Set2", name = "") +
  scale_shape_manual(values = c(7, 1)) +
  coord_cartesian(ylim = c(20, 60))


pl_task_setup <- 
  arrangeGrob(
    arrangeGrob(
      pl_horizon_equal + ggtitle("Horizon: Equal Information"), pl_horizon_unequal + ggtitle("Horizon: Unequal Information"), nrow = 2, heights = c(.46, .54)
    ), pl_2armed + ggtitle("Two-Armed"), pl_restless + ggtitle("Restless"), nrow = 1
  )
grid.draw(pl_task_setup)

save_my_pdf_and_tiff(pl_task_setup, "figures/figures-ms/submission-1/task-setup", 19, 8)





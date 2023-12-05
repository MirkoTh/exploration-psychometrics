plot_pc_against_ss <- function(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci, tbl_wmu_ss_agg_ci, is_recall = TRUE) {
  pl <- ggplot(rbind(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci), aes(set_size, prop_correct, group = 1)) +
    geom_errorbar(aes(x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci), width = .5) +
    geom_line() +
    geom_point(color = "white", size = 4) +
    geom_point() +
    facet_wrap(~ task) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    labs(x = "Set Size", y = "Proportion Correct") + 
    theme(strip.background = element_rect(fill = "white"))
  
  if (is_recall) {
    pl <- pl +
      geom_errorbar(
        data = tbl_wmu_ss_agg_ci, aes(
          x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci
        ), width = .5
      ) +
      geom_point(data = tbl_wmu_ss_agg_ci, aes(set_size, prop_correct, group = 1)) 
  }
  
  return(pl)
}

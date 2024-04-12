plot_pc_against_ss <- function(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci, tbl_wmu_ss_agg_ci, is_recall = TRUE) {
  pl <- ggplot(
    rbind(tbl_os_ss_agg_ci, tbl_ss_ss_agg_ci) %>% mutate(session_id = as.character(as.numeric(as.character(session_id)) + 1)),
    aes(set_size, prop_correct, group = session_id)
    ) +
    geom_hline(yintercept = 1, linetype = "dotdash", alpha = .2) +
    geom_errorbar(aes(x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci, color = session_id), width = .5) +
    geom_line(aes(color = session_id)) +
    geom_point(color = "white", size = 4) +
    geom_point(aes(color = session_id)) +
    facet_wrap( ~ task) +
    scale_color_brewer(palette = "Set1", name = "Session") +
    theme_bw() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    labs(x = "Set Size", y = "Proportion Correct") + 
    theme(
      strip.background = element_rect(fill = "white"),
      text = element_text(size = 22)
    )
  
  if (is_recall) {
    pl <- pl +
      geom_errorbar(
        data = tbl_wmu_ss_agg_ci, aes(
          x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci, color = session_id
        ), width = .5
      ) +
      geom_point(data = tbl_wmu_ss_agg_ci, aes(set_size, prop_correct, color = session_id)) 
  }
  
  return(pl)
}

save_my_tiff <- function(pl, path_fl, w, h) {
  tiff(path_fl, w, h, "in", res = 300)
  grid.draw(pl)
  dev.off()
}

save_my_pdf <- function(pl, path_fl, w, h) {
  pdf(path_fl, w, h, paper = "special")
  grid.draw(pl)
  dev.off()
}

save_my_pdf_and_tiff <- function(pl, path_fl, w, h) {
  save_my_pdf(pl, str_c(path_fl, ".pdf"), w, h)
  save_my_tiff(pl, str_c(path_fl, ".tiff"), w, h)
}

plot_my_chance_hist <- function(tbl_overall, ttl, pchance, is_caption = FALSE) {
  
  pl <- ggplot(tbl_overall, aes(optimal)) + 
    geom_histogram(alpha = 0.5, fill = "skyblue2", color = "white", binwidth = .0125) + 
    geom_vline(xintercept = pchance, color = "grey40", linetype = "dotdash", linewidth = 1)+
    labs(
      x = "Prop. Optimal Choice", 
      y = "Nr. Participants",
      title = ttl
    ) +
    theme_bw() +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22)
    ) + 
    scale_color_manual(values = c("skyblue2", "tomato4"), name = "") +
    coord_cartesian(xlim = c(0, .95))
  if (is_caption) pl <- pl + labs(caption = "note. vertical line = 95% of chance distribution")
  if (!is_caption) pl <- pl + labs(caption = " ")
  return(pl)
}


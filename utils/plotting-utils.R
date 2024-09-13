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
          x = set_size, y = prop_correct, ymin = prop_correct - ci, ymax = prop_correct + ci, color = as.character(as.numeric(as.character(session_id)) + 1)
        ), width = .5
      ) +
      geom_point(data = tbl_wmu_ss_agg_ci, aes(set_size, prop_correct, color = as.character(as.numeric(as.character(session_id)) + 1))) 
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

by_participant_maps <- function(tbl_draws, f_time, pars_interest) {
  tbl_draws %>% select(starts_with(pars_interest)) %>%
    mutate(n_sample = 1:nrow(.)) %>%
    pivot_longer(-n_sample) %>%
    mutate(
      ID = as.integer(str_extract(name, "[0-9]+")),
      parameter = str_extract(name, "^[a-z]*")
    ) %>%
    group_by(ID, parameter) %>%
    summarize(map = mean(value)) %>%
    ungroup() %>%
    mutate(fit_time = f_time)
}

map_cor <- function(tbl_draws_hc, tbl_draws_hc_recovery, pars_interest) {
  
  l_tbl_draws <- list(tbl_draws_hc, tbl_draws_hc_recovery)
  l_tbl_maps <- map2(l_tbl_draws, c("data", "preds"), by_participant_maps, pars_interest = pars_interest)
  
  tbl_recovery <- reduce(
    l_tbl_maps, 
    ~ left_join(.x, .y, by = c("ID", "parameter"), suffix = c("_data", "_preds"))
  )
  cor_recovery <- tbl_recovery %>%
    pivot_wider(names_from = parameter, values_from = c(map_data, map_preds)) %>%
    select(-c(fit_time_data, fit_time_preds)) %>%
    summarize(
      beta_in_beta_out = cor(map_data_beta, map_preds_beta),
      tau_in_tau_out = cor(map_data_tau, map_preds_tau),
      beta_in_tau_out = cor(map_data_beta, map_preds_tau),
      tau_in_beta_out = cor(map_data_tau, map_preds_beta)
    )
  
  return(list(tbl_recovery = tbl_recovery, cor_recovery = cor_recovery))
}

recovery_heatmap <- function(l_recovery, ttl, param_relabel = NULL) {
  tbl_in_out <- l_recovery$cor_recovery %>%
    pivot_longer(colnames(.)) %>%
    mutate(
      param_in = str_match(name, "([a-z]*)_")[, 2],
      param_out = str_match(name, "_([a-z]*)_out")[, 2]
    )
  if (!is.null(param_relabel)){
    tbl_in_out$param_in <- factor(tbl_in_out$param_in, labels = param_relabel)
    tbl_in_out$param_out <- factor(tbl_in_out$param_out, labels = param_relabel)
    
  }
  pl <- ggplot(tbl_in_out, aes(param_in, param_out)) +
    geom_tile(aes(fill = value)) +
    geom_label(aes(label = round(value, 2)), size = 6) +
    theme_bw() +
    scale_x_discrete(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22),
      legend.position = "bottom"
    ) + 
    scale_fill_gradient2(name = "", high = "#66C2A5", low = "#FC8D62", mid = "white", midpoint = 0, guide = "none") +
    labs(x = "MAP (Data)", y = "MAP (Prediction)", title = ttl) + 
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22)
    )
  return(pl)
}


my_corr_plot <- function(cortable, x_lab, y_lab, ttl, type = "bandit") {
  my_tbl <- as.data.frame(cortable) %>%
    mutate(rwn = rownames(.)) %>%
    pivot_longer(-rwn)
  if (type == "bandit") {
    if (is_ucb) {
      my_tbl$name <- factor(my_tbl$name, levels = c("Value-Guided Horizon", "Value-Guided Two-Armed", "Value-Guided Restless", "Directed Horizon", "Directed Two-Armed", "Directed Restless"), ordered = TRUE)
      my_tbl$rwn <- factor(my_tbl$rwn, levels = c("Value-Guided Horizon", "Value-Guided Two-Armed", "Value-Guided Restless", "Directed Horizon", "Directed Two-Armed", "Directed Restless"), ordered = TRUE)
    } else {
      my_tbl$name <- factor(my_tbl$name, levels = c("Value-Guided Horizon", "Value-Guided Two-Armed", "Value-Guided Restless", "Directed Horizon", "Directed Two-Armed", "Directed Restless", "Random Two-Armed"), ordered = TRUE)
      my_tbl$rwn <- factor(my_tbl$rwn, levels = c("Value-Guided Horizon", "Value-Guided Two-Armed", "Value-Guided Restless", "Directed Horizon", "Directed Two-Armed", "Directed Restless", "Random Two-Armed"), ordered = TRUE)
    }
    
  }
  if (type == "switch") {
    my_tbl$name <- factor(my_tbl$name, levels = c("Horizon", "Two-Armed", "Restless"), ordered = TRUE)
    my_tbl$rwn <- factor(my_tbl$rwn, levels = c("Horizon", "Two-Armed", "Restless"), ordered = TRUE)
    
  }
  
  if (type == "latent") {
    my_tbl$name <- factor(my_tbl$name, levels = c("G Value Guided", "G Directed", "WMC"), ordered = TRUE)
    my_tbl$rwn <- factor(my_tbl$rwn, levels = c("G Value Guided", "G Directed", "WMC"), ordered = TRUE)
  }
  
  ggplot(my_tbl, aes(rwn, name)) +
    geom_tile(aes(fill = value)) +
    geom_label(aes(label = round(value, 2))) +
    theme_bw() +
    scale_x_discrete(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0), limits = rev) +
    labs(x = x_lab, y = y_lab, title = ttl) + 
    theme(
      strip.background = element_rect(fill = "white"), 
      text = element_text(size = 22),
      axis.text.x = element_text(angle = 90, vjust = .3),
      legend.position = "bottom"
    ) +
    scale_fill_gradient2(name = "", high = "#66C2A5", low = "#FC8D62", mid = "white") +
    guides(fill = "none")
}


heatmap <- function(df, x = x, y = y, limits = c(-1,1)){
  
  ggplot(df, aes(x = x, y = y, fill = cor)) + geom_raster() + 
    scale_fill_gradient2(high = "#66C2A5", low = "#FC8D62", mid = "white", limits = limits)+
    geom_label(aes(label = round(cor, digits = 2)), fill = "white") 
  
  
  
}



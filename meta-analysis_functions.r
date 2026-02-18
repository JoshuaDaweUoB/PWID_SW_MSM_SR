#  load packages
pacman::p_load("tidyverse")

# convert to numeric
convert_to_numeric <- function(df) {
  df <- transform(df, 
                  effect_unadj = as.numeric(effect_unadj), 
                  effect_unadj_lb = as.numeric(effect_unadj_lb),
                  effect_unadj_ub = as.numeric(effect_unadj_ub),
                  effect_adj = as.numeric(effect_adj), 
                  effect_adj_lb = as.numeric(effect_adj_lb),
                  effect_adj_ub = as.numeric(effect_adj_ub))
  return(df)
}

# log transform
log_transform <- function(df) {
  df <- transform(df, 
                  effect_unadj_ln = log(effect_unadj),
                  effect_unadj_lb_ln = log(effect_unadj_lb),
                  effect_unadj_ub_ln = log(effect_unadj_ub),
                  effect_adj_ln = log(effect_adj),
                  effect_adj_lb_ln = log(effect_adj_lb),
                  effect_adj_ub_ln = log(effect_adj_ub))
  return(df)
}

# recode lifetime OAT, homelessness, prison % to NA
recode_to_na <- function(df) {
  # percentage fields to numeric
  for (nm in c("oat_perc", "homeless_perc", "prison_perc")) {
    if (nm %in% names(df)) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  # time frame labels
  norm <- function(x) tolower(trimws(as.character(x)))

  # OAT
  if (all(c("oat_perc", "oat_time_frame_bin") %in% names(df))) {
    is_lt <- norm(df[["oat_time_frame_bin"]]) == "lifetime"
    df[["oat_perc"]][is_lt] <- NA_real_
  }

  # homelessness
  if (all(c("homeless_perc", "homeless_time_frame_bin") %in% names(df))) {
    is_lt <- norm(df[["homeless_time_frame_bin"]]) == "lifetime"
    df[["homeless_perc"]][is_lt] <- NA_real_
  }

  # incarceration
  if (all(c("prison_perc", "prison_time_frame_bin") %in% names(df))) {
    is_lt <- norm(df[["prison_time_frame_bin"]]) == "lifetime"
    df[["prison_perc"]][is_lt] <- NA_real_
  }

  df
}

# function to save dataframes
save_dataframes_to_excel <- function(dfs, sheet_names, file_path) {
  # create a named list of dataframes
  named_dfs <- setNames(dfs, sheet_names)
  
  # write the dataframes to an Excel file
  write_xlsx(named_dfs, path = file_path)
}

# function to merge hiv_inc and hcv_inc into dfs list
merge_study_characteristics <- function(dfs_list, study_char_df) {
  # select relevant studies
  char_to_merge <- study_char_df %>%
    select(study, hiv_inc_bin, hcv_inc_bin)
  
  # Merge with each dataframe in the list
  merged_dfs <- purrr::map(dfs_list, function(df) {
    df %>%
      left_join(char_to_merge, by = "study")
  })
  
  return(merged_dfs)
}

# calculate and add effect_unadj_se
add_effect_unadj_se <- function(df) {
  if (!all(c("effect_unadj_lb", "effect_unadj_ub") %in% names(df))) {
    stop("The columns 'effect_unadj_lb' and 'effect_unadj_ub' are required to calculate 'effect_unadj_se'.")
  }
  
  df <- df %>%
    mutate(
      effect_unadj_se = (log(effect_unadj_ub) - log(effect_unadj_lb)) / (2 * 1.96)
    )
  
  return(df)
}

# meta analysis

# recent unadjusted estimates
recent_unadj_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(moa_unadj))
  
  forest_plot <- metagen(TE = effect_unadj_ln,
                         lower = effect_unadj_lb_ln,
                         upper = effect_unadj_ub_ln,
                         studlab = study,
                         data = filtered_df,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = TRUE, 
                         backtransf = TRUE,
                         subgroup = pub_status,
                         text.random = "Overall")
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 30, height = 20, units = "cm", res = 500)
  
  forest_sw <- forest(forest_plot, 
                      sortvar = country,
                      xlim = c(0.2, 4),             
                      leftcols = c("study", "cohort", "country"),
                      leftlabs = c("Study", "Cohort", "Country"),
                      digits = 2,
                      digits.tau2 = 1,
                      digits.I2 = 1,
                      digits.pval.Q = 3,
                      col.inside = "black",
                      subgroup.name = "",
                      subgroup = TRUE,
                      print.byvar = FALSE,
                      col.subgroup = "black") 
  dev.off()
}

# recent unadjusted estimates (combined)
recent_unadj_forest_plot_combined <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(moa_unadj))
  
  forest_plot <- metagen(
    TE = filtered_df[[effect_col]],
    lower = filtered_df[[lower_col]],
    upper = filtered_df[[upper_col]],
    studlab = filtered_df[[studlab_col]],
    data = filtered_df,
    sm = "RR",
    method.tau = "DL",
    common = FALSE,
    random = TRUE, 
    backtransf = TRUE,
    subgroup = filtered_df[[byvar_col]],
    text.random = "Overall"
  )
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 35, height = 35, units = "cm", res = 500)
  
  forest_sw <- forest(
    forest_plot, 
    sortvar = country,
    xlim = c(0.2, 4),             
    leftcols = c("study", "cohort", "country"),
    leftlabs = c("Study", "Cohort", "Country"),
    digits = 2,
    digits.tau2 = 1,
    digits.I2 = 1,
    digits.pval.Q = 3,
    col.inside = "black",
    subgroup.name = "",
    subgroup = TRUE,
    print.byvar = FALSE,
    col.subgroup = "black",
    overall = FALSE,
    overall.hetstat = FALSE,
    test.subgroup = FALSE
  ) 
  dev.off()
}

# recent adjusted estimates
recent_adj_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(moa_adj)) %>%
    filter(moa_adj != "NR")
  
  forest_plot <- metagen(TE = effect_adj_ln,
                         lower = effect_adj_lb_ln,
                         upper = effect_adj_ub_ln,
                         studlab = study,
                         data = filtered_df,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = TRUE, 
                         backtransf = TRUE,
                         subgroup = pub_status,
                         text.random = "Overall")
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 30, height = 20, units = "cm", res = 500)
  
  forest_sw <- forest(forest_plot, 
                      sortvar = country,
                      xlim = c(0.2, 4),             
                      leftcols = c("study", "cohort", "country"),
                      leftlabs = c("Study", "Cohort", "Country"),
                      digits = 2,
                      digits.tau2 = 1,
                      digits.I2 = 1,
                      digits.pval.Q = 3,
                      col.inside = "black",
                      subgroup.name = "",
                      subgroup = TRUE,
                      print.byvar = FALSE,
                      col.subgroup = "black") 
  dev.off()
}

# lifetime unadjusted estimates
lifetime_unadj_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(moa_unadj))%>%
    filter(moa_unadj != "NR")
  
  forest_plot <- metagen(TE = effect_unadj_ln,
                         lower = effect_unadj_lb_ln,
                         upper = effect_unadj_ub_ln,
                         studlab = study,
                         data = filtered_df,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = TRUE, 
                         backtransf = TRUE,
                         subgroup = pub_status,
                         text.random = "Overall")
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 30, height = 20, units = "cm", res = 500)
  
  forest_sw <- forest(forest_plot, 
                      sortvar = country,
                      xlim = c(0.2, 4),             
                      leftcols = c("study", "cohort", "country"),
                      leftlabs = c("Study", "Cohort", "Country"),
                      digits = 2,
                      digits.tau2 = 1,
                      digits.I2 = 1,
                      digits.pval.Q = 3,
                      col.inside = "black",
                      subgroup.name = "",
                      subgroup = TRUE,
                      print.byvar = FALSE,
                      col.subgroup = "black") 
  dev.off()
}

# lifetime unadjusted estimates (combined)
lifetime_unadj_forest_plot_combined <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(moa_unadj))
  
  forest_plot <- metagen(
    TE = filtered_df[[effect_col]],
    lower = filtered_df[[lower_col]],
    upper = filtered_df[[upper_col]],
    studlab = filtered_df[[studlab_col]],
    data = filtered_df,
    sm = "RR",
    method.tau = "DL",
    common = FALSE,
    random = TRUE, 
    backtransf = TRUE,
    subgroup = filtered_df[[byvar_col]],
    text.random = "Overall"
  )
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 35, height = 35, units = "cm", res = 500)
  
  forest_sw <- forest(
    forest_plot, 
    sortvar = country,
    xlim = c(0.2, 4),             
    leftcols = c("study", "cohort", "country", "pub_status"), 
    leftlabs = c("Study", "Cohort", "Country", "Publication Status"),
    digits = 2,
    digits.tau2 = 1,
    digits.I2 = 1,
    digits.pval.Q = 3,
    col.inside = "black",
    subgroup.name = "",
    subgroup = TRUE,
    print.byvar = FALSE,
    col.subgroup = "black",
    overall = FALSE,
    overall.hetstat = FALSE,
    test.subgroup = FALSE
  ) 
  dev.off()
}

# lifetime adjusted estimates
lifetime_adj_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(moa_adj)) %>%
    filter(moa_adj != "NR")
  
  forest_plot <- metagen(TE = effect_adj_ln,
                         lower = effect_adj_lb_ln,
                         upper = effect_adj_ub_ln,
                         studlab = study,
                         data = filtered_df,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = TRUE, 
                         backtransf = TRUE,
                         subgroup = pub_status,
                         text.random = "Overall")
  summary(forest_plot)
  
  print(paste("Saving plot to:", filename))
  png(filename = filename, width = 30, height = 20, units = "cm", res = 500)
  
  forest_sw <- forest(forest_plot, 
                      sortvar = country,
                      xlim = c(0.2, 4),             
                      leftcols = c("study", "cohort", "country"),
                      leftlabs = c("Study", "Cohort", "Country"),
                      digits = 2,
                      digits.tau2 = 1,
                      digits.I2 = 1,
                      digits.pval.Q = 3,
                      col.inside = "black",
                      subgroup.name = "",
                      subgroup = TRUE,
                      print.byvar = FALSE,
                      col.subgroup = "black") 
  dev.off()
}

# append dataframes to make three forest plots in one figure for HIV and HCV
combine_and_convert <- function(dfs, idx, labels, desired_cols) {
  combined <- purrr::map2(dfs[idx], labels, ~ .x %>%
    mutate(df = .y) %>%
    select(all_of(desired_cols)) %>%
    mutate(across(all_of(desired_cols), as.character))
  ) %>%
    bind_rows() %>%
    mutate(
      effect_unadj_ln = as.numeric(effect_unadj_ln),
      effect_unadj_lb_ln = as.numeric(effect_unadj_lb_ln),
      effect_unadj_ub_ln = as.numeric(effect_unadj_ub_ln)
    )
  return(combined)
}

# subgroup analysis

# subgroup analyses for recent unadjusted estimates

subgroup_analysis_recent_unadj <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # recent exposure 
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_unadj_ln)) %>%
    filter(effect_unadj_ln != "NR")
  
  # Initialize results list
  all_results <- list()
  
  # loop through subgroup variables
  for (subgroup_var in subgroup_vars) {
    # exclude rows with missing values in subgroup variables
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
    # Skip if no studies available
    if (nrow(subgroup_filtered_df) < 2) {
      message(paste0("Skipping ", subgroup_var, " - insufficient studies (n=", nrow(subgroup_filtered_df), ")"))
      next
    }
    
    # forest plot for subgroups
    forest_plot <- metagen(
      TE = subgroup_filtered_df$effect_unadj_ln,
      lower = subgroup_filtered_df$effect_unadj_lb_ln,
      upper = subgroup_filtered_df$effect_unadj_ub_ln,
      studlab = subgroup_filtered_df[[studlab_col]],
      data = subgroup_filtered_df,
      sm = "RR",
      method.tau = "DL",
      common = FALSE,
      random = TRUE,
      backtransf = TRUE,
      subgroup = subgroup_filtered_df[[subgroup_var]],
      text.random = "Overall"
    )
    
    # Extract subgroup results
    subgroup_levels <- unique(subgroup_filtered_df[[subgroup_var]])
    for (i in seq_along(subgroup_levels)) {
      level <- subgroup_levels[i]
      all_results[[length(all_results) + 1]] <- data.frame(
        subgroup_variable = subgroup_var,
        subgroup_level = as.character(level),
        n_studies = forest_plot$k.w[i],
        effect = exp(forest_plot$TE.random.w[i]),
        lower = exp(forest_plot$lower.random.w[i]),
        upper = exp(forest_plot$upper.random.w[i]),
        I2 = forest_plot$I2.w[i] * 100,
        p_value = forest_plot$pval.random.w[i],
        stringsAsFactors = FALSE
      )
    }
    
    # Add overall result
    all_results[[length(all_results) + 1]] <- data.frame(
      subgroup_variable = subgroup_var,
      subgroup_level = "Overall",
      n_studies = forest_plot$k,
      effect = exp(forest_plot$TE.random),
      lower = exp(forest_plot$lower.random),
      upper = exp(forest_plot$upper.random),
      I2 = forest_plot$I2 * 100,
      p_value = forest_plot$pval.random,
      stringsAsFactors = FALSE
    )
    
    # Add test for subgroup differences
    all_results[[length(all_results) + 1]] <- data.frame(
      subgroup_variable = subgroup_var,
      subgroup_level = "Test for subgroup differences",
      n_studies = NA,
      effect = NA,
      lower = NA,
      upper = NA,
      I2 = NA,
      p_value = forest_plot$pval.Q.b.random,
      stringsAsFactors = FALSE
    )
    
    # filename for subgroup plot
    subgroup_filename <- gsub("\\.png$", paste0("_", subgroup_var, ".png"), base_filename)
    
    # save plot
    png(filename = subgroup_filename, width = 30, height = 20, units = "cm", res = 500)
    forest(
      forest_plot,
      sortvar = subgroup_filtered_df[[studlab_col]],
      xlim = c(0.2, 4),
      leftcols = c("study", "country"),
      leftlabs = c("Study", "Country"),
      digits = 2,
      digits.tau2 = 1,
      digits.I2 = 1,
      digits.pval.Q = 3,
      col.inside = "black",
      subgroup.name = "",
      subgroup = TRUE,
      print.byvar = FALSE,
      col.subgroup = "black"
    )
    dev.off()
  }
  
  # Combine and save results to Excel
  if (length(all_results) > 0) {
    results_df <- bind_rows(all_results)
    excel_filename <- gsub("\\.png$", "_results.xlsx", base_filename)
    writexl::write_xlsx(results_df, path = excel_filename)
    message(paste0("Results saved to: ", excel_filename))
  }
  
  return(if (length(all_results) > 0) bind_rows(all_results) else NULL)
}

subgroup_analysis_lifetime_unadj <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # lifetime exposure 
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "lifetime") %>%
    filter(!is.na(effect_unadj_ln)) %>%
    filter(effect_unadj_ln != "NR")
  
  # Initialize results list
  all_results <- list()
  
  # loop through subgroup variables
  for (subgroup_var in subgroup_vars) {
    # exclude rows with missing values in subgroup variables
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
    # Skip if no studies available
    if (nrow(subgroup_filtered_df) < 2) {
      message(paste0("Skipping ", subgroup_var, " - insufficient studies (n=", nrow(subgroup_filtered_df), ")"))
      next
    }
    
    # forest plot for subgroups
    forest_plot <- metagen(
      TE = subgroup_filtered_df$effect_unadj_ln,
      lower = subgroup_filtered_df$effect_unadj_lb_ln,
      upper = subgroup_filtered_df$effect_unadj_ub_ln,
      studlab = subgroup_filtered_df[[studlab_col]],
      data = subgroup_filtered_df,
      sm = "RR",
      method.tau = "DL",
      common = FALSE,
      random = TRUE,
      backtransf = TRUE,
      subgroup = subgroup_filtered_df[[subgroup_var]],
      text.random = "Overall"
    )
    
    # Extract subgroup results
    subgroup_levels <- unique(subgroup_filtered_df[[subgroup_var]])
    for (i in seq_along(subgroup_levels)) {
      level <- subgroup_levels[i]
      all_results[[length(all_results) + 1]] <- data.frame(
        subgroup_variable = subgroup_var,
        subgroup_level = as.character(level),
        n_studies = forest_plot$k.w[i],
        effect = exp(forest_plot$TE.random.w[i]),
        lower = exp(forest_plot$lower.random.w[i]),
        upper = exp(forest_plot$upper.random.w[i]),
        I2 = forest_plot$I2.w[i] * 100,
        p_value = forest_plot$pval.random.w[i],
        stringsAsFactors = FALSE
      )
    }
    
    # Add overall result
    all_results[[length(all_results) + 1]] <- data.frame(
      subgroup_variable = subgroup_var,
      subgroup_level = "Overall",
      n_studies = forest_plot$k,
      effect = exp(forest_plot$TE.random),
      lower = exp(forest_plot$lower.random),
      upper = exp(forest_plot$upper.random),
      I2 = forest_plot$I2 * 100,
      p_value = forest_plot$pval.random,
      stringsAsFactors = FALSE
    )
    
    # Add test for subgroup differences
    all_results[[length(all_results) + 1]] <- data.frame(
      subgroup_variable = subgroup_var,
      subgroup_level = "Test for subgroup differences",
      n_studies = NA,
      effect = NA,
      lower = NA,
      upper = NA,
      I2 = NA,
      p_value = forest_plot$pval.Q.b.random,
      stringsAsFactors = FALSE
    )
    
    # filename for subgroup plot
    subgroup_filename <- gsub("\\.png$", paste0("_", subgroup_var, ".png"), base_filename)
    
    # save plot
    png(filename = subgroup_filename, width = 30, height = 20, units = "cm", res = 500)
    forest(
      forest_plot,
      sortvar = subgroup_filtered_df[[studlab_col]],
      xlim = c(0.2, 4),
      leftcols = c("study", "country"),
      leftlabs = c("Study", "Country"),
      digits = 2,
      digits.tau2 = 1,
      digits.I2 = 1,
      digits.pval.Q = 3,
      col.inside = "black",
      subgroup.name = "",
      subgroup = TRUE,
      print.byvar = FALSE,
      col.subgroup = "black"
    )
    dev.off()
  }
  
  # Combine and save results to Excel
  if (length(all_results) > 0) {
    results_df <- bind_rows(all_results)
    excel_filename <- gsub("\\.png$", "_results.xlsx", base_filename)
    writexl::write_xlsx(results_df, path = excel_filename)
    message(paste0("Results saved to: ", excel_filename))
  }
  
  return(if (length(all_results) > 0) bind_rows(all_results) else NULL)
}

# publication bias
test_publication_bias <- function(df, yi_col, sei_col, plot_filename = NULL) {
  # remove rows with missing values
  df <- df %>% filter(!is.na(.data[[yi_col]]), !is.na(.data[[sei_col]]))
  
  # random-effects model
  res <- metafor::rma(yi = df[[yi_col]], sei = df[[sei_col]], method = "DL")
  
  # Egger's test
  egger <- metafor::regtest(res, model = "rma", predictor = "sei")
  
  # title with p-value
  p_value <- round(egger$pval, 3)
  plot_title <- paste0("Funnel plot\nEgger's test p = ", p_value)
  
  # colors based on pub_status
  pub_status_levels <- unique(df$pub_status)
  colors <- rainbow(length(pub_status_levels))
  names(colors) <- pub_status_levels
  point_colors <- colors[df$pub_status]
  
  # funnel plot
  if (!is.null(plot_filename)) {
    png(plot_filename, width = 1200, height = 900, res = 150)
    metafor::funnel(res, main = plot_title, col = point_colors, pch = 19)
    
    # legend
    legend("topright", 
           legend = names(colors), 
           col = colors, 
           pch = 19, 
           title = "Publication Status",
           bty = "n")
    dev.off()
  } else {
    metafor::funnel(res, main = plot_title, col = point_colors, pch = 19)
    
    # legend
    legend("topright", 
           legend = names(colors), 
           col = colors, 
           pch = 19, 
           title = "Publication Status",
           bty = "n")
  }
  
  print(egger)
  return(egger)
}

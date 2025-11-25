#  load packages
pacman::p_load("tidyverse")

## replace adj1 with adj2 if missing
replace_adj1_with_adj2_if_missing <- function(df) {
  df <- df %>%
    mutate(
      effect_adj1 = ifelse(is.na(effect_adj1), effect_adj2, effect_adj1),
      effect_adj_lb1 = ifelse(is.na(effect_adj_lb1), effect_adj_lb2, effect_adj_lb1),
      effect_adj_ub1 = ifelse(is.na(effect_adj_ub1), effect_adj_ub2, effect_adj_ub1),
      moa_adj1 = ifelse(is.na(moa_adj1) | moa_adj1 == "NR", moa_adj2, moa_adj1)
    )
  return(df)
}

## replace adj2 with adj1 if missing
replace_adj2_with_adj1_if_missing <- function(df) {
  df <- df %>%
    mutate(
      effect_adj2 = ifelse(is.na(effect_adj2), effect_adj1, effect_adj2),
      effect_adj_lb2 = ifelse(is.na(effect_adj_lb2), effect_adj_lb1, effect_adj_lb2),
      effect_adj_ub2 = ifelse(is.na(effect_adj_ub2), effect_adj_ub1, effect_adj_ub2),
      moa_adj2 = ifelse(is.na(moa_adj2) | moa_adj2 == "NR", moa_adj1, moa_adj2)
    )
  return(df)
}

# keep studies where use equals "yes"
filter_use_yes <- function(df) {
  df <- df %>%
    filter(use == "yes")
  return(df)
}

# function to save dataframes as Excel sheets
save_dataframes_to_excel <- function(dfs, sheet_names, file_path) {
  # create a named list of dataframes
  named_dfs <- setNames(dfs, sheet_names)
  
  # write the dataframes to an Excel file
  write_xlsx(named_dfs, path = file_path)
}

# Function to merge study_characteristics with dataframes in dfs list
merge_study_characteristics <- function(dfs_list, study_char_df) {
  # Select only the binary variables and study column from study_characteristics
  char_to_merge <- study_char_df %>%
    select(study, hiv_inc_bin, hcv_inc_bin)
  
  # Merge with each dataframe in the list
  merged_dfs <- purrr::map(dfs_list, function(df) {
    df %>%
      left_join(char_to_merge, by = "study")
  })
  
  return(merged_dfs)
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
  
  print(paste("Saving plot to:", filename))  # Print the filename to confirm
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
  
  print(paste("Saving plot to:", filename))  # Print the filename to confirm
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
    overall = FALSE,             # Remove overall pooled estimate
    overall.hetstat = FALSE,     # Remove overall heterogeneity
    test.subgroup = FALSE        # Remove test for subgroup differences
  ) 
  dev.off()
}

# recent adjusted estimates (structural factors)
recent_adj1_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(moa_adj1)) %>%
    filter(moa_adj1 != "NR")
  
  forest_plot <- metagen(TE = effect_adj1_ln,
                         lower = effect_adj_lb1_ln,
                         upper = effect_adj_ub1_ln,
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
  
  print(paste("Saving plot to:", filename))  # Print the filename to confirm
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

# recent adjusted estimates (injecting risk factors)
recent_adj2_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(moa_adj2)) %>%
    filter(moa_adj2 != "NR")
  
  forest_plot <- metagen(TE = effect_adj2_ln,
                         lower = effect_adj_lb2_ln,
                         upper = effect_adj_ub2_ln,
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
  
  print(paste("Saving plot to:", filename))  # Print the filename to confirm
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

# lifetime adjusted estimates (structural factors)
lifetime_adj1_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(moa_adj1)) %>%
    filter(moa_adj1 != "NR")
  
  forest_plot <- metagen(TE = effect_adj1_ln,
                         lower = effect_adj_lb1_ln,
                         upper = effect_adj_ub1_ln,
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

# lifetime adjusted estimates (injecting risk factors)
lifetime_adj2_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(moa_adj2)) %>%
    filter(moa_adj2 != "NR")
  
  forest_plot <- metagen(TE = effect_adj2_ln,
                         lower = effect_adj_lb2_ln,
                         upper = effect_adj_ub2_ln,
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
                      leftcols = c("country", "cohort"), 
                      leftlabs = c("Country", "Cohort"),
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

# subgroup analysis of recent estimates

# Function to conduct subgroup analyses
subgroup_analysis_recent <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, subgroup_vars, base_filename) {
  # Filter the dataframe for recent exposure time frame
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_unadj_ln)) %>%
    filter(effect_unadj_ln != "NR")
  
  # Loop through each subgroup variable
  for (subgroup_var in subgroup_vars) {
    # Filter out rows with missing values in the subgroup variable
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
    # Generate the forest plot for the current subgroup
    forest_plot <- metagen(
      TE = subgroup_filtered_df[[effect_col]],
      lower = subgroup_filtered_df[[lower_col]],
      upper = subgroup_filtered_df[[upper_col]],
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
    
    # Construct the filename for the current subgroup plot
    subgroup_filename <- gsub("\\.png$", paste0("_", subgroup_var, ".png"), base_filename)
    
    # Save the forest plot as a PNG
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
}

# conduct subgroup analyses for unadjusted estimates
subgroup_analysis_recent_unadj <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # recent exposure 
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_unadj_ln)) %>%
    filter(effect_unadj_ln != "NR")
  
  # loop through subgroup variables
  for (subgroup_var in subgroup_vars) {
    # exclude rows with missing values in subgroup variables
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
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
    
    # filename for subgroup plot
    subgroup_filename <- gsub("\\.png$", paste0("_", subgroup_var, ".png"), base_filename)
    
    # save
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
}

# subgroup analyses for recent adjusted
subgroup_analysis_recent_adj1 <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # recent exposure time frame
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_adj1_ln)) %>%
    filter(effect_adj1_ln != "NR")
  
  # loop through subgroup variables
  for (subgroup_var in subgroup_vars) {
    # remove rows with missing values in the subgroup variable
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
    # forest plot for the current subgroup
    forest_plot <- metagen(
      TE = subgroup_filtered_df$effect_adj1_ln,
      lower = subgroup_filtered_df$effect_adj_lb1_ln,
      upper = subgroup_filtered_df$effect_adj_ub1_ln,
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
    
    # filename for the current subgroup plot
    subgroup_filename <- gsub("\\.png$", paste0("_", subgroup_var, ".png"), base_filename)
    
    # save
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
}

# meta regression
meta_regress_strata_summary <- function(df) {
  filtered_df <- df %>%
    filter(use == "yes") %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_unadj_ln))

  filtered_df$inj_age_num <- suppressWarnings(readr::parse_number(filtered_df$inj_age))

  if ("rob_3cat" %in% names(filtered_df)) {
    idx <- filtered_df$rob_3cat %in% c("Good", "Satisfactory", "Very good")
    filtered_df <- filtered_df[idx, , drop = FALSE]
    filtered_df$rob_3cat1 <- ifelse(filtered_df$rob_3cat == "Satisfactory", 1,
                                    ifelse(filtered_df$rob_3cat == "Good", 0, NA))
    filtered_df$rob_3cat2 <- ifelse(filtered_df$rob_3cat == "Very good", 1,
                                    ifelse(filtered_df$rob_3cat == "Good", 0, NA))
  }

  continuous_vars <- c("age", "female_perc", "inj_age_num", "oat_perc", "homeless_perc", "prison_perc")
  categorical_vars <- c("2016_bin", "incidence_method", "lmic_bin", "rob_3cat1", "rob_3cat2", "pub_status")
  vars <- c(categorical_vars, continuous_vars)

  medians_used <- list()
  for (v in continuous_vars) {
    if (v %in% names(filtered_df)) {
      filtered_df[[v]] <- suppressWarnings(as.numeric(filtered_df[[v]]))
      if (any(is.na(filtered_df[[v]]))) {
        warning("Variable ", v, " contains non-numeric values and has been partially converted.")
      }
      med <- median(filtered_df[[v]], na.rm = TRUE)
      medians_used[[v]] <- med
      filtered_df[[v]] <- ifelse(filtered_df[[v]] >= med, 1, 0)
    }
  }

  levels_used <- list()
  for (v in categorical_vars) {
    if (v %in% names(filtered_df)) {
      if (is.character(filtered_df[[v]]) || is.factor(filtered_df[[v]])) {
        filtered_df[[v]] <- as.factor(filtered_df[[v]])
        lvls <- levels(droplevels(filtered_df[[v]]))
        levels_used[[v]] <- paste(lvls, collapse = ";")
        if (length(lvls) == 2) {
          filtered_df[[v]] <- as.numeric(filtered_df[[v]]) - 1
        } else {
          warning("Variable ", v, " has more than two levels and will be retained as a factor.")
        }
      } else {
        warning("Variable ", v, " is not categorical and cannot be converted.")
      }
    }
  }

  results <- list()
  for (v in vars) {
    if (!(v %in% names(filtered_df))) {
      warning("Variable ", v, " is missing from the dataframe.")
      next
    }
    dat <- filtered_df %>% filter(!is.na(.data[[v]]))
    lvls <- if (v %in% names(levels_used)) levels_used[[v]] else NA_character_
    med_cutoff <- if (v %in% names(medians_used)) medians_used[[v]] else NA_real_
    if (nrow(dat) > 2 && length(unique(dat[[v]])) > 1) {
      dat$group <- as.factor(dat[[v]])
      res_mod <- tryCatch(
        metafor::rma(
          yi = effect_unadj_ln,
          sei = dat$effect_unadj_se,
          mods = ~ group,
          data = dat,
          method = "DL"
        ),
        error = function(e) {
          message("Error in meta-regression for variable: ", v, " - ", e$message)
          NULL
        }
      )

      logRR0 <- if (!is.null(res_mod)) as.numeric(res_mod$b[1]) else NA
      logRR1 <- if (!is.null(res_mod) && length(res_mod$b) > 1) as.numeric(res_mod$b[1] + res_mod$b[2]) else NA
      RR0 <- if (!is.null(res_mod)) exp(logRR0) else NA
      RR1 <- if (!is.null(res_mod) && length(res_mod$b) > 1) exp(logRR1) else NA

      RR0_lb <- if (!is.null(res_mod)) exp(res_mod$ci.lb[1]) else NA
      RR0_ub <- if (!is.null(res_mod)) exp(res_mod$ci.ub[1]) else NA
      RR1_lb <- if (!is.null(res_mod) && length(res_mod$ci.lb) > 1) exp(res_mod$ci.lb[1] + res_mod$ci.lb[2]) else NA
      RR1_ub <- if (!is.null(res_mod) && length(res_mod$ci.ub) > 1) exp(res_mod$ci.ub[1] + res_mod$ci.ub[2]) else NA

      logRR_ratio <- if (!is.null(res_mod) && length(res_mod$b) > 1) as.numeric(res_mod$b[2]) else NA
      se_logRR_ratio <- if (!is.null(res_mod) && length(res_mod$vb) > 1) sqrt(res_mod$vb[2,2]) else NA
      RR_ratio <- if (!is.na(logRR_ratio)) exp(logRR_ratio) else NA
      RR_ratio_lb <- if (!is.na(logRR_ratio) && !is.na(se_logRR_ratio)) exp(logRR_ratio - 1.96 * se_logRR_ratio) else NA
      RR_ratio_ub <- if (!is.na(logRR_ratio) && !is.na(se_logRR_ratio)) exp(logRR_ratio + 1.96 * se_logRR_ratio) else NA

      tau2_ratio <- if (!is.null(res_mod)) res_mod$tau2 else NA_real_
      R2_ratio <- if (!is.null(res_mod) && !is.null(res_mod$R2)) res_mod$R2 else NA_real_

      results[[v]] <- tibble::tibble(
        variable = v,
        levels_used = lvls,
        median_cutoff = med_cutoff,
        RR_stratum0 = RR0,
        RR_stratum0_lb = RR0_lb,
        RR_stratum0_ub = RR0_ub,
        RR_stratum1 = RR1,
        RR_stratum1_lb = RR1_lb,
        RR_stratum1_ub = RR1_ub,
        RR_ratio = RR_ratio,
        RR_ratio_lb = RR_ratio_lb,
        RR_ratio_ub = RR_ratio_ub,
        RR_estimates0 = sum(dat[[v]] == 0, na.rm = TRUE),
        RR_estimates1 = sum(dat[[v]] == 1, na.rm = TRUE),
        tau2_ratio = tau2_ratio,
        R2_ratio = R2_ratio
      )
    } else {
      results[[v]] <- tibble::tibble(
        variable = v,
        levels_used = lvls,
        median_cutoff = med_cutoff,
        RR_stratum0 = NA_real_,
        RR_stratum0_lb = NA_real_,
        RR_stratum0_ub = NA_real_,
        RR_stratum1 = NA_real_,
        RR_stratum1_lb = NA_real_,
        RR_stratum1_ub = NA_real_,
        RR_ratio = NA_real_,
        RR_ratio_lb = NA_real_,
        RR_ratio_ub = NA_real_,
        RR_estimates0 = NA_integer_,
        RR_estimates1 = NA_integer_,
        tau2_ratio = NA_real_,
        R2_ratio = NA_real_
      )
    }
  }
  if (length(results) == 0) {
    return(tibble::tibble(
      variable = character(),
      levels_used = character(),
      median_cutoff = numeric(),
      RR_stratum0 = numeric(),
      RR_stratum0_lb = numeric(),
      RR_stratum0_ub = numeric(),
      RR_stratum1 = numeric(),
      RR_stratum1_lb = numeric(),
      RR_stratum1_ub = numeric(),
      RR_ratio = numeric(),
      RR_ratio_lb = numeric(),
      RR_ratio_ub = numeric(),
      RR_estimates0 = integer(),
      RR_estimates1 = integer(),
      tau2_ratio = numeric(),
      R2_ratio = numeric()
    ))
  }
  dplyr::bind_rows(results)
}

## publication bias
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

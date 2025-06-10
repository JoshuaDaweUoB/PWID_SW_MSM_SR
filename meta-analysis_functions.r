#  load packages
pacman::p_load("tidyverse")

# create best effect variable 
create_effect_best <- function(df) {
  # Convert "-" to NA
  df <- df %>%
    mutate(
      effect_adj1 = na_if(effect_adj1, "-"),
      effect_adj_lb1 = na_if(effect_adj_lb1, "-"),
      effect_adj_ub1 = na_if(effect_adj_ub1, "-")
    )
  
  print("Before mutate:")
  print(df %>% select(effect_adj1, effect_unadj))  # Print relevant columns before mutate
  
  df <- df %>%
    mutate(
      effect_best = ifelse(!is.na(effect_adj1), effect_adj1, effect_unadj),
      effect_best_lb = ifelse(!is.na(effect_adj_lb1), effect_adj_lb1, effect_unadj_lb),
      effect_best_ub = ifelse(!is.na(effect_adj_ub1), effect_adj_ub1, effect_unadj_ub)
    )
  
  print("After mutate:")
  print(df %>% select(effect_adj1, effect_unadj, effect_best))  # Print relevant columns after mutate
  
  return(df)
}

# convert to numeric
convert_to_numeric <- function(df) {
  df <- transform(df, 
                  effect_unadj = as.numeric(effect_unadj), 
                  effect_unadj_lb = as.numeric(effect_unadj_lb),
                  effect_unadj_ub = as.numeric(effect_unadj_ub),
                  effect_adj1 = as.numeric(effect_adj1), 
                  effect_adj_lb1 = as.numeric(effect_adj_lb1),
                  effect_adj_ub1 = as.numeric(effect_adj_ub1),
                  effect_adj2 = as.numeric(effect_adj2), 
                  effect_adj_lb2 = as.numeric(effect_adj_lb2),
                  effect_adj_ub2 = as.numeric(effect_adj_ub2),
                  effect_best = as.numeric(effect_best),
                  effect_best_lb = as.numeric(effect_best_lb),
                  effect_best_ub = as.numeric(effect_best_ub))
  return(df)
}

# log transform
log_transform <- function(df) {
  df <- transform(df, 
                  effect_unadj_ln = log(effect_unadj),
                  effect_unadj_lb_ln = log(effect_unadj_lb),
                  effect_unadj_ub_ln = log(effect_unadj_ub),
                  effect_adj1_ln = log(effect_adj1),
                  effect_adj_lb1_ln = log(effect_adj_lb1),
                  effect_adj_ub1_ln = log(effect_adj_ub1),
                  effect_adj2_ln = log(effect_adj2),
                  effect_adj_lb2_ln = log(effect_adj_lb2),
                  effect_adj_ub2_ln = log(effect_adj_ub2),
                  effect_best_ln = log(effect_best),
                  effect_best_lb_ln = log(effect_best_lb),
                  effect_best_ub_ln = log(effect_best_ub))
  return(df)
}

## replace adj1 with adj2 if missing
replace_adj1_with_adj2_if_missing <- function(df) {
  df <- df %>%
    mutate(
      effect_adj1 = ifelse(is.na(effect_adj1), effect_adj2, effect_adj1),
      effect_adj_lb1 = ifelse(is.na(effect_adj_lb1), effect_adj_lb2, effect_adj_lb1),
      effect_adj_ub1 = ifelse(is.na(effect_adj_ub1), effect_adj_ub2, effect_adj_ub1)
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
                      sortvar = study,
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
    sortvar = study,
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
                      sortvar = study,
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
                      sortvar = study,
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

# recent best estimates
recent_best_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "recent") %>% 
    filter(!is.na(effect_best)) %>%
    filter(effect_best != "NR")
  
  forest_plot <- metagen(TE = effect_best_ln,
                         lower = effect_best_lb_ln,
                         upper = effect_best_ub_ln,
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
                      sortvar = study,
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
                      sortvar = study,
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
    sortvar = study,
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
                      sortvar = study,
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
                      sortvar = study,
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

# lifetime best estimates
lifetime_best_forest_plot <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, byvar_col, filename) {
  filtered_df <- df %>% 
    filter(exposure_time_frame_bin == "lifetime") %>% 
    filter(!is.na(effect_best)) %>%
    filter(effect_best != "NR")
  
  forest_plot <- metagen(TE = effect_best_ln,
                         lower = effect_best_lb_ln,
                         upper = effect_best_ub_ln,
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
                      sortvar = study,
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

# subgroup analysis of recent best estimates

# Function to conduct subgroup analyses
subgroup_analysis_recent_best <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, subgroup_vars, base_filename) {
  # Filter the dataframe for recent exposure time frame
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_best)) %>%
    filter(effect_best != "NR")
  
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

# Function to conduct subgroup analyses for unadjusted estimates
subgroup_analysis_recent_unadj <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # Filter the dataframe for recent exposure time frame and non-missing unadjusted estimates
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

# Function to conduct subgroup analyses for recent adjusted estimates
subgroup_analysis_recent_adj1 <- function(df, exposure_time_frame, studlab_col, subgroup_vars, base_filename) {
  # Filter the dataframe for recent exposure time frame and non-missing adjusted estimates
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_adj1_ln)) %>%
    filter(effect_adj1_ln != "NR")
  
  # Loop through each subgroup variable
  for (subgroup_var in subgroup_vars) {
    # Filter out rows with missing values in the subgroup variable
    subgroup_filtered_df <- filtered_df %>%
      filter(!is.na(.data[[subgroup_var]]))
    
    # Generate the forest plot for the current subgroup
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

## meta regression

meta_regress_best_recent_univariate <- function(df) {
  # Filter for recent, non-missing, and valid best estimates
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_best_ln)) %>%
    filter(effect_best_ln != "NR")
  
  # Ensure variables are numeric where appropriate
  for (v in c("oat_perc", "homeless_perc", "prison_perc", "age", "inj_age_num")) {
    filtered_df[[v]] <- as.numeric(filtered_df[[v]])
  }
  filtered_df$inj_age_num <- readr::parse_number(filtered_df$inj_age)
  
  # Relevel to binary based on median
  for (v in c("oat_perc", "homeless_perc", "prison_perc", "age", "inj_age_num")) {
    med <- median(filtered_df[[v]], na.rm = TRUE)
    filtered_df[[v]] <- ifelse(filtered_df[[v]] >= med, 1, 0)
  }
  
  # List of variables to meta-regress
  vars <- c(
    "2016_bin", "incidence_method", "age", "inj_age_num", "oat_perc",
    "homeless_perc", "prison_perc", "lmic_bin", "rob_3cat", "pub_status"
  )
  
  # Store results in a named list
  results <- list()
  
  for (v in vars) {
    dat <- filtered_df %>% filter(!is.na(.data[[v]]))
    if (nrow(dat) > 2 && length(unique(dat[[v]])) > 1) {
      res <- metafor::rma(
        yi = dat$effect_best_ln,
        sei = (dat$effect_best_ub_ln - dat$effect_best_lb_ln) / (2 * 1.96),
        mods = ~ dat[[v]],
        data = dat,
        method = "DL"
      )
      results[[v]] <- res
    } else {
      results[[v]] <- NA
    }
  }
  return(results)
}

meta_regress_strata_summary <- function(df) {
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_best_ln)) %>%
    filter(effect_best_ln != "NR")

  filtered_df$inj_age_num <- suppressWarnings(readr::parse_number(filtered_df$inj_age))

  continuous_vars <- c("age", "inj_age_num", "oat_perc", "homeless_perc", "prison_perc")
  categorical_vars <- c("2016_bin", "incidence_method", "lmic_bin", "rob_3cat", "pub_status")
  vars <- c(categorical_vars, continuous_vars)

  # Dichotomize continuous variables by median
  for (v in continuous_vars) {
    if (v %in% names(filtered_df)) {
      filtered_df[[v]] <- as.numeric(filtered_df[[v]])
      med <- median(filtered_df[[v]], na.rm = TRUE)
      filtered_df[[v]] <- ifelse(filtered_df[[v]] >= med, 1, 0)
    }
  }

  # For categorical variables, convert to factor and then to 0/1 if binary
  levels_used <- list()
  for (v in categorical_vars) {
    if (v %in% names(filtered_df)) {
      filtered_df[[v]] <- as.factor(filtered_df[[v]])
      lvls <- levels(droplevels(filtered_df[[v]]))
      levels_used[[v]] <- paste(lvls, collapse = ";")
      if (length(lvls) == 2) {
        filtered_df[[v]] <- as.numeric(filtered_df[[v]]) - 1  # 0/1 coding
      } else {
        filtered_df[[v]] <- NA
      }
    }
  }

  results <- list()

  for (v in vars) {
    if (!(v %in% names(filtered_df))) next
    dat <- filtered_df %>% filter(!is.na(.data[[v]]))
    lvls <- if (v %in% names(levels_used)) levels_used[[v]] else NA_character_
    if (nrow(dat) > 2 && length(unique(dat[[v]])) > 1) {
      dat0 <- dat %>% filter(.data[[v]] == 0)
      dat1 <- dat %>% filter(.data[[v]] == 1)
      if (nrow(dat0) > 1 && nrow(dat1) > 1) {
        res0 <- tryCatch(
          metafor::rma(
            yi = effect_best_ln,
            sei = (effect_best_ub_ln - effect_best_lb_ln) / (2 * 1.96),
            data = dat0,
            method = "DL"
          ),
          error = function(e) NULL
        )
        res1 <- tryCatch(
          metafor::rma(
            yi = effect_best_ln,
            sei = (effect_best_ub_ln - effect_best_lb_ln) / (2 * 1.96),
            data = dat1,
            method = "DL"
          ),
          error = function(e) NULL
        )
        logRR0 <- if (!is.null(res0)) res0$b[1,1] else NA
        logRR1 <- if (!is.null(res1)) res1$b[1,1] else NA
        se0 <- if (!is.null(res0)) res0$se[1] else NA
        se1 <- if (!is.null(res1)) res1$se[1] else NA

        RR0 <- if (!is.null(res0)) exp(logRR0) else NA
        RR0_lb <- if (!is.null(res0)) exp(res0$ci.lb) else NA
        RR0_ub <- if (!is.null(res0)) exp(res0$ci.ub) else NA
        RR1 <- if (!is.null(res1)) exp(logRR1) else NA
        RR1_lb <- if (!is.null(res1)) exp(res1$ci.lb) else NA
        RR1_ub <- if (!is.null(res1)) exp(res1$ci.ub) else NA

        logRR_ratio <- if (!is.na(logRR0) && !is.na(logRR1)) logRR1 - logRR0 else NA
        se_logRR_ratio <- if (!is.na(se0) && !is.na(se1)) sqrt(se0^2 + se1^2) else NA
        RR_ratio <- if (!is.na(logRR_ratio)) exp(logRR_ratio) else NA
        RR_ratio_lb <- if (!is.na(logRR_ratio) && !is.na(se_logRR_ratio)) exp(logRR_ratio - 1.96 * se_logRR_ratio) else NA
        RR_ratio_ub <- if (!is.na(logRR_ratio) && !is.na(se_logRR_ratio)) exp(logRR_ratio + 1.96 * se_logRR_ratio) else NA

        results[[v]] <- tibble::tibble(
          variable = v,
          levels_used = lvls,
          RR_stratum0 = RR0,
          RR_stratum0_lb = RR0_lb,
          RR_stratum0_ub = RR0_ub,
          RR_stratum1 = RR1,
          RR_stratum1_lb = RR1_lb,
          RR_stratum1_ub = RR1_ub,
          RR_ratio = RR_ratio,
          RR_ratio_lb = RR_ratio_lb,
          RR_ratio_ub = RR_ratio_ub,
          RR_estimates0 = nrow(dat0),
          RR_estimates1 = nrow(dat1)
        )
      } else {
        results[[v]] <- tibble::tibble(
          variable = v,
          levels_used = lvls,
          RR_stratum0 = NA_real_,
          RR_stratum0_lb = NA_real_,
          RR_stratum0_ub = NA_real_,
          RR_stratum1 = NA_real_,
          RR_stratum1_lb = NA_real_,
          RR_stratum1_ub = NA_real_,
          RR_ratio = NA_real_,
          RR_ratio_lb = NA_real_,
          RR_ratio_ub = NA_real_,
          RR_estimates0 = nrow(dat0),
          RR_estimates1 = nrow(dat1)
        )
      }
    } else {
      results[[v]] <- tibble::tibble(
        variable = v,
        levels_used = lvls,
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
        RR_estimates1 = NA_integer_
      )
    }
  }
  dplyr::bind_rows(results)
}

## publication bias

test_publication_bias <- function(df, yi_col, sei_col, plot_filename = NULL) {
  # Remove rows with missing values
  df <- df %>% filter(!is.na(.data[[yi_col]]), !is.na(.data[[sei_col]]))
  
  # Fit random-effects model
  res <- metafor::rma(yi = df[[yi_col]], sei = df[[sei_col]], method = "DL")
  
  # Funnel plot
  if (!is.null(plot_filename)) {
    png(plot_filename, width = 1200, height = 900, res = 150)
    metafor::funnel(res, main = "Funnel plot")
    dev.off()
  } else {
    metafor::funnel(res, main = "Funnel plot")
  }
  
  # Egger's test
  egger <- metafor::regtest(res, model = "rma", predictor = "sei")
  print(egger)
  return(egger)
}

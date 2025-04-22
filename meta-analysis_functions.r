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

# subgroup analysis of recent best estimates

# Function to conduct subgroup analyses
subgroup_analysis <- function(df, exposure_time_frame, effect_col, lower_col, upper_col, studlab_col, subgroup_vars, output_dir) {
  # Filter the dataframe for recent exposure time frame
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == "recent") %>%
    filter(!is.na(effect_best)) %>%
    filter(effect_best != "NR")
  
  # Loop through each subgroup variable
  for (subgroup_var in subgroup_vars) {
    # Generate the forest plot for the current subgroup
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
      subgroup = filtered_df[[subgroup_var]],
      text.random = "Overall"
    )
    
    # Print summary of the forest plot
    summary(forest_plot)
    
    # Define the output filename
    filename <- file.path(output_dir, paste0("forest_plot_", subgroup_var, ".png"))
    print(paste("Saving plot to:", filename))  # Print the filename to confirm
    
    # Save the forest plot as a PNG
    png(filename = filename, width = 30, height = 20, units = "cm", res = 500)
    forest(
      forest_plot,
      sortvar = filtered_df[[studlab_col]],
      xlim = c(0.2, 4),
      leftcols = c("country", "cohort"),
      leftlabs = c("Country", "Cohort"),
      digits = 2,
      digits.tau2 = 1,
      digits.I2 = 1,
      digits.pval.Q = 3,
      col.inside = "black",
      subgroup.name = subgroup_var,
      subgroup = TRUE,
      print.byvar = FALSE,
      col.subgroup = "black"
    )
    dev.off()
  }
}

# Example usage of the subgroup_analysis function
subgroup_vars <- c("pub_status", "2016_bin", "incidence_method", "who_region", "lmic_4cat", "hiv_crim", "rob_3cat")
output_dir <- "path/to/output/directory"  # Replace with your desired output directory

# Call the function for subgroup analysis
subgroup_analysis(
  df = your_dataframe,  # Replace with your dataframe
  exposure_time_frame = "recent",
  effect_col = "effect_best_ln",
  lower_col = "effect_best_lb_ln",
  upper_col = "effect_best_ub_ln",
  studlab_col = "study",
  subgroup_vars = subgroup_vars,
  output_dir = output_dir
)
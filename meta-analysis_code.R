# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl",, "writexl", "tidyverse")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframes
hiv_sw_all <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - All") 
hiv_sw_males <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - Male") 
hiv_sw_females <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - Female") 
hiv_msm <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - MSM") 
hcv_sw_all <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - All") 
hcv_sw_males <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - Male") 
hcv_sw_females <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - Female") 
hcv_msm <- read_excel("Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - MSM") 

# define dataframes
dfs <- list(hiv_sw_all, hiv_sw_males, hiv_sw_females, hiv_msm, hcv_sw_all, hcv_sw_males, hcv_sw_females, hcv_msm)
rec_unadj <- c("sw_recent_hiv_all_unadj.png", "sw_recent_hiv_males_unadj.png", "sw_recent_hiv_females_unadj.png", "sw_recent_hiv_msm_unadj.png", "sw_recent_hcv_all_unadj.png", "sw_recent_hcv_males_unadj.png", "sw_recent_hcv_females_unadj.png", "sw_recent_hcv_msm_unadj.png")
rec_adj1 <- c("sw_recent_hiv_all_adj1.png", "sw_recent_hiv_males_adj1.png", "sw_recent_hiv_females_adj1.png", "sw_recent_hiv_msm_adj1.png", "sw_recent_hcv_all_adj1.png", "sw_recent_hcv_males_adj1.png", "sw_recent_hcv_females_adj1.png", "sw_recent_hcv_msm_adj1.png")
rec_adj2 <- c("sw_recent_hiv_all_adj2.png", "sw_recent_hiv_males_adj2.png", "sw_recent_hiv_females_adj2.png", "sw_recent_hiv_msm_adj2.png", "sw_recent_hcv_all_adj2.png", "sw_recent_hcv_males_adj2.png", "sw_recent_hcv_females_adj2.png", "sw_recent_hcv_msm_adj2.png")
rec_best <- c("sw_recent_hiv_all_best.png", "sw_recent_hiv_males_best.png", "sw_recent_hiv_females_best.png", "sw_recent_hiv_msm_best.png", "sw_recent_hcv_all_best.png", "sw_recent_hcv_males_best.png", "sw_recent_hcv_females_best.png", "sw_recent_hcv_msm_best.png")
sheet_names <- c("HIV_Sex_Work_All", "HIV_Sex_Work_Males", "HIV_Sex_Work_Females", "HIV_MSM", "HCV_Sex_Work_All", "HCV_Sex_Work_Males", "HCV_Sex_Work_Females", "HCV_MSM")

# data cleaning

# convert to numeric and log transform
for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  df <- create_effect_best(df)
  df <- convert_to_numeric(df)
  df <- log_transform(df)
  dfs[[i]] <- df
}

# save dataframes to excel
file_path <- "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/processed_data.xlsx"
save_dataframes_to_excel(dfs, sheet_names, file_path)

# recent unadjusted forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", rec_unadj[i])
  print(filename)  # Print the filename to confirm
  recent_unadj_forest_plot(dfs[[i]], "recent", "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln", "lead_author", "pub_status", filename)
}

# recent adjusted forest plots - structural factors only
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", rec_adj1[i])
  print(filename)  # Print the filename to confirm
  recent_adj1_forest_plot(dfs[[i]], "recent", "effect_adj1_ln", "effect_adj_lb1_ln", "effect_adj_ub1_ln", "lead_author", "pub_status", filename)
}

# recent adjusted forest plots - injecting risk factors
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", rec_adj2[i])
  print(filename)  # Print the filename to confirm
  recent_adj2_forest_plot(dfs[[i]], "recent", "effect_adj2_ln", "effect_adj2_lb_ln", "effect_adj2_ub_ln", "lead_author", "pub_status", filename)
}

# recent best effect forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", rec_best[i])
  print(filename)  # Print the filename to confirm
  recent_best_forest_plot(dfs[[i]], "recent", "effect_best_ln", "effect_best_lb_ln", "effect_best_ub_ln", "lead_author", "pub_status", filename)
}
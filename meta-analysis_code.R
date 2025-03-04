# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "tidyverse")

# source functions
source("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/code/meta-analysis_functions.r")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# load dataframes
hiv_sw_all <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - All") 
hiv_sw_males <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - Male") 
hiv_sw_females <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - Sex work - Female") 
hiv_msm <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HIV - MSM") 
hcv_sw_all <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - All") 
hcv_sw_males <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - Male") 
hcv_sw_females <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - Sex work - Female") 
hcv_msm <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Data extraction/Full data extraction_24022025.xlsx", sheet = "HCV - MSM") 

# define dataframes
dfs <- list(hiv_sw_all, hiv_sw_males, hiv_sw_females, hiv_msm, hcv_sw_all, hcv_sw_males, hcv_sw_females, hcv_msm)
filenames <- c("sw_recent_hiv_all_unadj.png", "sw_recent_hiv_males_unadj.png", "sw_recent_hiv_females_unadj.png", "sw_recent_hiv_msm_unadj.png", "sw_recent_hcv_all_unadj.png", "sw_recent_hcv_males_unadj.png", "sw_recent_hcv_females_unadj.png", "sw_recent_hcv_msm_unadj.png")

# data cleaning

# convert to numeric and log transform
for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  df <- convert_to_numeric(df)
  df <- log_transform(df)
  dfs[[i]] <- df
}

# Apply the function to each dataframe
for (i in 1:length(dfs)) {
  generate_forest_plot(dfs[[i]], "recent", "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln", "lead_author", "pub_status", paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", filenames[i]))
}

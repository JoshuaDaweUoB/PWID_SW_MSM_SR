# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "writexl", "tidyverse", "purrr")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframes
hiv_sw_all <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HIV - Sex work - All") 
hiv_sw_males <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HIV - Sex work - Male") 
hiv_sw_females <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HIV - Sex work - Female") 
hiv_msm <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HIV - MSM") 
hcv_sw_all <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HCV - Sex work - All") 
hcv_sw_males <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HCV - Sex work - Male") 
hcv_sw_females <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HCV - Sex work - Female") 
hcv_msm <- read_excel("Data extraction/Full data extraction_08042025.xlsx", sheet = "HCV - MSM") 

# define dataframes and lists
dfs <- list(hiv_sw_all, hiv_sw_males, hiv_sw_females, hiv_msm, hcv_sw_all, hcv_sw_males, hcv_sw_females, hcv_msm)
rec_unadj <- c("sw_recent_hiv_all_unadj.png", "sw_recent_hiv_males_unadj.png", "sw_recent_hiv_females_unadj.png", "sw_recent_hiv_msm_unadj.png", "sw_recent_hcv_all_unadj.png", "sw_recent_hcv_males_unadj.png", "sw_recent_hcv_females_unadj.png", "sw_recent_hcv_msm_unadj.png")
rec_adj1 <- c("sw_recent_hiv_all_adj1.png", "sw_recent_hiv_males_adj1.png", "sw_recent_hiv_females_adj1.png", "sw_recent_hiv_msm_adj1.png", "sw_recent_hcv_all_adj1.png", "sw_recent_hcv_males_adj1.png", "sw_recent_hcv_females_adj1.png", "sw_recent_hcv_msm_adj1.png")
rec_adj2 <- c("sw_recent_hiv_all_adj2.png", "sw_recent_hiv_males_adj2.png", "sw_recent_hiv_females_adj2.png", "sw_recent_hiv_msm_adj2.png", "sw_recent_hcv_all_adj2.png", "sw_recent_hcv_males_adj2.png", "sw_recent_hcv_females_adj2.png", "sw_recent_hcv_msm_adj2.png")
rec_best <- c("sw_recent_hiv_all_best.png", "sw_recent_hiv_males_best.png", "sw_recent_hiv_females_best.png", "sw_recent_hiv_msm_best.png", "sw_recent_hcv_all_best.png", "sw_recent_hcv_males_best.png", "sw_recent_hcv_females_best.png", "sw_recent_hcv_msm_best.png")
sheet_names <- c("HIV_Sex_Work_All", "HIV_Sex_Work_Males", "HIV_Sex_Work_Females", "HIV_MSM", "HCV_Sex_Work_All", "HCV_Sex_Work_Males", "HCV_Sex_Work_Females", "HCV_MSM")
subgroup_names <- c("pub_status", "2016_bin", "incidence_method", "who_region", "lmic_4cat", "hiv_crim", "rob_3cat")
rec_best_subgroup <- c("recent_hiv_all_best_subgroup.png", "recent_hiv_males_best_subgroup.png", "recent_hiv_females_best_subgroup.png", "recent_hiv_msm_best_subgroup.png", "recent_hcv_all_best_subgroup.png", "recent_hcv_males_best_subgroup.png", "recent_hcv_females_best_subgroup.png", "recent_hcv_msm_best_subgroup.png")

# data cleaning

# convert to numeric and log transform
for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  df <- create_effect_best(df)
  df <- convert_to_numeric(df)
  df <- log_transform(df)
  dfs[[i]] <- df
}

# keep where use equals "yes"
for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  df <- filter_use_yes(df)
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

desired_cols <- c(
  "study", "rob_3cat", "cohort", "pub_status", "use", "year_start", "year_end", "mid_year", "2016_bin", "exposure_time_frame_bin", "country", "df",
  "moa_unadj", "effect_unadj", "effect_unadj_lb", "effect_unadj_ub", "effect_unadj_p",
  "moa_adj1", "effect_adj1", "effect_adj_lb1", "effect_adj_ub1", "effect_adj_p1", "effect_adj1_vars",
  "moa_adj2", "effect_adj2", "effect_adj_lb2", "effect_adj_ub2", "effect_adj_p2", "effect_adj2_vars",
  "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln"
)

virus_idx <- list(hiv = 1:3, hcv = 5:7)
labels <- c("Males and females", "Males", "Females")

for (virus in names(virus_idx)) {
  combined <- combine_and_convert(dfs, virus_idx[[virus]], labels, desired_cols)
  assign(paste0(virus, "_sw_combined"), combined, envir = .GlobalEnv)
}

# recent unadjusted sex work and HCV/HIV, overall and by sex
for (virus in c("hiv", "hcv")) {
  filename_combined <- paste0(
    "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_",
    virus,
    "_combined_unadj.png"
  )
  recent_unadj_forest_plot_combined(
    get(paste0(virus, "_sw_combined")),
    "recent",
    "effect_unadj_ln",
    "effect_unadj_lb_ln",
    "effect_unadj_ub_ln",
    "lead_author",
    "df",
    filename_combined
  )
}

hiv_msm_combine <- dfs[[4]] %>%
  mutate(df = "HIV") %>%
  select(all_of(desired_cols)) %>%
  mutate(across(all_of(desired_cols), as.character))

hcv_msm_combine <- dfs[[8]] %>%
  mutate(df = "HCV") %>%
  select(all_of(desired_cols)) %>%
  mutate(across(all_of(desired_cols), as.character))

msm_combined <- bind_rows(hiv_msm_combine, hcv_msm_combine)

filename_msm_combined <- "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_msm_combined_unadj.png"

msm_combined <- msm_combined %>%
  mutate(
    effect_unadj_ln = as.numeric(effect_unadj_ln),
    effect_unadj_lb_ln = as.numeric(effect_unadj_lb_ln),
    effect_unadj_ub_ln = as.numeric(effect_unadj_ub_ln)
  )

recent_unadj_forest_plot_combined(
  msm_combined,
  "recent",
  "effect_unadj_ln",
  "effect_unadj_lb_ln",
  "effect_unadj_ub_ln",
  "lead_author",
  "df",
  filename_msm_combined
)















# subgroup forest plots

# recent best effect forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/", rec_best_subgroup[i])
  print(filename)  # Print the filename to confirm
  subgroup_analysis_recent_best(dfs[[i]], "recent", "effect_best_ln", "effect_best_lb_ln", "effect_best_ub_ln", "lead_author", subgroup_names, filename)
}


# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "writexl", "tidyverse", "purrr")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframes
hiv_sw_all <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - All") 
hiv_sw_males <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - Male") 
hiv_sw_females <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - Female") 
hiv_msm <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - MSM") 
hcv_sw_all <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - All") 
hcv_sw_males <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - Male") 
hcv_sw_females <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - Female") 
hcv_msm <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - MSM") 
study_characteristics <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "Study characteristics") 

# define dataframes and lists
dfs <- list(hiv_sw_all, hiv_sw_males, hiv_sw_females, hiv_msm, hcv_sw_all, hcv_sw_males, hcv_sw_females, hcv_msm)
rec_unadj <- c("sw_recent_hiv_all_unadj.png", "sw_recent_hiv_males_unadj.png", "sw_recent_hiv_females_unadj.png", "sw_recent_hiv_msm_unadj.png", "sw_recent_hcv_all_unadj.png", "sw_recent_hcv_males_unadj.png", "sw_recent_hcv_females_unadj.png", "sw_recent_hcv_msm_unadj.png")
rec_adj <- c("sw_recent_hiv_all_adj.png", "sw_recent_hiv_males_adj.png", "sw_recent_hiv_females_adj.png", "sw_recent_hiv_msm_adj.png", "sw_recent_hcv_all_adj.png", "sw_recent_hcv_males_adj.png", "sw_recent_hcv_females_adj.png", "sw_recent_hcv_msm_adj.png")
sheet_names <- c("HIV_Sex_Work_All", "HIV_Sex_Work_Males", "HIV_Sex_Work_Females", "HIV_MSM", "HCV_Sex_Work_All", "HCV_Sex_Work_Males", "HCV_Sex_Work_Females", "HCV_MSM")
subgroup_names <- c("pub_status", "2010_bin", "incidence_method", "lmic_bin", "hiv_inc_bin", "hcv_inc_bin")
rec_unadj_subgroup <- c("recent_hiv_all_subgroup.png", "recent_hiv_males_subgroup.png", "recent_hiv_females_subgroup.png", "recent_hiv_msm_subgroup.png", "recent_hcv_all_subgroup.png", "recent_hcv_males_subgroup.png", "recent_hcv_females_subgroup.png", "recent_hcv_msm_subgroup.png")

# data cleaning

# incidence binary variables for study_characteristics
study_characteristics <- study_characteristics %>%
  select(study, hiv_cases, hiv_follow_up_years, hiv_inc, hiv_inc_95ci, 
         hcv_cases, hcv_follow_up_years, hcv_inc, hcv_inc_95ci) %>%
  # conver incidence cols to numeric, treating NR as NA
  mutate(
    hiv_inc = case_when(
      hiv_inc %in% c("NR") ~ NA_real_,
      TRUE ~ as.numeric(hiv_inc)
    ),
    hcv_inc = case_when(
      hcv_inc %in% c("NR") ~ NA_real_,
      TRUE ~ as.numeric(hcv_inc)
    )
  ) %>%
  # 3-level variables
  mutate(
    hiv_inc_bin = case_when(
      is.na(hiv_inc) ~ "Missing",
      hiv_inc < 2 ~ "Low",
      TRUE ~ "High"
    ),
    hcv_inc_bin = case_when(
      is.na(hcv_inc) ~ "Missing",
      hcv_inc < 15 ~ "Low",
      TRUE ~ "High"
    )
  ) %>%
  # factor
  mutate(
    hiv_inc_bin = factor(hiv_inc_bin, levels = c("Low", "High", "Missing")),
    hcv_inc_bin = factor(hcv_inc_bin, levels = c("Low", "High", "Missing"))
  )

# merge
dfs <- merge_study_characteristics(dfs, study_characteristics)

hiv_sw_males   <- dfs[[2]]
hiv_sw_females <- dfs[[3]]
hiv_msm        <- dfs[[4]]
hcv_sw_all     <- dfs[[5]]
hcv_sw_males   <- dfs[[6]]
hcv_sw_females <- dfs[[7]]
hcv_msm        <- dfs[[8]]

# convert to numeric and log transform
for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  df <- convert_to_numeric(df)
  df <- log_transform(df)
  dfs[[i]] <- df
}

# Recode lifetime OAT, homelessness, prison to NA before any plots/analyses
dfs <- lapply(dfs, recode_to_na)

# filtered data frames back to original variables
hiv_sw_all     <- dfs[[1]]
hiv_sw_males   <- dfs[[2]]
hiv_sw_females <- dfs[[3]]
hiv_msm        <- dfs[[4]]
hcv_sw_all     <- dfs[[5]]
hcv_sw_males   <- dfs[[6]]
hcv_sw_females <- dfs[[7]]
hcv_msm        <- dfs[[8]]

# recent unadjusted forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/unadjusted/recent/", rec_unadj[i])
  print(filename)
  recent_unadj_forest_plot(dfs[[i]], "recent", "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln", "lead_author", "pub_status", filename)
}

# recent adjusted forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/adjusted/recent/", rec_adj[i])
  print(filename)
  recent_adj_forest_plot(dfs[[i]], "recent", "effect_adj_ln", "effect_adj_lb_ln", "effect_adj_ub_ln", "lead_author", "pub_status", filename)
}

# lifetime unadjusted forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/unadjusted/lifetime/", gsub("recent", "lifetime", rec_unadj[i]))
  print(filename)
  lifetime_unadj_forest_plot(dfs[[i]], "lifetime", "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln", "lead_author", "pub_status", filename)
}

# lifetime adjusted forest plots
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/adjusted/lifetime/", gsub("recent", "lifetime", rec_adj[i]))
  print(filename)
  lifetime_adj_forest_plot(dfs[[i]], "lifetime", "effect_adj_ln", "effect_adj_lb_ln", "effect_adj_ub_ln", "lead_author", "pub_status", filename)
}

# run meta analyses and save results to workbook
summarise_pool <- function(df, time_bin, is_adjusted) {
  filtered_df <- df %>%
    filter(exposure_time_frame_bin == time_bin)

  if (is_adjusted) {
    filtered_df <- filtered_df %>% filter(!is.na(moa_adj))
    m <- metagen(
      TE = effect_adj_ln,
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
      text.random = "Overall"
    )
  } else {
    filtered_df <- filtered_df %>% filter(!is.na(moa_unadj))
    m <- metagen(
      TE = effect_unadj_ln,
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
      text.random = "Overall"
    )
  }

  data.frame(
    n_estimates = m$k,
    effect = exp(as.numeric(m$TE.random)),
    lower  = exp(as.numeric(m$lower.random)),
    upper  = exp(as.numeric(m$upper.random)),
    I2 = as.numeric(m$I2),
    p_value = as.numeric(m$pval.random)
  )
}

recent_unadjusted_rows <- vector("list", length(dfs))
recent_adjusted_rows   <- vector("list", length(dfs))
lifetime_unadjusted_rows <- vector("list", length(dfs))
lifetime_adjusted_rows   <- vector("list", length(dfs))

for (i in seq_along(dfs)) {
  recent_unadjusted_rows[[i]] <- cbind(
    model = sheet_names[i],
    summarise_pool(dfs[[i]], time_bin = "recent",   is_adjusted = FALSE)
  )
  recent_adjusted_rows[[i]] <- cbind(
    model = sheet_names[i],
    summarise_pool(dfs[[i]], time_bin = "recent",   is_adjusted = TRUE)
  )
  lifetime_unadjusted_rows[[i]] <- cbind(
    model = sheet_names[i],
    summarise_pool(dfs[[i]], time_bin = "lifetime", is_adjusted = FALSE)
  )
  lifetime_adjusted_rows[[i]] <- cbind(
    model = sheet_names[i],
    summarise_pool(dfs[[i]], time_bin = "lifetime", is_adjusted = TRUE)
  )
}

recent_unadjusted     <- dplyr::bind_rows(recent_unadjusted_rows)
recent_adjusted       <- dplyr::bind_rows(recent_adjusted_rows)
lifetime_unadjusted   <- dplyr::bind_rows(lifetime_unadjusted_rows)
lifetime_adjusted     <- dplyr::bind_rows(lifetime_adjusted_rows)

writexl::write_xlsx(
  list(
    recent_unadjusted = recent_unadjusted,
    recent_adjusted = recent_adjusted,
    lifetime_unadjusted = lifetime_unadjusted,
    lifetime_adjusted = lifetime_adjusted
  ),
  path = "code/forest_plot_results.xlsx"
)

# combined plotting

desired_cols <- c(
  "study", "rob_3cat", "cohort", "pub_status", "use", "year_start", "year_end", "mid_year", "2016_bin", "exposure_time_frame_bin", "country", "df",
  "moa_unadj", "effect_unadj", "effect_unadj_lb", "effect_unadj_ub", "effect_unadj_p",
  "moa_adj", "effect_adj", "effect_adj_lb", "effect_adj_ub", "effect_adj_p", "effect_adj_vars",
  "effect_unadj_ln", "effect_unadj_lb_ln", "effect_unadj_ub_ln"
)

virus_idx <- list(hiv = 1:3, hcv = 5:7)
labels <- c("Males and females", "Males", "Females")

for (virus in names(virus_idx)) {
  combined <- combine_and_convert(dfs, virus_idx[[virus]], labels, desired_cols)
  assign(paste0(virus, "_sw_combined"), combined, envir = .GlobalEnv)
}

# unadjusted sex work and HCV/HIV, overall and by sex
for (virus in c("hiv", "hcv")) {

  # removes rows with missing MoA
  filtered_data <- get(paste0(virus, "_sw_combined")) %>%
    filter(!is.na(effect_unadj_ln), !is.na(effect_unadj_lb_ln), !is.na(effect_unadj_ub_ln))
  
  # recent plot
  filename_combined_recent <- paste0(
    "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_",
    virus,
    "_combined_unadj.png"
  )
  recent_unadj_forest_plot_combined(
    filtered_data,
    "recent",
    "effect_unadj_ln",
    "effect_unadj_lb_ln",
    "effect_unadj_ub_ln",
    "lead_author",
    "df",
    filename_combined_recent
  )
  
  # lifetime plot
  filename_combined_lifetime <- paste0(
    "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_lifetime_",
    virus,
    "_combined_unadj.png"
  )
  lifetime_unadj_forest_plot_combined(
    filtered_data,
    "lifetime",
    "effect_unadj_ln",
    "effect_unadj_lb_ln",
    "effect_unadj_ub_ln",
    "lead_author",
    "df",
    filename_combined_lifetime
  )
}

# combine MSM data for recent and lifetime
hiv_msm_combine <- dfs[[4]] %>%
  mutate(df = "HIV") %>%
  select(all_of(desired_cols)) %>%
  mutate(across(all_of(desired_cols), as.character))

hcv_msm_combine <- dfs[[8]] %>%
  mutate(df = "HCV") %>%
  select(all_of(desired_cols)) %>%
  mutate(across(all_of(desired_cols), as.character))

msm_combined <- bind_rows(hiv_msm_combine, hcv_msm_combine) %>%
  filter(!is.na(effect_unadj_ln), !is.na(effect_unadj_lb_ln), !is.na(effect_unadj_ub_ln))

# recent MSM plot
filename_msm_combined_recent <- "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_msm_combined_unadj.png"

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
  filename_msm_combined_recent
)

# lifetime MSM plot
filename_msm_combined_lifetime <- "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_lifetime_msm_combined_unadj.png"

lifetime_unadj_forest_plot_combined(
  msm_combined,
  "lifetime",
  "effect_unadj_ln",
  "effect_unadj_lb_ln",
  "effect_unadj_ub_ln",
  "lead_author",
  "df",
  filename_msm_combined_lifetime
)

# subgroup forest plots

# recent unadjusted effect forest plots by subgroup
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/subgroups/unadjusted/", rec_unadj[i])
  print(filename)
  subgroup_analysis_recent_unadj(
    dfs[[i]],
    "recent",
    "lead_author",
    subgroup_names,
    filename
  )
}

# lifetime unadjusted effect forest plots by subgroup
for (i in 1:length(dfs)) {
  filename <- paste0("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/subgroups/unadjusted/", gsub("recent", "lifetime", rec_unadj[i]))
  print(filename)
  subgroup_analysis_lifetime_unadj(
    dfs[[i]],
    "lifetime",
    "lead_author",
    subgroup_names,
    filename
  )
}

# meta regression

# Add effect_unadj_se to each data frame
hiv_sw_all <- add_effect_unadj_se(hiv_sw_all)
hiv_sw_males <- add_effect_unadj_se(hiv_sw_males)
hiv_sw_females <- add_effect_unadj_se(hiv_sw_females)
hiv_msm <- add_effect_unadj_se(hiv_msm)
hcv_sw_all <- add_effect_unadj_se(hcv_sw_all)
hcv_sw_males <- add_effect_unadj_se(hcv_sw_males)
hcv_sw_females <- add_effect_unadj_se(hcv_sw_females)
hcv_msm <- add_effect_unadj_se(hcv_msm)

# apply meta-regression to each df
meta_regress_hiv_sw_all_strata     <- meta_regress_strata_summary(hiv_sw_all)
meta_regress_hiv_sw_males_strata   <- meta_regress_strata_summary(hiv_sw_males)
meta_regress_hiv_sw_females_strata <- meta_regress_strata_summary(hiv_sw_females)
meta_regress_hiv_msm_strata        <- meta_regress_strata_summary(hiv_msm)
meta_regress_hcv_sw_all_strata     <- meta_regress_strata_summary(hcv_sw_all)
meta_regress_hcv_sw_males_strata   <- meta_regress_strata_summary(hcv_sw_males)
meta_regress_hcv_sw_females_strata <- meta_regress_strata_summary(hcv_sw_females)
meta_regress_hcv_msm_strata        <- meta_regress_strata_summary(hcv_msm)

# save
writexl::write_xlsx(
  list(
    hiv_sw_all     = meta_regress_hiv_sw_all_strata,
    hiv_sw_males   = meta_regress_hiv_sw_males_strata,
    hiv_sw_females = meta_regress_hiv_sw_females_strata,
    hiv_msm        = meta_regress_hiv_msm_strata,
    hcv_sw_all     = meta_regress_hcv_sw_all_strata,
    hcv_sw_males   = meta_regress_hcv_sw_males_strata,
    hcv_sw_females = meta_regress_hcv_sw_females_strata,
    hcv_msm        = meta_regress_hcv_msm_strata
  ),
  path = "code/meta_regression_strata_summary.xlsx"
)

# publication bias

# publication bias tests for each dataframe (recent)
test_publication_bias(
  hiv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_all_unadj.png"
)

test_publication_bias(
  hiv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_males_unadj.png"
)

test_publication_bias(
  hiv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_females_unadj.png"
)

test_publication_bias(
  hiv_msm %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_msm_unadj.png"
)

test_publication_bias(
  hcv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_all_unadj.png"
)

test_publication_bias(
  hcv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_males_unadj.png"
)

test_publication_bias(
  hcv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_females_unadj.png"
)

test_publication_bias(
  hcv_msm %>% filter(exposure_time_frame_bin == "recent"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_msm_unadj.png"
)

# filtered dataframes and plot titles
funnel_dfs <- list(
  hiv_sw_all     = hiv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  hiv_sw_males   = hiv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  hiv_sw_females = hiv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  hiv_msm        = hiv_msm %>% filter(exposure_time_frame_bin == "recent"),
  hcv_sw_all     = hcv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  hcv_sw_males   = hcv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  hcv_sw_females = hcv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  hcv_msm        = hcv_msm %>% filter(exposure_time_frame_bin == "recent")
)
titles <- c(
  "HIV SW All", "HIV SW Males", "HIV SW Females", "HIV MSM",
  "HCV SW All", "HCV SW Males", "HCV SW Females", "HCV MSM"
)

# filename
png("code/plots/publication bias/funnel_combined_recent_unadj.png", width = 2400, height = 1200, res = 200)
par(mfrow = c(2, 4), oma = c(0, 0, 2, 0)) # 2 rows, 4 columns

for (i in seq_along(funnel_dfs)) {
  df <- funnel_dfs[[i]]
  
  # colors based on pub_status
  pub_status_levels <- unique(df$pub_status)
  colors <- rainbow(length(pub_status_levels))
  names(colors) <- pub_status_levels
  point_colors <- colors[df$pub_status]
  
  # meta-analysis model
  res <- metafor::rma(yi = df$effect_unadj_ln, sei = df$effect_unadj_se, method = "DL")
  
  # Egger's test
  egger <- metafor::regtest(res, model = "rma", predictor = "sei")
  
  # title with p-value
  p_value <- round(egger$pval, 3)
  plot_title <- paste0(titles[i], "\nEgger's test p = ", p_value)
  
  # funnel plot with colors
  metafor::funnel(res, main = plot_title, col = point_colors, pch = 19)
  
  # legend
  legend("topright", 
         legend = names(colors), 
         col = colors, 
         pch = 19, 
         title = "Publication Status",
         bty = "n",
         cex = 0.6)  # Even smaller text for all plots
}

mtext("Funnel Plots for Publication Bias (Recent Unadjusted Estimates)", outer = TRUE, cex = 1.5)
dev.off()

# publication bias tests for each dataframe (lifetime)
test_publication_bias(
  hiv_sw_all %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_all_lifetime_unadj.png"
)

test_publication_bias(
  hiv_sw_males %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_males_lifetime_unadj.png"
)

test_publication_bias(
  hiv_sw_females %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_sw_females_lifetime_unadj.png"
)

test_publication_bias(
  hiv_msm %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hiv_msm_lifetime_unadj.png"
)

test_publication_bias(
  hcv_sw_all %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_all_lifetime_unadj.png"
)

test_publication_bias(
  hcv_sw_males %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_males_lifetime_unadj.png"
)

test_publication_bias(
  hcv_sw_females %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_sw_females_lifetime_unadj.png"
)

test_publication_bias(
  hcv_msm %>% filter(exposure_time_frame_bin == "lifetime"),
  yi_col = "effect_unadj_ln",
  sei_col = "effect_unadj_se",
  plot_filename = "code/plots/publication bias/funnel_hcv_msm_lifetime_unadj.png"
)

# filename
png("code/plots/publication bias/funnel_combined_lifetime_unadj.png", width = 2400, height = 1200, res = 200)
par(mfrow = c(2, 4), oma = c(0, 0, 2, 0)) # 2 rows, 4 columns

for (i in seq_along(funnel_dfs_lifetime)) {
  df <- funnel_dfs_lifetime[[i]]
  
  # colors based on pub_status
  pub_status_levels <- unique(df$pub_status)
  colors <- rainbow(length(pub_status_levels))
  names(colors) <- pub_status_levels
  point_colors <- colors[df$pub_status]
  
  # meta-analysis model
  res <- metafor::rma(yi = df$effect_unadj_ln, sei = df$effect_unadj_se, method = "DL")
  
  # Egger's test
  egger <- metafor::regtest(res, model = "rma", predictor = "sei")
  
  # title with p-value
  p_value <- round(egger$pval, 3)
  plot_title <- paste0(titles_lifetime[i], "\nEgger's test p = ", p_value)
  
  # funnel plot with colors
  metafor::funnel(res, main = plot_title, col = point_colors, pch = 19)
  
  # legend
  legend("topright", 
         legend = names(colors), 
         col = colors, 
         pch = 19, 
         title = "Publication Status",
         bty = "n",
         cex = 0.6)  # Small text for combined plot
}

mtext("Funnel Plots for Publication Bias (Lifetime Unadjusted Estimates)", outer = TRUE, cex = 1.5)
dev.off()

# Egger's test by publication status
egger_test_by_pub_status <- function(df, yi_col, sei_col) {
  # remove missing values
  df <- df %>% filter(!is.na(.data[[yi_col]]), !is.na(.data[[sei_col]]))
  
  results <- list()
  
  # unique publication statuses
  pub_statuses <- unique(df$pub_status)
  
  for (status in pub_statuses) {
    subset_df <- df %>% filter(pub_status == status)
    
    if (nrow(subset_df) >= 3) {  # Need at least 3 studies for Egger's test
      # random-effects model
      res <- metafor::rma(yi = subset_df[[yi_col]], sei = subset_df[[sei_col]], method = "DL")
      
      # Egger's test
      egger <- metafor::regtest(res, model = "rma", predictor = "sei")
      
      results[[status]] <- list(
        n_studies = nrow(subset_df),
        egger_test = egger,
        p_value = egger$pval,
        estimate = egger$est,
        se = egger$se,
        ci_lower = egger$ci.lb,
        ci_upper = egger$ci.ub
      )
      
      cat("Publication Status:", status, "\n")
      cat("Number of studies:", nrow(subset_df), "\n")
      cat("Egger's test p-value:", round(egger$pval, 3), "\n")
      cat("Estimate:", round(egger$est, 3), "\n")
      cat("95% CI:", round(egger$ci.lb, 3), "-", round(egger$ci.ub, 3), "\n")
      cat("---\n")
      
    } else {
      results[[status]] <- list(
        n_studies = nrow(subset_df),
        egger_test = NULL,
        p_value = NA,
        message = "Insufficient studies for Egger's test"
      )
      
      cat("Publication Status:", status, "\n")
      cat("Number of studies:", nrow(subset_df), "\n")
      cat("Insufficient studies for Egger's test\n")
      cat("---\n")
    }
  }
  
  return(results)
}

egger_by_pub_status_results <- list()

datasets <- list(
  "HIV_SW_All" = hiv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  "HIV_SW_Males" = hiv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  "HIV_SW_Females" = hiv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  "HIV_MSM" = hiv_msm %>% filter(exposure_time_frame_bin == "recent"),
  "HCV_SW_All" = hcv_sw_all %>% filter(exposure_time_frame_bin == "recent"),
  "HCV_SW_Males" = hcv_sw_males %>% filter(exposure_time_frame_bin == "recent"),
  "HCV_SW_Females" = hcv_sw_females %>% filter(exposure_time_frame_bin == "recent"),
  "HCV_MSM" = hcv_msm %>% filter(exposure_time_frame_bin == "recent")
)

for (dataset_name in names(datasets)) {
  cat("\n========================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("========================================\n")
  
  egger_by_pub_status_results[[dataset_name]] <- egger_test_by_pub_status(
    datasets[[dataset_name]], 
    "effect_unadj_ln", 
    "effect_unadj_se"
  )
}

# summary table
create_egger_summary_table <- function(results_list) {
  summary_rows <- list()
  
  for (dataset_name in names(results_list)) {
    dataset_results <- results_list[[dataset_name]]
    
    for (pub_status in names(dataset_results)) {
      result <- dataset_results[[pub_status]]
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Dataset = dataset_name,
        Publication_Status = pub_status,
        N_Studies = result$n_studies,
        Egger_P_Value = ifelse(is.null(result$p_value), NA, result$p_value),
        Egger_Estimate = ifelse(is.null(result$estimate), NA, result$estimate),
        Egger_SE = ifelse(is.null(result$se), NA, result$se),
        Egger_CI_Lower = ifelse(is.null(result$ci_lower), NA, result$ci_lower),
        Egger_CI_Upper = ifelse(is.null(result$ci_upper), NA, result$ci_upper),
        Sufficient_Studies = !is.null(result$egger_test)
      )
    }
  }
  
  return(bind_rows(summary_rows))
}

# summary table
egger_summary_table <- create_egger_summary_table(egger_by_pub_status_results)

# save
write_xlsx(egger_summary_table, "code/egger_test_by_publication_status.xlsx")

# summary table
print(egger_summary_table)
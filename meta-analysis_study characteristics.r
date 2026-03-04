# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "writexl", "tidyverse", "purrr")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframe
study_characteristics <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "Study characteristics") 

# total number of unique studies
total_studies <- study_characteristics %>%
  summarise(unique_studies = n_distinct(study)) %>%
  pull(unique_studies)
total_studies

# total number of estimates
total_estimates <- study_characteristics %>%
  summarise(total_estimates = sum(estimates, na.rm = TRUE)) %>%
  pull(total_estimates)
total_estimates

# total estimates and studies for HIV/HCV
summary_table <- study_characteristics %>%
  mutate(group = case_when(
    hiv == "Yes" & hcv == "Yes" ~ "Both HIV and HCV",
    hiv == "Yes" & hcv != "Yes" ~ "HIV only",
    hcv == "Yes" & hiv != "Yes" ~ "HCV only"
  )) %>%
  group_by(group) %>%
  summarise(
    total_estimates = sum(estimates, na.rm = TRUE),
    total_studies = n_distinct(study),
    .groups = "drop"
  )

# total estimates and studies for Sex work/MSM
sw_msm_table <- study_characteristics %>%
  mutate(group = case_when(
    sex_work == "Yes" & msm == "Yes" ~ "Both Sex work and MSM",
    sex_work == "Yes" & msm != "Yes" ~ "Sex work only",
    msm == "Yes" & sex_work != "Yes" ~ "MSM only"
  )) %>%
  group_by(group) %>%
  summarise(
    total_estimates = sum(estimates, na.rm = TRUE),
    total_studies = n_distinct(study),
    .groups = "drop"
  )

# estimates by article_type
estimates_table <- study_characteristics %>%
  rename(group = article_type) %>%
  group_by(group) %>%
  summarise(
    total_estimates = sum(estimates, na.rm = TRUE),
    total_studies = n_distinct(study),
    .groups = "drop"
  )

# total row
total_table <- tibble(
  group = "Total",
  total_estimates = total_estimates,
  total_studies = total_studies
)

# combine tables
final_summary <- bind_rows(
  summary_table,
  sw_msm_table,
  estimates_table,
  total_table
) %>%
  select(group, total_studies, total_estimates)

# print the final summary table
print(final_summary)

# columns to keep
cols_to_keep <- c(
  "title", "study", "lead_author", "published", "city", "country", "who_region", "lmic_4cat", "lmic_bin", "msm_decrim", "hiv_crim", "rob_3cat", "cohort", "pub_status", "use", "exposure_time_frame_bin", "incidence_method", "male_num", "male_perc", "msm_num", "msm_perc", "msm_definition", "exposure_time_frame", "exposed_num", "exposed_perc", "exposed_incidence_100py", "exposed_incidence_lb", "exposed_incidence_ub", "unexposed_num", "unexposed_perc", "unexposed_incidence_100py", "unexposed_incidence_lb", "unexposed_incidence_ub", "disease"
)

# Ensure 'male_perc' is numeric and add 'disease' column
hiv_msm_subset <- hiv_msm %>%
  mutate(
    male_perc = as.numeric(male_perc),
    disease = "hiv"
  )

hcv_msm_subset <- hcv_msm_sens %>%
  mutate(
    male_perc = as.numeric(male_perc),
    disease = "hcv"
  )

# combine columns from hiv_msm and hcv_msm
msm_all <- bind_rows(
  hiv_msm_subset %>% select(any_of(cols_to_keep)),
  hcv_msm_subset %>% select(any_of(cols_to_keep))
)

# unique studies in msm_all
unique_studies_msm <- msm_all %>%
  summarise(unique_studies = n_distinct(study)) %>%
  pull(unique_studies)

print(unique_studies_msm)

# studies reporting recent and lifetime exposure_time_frame_bin, overall and by disease
study_counts <- msm_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  group_by(disease, exposure_time_frame_bin) %>%
  summarise(num_studies = n_distinct(study), .groups = "drop") %>%
  bind_rows(
    msm_all %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(num_studies = n_distinct(study)) %>%
      mutate(disease = "Overall")
  ) %>%
  select(disease, exposure_time_frame_bin, num_studies)

print(study_counts)

# deduplicate studies which have both hiv and hcv estimates
msm_overall_dedup <- msm_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(study, exposure_time_frame_bin) %>%
  summarise(
    exposed_num = sum(exposed_num, na.rm = TRUE),
    unexposed_num = sum(unexposed_num, na.rm = TRUE),
    total_n = exposed_num + unexposed_num,
    .groups = "drop"
  ) %>%
  filter(total_n > 0)

# number and proportion of individuals reporting recent and lifetime MSM exposure, overall and by disease
msm_exposure_summary <- msm_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(disease, exposure_time_frame_bin) %>%
  summarise(
    total_exposed = sum(exposed_num, na.rm = TRUE),
    total_unexposed = sum(unexposed_num, na.rm = TRUE),
    total = total_exposed + total_unexposed,
    proportion_exposed = total_exposed / total,
    .groups = "drop"
  ) %>%
  bind_rows(
    msm_overall_dedup %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total = sum(total_n, na.rm = TRUE),
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(disease = "Overall")
  ) %>%
  select(disease, exposure_time_frame_bin, total_exposed, total, proportion_exposed)

print(as.data.frame(msm_exposure_summary))

# meta-analysis for MSM exposure proportions
msm_meta_results <- list()

# meta-analysis for each disease/exposure combination
perform_meta_analysis <- function(data, disease_filter, exposure_filter) {
  subset_data <- data %>%
    filter(disease == disease_filter, exposure_time_frame_bin == exposure_filter) %>%
    mutate(
      exposed_num = as.numeric(exposed_num),
      unexposed_num = as.numeric(unexposed_num),
      total_n = exposed_num + unexposed_num
    ) %>%
    filter(!is.na(exposed_num), !is.na(total_n), total_n > 0)
  
  if (nrow(subset_data) > 1) {
    meta_result <- metaprop(
      event = subset_data$exposed_num,
      n = subset_data$total_n,
      studlab = subset_data$study,
      method = "GLMM",
      sm = "PLOGIT",
      random = TRUE,
      prediction = TRUE,
      title = paste("MSM exposure -", disease_filter, exposure_filter)
    )
    return(meta_result)
  } else {
    return(NULL)
  }
}

# perform meta-analysis for overall
perform_overall_meta_analysis <- function(data, exposure_filter) {
  subset_data <- data %>%
    filter(exposure_time_frame_bin == exposure_filter) %>%
    mutate(
      exposed_num = as.numeric(exposed_num),
      unexposed_num = as.numeric(unexposed_num),
      total_n = exposed_num + unexposed_num
    ) %>%
    filter(!is.na(exposed_num), !is.na(total_n), total_n > 0)
  
  if (nrow(subset_data) > 1) {
    meta_result <- metaprop(
      event = subset_data$exposed_num,
      n = subset_data$total_n,
      studlab = subset_data$study,
      method = "GLMM",
      sm = "PLOGIT",
      random = TRUE,
      prediction = TRUE,
      title = paste("MSM exposure - Overall", exposure_filter)
    )
    return(meta_result)
  } else {
    return(NULL)
  }
}

# combinations of disease and exposure_time_frame_bin
combinations <- msm_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(disease, exposure_time_frame_bin)

# meta-analysis for each disease/exposure combination
for (i in 1:nrow(combinations)) {
  disease_val <- combinations$disease[i]
  exposure_val <- combinations$exposure_time_frame_bin[i]
  
  meta_result <- perform_meta_analysis(msm_all, disease_val, exposure_val)
  
  if (!is.null(meta_result)) {
    msm_meta_results[[paste(disease_val, exposure_val, sep = "_")]] <- meta_result
  }
}

# meta-analysis for overall combinations
unique_exposures <- msm_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(exposure_time_frame_bin)

for (i in 1:nrow(unique_exposures)) {
  exposure_val <- unique_exposures$exposure_time_frame_bin[i]
  
  meta_result <- perform_overall_meta_analysis(msm_overall_dedup, exposure_val)
  
  if (!is.null(meta_result)) {
    msm_meta_results[[paste("Overall", exposure_val, sep = "_")]] <- meta_result
  }
}

# pooled proportions
msm_pooled_proportions <- tibble()

for (name in names(msm_meta_results)) {
  meta_obj <- msm_meta_results[[name]]
  parts <- strsplit(name, "_")[[1]]
  disease_name <- parts[1]
  exposure_name <- paste(parts[-1], collapse = "_")

  pooled_prop <- tibble(
    disease = disease_name,
    exposure_time_frame_bin = exposure_name,
    pooled_proportion = exp(meta_obj$TE.random) / (1 + exp(meta_obj$TE.random)),
    pooled_lower = exp(meta_obj$lower.random) / (1 + exp(meta_obj$lower.random)),
    pooled_upper = exp(meta_obj$upper.random) / (1 + exp(meta_obj$upper.random)),
    i2 = meta_obj$I2,
    tau2 = meta_obj$tau2,
    p_value = meta_obj$pval.random,
    n_studies = meta_obj$k    # NEW: count of studies contributing
  )

  msm_pooled_proportions <- bind_rows(msm_pooled_proportions, pooled_prop)
}

msm_exposure_final <- msm_exposure_summary %>%
  left_join(msm_pooled_proportions, by = c("disease", "exposure_time_frame_bin"))

print(as.data.frame(msm_exposure_final))

# Save MSM exposure results to Excel
write_xlsx(msm_exposure_final, "Drafts/Study characteristics/msm_exposure_final.xlsx")

# Define the columns to keep
cols_to_keep <- c(
  "title", "study", "lead_author", "published", "city", "country", "who_region", "lmic_4cat", "lmic_bin", "hiv_crim", "rob_3cat", "cohort", "pub_status", "use", "exposure_time_frame_bin", "incidence_method", "female_num", "female_perc", "exposure_time_frame", "exposed_num", "exposed_perc", "exposed_incidence_100py", "exposed_incidence_lb", "exposed_incidence_ub", "unexposed_num", "unexposed_perc", "unexposed_incidence_100py", "unexposed_incidence_lb", "unexposed_incidence_ub", "disease"
)

# Ensure 'female_perc' is numeric and add 'disease' column
hiv_sw_all_subset <- hiv_sw_all %>%
  mutate(
    female_perc = as.numeric(female_perc),
    disease = "hiv"
  )

hcv_sw_all_subset <- hcv_sw_all %>%
  mutate(
    female_perc = as.numeric(female_perc),
    disease = "hcv"
  )

# combine
sw_all <- bind_rows(
  hiv_sw_all_subset %>% select(any_of(cols_to_keep)),
  hcv_sw_all_subset %>% select(any_of(cols_to_keep))
)

# unique studies in sw_all
unique_studies_sw <- sw_all %>%
  summarise(unique_studies = n_distinct(study)) %>%
  pull(unique_studies)

print(unique_studies_sw)

# studies reporting recent and lifetime exposure_time_frame_bin, overall and by disease
sw_study_counts <- sw_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  group_by(disease, exposure_time_frame_bin) %>%
  summarise(num_studies = n_distinct(study), .groups = "drop") %>%
  bind_rows(
    sw_all %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(num_studies = n_distinct(study)) %>%
      mutate(disease = "Overall")
  ) %>%
  select(disease, exposure_time_frame_bin, num_studies)

print(sw_study_counts)

# combined dataset for males and females
sw_males <- bind_rows(
  hiv_sw_males %>% 
    mutate(disease = "hiv") %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(exposed_num, unexposed_num), as.numeric)),
  hcv_sw_males %>% 
    mutate(disease = "hcv") %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(exposed_num, unexposed_num), as.numeric))
)

sw_females <- bind_rows(
  hiv_sw_females %>% 
    mutate(disease = "hiv") %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(exposed_num, unexposed_num), as.numeric)),
  hcv_sw_females %>% 
    mutate(disease = "hcv") %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(exposed_num, unexposed_num), as.numeric))
)

# deduplicate studies which report hiv and hcv estimates
sw_all_overall_dedup <- sw_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(study, exposure_time_frame_bin) %>%
  summarise(
    exposed_num = sum(exposed_num, na.rm = TRUE),
    unexposed_num = sum(unexposed_num, na.rm = TRUE),
    total_n = exposed_num + unexposed_num,
    .groups = "drop"
  ) %>%
  filter(total_n > 0)

sw_males_overall_dedup <- sw_males %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(study, exposure_time_frame_bin) %>%
  summarise(
    exposed_num = sum(exposed_num, na.rm = TRUE),
    unexposed_num = sum(unexposed_num, na.rm = TRUE),
    total_n = exposed_num + unexposed_num,
    .groups = "drop"
  ) %>%
  filter(total_n > 0)

sw_females_overall_dedup <- sw_females %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(study, exposure_time_frame_bin) %>%
  summarise(
    exposed_num = sum(exposed_num, na.rm = TRUE),
    unexposed_num = sum(unexposed_num, na.rm = TRUE),
    total_n = exposed_num + unexposed_num,
    .groups = "drop"
  ) %>%
  filter(total_n > 0)

# number and proportion of individuals reporting recent and lifetime sex work exposure
sw_exposure_summary <- sw_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(disease, exposure_time_frame_bin) %>%
  summarise(
    total_exposed = sum(exposed_num, na.rm = TRUE),
    total_unexposed = sum(unexposed_num, na.rm = TRUE),
    total = total_exposed + total_unexposed,
    proportion_exposed = total_exposed / total,
    .groups = "drop"
  ) %>%
  mutate(sex = "All") %>%
  bind_rows(
    # males
    sw_males %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      mutate(
        exposed_num = as.numeric(exposed_num),
        unexposed_num = as.numeric(unexposed_num)
      ) %>%
      group_by(disease, exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total_unexposed = sum(unexposed_num, na.rm = TRUE),
        total = total_exposed + total_unexposed,
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(sex = "Males")
  ) %>%
  bind_rows(
    # females
    sw_females %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      mutate(
        exposed_num = as.numeric(exposed_num),
        unexposed_num = as.numeric(unexposed_num)
      ) %>%
      group_by(disease, exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total_unexposed = sum(unexposed_num, na.rm = TRUE),
        total = total_exposed + total_unexposed,
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(sex = "Females")
  ) %>%
  bind_rows(
    # males and females for hiv and hcv
    sw_all_overall_dedup %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total = sum(total_n, na.rm = TRUE),
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(disease = "Overall", sex = "All")
    ) %>%
    bind_rows(
    # males for hiv and hcv
    sw_males_overall_dedup %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total = sum(total_n, na.rm = TRUE),
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(disease = "Overall", sex = "Males")
    ) %>%
    bind_rows(
    # females for hiv and hcv
    sw_females_overall_dedup %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total = sum(total_n, na.rm = TRUE),
        proportion_exposed = total_exposed / total,
        .groups = "drop"
      ) %>%
      mutate(disease = "Overall", sex = "Females")
  ) %>%
  select(disease, sex, exposure_time_frame_bin, total_exposed, total, proportion_exposed)

print(as.data.frame(sw_exposure_summary))

# sex work exposure proportions
sw_meta_results <- list()

# include sex in meta-analysis
perform_sw_meta_analysis_with_sex <- function(data, disease_filter, exposure_filter, sex_filter = NULL) {
  if (is.null(sex_filter)) {
    subset_data <- data %>%
      filter(disease == disease_filter, exposure_time_frame_bin == exposure_filter)
  } else {
    subset_data <- data %>%
      filter(exposure_time_frame_bin == exposure_filter)
  }
  
  subset_data <- subset_data %>%
    mutate(
      exposed_num = as.numeric(exposed_num),
      unexposed_num = as.numeric(unexposed_num),
      total_n = exposed_num + unexposed_num
    ) %>%
    filter(!is.na(exposed_num), !is.na(total_n), total_n > 0)
  
  if (nrow(subset_data) > 1) {
    title_suffix <- if(is.null(sex_filter)) paste(disease_filter, exposure_filter) else paste(sex_filter, disease_filter, exposure_filter)
    meta_result <- metaprop(
      event = subset_data$exposed_num,
      n = subset_data$total_n,
      studlab = subset_data$study,
      method = "GLMM",
      sm = "PLOGIT",
      random = TRUE,
      prediction = TRUE,
      title = paste("Sex work exposure -", title_suffix)
    )
    return(meta_result)
  } else {
    return(NULL)
  }
}

# overall analyses (combining hiv and hcv)
perform_sw_overall_meta_analysis_with_sex <- function(data, exposure_filter) {
  subset_data <- data %>%
    filter(exposure_time_frame_bin == exposure_filter) %>%
    mutate(
      exposed_num = as.numeric(exposed_num),
      unexposed_num = as.numeric(unexposed_num),
      total_n = exposed_num + unexposed_num
    ) %>%
    filter(!is.na(exposed_num), !is.na(total_n), total_n > 0)
  
  if (nrow(subset_data) > 1) {
    meta_result <- metaprop(
      event = subset_data$exposed_num,
      n = subset_data$total_n,
      studlab = subset_data$study,
      method = "GLMM",
      sm = "PLOGIT",
      random = TRUE,
      prediction = TRUE,
      title = paste("Sex work exposure - Overall", exposure_filter)
    )
    return(meta_result)
  } else {
    return(NULL)
  }
}

# unique combinations for each analysis
sw_combinations <- sw_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(disease, exposure_time_frame_bin)

# male and female analayses
for (i in 1:nrow(sw_combinations)) {
  disease_val <- sw_combinations$disease[i]
  exposure_val <- sw_combinations$exposure_time_frame_bin[i]
  
  meta_result <- perform_sw_meta_analysis_with_sex(sw_all, disease_val, exposure_val)
  
  if (!is.null(meta_result)) {
    sw_meta_results[[paste(disease_val, "All", exposure_val, sep = "_")]] <- meta_result
  }
}

# male analyses
sw_males_combinations <- sw_males %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(disease, exposure_time_frame_bin)

for (i in 1:nrow(sw_males_combinations)) {
  disease_val <- sw_males_combinations$disease[i]
  exposure_val <- sw_males_combinations$exposure_time_frame_bin[i]
  
  meta_result <- perform_sw_meta_analysis_with_sex(sw_males, disease_val, exposure_val, "Males")
  
  if (!is.null(meta_result)) {
    sw_meta_results[[paste(disease_val, "Males", exposure_val, sep = "_")]] <- meta_result
  }
}

# female analyses
sw_females_combinations <- sw_females %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(disease, exposure_time_frame_bin)

for (i in 1:nrow(sw_females_combinations)) {
  disease_val <- sw_females_combinations$disease[i]
  exposure_val <- sw_females_combinations$exposure_time_frame_bin[i]
  
  meta_result <- perform_sw_meta_analysis_with_sex(sw_females, disease_val, exposure_val, "Females")
  
  if (!is.null(meta_result)) {
    sw_meta_results[[paste(disease_val, "Females", exposure_val, sep = "_")]] <- meta_result
  }
}

# combined hiv and hcv for males and females
sw_unique_exposures <- sw_all %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  distinct(exposure_time_frame_bin)

for (i in 1:nrow(sw_unique_exposures)) {
  exposure_val <- sw_unique_exposures$exposure_time_frame_bin[i]
  
meta_result <- perform_sw_overall_meta_analysis_with_sex(sw_all_overall_dedup, exposure_val)

  if (!is.null(meta_result)) {
    sw_meta_results[[paste("Overall", "All", exposure_val, sep = "_")]] <- meta_result
  }
}

# combined hiv and hcv for males
for (i in 1:nrow(sw_unique_exposures)) {
  exposure_val <- sw_unique_exposures$exposure_time_frame_bin[i]
  
  meta_result <- perform_sw_overall_meta_analysis_with_sex(sw_males_overall_dedup, exposure_val)
  
  if (!is.null(meta_result)) {
    sw_meta_results[[paste("Overall", "Males", exposure_val, sep = "_")]] <- meta_result
  }
}

# combined hiv and hcv for females
for (i in 1:nrow(sw_unique_exposures)) {
  exposure_val <- sw_unique_exposures$exposure_time_frame_bin[i]
  
  meta_result <- perform_sw_overall_meta_analysis_with_sex(sw_females_overall_dedup, exposure_val)
  
  if (!is.null(meta_result)) {
    sw_meta_results[[paste("Overall", "Females", exposure_val, sep = "_")]] <- meta_result
  }
}

# pooled proportions
sw_pooled_proportions <- tibble()

for (name in names(sw_meta_results)) {
  meta_obj <- sw_meta_results[[name]]
  parts <- strsplit(name, "_")[[1]]
  disease_name <- parts[1]
  sex_name <- parts[2]
  exposure_name <- paste(parts[3:length(parts)], collapse = "_")

  pooled_prop <- tibble(
    disease = disease_name,
    sex = sex_name,
    exposure_time_frame_bin = exposure_name,
    pooled_proportion = exp(meta_obj$TE.random) / (1 + exp(meta_obj$TE.random)),
    pooled_lower = exp(meta_obj$lower.random) / (1 + exp(meta_obj$lower.random)),
    pooled_upper = exp(meta_obj$upper.random) / (1 + exp(meta_obj$upper.random)),
    i2 = meta_obj$I2,
    tau2 = meta_obj$tau2,
    p_value = meta_obj$pval.random,
    n_studies = meta_obj$k
  )

  sw_pooled_proportions <- bind_rows(sw_pooled_proportions, pooled_prop)
}

sw_exposure_final <- sw_exposure_summary %>%
  left_join(sw_pooled_proportions, by = c("disease", "sex", "exposure_time_frame_bin"))

print(as.data.frame(sw_exposure_final))

# save
write_xlsx(sw_exposure_final, "Drafts/Study characteristics/sw_exposure_final_with_sex.xlsx")

# study characteristics

# load dataframe
study_characteristics <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "Study characteristics") 

# study details
study_characteristics_details <- study_characteristics %>%
  select(study, journal, cohort, city, country, design, sampling, recruitment_location, inclusion, exclusion)

# participant details
study_characteristics_participants <- study_characteristics %>%
  select(study, median_age, mean_age, perc_prison, perc_homeless_rec, perc_oat_rec, inj_duration)

# hiv and hcv outcomes
study_characteristics_outcomes <- study_characteristics %>%
  select(study, testing_frequency_mths, hiv_sample_size, hcv_sample_size, hiv_cases, hiv_follow_up_years, hiv_inc, hiv_inc_95ci, hcv_cases, hcv_follow_up_years, hcv_inc, hcv_inc_95ci) %>%
  mutate(
    across(c(hiv_inc, hcv_inc, hiv_inc_95ci, hcv_inc_95ci, hiv_follow_up_years, hcv_follow_up_years), ~ round(as.numeric(.), 1))
  ) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "NR", .)))


write_xlsx(study_characteristics_details, "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Drafts/Study characteristics/study_characteristics_details.xlsx")
write_xlsx(study_characteristics_participants, "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Drafts/Study characteristics/study_characteristics_participants.xlsx")
write_xlsx(study_characteristics_outcomes, "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Drafts/Study characteristics/study_characteristics_outcomes.xlsx")




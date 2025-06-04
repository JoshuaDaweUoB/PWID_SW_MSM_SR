# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "writexl", "tidyverse", "purrr")

# housekeeping
settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframe
study_characteristics <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "Study characteristics") 

# drop rows where article type is missing
study_characteristics <- study_characteristics %>%
  filter(!is.na(article_type))

# number of countries 
num_countries <- study_characteristics %>%
  filter(country != "multiple") %>%
  summarise(num_countries = n_distinct(country)) %>%
  ungroup()

## total number of estimates
total_estimates <- study_characteristics %>%
  summarise(total_estimates = sum(estimates, na.rm = TRUE)) %>%
  pull(total_estimates)

## sum of estimates by article_type
estimates_by_article_type <- study_characteristics %>%
  filter(article_type %in% c("Unpublished", "Manuscript")) %>%
  group_by(article_type) %>%
  summarise(total_estimates = sum(estimates, na.rm = TRUE)) %>%
  ungroup()

# Number of rows where both hiv and hcv == "Yes"
num_both_yes <- study_characteristics %>%
  filter(hiv == "Yes", hcv == "Yes") %>%
  nrow()

# Number of rows where hiv == "Yes" and hcv != "Yes"
num_hiv_yes <- study_characteristics %>%
  filter(hiv == "Yes", hcv != "Yes") %>%
  nrow()

# Number of rows where hcv == "Yes" and hiv != "Yes"
num_hcv_yes <- study_characteristics %>%
  filter(hcv == "Yes", hiv != "Yes") %>%
  nrow()

# Table for HIV/HCV overlap
summary_table <- tibble(
  group = c("HIV only", "HCV only", "Both HIV and HCV"),
  n = c(num_hiv_yes, num_hcv_yes, num_both_yes)
)

# Table for sum of estimates by article_type
estimates_table <- estimates_by_article_type %>%
  rename(group = article_type, n = total_estimates)

# Number of rows where both sex_work and msm == "Yes"
num_both_sw_msm <- study_characteristics %>%
  filter(sex_work == "Yes", msm == "Yes") %>%
  nrow()

# Number of rows where sex_work == "Yes" and msm != "Yes"
num_sex_work_yes <- study_characteristics %>%
  filter(sex_work == "Yes", msm != "Yes") %>%
  nrow()

# Number of rows where msm == "Yes" and sex_work != "Yes"
num_msm_yes <- study_characteristics %>%
  filter(msm == "Yes", sex_work != "Yes") %>%
  nrow()

# Table for Sex work/MSM overlap
sw_msm_table <- tibble(
  group = c("Sex work only", "MSM only", "Both Sex work and MSM"),
  n = c(num_sex_work_yes, num_msm_yes, num_both_sw_msm)
)

# Combine all tables
final_summary <- bind_rows(
  summary_table,      # HIV/HCV overlap
  sw_msm_table,       # Sex work/MSM overlap
  estimates_table,    # Estimates by article_type
  total_table         # Total estimates
)

print(final_summary)

# Define the columns to keep
cols_to_keep <- c(
  "title", "study", "lead_author", "published", "city", "country", "who_region", "lmic_4cat", "lmic_bin", "msm_decrim", "hiv_crim", "rob_3cat", "cohort", "pub_status", "use", "exposure_time_frame_bin", "incidence_method", "male_num", "male_perc", "msm_num", "msm_perc", "msm_definition", "exposure_time_frame", "exposed_num", "exposed_perc", "exposed_incidence_100py", "exposed_incidence_lb", "exposed_incidence_ub", "unexposed_num", "unexposed_perc", "unexposed_incidence_100py", "unexposed_incidence_lb", "unexposed_incidence_ub", "disease"
)

# Ensure 'male_perc' is numeric and add 'disease' column
hiv_msm_subset <- hiv_msm %>%
  mutate(
    male_perc = as.numeric(male_perc),
    disease = "hiv"
  )

hcv_msm_subset <- hcv_msm %>%
  mutate(
    male_perc = as.numeric(male_perc),
    disease = "hcv"
  )

# Combine only the specified columns from hiv_msm and hcv_msm
msm_all <- bind_rows(
  hiv_msm_subset %>% select(any_of(cols_to_keep)),
  hcv_msm_subset %>% select(any_of(cols_to_keep))
)

# 1. Number of studies reporting recent and lifetime exposure_time_frame_bin, overall and by disease
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

# 2. Overall number and proportion of individuals reporting recent and lifetime MSM exposure, overall and by disease
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
    msm_all %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      mutate(
        exposed_num = as.numeric(exposed_num),
        unexposed_num = as.numeric(unexposed_num)
      ) %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total_unexposed = sum(unexposed_num, na.rm = TRUE),
        total = total_exposed + total_unexposed,
        proportion_exposed = total_exposed / total
      ) %>%
      mutate(disease = "Overall")
  ) %>%
  select(disease, exposure_time_frame_bin, total_exposed, total, proportion_exposed)

print(as.data.frame(msm_exposure_summary))

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

# Combine only the specified columns 
sw_all <- bind_rows(
  hiv_sw_all_subset %>% select(any_of(cols_to_keep)),
  hcv_sw_all_subset %>% select(any_of(cols_to_keep))
)

# 1. Number of studies reporting recent and lifetime exposure_time_frame_bin, overall and by disease
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

# 2. Overall number and proportion of individuals reporting recent and lifetime sex work exposure, overall and by disease
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
  bind_rows(
    sw_all %>%
      filter(!is.na(exposure_time_frame_bin)) %>%
      mutate(
        exposed_num = as.numeric(exposed_num),
        unexposed_num = as.numeric(unexposed_num)
      ) %>%
      group_by(exposure_time_frame_bin) %>%
      summarise(
        total_exposed = sum(exposed_num, na.rm = TRUE),
        total_unexposed = sum(unexposed_num, na.rm = TRUE),
        total = total_exposed + total_unexposed,
        proportion_exposed = total_exposed / total
      ) %>%
      mutate(disease = "Overall")
  ) %>%
  select(disease, exposure_time_frame_bin, total_exposed, total, proportion_exposed)

print(as.data.frame(sw_exposure_summary))

# Helper function to summarise female counts for a given dataframe and disease label
summarise_females <- function(df, disease_label) {
  df %>%
    filter(!is.na(exposure_time_frame_bin)) %>%
    mutate(
      female_num = as.numeric(female_num),
      exposed_num = as.numeric(exposed_num),
      unexposed_num = as.numeric(unexposed_num)
    ) %>%
    group_by(exposure_time_frame_bin) %>%
    summarise(
      total_female = sum(female_num, na.rm = TRUE),
      total_participants = sum(exposed_num, na.rm = TRUE) + sum(unexposed_num, na.rm = TRUE),
      proportion_female = total_female / total_participants
    ) %>%
    mutate(disease = disease_label) %>%
    select(disease, exposure_time_frame_bin, total_female, total_participants, proportion_female)
}

# Get summaries for each disease
female_summary_hiv <- summarise_females(hiv_sw_all, "hiv")
female_summary_hcv <- summarise_females(hcv_sw_all, "hcv")

# Overall summary (combine both datasets)
female_summary_overall <- bind_rows(hiv_sw_all, hcv_sw_all) %>%
  filter(!is.na(exposure_time_frame_bin)) %>%
  mutate(
    female_num = as.numeric(female_num),
    exposed_num = as.numeric(exposed_num),
    unexposed_num = as.numeric(unexposed_num)
  ) %>%
  group_by(exposure_time_frame_bin) %>%
  summarise(
    total_female = sum(female_num, na.rm = TRUE),
    total_participants = sum(exposed_num, na.rm = TRUE) + sum(unexposed_num, na.rm = TRUE),
    proportion_female = total_female / total_participants
  ) %>%
  mutate(disease = "Overall") %>%
  select(disease, exposure_time_frame_bin, total_female, total_participants, proportion_female)

# Combine all summaries
female_summary <- bind_rows(
  female_summary_hiv,
  female_summary_hcv,
  female_summary_overall
)

print(as.data.frame(female_summary))
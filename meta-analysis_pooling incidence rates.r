# Your data
rates <- c(2.4, 1.0)
lower_ci <- c(1.6, 0.8)
upper_ci <- c(3.5, 1.3)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

 # ukraine

# Assuming 3 studies: rate, lower_ci, upper_ci for each
rates <- c(6.7, 21.7)
lower_ci <- c(0.01, 19.1)
upper_ci <- c(14.3, 24.4)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# ansps

# New data to pool
rates <- c(8.958, 6.782)
lower_ci <- c(6.970, 6.297)
upper_ci <- c(11.514, 7.303)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# HIV incidence rates to pool
rates <- c(0.142, 0.068)
lower_ci <- c(0.0533, 0.044)
upper_ci <- c(0.378, 0.103)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled HIV incidence rate:", round(pooled_rate, 3), 
    "(95% CI:", round(pooled_lower, 3), "-", round(pooled_upper, 3), ")\n")

# india

# Incidence rates to pool
rates <- c(1.0, 2.9)
lower_ci <- c(0, 0)
upper_ci <- c(3.4, 12.4)

# Handle zero lower bounds by adding small constant
lower_ci[lower_ci == 0] <- 0.01

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# india avhi

# New incidence rates to pool
rates <- c(6.23, 9.09)
lower_ci <- c(2.01, 8.11)
upper_ci <- c(19.33, 10.19)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# new delhi

# new delhi incidence rates to pool
rates <- c(20.5, 21.6)
lower_ci <- c(14.6, 18.1)
upper_ci <- c(28.8, 25.7)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled New Delhi incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

    # Calculate years at risk from Vancouver study data
total_cases_vancouver <- 142
incidence_rate_vancouver <- 7.6  # per 100 person-years
years_at_risk_vancouver <- total_cases_vancouver / (incidence_rate_vancouver / 100)

cat("Vancouver study - Total cases:", total_cases_vancouver, "\n")
cat("Vancouver study - Incidence rate per 100 person-years:", incidence_rate_vancouver, "\n") 
cat("Vancouver study - Years at risk:", round(years_at_risk_vancouver, 1), "\n")

# arys and vidus
cases_arys_vidus <- 43
person_years_arys_vidus <- 8141

# Calculate incidence rate per 100 person-years
incidence_rate_arys_vidus <- (cases_arys_vidus / person_years_arys_vidus) * 100

cat("ARYS and VIDUS study - Total cases:", cases_arys_vidus, "\n")
cat("ARYS and VIDUS study - Person-years:", person_years_arys_vidus, "\n")
cat("ARYS and VIDUS study - Incidence rate per 100 person-years:", round(incidence_rate_arys_vidus, 2), "\n")

# Calculate 95% CI for the incidence rate using Poisson distribution
ci_lower <- qchisq(0.025, 2*cases_arys_vidus) / (2*person_years_arys_vidus) * 100
ci_upper <- qchisq(0.975, 2*(cases_arys_vidus+1)) / (2*person_years_arys_vidus) * 100

cat("ARYS and VIDUS study - 95% CI:", round(ci_lower, 2), "-", round(ci_upper, 2), "\n")

incidence_rate_arys_vidus <- (cases_arys_vidus / person_years_arys_vidus) * 100

# New study data
cases_new_study <- 170
person_years_new_study <- 1653

# Calculate incidence rate per 100 person-years
incidence_rate_new_study <- (cases_new_study / person_years_new_study) * 100

cat("New study - Total cases:", cases_new_study, "\n")
cat("New study - Person-years:", person_years_new_study, "\n")
cat("New study - Incidence rate per 100 person-years:", round(incidence_rate_new_study, 2), "\n")

# Calculate 95% CI for the incidence rate using Poisson distribution
ci_lower_new <- qchisq(0.025, 2*cases_new_study) / (2*person_years_new_study) * 100
ci_upper_new <- qchisq(0.975, 2*(cases_new_study+1)) / (2*person_years_new_study) * 100

cat("New study - 95% CI:", round(ci_lower_new, 2), "-", round(ci_upper_new, 2), "\n")

# geddes 2020

rates <- c(16.5, 7.6)
lower_ci <- c(13.1, 6.0)
upper_ci <- c(20.7, 9.5)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis
library(metafor)
res <- rma(yi = log_rates, sei = se_log_rates, method = "DL")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Pooled HIV incidence rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# survudi
# survudi with population-based weighting
rates <- c(1.52, 0.87)
lower_ci <- c(0.85, 0.67)
upper_ci <- c(2.19, 1.07)
population_sizes <- c(275, 1910)

# Calculate log rates and standard errors
log_rates <- log(rates)
log_lower <- log(lower_ci)
log_upper <- log(upper_ci)
se_log_rates <- (log_upper - log_lower) / (2 * 1.96)

# Perform meta-analysis with population sizes as weights
res <- rma(yi = log_rates, sei = se_log_rates, weights = population_sizes, method = "FE")

# Get pooled estimate and CI on original scale
pooled_rate <- exp(res$b[1])
pooled_lower <- exp(res$ci.lb)
pooled_upper <- exp(res$ci.ub)

cat("Population-weighted pooled rate:", round(pooled_rate, 2), 
    "(95% CI:", round(pooled_lower, 2), "-", round(pooled_upper, 2), ")\n")

# Display effective weights
weights_used <- weights(res)
cat("Study 1 effective weight:", round(weights_used[1], 1), "%\n")
cat("Study 2 effective weight:", round(weights_used[2], 1), "%\n")

# uk associations

# Create the 2x2 table with half-case correction
exposed_cases <- 0 + 0.5
exposed_total <- 7 + 0.5
unexposed_cases <- 11 + 0.5
unexposed_total <- 724 + 0.5

# Calculate rates per 100 person-years (or per 100 participants)
exposed_rate <- (exposed_cases / exposed_total) * 100
unexposed_rate <- (unexposed_cases / unexposed_total) * 100

# Calculate rate ratio
rate_ratio <- exposed_rate / unexposed_rate

# Calculate 95% CI for rate ratio using log transformation
log_rr <- log(rate_ratio)
se_log_rr <- sqrt((1/exposed_cases) + (1/unexposed_cases) - (1/exposed_total) - (1/unexposed_total))
ci_lower <- exp(log_rr - 1.96 * se_log_rr)
ci_upper <- exp(log_rr + 1.96 * se_log_rr)

# Create summary table
msm_hcv_rr_table <- tibble(
  exposure = "MSM (past 12 months)",
  exposed_cases = exposed_cases - 0.5,  # Show original values
  exposed_total = exposed_total - 0.5,
  unexposed_cases = unexposed_cases - 0.5,
  unexposed_total = unexposed_total - 0.5,
  exposed_rate = round(exposed_rate, 2),
  unexposed_rate = round(unexposed_rate, 2),
  rate_ratio = round(rate_ratio, 2),
  ci_lower = round(ci_lower, 2),
  ci_upper = round(ci_upper, 2),
  ci_95 = paste0("(", round(ci_lower, 2), "-", round(ci_upper, 2), ")")
)

options(width = 1000)
print(msm_hcv_rr_table)
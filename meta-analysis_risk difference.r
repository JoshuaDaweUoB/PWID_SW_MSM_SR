# load packages
pacman::p_load("meta", "metafor", "readxl", "writexl", "tidyverse", "metaviz", "readr")

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# data 
hiv_sw_all <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - All") 
hiv_sw_males <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - Male") 
hiv_sw_females <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - Sex work - Female") 
hiv_msm <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HIV - MSM") 
hcv_sw_all <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - All") 
hcv_sw_males <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - Male") 
hcv_sw_females <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - Sex work - Female") 
hcv_msm <- read_excel("Data extraction/Full data extraction.xlsx", sheet = "HCV - MSM")

hiv_sw_all <- hiv_sw_all %>%
  mutate(
    exposed_ir_fmt = ifelse(
      !is.na(exposed_incidence_100py),
      paste0("IR: ", round(as.numeric(exposed_incidence_100py), 2),
             " (", round(as.numeric(exposed_incidence_lb), 2),
             "-", round(as.numeric(exposed_incidence_ub), 2), ")"),
      NA
    ),
    unexposed_ir_fmt = ifelse(
      !is.na(unexposed_incidence_100py),
      paste0("IR: ", round(as.numeric(unexposed_incidence_100py), 2),
             " (", round(as.numeric(unexposed_incidence_lb), 2),
             "-", round(as.numeric(unexposed_incidence_ub), 2), ")"),
      NA
    )
  )

# build numeric rd_dat safely
rd_dat <- hiv_sw_all %>%
  mutate(
    exposed_rate   = parse_number(exposed_incidence_100py),
    exposed_lb     = parse_number(exposed_incidence_lb),
    exposed_ub     = parse_number(exposed_incidence_ub),
    unexposed_rate = parse_number(unexposed_incidence_100py),
    unexposed_lb   = parse_number(unexposed_incidence_lb),
    unexposed_ub   = parse_number(unexposed_incidence_ub)
  ) %>%
  filter(
    !is.na(exposed_rate), !is.na(unexposed_rate),
    !is.na(exposed_lb),   !is.na(unexposed_lb),
    !is.na(exposed_ub),   !is.na(unexposed_ub)
  ) %>%
  mutate(
    te     = exposed_rate - unexposed_rate,
    se_exp = (exposed_ub - exposed_lb) / (2 * 1.96),
    se_un  = (unexposed_ub - unexposed_lb) / (2 * 1.96),
    se_te  = sqrt(se_exp^2 + se_un^2),
    lower  = te - 1.96 * se_te,
    upper  = te + 1.96 * se_te,
    exposed_ir_fmt   = if_else(!is.na(exposed_rate),   paste0(round(exposed_rate,2),   " (", round(exposed_lb,2),   "-", round(exposed_ub,2),   ")"), NA_character_),
    unexposed_ir_fmt = if_else(!is.na(unexposed_rate), paste0(round(unexposed_rate,2), " (", round(unexposed_lb,2), "-", round(unexposed_ub,2), ")"), NA_character_)
  )

res_rd <- metagen(
  TE = te,
  lower = lower,
  upper = upper,
  studlab = study,
  data = rd_dat,
  sm = "IRD",
  comb.random = TRUE,  
  comb.fixed  = FALSE,
  method.tau  = "REML"
)

# Plot using leftcols to print Study + the two IR columns
forest(res_rd,
       xlab = "Rate difference (per 100 person-years)",
       leftcols = c("studlab", "exposed_ir_fmt", "unexposed_ir_fmt"),
       leftlabs = c("Study", "Exposed IR", "Unexposed IR"),
       leftcols_width = c(0.45, 0.25, 0.25), # adjust widths if needed
       colgap.left = unit(2, "mm"),          # spacing between left columns
       cex = 0.8)

plots_dir <- "code/plots/risk difference"
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

df_list <- list(
  hiv_sw_all = hiv_sw_all,
  hiv_sw_males = hiv_sw_males,
  hiv_sw_females = hiv_sw_females,
  hiv_msm = hiv_msm,
  hcv_sw_all = hcv_sw_all,
  hcv_sw_males = hcv_sw_males,
  hcv_sw_females = hcv_sw_females,
  hcv_msm = hcv_msm
)

for (nm in names(df_list)) {
  df <- df_list[[nm]]

  rd_dat_i <- df %>%
    mutate(
      exposed_rate   = parse_number(exposed_incidence_100py),
      exposed_lb     = parse_number(exposed_incidence_lb),
      exposed_ub     = parse_number(exposed_incidence_ub),
      unexposed_rate = parse_number(unexposed_incidence_100py),
      unexposed_lb   = parse_number(unexposed_incidence_lb),
      unexposed_ub   = parse_number(unexposed_incidence_ub)
    ) %>%
    filter(
      !is.na(exposed_rate), !is.na(unexposed_rate),
      !is.na(exposed_lb),   !is.na(unexposed_lb),
      !is.na(exposed_ub),   !is.na(unexposed_ub)
    ) %>%
    mutate(
      te     = exposed_rate - unexposed_rate,
      se_exp = (exposed_ub - exposed_lb) / (2 * 1.96),
      se_un  = (unexposed_ub - unexposed_lb) / (2 * 1.96),
      se_te  = sqrt(se_exp^2 + se_un^2),
      lower  = te - 1.96 * se_te,
      upper  = te + 1.96 * se_te,
      exposed_ir_fmt   = if_else(!is.na(exposed_rate),   paste0(round(exposed_rate,2),   " (", round(exposed_lb,2),   "-", round(exposed_ub,2),   ")"), NA_character_),
      unexposed_ir_fmt = if_else(!is.na(unexposed_rate), paste0(round(unexposed_rate,2), " (", round(unexposed_lb,2), "-", round(unexposed_ub,2), ")"), NA_character_)
    )

  if (nrow(rd_dat_i) == 0) next

  res_i <- tryCatch(
    metagen(TE = te, lower = lower, upper = upper, studlab = study, data = rd_dat_i,
            sm = "IRD", comb.random = TRUE, comb.fixed = FALSE, method.tau = "REML"),
    error = function(e) NULL
  )
  if (is.null(res_i)) next

  x_min <- min(rd_dat_i$lower, na.rm = TRUE)
  x_max <- max(rd_dat_i$upper, na.rm = TRUE)
  if (is.finite(x_min) & is.finite(x_max)) {
    x_range <- x_max - x_min
    xlim <- c(x_min - 0.2 * x_range, x_max + 0.2 * x_range)
  } else {
    xlim <- NULL
  }

  out_file <- file.path(plots_dir, paste0(nm, "_risk_difference.png"))
  png(out_file, width = 2200, height = 900, res = 150)
  op <- par(no.readonly = TRUE)
  par(mar = c(4, 1, 2, 1))
  forest(res_i,
         xlab = "Rate difference (per 100 person-years)",
         leftcols = c("studlab", "exposed_ir_fmt", "unexposed_ir_fmt"),
         leftlabs = c("Study", "Exposed IR", "Unexposed IR"),
         leftcols_width = c(0.5, 0.22, 0.22),
         colgap.left = grid::unit(3, "mm"),
         cex = 0.75,
         xlim = xlim)
  par(op)
  dev.off()
}


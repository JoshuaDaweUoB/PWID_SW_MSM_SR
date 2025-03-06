# load packages
pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "tidyverse")

# housekeeping
settings.meta(CIbracket = "(")
settings.meta(CIseparator = "-")

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV")

# load dataframes
overall_data_recent <- read_excel("Data extraction/Overall estimates.xlsx", sheet = "Overall estimates")

overall_data_recent$i2 <- round(overall_data_recent$i2 * 100, 0)  # Convert i2 to percentage and round to no decimals
overall_data_recent$i2 <- paste0(overall_data_recent$i2, "%")  # Add percentage sign to i2 values

# Perform meta-analysis
meta_analysis <- metagen(TE = effect_2_ln,
                         lower = lower_2_ln,
                         upper = upper_2_ln,
                         studlab = exposure,
                         data = overall_data_recent,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = FALSE, 
                         backtransf = TRUE,
                         byvar = Outcome,
                         text.random = "Overall")

summary(meta_analysis) 

filename <- paste0("code/plots/Overall_recent.png")
png(filename = filename, width = 25, height = 14, units = "cm", res = 600)

forest(meta_analysis, 
       sortvar = num,
       xlim = c(0.2, 4),             
       leftcols = c("exposure", "group", "Model", "studies"), 
       leftlabs = c("Exposure", "Group", "Models", "NB of estimates"),
       rightcols = c("rr_95_2", "i2"), 
       rightlabs = c("Effect size (95% CI)", "I²"), 
       pooled.totals = F,
       xintercept = 1,
       addrow.overall = T,
       test.subgroup = F,
       overall.hetstat = F,
       overall = F,
       labeltext = TRUE,
       col.subgroup = "black",
       print.subgroup.name = FALSE)
dev.off()

# Perform meta-analysis
meta_analysis <- metagen(TE = effect_2_ln,
                         lower = lower_2_ln,
                         upper = upper_2_ln,
                         studlab = exposure,
                         data = overall_data_recent,
                         sm = "RR",
                         method.tau = "DL",
                         common = FALSE,
                         random = FALSE, 
                         backtransf = TRUE,
                         byvar = Outcome,
                         text.random = "Overall")

summary(meta_analysis) 

filename <- paste0("code/plots/Overall_recent.png")
png(filename = filename, width = 25, height = 14, units = "cm", res = 600)

forest(meta_analysis, 
       sortvar = num,
       xlim = c(0.2, 4),             
       leftcols = c("exposure", "group", "studies"), 
       leftlabs = c("Exposure", "Group", "NB of estimates"),
       rightcols = c("rr_95_2", "i2"), 
       rightlabs = c("Effect size (95% CI)", "I²"), 
       pooled.totals = F,
       xintercept = 1,
       addrow.overall = T,
       test.subgroup = F,
       overall.hetstat = F,
       overall = F,
       labeltext = TRUE,
       col.subgroup = "black",
       print.subgroup.name = FALSE)
dev.off()
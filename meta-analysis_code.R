pacman::p_load("meta", "metafor", "DescTools", "car", "readxl", "tidyverse") # load packages

rm(list=ls()) # Clear the global environment

settings.meta(CIbracket = "(") 
settings.meta(CIseparator = "-") 

########## sex work ############

hiv_sw <- read_excel("C:/Users/joshua.dawe/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/data/hiv data.xlsx", sheet = "sex_work") # load data
hiv_sw <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/data/hiv data.xlsx", sheet = "sex_work") # load data

hiv_sw <- transform(hiv_sw, cases = as.numeric(cases), 
                         prs_yrs = as.numeric(prs_yrs),
                         effect_LB_past = as.numeric(effect_LB_past),
                         effect_UB_past = as.numeric(effect_UB_past)) # convert to numeric                        
                      
hiv_sw <- transform(hiv_sw, effect_past_ln = log(effect_past),
                         effect_past_LB_ln = log(effect_LB_past),
                         effect_past_UB_ln = log(effect_UB_past),
                         effect_recent_ln = log(effect_recent),
                         effect_recent_LB_ln = log(effect_LB_recent),
                         effect_recent_UB_ln = log(effect_UB_recent)) # convert to log

HIV_sw_recent <- hiv_sw %>% drop_na(effect_recent) # drop missing for aesthetics

RR_HIV_sw_recent <- metagen(TE = effect_recent_ln,
                           lower = effect_recent_LB_ln,
                           upper = effect_recent_UB_ln,
                           studlab = author_inci,
                           data = HIV_sw_recent,
                           sm = "RR",
                           method.tau = "DL",
                           comb.fixed = FALSE,
                           comb.random = TRUE, 
                           backtransf = TRUE,
                           byvar = inci_publi_status,
                           text.random = "Overall")
summary(RR_HIV_sw_recent) # recent sex work

png(filename = "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_hiv.png", width = 30, height = 14, units = "cm",
    res = 500)

forest_sw <- forest.meta(RR_HIV_sw_recent, 
                         sortvar = WHO_region,
                         xlim=c(0.2, 4),             
                         leftcols = c("author_inci", "country", "WHO_region", "inci_publi_status", "percent_sw"), 
                         leftlabs = c("Study", "Country", "WHO region", "Publication status", "Sex work (%)"),
                         digits = 1,
                         digits.tau2=1,
                         digits.I2=1,
                         digits.pval.Q=3,
                         col.inside = "black",
                         subgroup.name = "",
                         subgroup = F,
                         print.byvar = FALSE,
                         col.by="black") 
dev.off()



# forest plot

########## msm ############

hiv_msm <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Sex work and risk of HIV and HCV/code/data/hiv data.xlsx", sheet = "msm") # load data

# log of estimates
hiv_msm <- transform(hiv_msm, effect_recent_ln = log(effect_recent),
                    effect_recent_LB_ln = log(effect_LB_recent),
                    effect_recent_UB_ln = log(effect_UB_recent))

# recent msm
HIV_msm_recent <- hiv_msm %>% drop_na(effect_recent)

RR_HIV_msm_recent <- metagen(TE = effect_recent_ln,
                            lower = effect_recent_LB_ln,
                            upper = effect_recent_UB_ln,
                            studlab = author_inci,
                            data = HIV_msm_recent,
                            sm = "RR",
                            method.tau = "DL",
                            comb.fixed = FALSE,
                            comb.random = TRUE, backtransf = TRUE,
                            title = "RR comparing HIV incidence between msm vs. no msm in PWID",
                            text.random = "Overall")
summary(RR_HIV_msm_recent)

png(filename = "msm_recent.png", width = 28, height = 10, units = "cm",
    res = 500)

forest_msm <- forest.meta(RR_HIV_msm_recent, 
                         sortvar = WHO_region,
                         xlim=c(0.2, 4),             
                         leftcols = c("author_inci", "country", "WHO_region", "percent_msm"), 
                         leftlabs = c("Study", "Country", "WHO region", "MSM (%)"),
                         digits = 1,
                         digits.tau2=1,
                         digits.I2=1,
                         digits.pval.Q=3,
                         col.inside = "black",
                         col.by="black") 

dev.off()

### HCV sex work ###

hcv_sw <- read_excel("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/data/hcv data.xlsx") # load data

hcv_sw <- transform(hcv_sw, 
                    effect_LB_recent = as.numeric(effect_LB_recent),
                    effect_UB_recent = as.numeric(effect_UB_recent)) # convert to numeric                        

hcv_sw <- transform(hcv_sw, effect_recent_ln = log(effect_recent),
                    effect_LB_recent_ln = log(effect_LB_recent),
                    effect_UB_recent_ln = log(effect_UB_recent)) # convert to log

HCV_sw_recent <- hcv_sw %>% drop_na(effect_recent) # drop missing for aesthetics

RR_HCV_sw_recent <- metagen(TE = effect_recent_ln,
                            lower = effect_LB_recent_ln,
                            upper = effect_UB_recent_ln,
                            studlab = author_inci,
                            data = HCV_sw_recent,
                            sm = "RR",
                            method.tau = "DL",
                            comb.fixed = FALSE,
                            comb.random = TRUE, backtransf = TRUE,
                            byvar = inci_publi_status,
                            text.random = "Overall")
summary(RR_HCV_sw_recent) # recent sex work

png(filename = "C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/code/plots/sw_recent_hcv.png", width = 28, height = 15, units = "cm",
    res = 500)

forest_sw_hcv <- forest.meta(RR_HCV_sw_recent, 
                         sortvar = WHO_region,
                         xlim=c(0.2, 4),             
                         leftcols = c("author_inci", "country", "WHO_region", "percent_sw"), 
                         leftlabs = c("Study", "Country", "WHO region", "Sex work (%)"),
                         digits = 1,
                         digits.tau2=1,
                         digits.I2=1,
                         digits.pval.Q=3,
                         col.inside = "black",
                         subgroup.name = "",
                         subgroup = F,
                         print.byvar = FALSE,
                         col.by="black") 

dev.off()

# hcv msm

hcv_msm <- read_excel("C:/Users/joshua.dawe/OneDrive - University of Bristol/Documents/Sex work and risk of HIV and HCV/code/data/hcv data.xlsx", sheet = "msm") # load data

# log of estimates
hcv_msm <- transform(hcv_msm, effect_recent_ln = log(effect_recent),
                     effect_recent_LB_ln = log(effect_LB_recent),
                     effect_recent_UB_ln = log(effect_UB_recent))

HCV_msm_ever <- hcv_msm %>% drop_na(effect_recent)

RR_HCV_msm_ever <- metagen(TE = effect_recent_ln,
                             lower = effect_recent_LB_ln,
                             upper = effect_recent_UB_ln,
                             studlab = author_inci,
                             data = HCV_msm_ever,
                             sm = "RR",
                             method.tau = "DL",
                             comb.fixed = FALSE,
                             comb.random = TRUE, backtransf = TRUE,
                             title = "RR comparing HCV incidence between msm vs. no msm in PWID",
                             text.random = "Overall")
summary(RR_HCV_msm_ever)

png(filename = "msm_hcv.png", width = 28, height = 10, units = "cm",
    res = 500)

forest_msm <- forest.meta(RR_HCV_msm_ever, 
                          sortvar = WHO_region,
                          xlim=c(0.2, 4),             
                          leftcols = c("author_inci", "country", "WHO_region", "percent_msm"), 
                          leftlabs = c("Study", "Country", "WHO region", "MSM (%)"),
                          digits = 1,
                          digits.tau2=1,
                          digits.I2=1,
                          digits.pval.Q=3,
                          col.inside = "black",
                          col.by="black") 

dev.off()


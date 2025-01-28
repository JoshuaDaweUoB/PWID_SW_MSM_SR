library(meta)
library(metafor)
rm(list=ls()) # Clear the global environment
setwd("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/Dec 20/Plots")

HIV_inci_64 <-HIV_64

# Sort by country and city
HIV_inci_64_R <- HIV_inci_64[order(HIV_inci_64$country, HIV_inci_64$city, HIV_inci_64$inci_years),]

# To sort the forest plot by WHO region:
HIV_inci_64_R$WHO_region <- factor(HIV_inci_64_R$WHO_region, levels = c("African Region", "Eastern Mediterranean Region",
                                                                        "European Region", "Region of the Americas", "South-East Asia Region",
                                                                        "Western Pacific Region", "Mixed regions"))
settings.meta(CIbracket = "(")
settings.meta(CIseparator = "-")

HIV_main_64 <- metarate(cases_R, prs_yrs_R, data = HIV_inci_64_R, #Use cases_R to display the rates with 0.5 cont correct and cases to not display; either way, the continuity corr is applied=same results
                     studlab = paste(author_inci),  
                     comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                     #method.incr = "only0",
                     irscale=100, irunit = "person-years", 
                     xlab= "HIV incidence rate per 100 person-years", 
                     text.random = "Overall", 
                     title = "Forest plot and meta-analysis results of HIV incidence among PWID", 
                     outclab="title")
summary(HIV_main_64)

HIV_main_64_subgroup<- update.meta(HIV_main_64, 
                                   subgroup = prop_OAT_R, 
                                   print.subgroup.name = FALSE,
                                   byvar = prop_OAT_R, 
                                   tau.common = FALSE)
summary(HIV_main_64_subgroup)

source("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/August 2nd/Plots/forest.metaJW.R") # update your file location or keep in working directory JW
environment(forest.meta.update) <- environment(forest.meta)

source("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/Dec 20/Plots/forest.metaAAA.R") # update your file location or keep in working directory JW
environment(forest.meta.updAAA) <- environment(forest.meta)


# Make sure to use forest.meta.update


pdf(file = "HIV_Forest_WHO.pdf", width = 15, height = 24, family = "Helvetica" )

png(filename = "HIV_Forest_WHO.png", width = 50, height = 80, family = "Helvetica", units = "cm",
   res = 500)
forest.meta.updAAA (HIV_main_64_subgroup, xlim=c(0, 14),  digits = 1, smlab=" ", xlab= "HIV incidence rate per 100 person-years", 
                    rightcols = c("event","time", "effect.ci"), rightlabs = c("cases", "time", "Rate"), col.by = 2, #col.by changes the color
                    leftcols = c("studlab", "country", "city", "inci_years"), 
                    leftlabs = c("Reference", "Country", "Sub-national location(s)", "Calendar period"),
                    colgap.forest.left = "15mm", colgap.forest.right = "5mm", colgap.forest = "100mm", 
                    bylab = "", byseparator = ": ",
                    digits.tau2=1,
                    digits.I2=1,
                    digits.pval.Q=3,
                    col.inside = "black",
                    text.random.w = "Subgroup",
                    just.addcols.right = "right",
                    just.addcols.left = "left",
                    squaresize= 5,
                    col.diamond="white",
                    col.diamond.lines="black",
                    col.square = "darkgrey",
)
dev.off()

# EPS format
setEPS()                                             
postscript("HIV_Forest_WHO.eps",  width = 20, height = 20)                       
forest.meta.updAAA (HIV_main_64_subgroup, xlim=c(0, 14),  digits = 1, smlab=" ", xlab= "HIV incidence rate per 100 person-years", 
                    rightcols = c("event","time", "effect.ci"), rightlabs = c("cases", "time", "Rate"), col.by = 2, #col.by changes the color
                    leftcols = c("studlab", "country", "city", "inci_years"), 
                    leftlabs = c("Reference", "Country", "Sub-national location(s)", "Calendar period"),
                    colgap.forest.left = "15mm", colgap.forest.right = "5mm", colgap.forest = "100mm", 
                    bylab = "", byseparator = ": ",
                    digits.tau2=1,
                    digits.I2=1,
                    digits.pval.Q=3,
                    col.inside = "black",
                    text.random.w = "Subgroup",
                    just.addcols.right = "right",
                    just.addcols.left = "left",
                    squaresize= 5,
                    col.diamond="white",
                    col.diamond.lines="black",
                    col.square = "darkgrey",
)                           # Create plot
dev.off()



pdf(file = "HIV_Weights.pdf", width = 15, height = 20, family = "Helvetica" )
forest.meta (HIV_main_64_subgroup)
dev.off()

png(filename = "HIV_Age.png", width = 40, height = 60, family = "Helvetica", units = "cm",
    res = 500)
forest.meta(RR_HIV_Age_subgroup, xlim=c(0.2, 5), 
            rightcols = c("cases_yr","prs_yrs_yr","cases_or","prs_yrs_orr","effect.ci"), rightlabs = c("Y", "PYy", "O", "PYo", "Relative risk"), col.by = 2,
            leftcols = c("studlab", "country"), leftlabs = c("Reference", "Country"),
            just.addcols.left="left",
            just.addcols.right="right",
            colgap.forest.right = "5mm",
            colgap.right = "5mm",
            digits = 1,
            digits.tau2=1,
            digits.I2=1,
            digits.pval.Q=3,
            colgap.forest.left = "25mm",
            text.random.w = "Subgroup",
            squaresize= 3,
            col.diamond="white",
            col.diamond.lines="black",
            col.square = "darkgrey") 
dev.off()

















# Sensitivity analyses
table(HIV_inci_64$design)
HIV_noRCT<- subset(HIV_inci_64,design!="randomized trial")

HIV_noRCT_meta <- metarate(cases, prs_yrs, data = HIV_noRCT, 
                        studlab = paste(author_inci),  
                        comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                        irscale=100, irunit = "person-years", 
                        xlab= "HIV incidence rate per 100 person-years", 
                        text.random = "Overall", 
                        title = "Forest plot and meta-analysis results of HIV incidence among PWID", 
                        outclab="title")
summary(HIV_noRCT_meta)

table(HIV_inci_64$inci_method)
HIV_noRecInf<- subset(HIV_inci_64,inci_method!=0)
HIV_noRecInf_meta <- metarate(cases, prs_yrs, data = HIV_noRecInf, 
                           studlab = paste(author_inci),  
                           comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                           irscale=100, irunit = "person-years", 
                           xlab= "HIV incidence rate per 100 person-years", 
                           text.random = "Overall", 
                           title = "Forest plot and meta-analysis results of HIV incidence among PWID", 
                           outclab="title")
summary(HIV_noRecInf_meta)


# By groups
HIV_main_63_by<- update.meta(HIV_main_63, 
                                   byvar = WBI_bin, 
                                   tau.common = FALSE)
summary(HIV_main_63_by)

HIV_main_63_by_subset<- update.meta(HIV_main_63, 
                              byvar = midpoint_quart, 
                              tau.common = FALSE,
                              subset=(study_duration_bin==0))
summary(HIV_main_63_by_subset)


############ Meta regression ##################
###############################################
###############################################

# 1. Meta regression for midpoint
is.numeric(HIV_inci_63_R$midpoint)
is.numeric(HIV_inci_63_R$ROB_cont)

# HCV_meta <- metareg(HCV_main_66, ~ midpoint)
# summary(HCV_meta )
# bubble.metareg(HCV_meta)
# This approach (metareg) does not allow to backtransf the incidences. Need to use rma for the meta-reg

HIV_inci_64_R$prs_yrs_100 = HIV_inci_64_R$prs_yrs_R/100

head(HIV_inci_64_R$prs_yrs_100)
head(HIV_inci_64_R$prs_yrs)
#To plot y-axis per 100 prs-yrs in regplot divide the prs_yrs in Excel by 100

HIV_meta_rma <- rma(measure = "IRLN",
                        xi = cases_R,
                        ti = prs_yrs_100,
                        data = HIV_inci_64_R,
                        method = "REML",
                        test = "z",
                        slab = author_inci,
                        mods = ~  midpoint 
                       + WBI_bin + recruitment_short + study_duration + maj_recent_PWID
                    ) 
summary(HIV_meta_rma)
round(exp(5*coef(summary(HIV_meta_rma))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)
round(exp(coef(summary(HIV_meta_rma))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

# mods = ~ midpoint 
# + WBI_bin + recruitment_short +
#     study_duration + maj_recent_PWID

# Covariates to adjust for:
# relevel(factor(design), ref = "prospective cohort")
# mods = ~ midpoint + WBI_bin + recruitment_short +
#         study_duration + maj_recent_PWID ) 

qqnorm(HIV_meta_rma, type="rstandard")
rma_res_HIV <-residuals(HIV_meta_rma)
# rma_res_stand <-rstandard(HCV_meta_rma)
# rma_res_stude <-rstudent(HCV_meta_rma)
# rma_QQplot2 <-qqPlot(rma_res)

ks.test(rma_res_HIV, "pnorm")


hist(rma_res_HIV,
     xlab = "Residuals",
     ylab = "Frequency")

# BLUPs
HIV_meta_blup <-blup(HIV_meta_rma)
HIV_meta_blup_pred <- HIV_meta_blup$pred

qqnorm(HIV_meta_blup_pred)
qqline(HIV_meta_blup_pred)

library("car") # To use with qqPlot
my_QQplot_HIV <-qqPlot(HIV_meta_blup_pred, distribution="norm", col.lines = "black",
                   pch=16, grid=F, id=F, ylab = "Sample Quantiles", xlab="Theoretical Quantiles")
                   
HIV_meta_glmm <- rma.glmm(measure = "IRLN",
                          xi = cases_R,
                          ti = prs_yrs_100,
                          data = HIV_inci_64_R,
                          method = "ML",
                          slab = author_inci,
                             mods = ~ midpoint + WBI_bin + recruitment_short +
                                      study_duration + maj_recent_PWID
                          ) 
summary(HIV_meta_glmm)
round(exp(5*coef(summary(HIV_meta_glmm))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)
round(exp(coef(summary(HIV_meta_glmm))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)


glmm_res_HIV <-residuals(HIV_meta_glmm)
glmm_res_s_HIV <-rstandard(HIV_meta_glmm)
qqnorm(glmm_res_HIV)
library("car")
glmm_QQplot <-qqPlot(glmm_res_HIV)





#############  Linearity check #############  

HIV_meta_rma_studyD <- rma(measure = "IRLN",
                    xi = cases_R,
                    ti = prs_yrs_100,
                    data = HIV_inci_64_R,
                    method = "REML",
                    test = "z",
                    slab = author_inci,
                    mods = ~ study_duration 
) 

# Direct to get residuals vs fitted
#  plot(HIV_meta_rma_midp)

# Indirect to get residuals vs fitted
#Midpoint
fitted_HIV_midpoint <-fitted(HIV_meta_rma_midp)
residuals_HIV_midpoint <-residuals(HIV_meta_rma_midp)
plot(fitted_HIV_midpoint, residuals_HIV_midpoint)

ggplot(data = HIV_inci_64_R, aes(x = fitted_HIV_midpoint, y = residuals_HIV_midpoint)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth() +
        theme_bw() +
        xlab("Fitted values") +
        ylab("Residuals") 

#Study duration
fitted_HIV_studyD <-fitted(HIV_meta_rma_studyD)
residuals_HIV_studyD <-residuals(HIV_meta_rma_studyD)
plot(fitted_HIV_studyD, residuals_HIV_studyD)

ggplot(data = HIV_inci_64_R, aes(x = fitted_HIV_studyD, y = residuals_HIV_studyD)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth() +
        theme_bw() +
        xlab("Fitted values") +
        ylab("Residuals") 

#Study duration: excluding the 2 outliers

HIV_inci_64_R_nooutliers <- subset(HIV_inci_64_R, author_inci!="Mehta SH/Astemborski J")
HIV_inci_64_R_nooutliers1 <- subset(HIV_inci_64_R_nooutliers, author_inci!="Iversen J 2014")

HIV_meta_rma_studyD_noOutliers <- rma(measure = "IRLN",
                           xi = cases_R,
                           ti = prs_yrs_100,
                           data = HIV_inci_64_R_nooutliers1,
                           method = "REML",
                           test = "z",
                           slab = author_inci,
                           mods = ~ study_duration 
) 

fitted_HIV_studyD_noOutliers <-fitted(HIV_meta_rma_studyD_noOutliers)
residuals_HIV_studyD_noOutliers <-residuals(HIV_meta_rma_studyD_noOutliers)

ggplot(data = HIV_inci_64_R_nooutliers1, aes(x = fitted_HIV_studyD_noOutliers, y = residuals_HIV_studyD_noOutliers)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth() +
        theme_bw() +
        xlab("Fitted values") +
        ylab("Residuals") 

#Study duration: (excluding the 2 outliers and) taking the log transformation of study D

HIV_inci_64_R$ln_study_duration <- log(HIV_inci_64_R$study_duration)

HIV_meta_rma_studyD_ln <- rma(measure = "IRLN",
                           xi = cases_R,
                           ti = prs_yrs_100,
                           data = HIV_inci_64_R,
                           method = "REML",
                           test = "z",
                           slab = author_inci,
                           mods = ~ ln_study_duration 
) 

fitted_HIV_studyD_ln <-fitted(HIV_meta_rma_studyD_ln)
residuals_HIV_studyD_ln <-residuals(HIV_meta_rma_studyD_ln)

ggplot(data = HIV_inci_64_R, aes(x = fitted_HIV_studyD_ln, y = residuals_HIV_studyD_ln)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth() +
        theme_bw()+
        xlab("Fitted values") +
        ylab("Residuals") 

#Study duration: modelling study D using splines
library(splines)

HIV_meta_rma_studyD_splines <- rma(measure = "IRLN",
                              xi = cases_R,
                              ti = prs_yrs_100,
                              data = HIV_inci_64_R,
                              method = "REML",
                              test = "z",
                              slab = author_inci,
                              mods = ~ ns(study_duration, df=4) 
) 

fitted_HIV_meta_rma_studyD_splines <-fitted(HIV_meta_rma_studyD_splines)
residuals_HIV_meta_rma_studyD_splines <-residuals(HIV_meta_rma_studyD_splines)


ggplot(data = HIV_inci_64_R, aes(x = fitted_HIV_meta_rma_studyD_splines, y = residuals_HIV_meta_rma_studyD_splines)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth() +
        theme_bw()+
        xlab("Fitted values") +
        ylab("Residuals") 

################### Bubble plot ###################
# regplot(HCV_meta_rma, label = F, transf=exp,
#        ylab = "Incidence rate",
#        xlab = "Midpoint of study years",
#        col="black",
#        plim = c(1,NA),
#        labsize = 0.5,
#        legend = F,
#        col = "red")

pdf(file = "HIV_Trends_nocorr_64.pdf", width = 10, height = 8)
regplot(HIV_meta_rma, label = F, transf=exp,
        ylab = "Incidence rate (per 100 person-years)",
        xlab = "Year",
        col="grey",
        bg=c(HIV_inci_64_R$bubble_col_WBI), #Make sure bubble colors match! Use 66_R
        plim = c(1.5,NA),
        labsize = 0.5,
        legend = F,
        pch=21)
dev.off()


# Excluding studies with FUP more than 10 years
table(HIV_inci_64_R$study_duration_bin)

HIV_inci_64_R_subset <- subset(HIV_inci_64_R, study_duration_bin==0)
HIV_meta_rma_subset <- rma(measure = "IRLN",
                    xi = cases_R,
                    ti = prs_yrs_100,
                    data = HIV_inci_64_R_subset,
                    method = "REML",
                    test = "z",
                    slab = author_inci,
                    mods = ~ midpoint + WBI_bin + recruitment_short +
                             study_duration + maj_recent_PWID) 
summary(HIV_meta_rma_subset)
round(exp(5*coef(summary(HIV_meta_rma_subset))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)
round(exp(coef(summary(HIV_meta_rma_subset))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)


pdf(file = "HIV_Trends_nocorr_59.pdf", width = 10, height = 8)
regplot(HIV_meta_rma_subset, label = F, transf=exp,
        ylab = "Incidence rate (per 100 person-years)",
        xlab = "Year",
        col="grey",
        bg=c(HIV_inci_63_R_subset$bubble_col_WBI),
        plim = c(1.5,NA),
        labsize = 0.5,
        legend = F,
        pch=21)
dev.off()
# Covariates to adjust for:
# mods = ~ WBI_bin + midpoint + relevel(factor(design), ref = "prospective cohort") +
#         study_duration + maj_recent_PWID)


# Time trends by WBI region
HIV_inci_64_R_subset_HIC_subset <- subset(HIV_inci_64_R_subset, WBI_bin =='High income')  # Excluding those with FUP>10 yrs, HIC
HIV_inci_64_R_subset_LMIC_subset <- subset(HIV_inci_64_R_subset, WBI_bin !='High income') # Excluding those with FUP>10 yrs, LMIC

HIV_inci_64_R_subset_HIC <- subset(HIV_inci_64_R, WBI_bin =='High income') # All studies HIC
HIV_inci_64_R_subset_LMIC <- subset(HIV_inci_64_R, WBI_bin !='High income') # All studies LMIC


HIV_meta_rma_subset_HIC <- rma(measure="IRLN", #Chnage HIC for LMIC
                           xi = cases_R,
                           ti = prs_yrs,
                           # add = 0.5,
                           # to = "only0",
                           data = HIV_inci_64_R_subset_HIC_subset,
                           method = "REML",
                           test = "z",
                           slab = author_inci,
                           mods = ~  midpoint )
summary(HIV_meta_rma_subset_HIC)
round(exp(5*coef(summary(HIV_meta_rma_subset_HIC))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

round(exp(coef(summary(HIV_meta_rma_subset_LMIC))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

# mods = ~ midpoint + recruitment_short +
#        study_duration + maj_recent_PWID)
###


# 2. Time trends for specific WHO regions
table(HIV_inci_63_R$WHO_region)
trend_dat_HIV_sub_SEA <- subset(HIV_inci_63_R, WHO_region=="South-East Asia Region")

HIV_meta_rma_SEA <- rma(measure = "IRLN",
                        xi = cases,
                        ti = prs_yrs,
                        add = 0.5,
                        to = "only0",
                        data = trend_dat_HIV_sub_SEA,
                        method = "DL",
                        test = "z",
                        slab = author_inci,
                        mods = ~ midpoint)
summary(HIV_meta_rma_SEA)
round(exp(coef(summary(HIV_meta_rma_SEA))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

# 3. Meta regression for other continuous variables
# Convert some continuous variables as numeric
HIV_inci_63_R$HCV_Ab_cont<-as.numeric(HIV_inci_63_R$HCV_Ab, na.strings = "not available")
HIV_inci_63_R$HIV_cont<-as.numeric(HIV_inci_63_R$HIV, na.strings = "not available")
HIV_inci_63_R$HCV_RNA_cont<-as.numeric(HIV_inci_63_R$HCV_RNA, na.strings = "not available")
HIV_inci_63_R$age_cont<-as.numeric(HIV_inci_63_R$age, na.strings = "not available")
HIV_inci_63_R$dur_IV_cont<-as.numeric(HIV_inci_63_R$dur_IV, na.strings = "not available")
HIV_inci_63_R$prop_homeless_cont<-as.numeric(HIV_inci_63_R$prop_homeless_R, na.strings = "not available")
HIV_inci_63_R$prop_incar_cont<-as.numeric(HIV_inci_63_R$prop_incar_R, na.strings = "not available")
HIV_inci_63_R$prop_OAT_cont<-as.numeric(HIV_inci_63_R$prop_OAT_R, na.strings = "not available")
HIV_inci_63_R$prop_women_cont<-as.numeric(HIV_inci_63_R$prop_women, na.strings = "not available")
HIV_inci_63_R$prop_young_cont<-as.numeric(HIV_inci_63_R$prop_young, na.strings = "not available")

HIV_inci_63_R$attrition_cont<-as.numeric(HIV_inci_63_R$attrition, na.strings = "not available")
HIV_inci_63_R$average_FUP_cont<-as.numeric(HIV_inci_63_R$average_FUP, na.strings = "not available")


#Probably over-adjusting if adjustsing for age and gender because these are not real confounders. 
#mods = ~ midpoint + WBI_bin + relevel(factor(design), ref = "prospective cohort") + 
#        study_duration + maj_recent_PWID + age_bin_25 + prop_women_median
by(HCV_inci_66_R$study_duration, HCV_inci_66_R$WBI_bin, summary)
wilcox.test(HCV_inci_66_R$study_duration ~ HCV_inci_66_R$WBI_bin) 

HIV_meta_rma_cont <- rma(measure = "IRLN",
                          xi = cases,
                          ti = prs_yrs_100,
                          add = 0.5,
                          to = "only0",
                          data = HIV_inci_63_R,
                          method = "DL",
                          test = "z",
                          slab = author_inci,
                          mods = ~  ROB_cont)
summary(HIV_meta_rma_cont)
round(exp(coef(summary(HIV_meta_rma_cont))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

regplot(HCV_meta_rma_cont, label = F, transf=exp,
        col = "grey",
        bg="red",
        ylab = "HCV incidence rate (/100py)",
        xlab = "Midpoint of study period",
        labsize = 0.5,
        legend = F)




############ Time trends using correlated data #########################################
############ Time trends using correlated data #########################################
############ Time trends using correlated data #########################################
# Import data:  HIV_full_R_79

HIV_full_R_80 <- HIV_Trends
HIV_full_R_80$prs_yrs_100 = HIV_full_R_80$prs_yrs_R/100
HIV_full_R_80 <- escalc(measure="IRLN", xi=cases_R, ti=prs_yrs_100, data=HIV_full_R_80)
summary.escalc(HIV_full_R_80)#To get the 95%CI


multi_level_m_HIV <- rma.mv(yi = yi, 
                            V = vi, 
                            slab = author_inci_r,
                            data = HIV_full_R_80,
                            random = ~ 1 |slab/ID, 
                            test = "z", 
                            method = "REML",
                            mods = ~  midpoint + recruitment_short + WBI_bin +
                                    study_duration + maj_recent_PWID)
summary(multi_level_m_HIV)
round(exp(5*coef(summary(multi_level_m_HIV))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)
round(exp(coef(summary(multi_level_m_HIV))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

############## REGPLOT ############## 

#Using rowsum weights for the bubble size
w_m_HIV <- weights(multi_level_m_HIV, type = "matrix")
w_m_HIV_adj_w <- rowSums(w_m_HIV) / sum(w_m_HIV) * 100
w_m_HIV_adj_w_scaled <- w_m_HIV_adj_w * 2

pdf(file = "HIV_Trends_CORR_79.pdf", width = 10, height = 8)
regplot(multi_level_m_HIV, label = F, transf=exp,
        ylab = "Incidence rate (per 100 person-years)",
        xlab = "Year",
        col="grey",
        #bg=c(bubble_c),
        #bg="aquamarine3",
        bg=c(HIV_full_R_79$bubble_col_WBI),
        labsize = 0.5,
        legend = F,
        #pch=c(symbol_c))
        pch=21,
        psize=c(w_m_HIV_adj_w_scaled))
dev.off()

# Covariates to adjust for
# mods = ~  midpoint + WBI_bin + recruitment_short +
# study_duration + maj_recent_PWID +
#        relevel(factor(design), ref = "prospective cohort")

### Same as above but excluding studies with FUP more than 5 or 10 years, or WBI_bin=1 or 2 ### 
table(HIV_full_R_79$study_duration_bin)


HIV_full_R_79_subset_71 <- subset(HIV_full_R_79,study_duration_bin==0)
trend_dat_HIV_subset_71 <- escalc(measure="IRLN", xi=cases, ti=prs_yrs_100, data=HIV_full_R_79_subset_71)
summary.escalc(trend_dat_HIV_subset_71)#To get the 95%CI
trend_dat_HIV_subset_71$bubble_col_WBI

multi_level_m_HIV_subset_71 <- rma.mv(yi = yi, 
                            V = vi, 
                            slab = author_inci_r,
                            data = trend_dat_HIV_subset_71,
                            random = ~ 1 |slab/ID, 
                            test = "z", 
                            method = "REML",
                            mods = ~ midpoint + WBI_bin + recruitment_short + study_duration + maj_recent_PWID 
)
summary(multi_level_m_HIV_subset_71)
round(exp(5*coef(summary(multi_level_m_HIV_subset_71))[-1,c("estimate", "ci.lb", "ci.ub")]), 2)

# Covariates:WBI_bin + 
# recruitment_short + study_duration + maj_recent_PWID 

# This is the only way to get the bubbles
bubble_c_subset_71 <- HIV_full_R_79_subset_71$bubble_col_WBI #Use different colors/symbols for HIC and LMIC

#Using rowsum weights for the bubble size
w_m_HIV_sub <- weights(multi_level_m_HIV_subset_71, type = "matrix")
w_m_HIV_adj_w_sub <- rowSums(w_m_HIV_sub) / sum(w_m_HIV_sub) * 100
w_m_HIV_adj_w_sub_scaled <- w_m_HIV_adj_w_sub * 2

pdf(file = "HIV_Trends_CORR_71.pdf", width = 10, height = 8)
regplot(multi_level_m_HIV_subset_71, label = F, transf=exp,
        ylab = "Incidence rate (per 100 person-years)",
        xlab = "Year",
        col="grey",
        bg=c(bubble_c_subset_71),
        labsize = 0.5,
        legend = F,
        pch=21,
        psize=c(w_m_HIV_adj_w_sub_scaled))
dev.off()

######################################################################  
######################################################################  
######################################################################  




######################## IRR by age ######################## 
######################## IRR by age ######################## 
######################## IRR by age ######################## 

# Import HCV_full_R_RR_AGE
# Created from HCV_full_R and add ln for inci, LB and UB
rm(list=ls()) # Clear the global environment

# Sort by country and city
HIV_age_R <- HIV_IRR_Age[order(HIV_IRR_Age$country, HIV_IRR_Age$author_age),]


# To sort the forest plot by WHO region:
HIV_age_R$WHO_region <- factor(HIV_age_R$WHO_region, levels = c("African Region", "Eastern Mediterranean Region",
                                                                        "European Region", "Region of the Americas", "South-East Asia Region",
                                                                        "Western Pacific Region", "Mixed regions"))
settings.meta(CIbracket = "(")
settings.meta(CIseparator = "-")


#ROB: low, moderate and high
HIV_age_R$ROB_2cat <- as.factor(ifelse(is.na(HIV_age_R$ROB_cont), 'Q5',
                                                ifelse(HIV_age_R$ROB_cont<7, 'High to mod', 'Low')))
table(HIV_age_R$ROB_2cat)

#Young age threshold: cut at the median
table(HIV_age_R$young_threshold)
HIV_age_R$young_threshold_bin <- as.factor(ifelse(is.na(HIV_age_R$young_threshold), 'Q5',
                                                  ifelse(HIV_age_R$young_threshold<25, '<25', '>=25')))
table(HIV_age_R$young_threshold_bin)

HIV_age_R$young_threshold_tert <- as.factor(ifelse(is.na(HIV_age_R$young_threshold), 'Q5',
                                                  ifelse(HIV_age_R$young_threshold<25, '<25',
                                                         ifelse(HIV_age_R$young_threshold==25, '25','>25'))))
table(HIV_age_R$young_threshold_tert)


RR_HIV_Age <- metagen(TE = ln_age_effect,
                      lower = ln_age_effect_LB,
                      upper = ln_age_effect_UB,
                      studlab = author_age,
                      data = HIV_age_R,
                      sm = "RR",
                      method.tau = "DL",
                      comb.fixed = FALSE,
                      comb.random = TRUE, backtransf = TRUE,
                      title = "RR comparing HIV incidence in young vs older PWID",
                      text.random = "Overall")
summary(RR_HIV_Age)

# Comparing results with rma.glmm
HIV_age_R_2by2 <- subset(HIV_age_R, cases_y!="not available")
HIV_age_R_2by2$cases_y<-as.numeric(HIV_age_R_2by2$cases_y)
HIV_age_R_2by2$prs_yrs_y_100<-as.numeric(HIV_age_R_2by2$prs_yrs_y_100)
HIV_age_R_2by2$cases_o<-as.numeric(HIV_age_R_2by2$cases_o)
HIV_age_R_2by2$prs_yrs_o_100<-as.numeric(HIV_age_R_2by2$prs_yrs_o_100)

RR_HIV_Age_glmm <- rma.glmm(measure = "IRR",
                          x1i = HIV_age_R_2by2$cases_y,
                          x2i = HIV_age_R_2by2$prs_yrs_y_100,
                          t1i = HIV_age_R_2by2$cases_o,
                          t2i = HIV_age_R_2by2$prs_yrs_o_100,
                          data = HIV_age_R_2by2,
                          method = "ML",
                          slab = author_age,
                        
) 
summary(RR_HIV_Age_glmm)
# 



RR_HIV_Age_subgroup<- update.meta(RR_HIV_Age, 
                                   subgroup = WHO_region, 
                                   print.subgroup.name = FALSE,
                                   byvar = WHO_region, 
                                   tau.common = FALSE)
summary(RR_HIV_Age_subgroup)


source("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/August 2nd/Plots/forest.metaJW.R") # update your file location or keep in working directory JW
environment(forest.meta.update) <- environment(forest.meta)


pdf(file = "HIV_Age.pdf", width = 10, height = 15)
png(filename = "HIV_Age.png", width = 40, height = 60, family = "Helvetica", units = "cm",
    res = 500)
forest.meta(RR_HIV_Age_subgroup, xlim=c(0.2, 5), 
            rightcols = c("cases_yr","prs_yrs_yr","cases_or","prs_yrs_orr","effect.ci"), rightlabs = c("Y", "PYy", "O", "PYo", "Relative risk"), col.by = 2,
            leftcols = c("studlab", "country"), leftlabs = c("Reference", "Country"),
            just.addcols.left="left",
            just.addcols.right="right",
            colgap.forest.right = "5mm",
            colgap.right = "5mm",
            digits = 1,
            digits.tau2=1,
            digits.I2=1,
            digits.pval.Q=3,
            colgap.forest.left = "25mm",
            text.random.w = "Subgroup",
            squaresize= 3,
            col.diamond="white",
            col.diamond.lines="black",
            col.square = "darkgrey") 
dev.off()

# EPS format
setEPS()                                             
postscript("HIV_Age.eps", width = 15, height = 12)                       
forest.meta(RR_HIV_Age_subgroup, xlim=c(0.2, 5), 
            rightcols = c("cases_yr","prs_yrs_yr","cases_or","prs_yrs_orr","effect.ci"), rightlabs = c("Y", "PYy", "O", "PYo", "Relative risk"), col.by = 2,
            leftcols = c("studlab", "country"), leftlabs = c("Reference", "Country"),
            just.addcols.left="left",
            just.addcols.right="right",
            colgap.forest.right = "5mm",
            colgap.right = "5mm",
            digits = 1,
            digits.tau2=1,
            digits.I2=1,
            digits.pval.Q=3,
            colgap.forest.left = "25mm",
            text.random.w = "Subgroup",
            squaresize= 3,
            col.diamond="white",
            col.diamond.lines="black",
            col.square = "darkgrey") 
dev.off()



RR_HCV_Age_subgroup<- update.meta(RR_HIV_Age, 
                                  byvar = young_threshold_tert, 
                                  tau.common = FALSE)
summary(RR_HCV_Age_subgroup)
metareg(RR_HCV_Age, ~ midpoint)

####################### INCIDENCE AMONG YOUNG ####################### 
table(HIV_age_R$cases_y)
HIV_young_dta <- subset(HIV_age_R, cases_y != "not available") 
HIV_young_dta$cases_y<-as.numeric(HIV_young_dta$cases_y)
HIV_young_dta$prs_yrs_y<-as.numeric(HIV_young_dta$prs_yrs_y)

HIV_young <- metarate(cases_y, prs_yrs_y, data = HIV_young_dta, 
                        studlab = paste(author_age),  
                        comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                        irscale=100, irunit = "person-years", 
                        xlab= "HCV incidence rate per 100 person-years", 
                        text.random = "Overall", 
                        title = "Forest plot and meta-analysis results of HCV incidence among young PWID", 
                        outclab="title")
summary(HIV_young)

# Compare results with rma.glmm
HIV_young_dta$prs_yrs_y_100 = HIV_young_dta$prs_yrs_y/100
HIV_young_glmm <- rma.glmm(measure = "IRLN",
                           xi = cases_y,
                           ti = prs_yrs_y_100,
                            data = HIV_young_dta,
                            method = "ML",
                            slab = author_age) 
summary(HIV_young_glmm)
####    



HIV_old_dta <- subset(HIV_age_R, cases_o != "not available") 
HIV_old_dta$cases_o<-as.numeric(HIV_old_dta$cases_o)
HIV_old_dta$prs_yrs_o<-as.numeric(HIV_old_dta$prs_yrs_o)

HIV_old <- metarate(cases_o, prs_yrs_o, data = HIV_old_dta, 
                      studlab = paste(author_age),  
                      comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                      irscale=100, irunit = "person-years", 
                      xlab= "HCV incidence rate per 100 person-years", 
                      text.random = "Overall", 
                      title = "Forest plot and meta-analysis results of HCV incidence among young PWID", 
                      outclab="title")
summary(HIV_old)

# Compare results with rma.glmm
HIV_old_dta$prs_yrs_o_100 = HIV_old_dta$prs_yrs_o/100
HIV_old_glmm <- rma.glmm(measure = "IRLN",
                           xi = cases_o,
                           ti = prs_yrs_o_100,
                           data = HIV_old_dta,
                           method = "ML",
                           slab = author_age) 
summary(HIV_old_glmm)
####    


######################## IRR by gender ######################## 
######################## IRR by gender ######################## 
######################## IRR by gender ######################## 

rm(list=ls()) # Clear the global environment

# Import HCV_full_R_RR_GENDER
# Created from HCV_full_R and add ln for inci, LB and UB

# Sort by country and city
HIV_gender_R <- HIV_IRR_gender[order(HIV_IRR_gender$country, HIV_IRR_gender$author_gender),]


# To sort the forest plot by WHO region:
HIV_gender_R$WHO_region <- factor(HIV_gender_R$WHO_region, levels = c("African Region", "Eastern Mediterranean Region",
                                                                "European Region", "Region of the Americas", "South-East Asia Region",
                                                                "Western Pacific Region", "Mixed regions"))
settings.meta(CIbracket = "(")
settings.meta(CIseparator = "-")

#ROB: low, moderate and high
HIV_gender_R$ROB_2cat <- as.factor(ifelse(is.na(HIV_gender_R$ROB_cont), 'Q5',
                                            ifelse(HIV_gender_R$ROB_cont<7, 'High to mod', 'Low')))
table(HIV_gender_R$ROB_2cat)

#HIV_gender_R_sub_tem <-subset(HIV_gender_R, author_inci !="Skaathun B 2022")

RR_HIV_Gender <- metagen(TE = ln_gender_effect,
                      lower = ln_gender_effect_LB,
                      upper = ln_gender_effect_UB,
                      studlab = author_gender,
                      data = HIV_gender_R,
                      sm = "RR",
                      method.tau = "DL",
                      comb.fixed = FALSE,
                      comb.random = TRUE, backtransf = TRUE,
                      title = "RR comparing HCV incidence in women vs men PWID",
                      text.random = "Overall")
summary(RR_HIV_Gender)

table(HIV_gender_R$def_sex_g)
RR_HIV_Gender_subgroup<- update.meta(RR_HIV_Gender, 
                                  subgroup = WHO_region, 
                                  print.subgroup.name = FALSE,
                                  byvar = WHO_region, 
                                  tau.common = FALSE)
summary(RR_HIV_Gender_subgroup)


source("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/August 2nd/Plots/forest.metaJW.R") # update your file location or keep in working directory JW
environment(forest.meta.update) <- environment(forest.meta)

pdf(file = "HIV_Gender.pdf", width = 10, height = 15)
png(filename = "HIV_Gender.png", width = 40, height = 60, family = "Helvetica", units = "cm",
    res = 500)
forest.meta(RR_HIV_Gender_subgroup, xlim=c(0.2, 5), 
            rightcols = c("cases_F_r", "prs_yrs_F_r", "cases_M_r", "prs_yrs_M_r", "effect.ci"), rightlabs = c("F", "PYf", "M", "PYm", "Relative risk"), col.by = 2,
            leftcols = c("studlab", "country"), leftlabs = c("Reference", "Country"),
            just.addcols.left="left",
            just.addcols.right="right",
            colgap.forest.right = "5mm",
            colgap.right = "5mm",
            digits = 1,
            digits.tau2=1,
            digits.I2=1,
            digits.pval.Q=3,
            colgap.forest.left = "25mm",
            text.random.w = "Subgroup",
            squaresize= 3,
            col.diamond="white",
            col.diamond.lines="black",
            col.square = "darkgrey") 
dev.off()


# EPS format
setEPS()                                             
postscript("HIV_Gender.eps", width = 15, height = 12)
forest.meta(RR_HIV_Gender_subgroup, xlim=c(0.2, 5), 
            rightcols = c("cases_F_r", "prs_yrs_F_r", "cases_M_r", "prs_yrs_M_r", "effect.ci"), rightlabs = c("F", "PYf", "M", "PYm", "Relative risk"), col.by = 2,
            leftcols = c("studlab", "country"), leftlabs = c("Reference", "Country"),
            just.addcols.left="left",
            just.addcols.right="right",
            colgap.forest.right = "5mm",
            colgap.right = "5mm",
            digits = 1,
            digits.tau2=1,
            digits.I2=1,
            digits.pval.Q=3,
            colgap.forest.left = "25mm",
            text.random.w = "Subgroup",
            squaresize= 3,
            col.diamond="white",
            col.diamond.lines="black",
            col.square = "darkgrey") 
dev.off()


forest.meta(RR_HIV_Gender) 

table(HIV_gender_R$def_sex_g)
RR_HIV_Gender_subgroup<- update.meta(RR_HIV_Gender, 
                                  byvar = maj_recent_PWID , 
                                  tau.common = FALSE)
summary(RR_HIV_Gender_subgroup)
metareg(RR_HCV_Gender, ~ midpoint)


############## Absolute incidence ############## 
HIV_F_dta <- subset(HIV_gender_R, cases_F != "not available") 
HIV_F_dta$cases_F<-as.numeric(HIV_F_dta$cases_F)
HIV_F_dta$prs_yrs_F<-as.numeric(HIV_F_dta$prs_yrs_F)

HIV_F_dta$ROB_3cat <- as.factor(ifelse(is.na(HIV_F_dta$ROB_cont), 'Q5',
                                            ifelse(HIV_F_dta$ROB_cont<7, 'High to mod', 'Low')))
table(HIV_F_dta$ROB_3cat)

HIV_F <- metarate(cases_F, prs_yrs_F, data = HIV_F_dta, 
                      studlab = paste(author_gender),  
                      comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                      irscale=100, irunit = "person-years", 
                      xlab= "HCV incidence rate per 100 person-years", 
                      text.random = "Overall", 
                      title = "Forest plot and meta-analysis results of HIV incidence among F PWID", 
                      outclab="title")
summary(HIV_F)

HIV_F_subgroup<- update.meta(HIV_F, 
                                     byvar = ROB_3cat, 
                                     tau.common = FALSE)
summary(HIV_F_subgroup)

# Compare results with rma.glmm
HIV_F_dta$prs_yrs_F_100 = HIV_F_dta$prs_yrs_F/100
HIV_F_glmm <- rma.glmm(measure = "IRLN",
                         xi = cases_F,
                         ti = prs_yrs_F_100,
                         data = HIV_F_dta,
                         method = "ML",
                         slab = author_age) 
summary(HIV_F_glmm)
#### 

HIV_M_dta <- subset(HIV_gender_R, cases_M != "not available") 
HIV_M_dta$cases_M<-as.numeric(HIV_M_dta$cases_M)
HIV_M_dta$prs_yrs_M<-as.numeric(HIV_M_dta$prs_yrs_M)

HIV_M_dta$ROB_3cat <- as.factor(ifelse(is.na(HIV_M_dta$ROB_cont), 'Q5',
                                       ifelse(HIV_M_dta$ROB_cont<7, 'High to mod', 'Low')))
table(HIV_M_dta$ROB_3cat)

HIV_M <- metarate(cases_M, prs_yrs_M, data = HIV_M_dta, 
                  studlab = paste(author_gender),  
                  comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                  irscale=100, irunit = "person-years", 
                  xlab= "HCV incidence rate per 100 person-years", 
                  text.random = "Overall", 
                  title = "Forest plot and meta-analysis results of HIV incidence among M PWID", 
                  outclab="title")
summary(HIV_M)
HIV_M_subgroup<- update.meta(HIV_M, 
                             byvar = ROB_3cat, 
                             tau.common = FALSE)
summary(HIV_M_subgroup)

# Compare results with rma.glmm
HIV_M_dta$prs_yrs_M_100 = HIV_M_dta$prs_yrs_M/100
HIV_M_glmm <- rma.glmm(measure = "IRLN",
                       xi = cases_M,
                       ti = prs_yrs_M_100,
                       data = HIV_M_dta,
                       method = "ML",
                       slab = author_age) 
summary(HIV_M_glmm)
#### 

#Re-do incidence in F and M in those that have  F and M inci data using the full dataset: HIV_full_R
HIV_IRR_gender_dta_F <- subset(HIV_full_R, cases_F != "not available") 
HIV_IRR_gender_dta_F$cases_F<-as.numeric(HIV_IRR_gender_dta_F$cases_F)
HIV_IRR_gender_dta_F$prs_yrs_F<-as.numeric(HIV_IRR_gender_dta_F$prs_yrs_F)

HIV_F_2 <- metarate(cases_F, prs_yrs_F, data = HIV_IRR_gender_dta_F, 
                  studlab = paste(author_gender),  
                  comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                  irscale=100, irunit = "person-years", 
                  xlab= "HCV incidence rate per 100 person-years", 
                  text.random = "Overall", 
                  title = "Forest plot and meta-analysis results of HIV incidence among F PWID", 
                  outclab="title")
summary(HIV_F_2)


HIV_IRR_gender_dta_M <- subset(HIV_full_R, cases_M != "not available") 
HIV_IRR_gender_dta_M$cases_M<-as.numeric(HIV_IRR_gender_dta_M$cases_M)
HIV_IRR_gender_dta_M$prs_yrs_M<-as.numeric(HIV_IRR_gender_dta_M$prs_yrs_M)

HIV_M_2 <- metarate(cases_M, prs_yrs_M, data = HIV_IRR_gender_dta_M, 
                    studlab = paste(author_gender),  
                    comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN", 
                    irscale=100, irunit = "person-years", 
                    xlab= "HCV incidence rate per 100 person-years", 
                    text.random = "Overall", 
                    title = "Forest plot and meta-analysis results of HIV incidence among M PWID", 
                    outclab="title")
summary(HIV_M_2)

######################## Additional subgroup analyses ######################## 
######################## Additional subgroup analyses ######################## 
######################## Additional subgroup analyses ######################## 

HCV_inci_66_RR <- HCV_inci_66_R
library("DescTools")
print(HCV_inci_66_RR$HCV_Ab_cont)

# Categorise into quartiles with CutQ
quantile(HCV_inci_66_RR$HCV_Ab_cont, na.rm=T)
HCV_inci_66_RR$HCV_Ab_quart = CutQ(HCV_inci_66_RR$HCV_Ab_cont)
HCV_inci_66_RR$HCV_Ab_quartna = addNA(HCV_inci_66_RR$HCV_Ab_quart)
levels(HCV_inci_66_RR$HCV_Ab_quartna) <- c(levels(HCV_inci_66_RR$HCV_Ab_quart), 88)
table(HCV_inci_66_RR$HCV_Ab_quartna)

# Categorise into quartiles manually
#Age
quantile(HCV_inci_66_RR$age_cont, na.rm=T)
HCV_inci_66_RR$age_quartiles <- as.factor(ifelse(is.na(HCV_inci_66_R$age_cont), 'Q5',
                                          ifelse(HCV_inci_66_R$age_cont<=25, 'Q1',
                                          ifelse(HCV_inci_66_R$age_cont>25 & HCV_inci_66_R$age_cont<=29.2, 'Q2',
                                          ifelse(HCV_inci_66_R$age_cont>29.2 & HCV_inci_66_R$age_cont<=34.7, 'Q3', 'Q4')))))
table(HCV_inci_66_RR$age_quartiles)

#Prop young
quantile(HCV_inci_66_RR$prop_young_cont, na.rm=T)
HCV_inci_66_RR$prop_y_quart <- as.factor(ifelse(is.na(HCV_inci_66_R$prop_young_cont), 'Q5',
                                                 ifelse(HCV_inci_66_R$prop_young_cont<=30.9, 'Q1',
                                                        ifelse(HCV_inci_66_R$prop_young_cont>30.9 & HCV_inci_66_R$prop_young_cont<=37.9, 'Q2',
                                                               ifelse(HCV_inci_66_R$prop_young_cont>37.9 & HCV_inci_66_R$prop_young_cont<=51.8, 'Q3', 'Q4')))))
table(HCV_inci_66_RR$prop_y_quart)

#Dur IV
quantile(HCV_inci_66_RR$study_duration, na.rm=T)
HCV_inci_66_RR$study_duration_quart <- as.factor(ifelse(is.na(HCV_inci_66_R$study_duration), 'Q5',
                                                  ifelse(HCV_inci_66_R$study_duration<3, 'Q1',
                                                  ifelse(HCV_inci_66_R$study_duration>=3 & HCV_inci_66_R$study_duration<4, 'Q2',
                                                  ifelse(HCV_inci_66_R$study_duration>=4 & HCV_inci_66_R$study_duration<7, 'Q3', 'Q4')))))
table(HCV_inci_66_RR$study_duration_quart)

#ROB: low, moderate and high
HCV_inci_66_RR$ROB_3cat <- as.factor(ifelse(is.na(HCV_inci_66_R$ROB_cont), 'Q5',
                                                        ifelse(HCV_inci_66_R$ROB_cont<7, 'High to mod', 'Low')))
                                                              
                                                                      
table(HCV_inci_66_RR$ROB_3cat)


#Prop women
quantile(HCV_inci_66_RR$prop_women_cont, na.rm=T)
HCV_inci_66_RR$prop_women_cont_q <- as.factor(ifelse(is.na(HCV_inci_66_RR$prop_women_cont), 'Q5',
                                                     ifelse(HCV_inci_66_RR$prop_women_cont<21.35, 'Q1',
                                                            ifelse(HCV_inci_66_RR$prop_women_cont>=21.35 & HCV_inci_66_RR$prop_women_cont<28.15, 'Q2',
                                                                   ifelse(HCV_inci_66_RR$prop_women_cont>=28.15 & HCV_inci_66_RR$prop_women_cont<35.0, 'Q3', 'Q4')))))
table(HCV_inci_66_RR$prop_women_cont_q)



# Sub-group analyes 
HCV_main_66_overall <- metarate(cases, prs_yrs, data = HCV_inci_66_RR, 
                        studlab = paste(author_inci),  
                        comb.fixed = FALSE, comb.random = TRUE, method.tau = "DL", sm= "IRLN",
                        irscale=100)
summary(HCV_main_66_overall)

HCV_main_66_subgroup<- update.meta(HCV_main_66_overall, 
                                   byvar = HCV_inci_66_RR$prop_women_cont_q, 
                                   tau.common = FALSE)
summary(HCV_main_66_subgroup)



####################### Publication bias ####################### 
####################### Publication bias ####################### 
####################### Publication bias ####################### 


setwd("/Users/adelinaartenie/Dropbox/Dropbox - Post doc/Review HCV HIV incidence/My Meta-Analysis/Data/Dec 20/Plots")

########### AGE ########### 

pdf(file = "Funnel_HIV_Age.pdf", width = 10, height = 5)
funnel.meta(RR_HIV_Age,
            studlab = FALSE,
            cex=1.5,
            bg=c(HIV_age_R$funel_c),
            lwd=2,
            backtransf = F,
            xlab = "Relative risk (log scale)"
            
)
dev.off()

## Egger's test
metabias(RR_HIV_Age, method.bias = "linreg") #Egger’s test of the intercept

## Incorporating heterogeneity

table(HCV_age_R_subset$cases_o)
HCV_age_R_subset <- subset(HCV_age_R, cases_o!="not available")
class(HCV_age_R_subset$cases_o)

HCV_age_R_subset$cases_y<-as.numeric(HCV_age_R_subset$cases_y)
HCV_age_R_subset$cases_o<-as.numeric(HCV_age_R_subset$cases_o)
HCV_age_R_subset$prs_yrs_y<-as.numeric(HCV_age_R_subset$prs_yrs_y)
HCV_age_R_subset$prs_yrs_o<-as.numeric(HCV_age_R_subset$prs_yrs_o)



RR_HIV_Age_rma <- rma(measure = "RR",
                    ai = cases_y,
                    bi = prs_yrs_y,
                    ci = cases_o,
                    di = prs_yrs_o,    
                    #   add = 0.5,
                    #   to = "only0",
                    data = HCV_age_R_subset,
                    method = "REML",
                    test = "z",
                    slab = author_inci)

funnel(RR_HIV_Age_rma,
            studlab = FALSE,
            cex=1.5,
            bg=c(HCV_age_R_subset$funel_c),
            lwd=2,
            backtransf = F,
            xlab = "Relative risk (log scale)",
       addtau2 = T
            
)
## Contour plots

# Define fill colors for contour
# col.contour = c("gray75", "gray85", "gray95")

pdf(file = "C_Funnel_HIV_Age.pdf", width = 10, height = 5)
#par(mar=c(10,10,10,10))
par(mar=c(5,4,4,4))
col.contour2 = c("mistyrose", "mistyrose2", "mistyrose3")
funnel.meta(RR_HIV_Age, 
            xlim = c(-3, 3),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour2,
            cex=1.5,
            bg=c(HIV_age_R$funel_c),
            ref=1,
            level=NULL,
            backtransf = F,
            xlab = "Relative risk (log scale)",
            studlab = F
)
# Add a legend
# legend("topright", inset = c(-0.05,-0.15),
#        legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
#        fill = col.contour2,
#        xpd=TRUE)
dev.off()

funnel.meta(RR_HIV_Age,
            studlab = FALSE,
            cex=1.5,
            bg=c(HIV_age_R$funel_c),
            lwd=4,
            backtransf = F,
            xlab = "Relative risk (log scale)"
            
)

########### GENDER ########### 
pdf(file = "Funnel_HIV_Gender.pdf", width = 10, height = 5)
funnel.meta(RR_HIV_Gender,
            studlab = FALSE,
            cex=1.5,
            bg=c(HIV_gender_R$funel_c),
            lwd=2,
            backtransf = F,
            xlab = "Relative risk (log scale)"
            
)
dev.off()


## Egger's test
metabias(RR_HIV_Gender, method.bias = "linreg") #Egger’s test of the intercept

pdf(file = "C_Funnel_HIV_Gender.pdf", width = 10, height = 5)
par(mar=c(5,4,4,4))
col.contour2 = c("mistyrose", "mistyrose2", "mistyrose3")
funnel.meta(RR_HIV_Gender, 
            xlim = c(-3, 3),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour2,
            cex=1.5,
            bg=c(HIV_gender_R$funel_c),
            ref=1,
            level=NULL,
            backtransf = F,
            xlab = "Relative risk (log scale)",
            studlab = F
)
# Add a legend
#legend("topright", inset = c(-0.05,-0.15),
#       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
#       fill = col.contour2,
#       xpd=TRUE)
dev.off()









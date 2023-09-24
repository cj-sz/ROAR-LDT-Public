library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(multilevel)
library(mediation)

#### Load data ####
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs")

# Same issue here. Newcodes versions exist. But not sure which to use.
# Trying to switch these to newcodes versions results in errors.
metadata <- read.csv('metadata_all.csv')
sub.data <- read.csv('LDT_summarymeasures_wide.csv')
metadata <- rename(metadata,subj = record_id)
sub.outliers <- read.csv('Subject_RT_Outliers.csv')
sub.outliers$subj <- as.character(sub.outliers$subj)

# match data types
sub.data$subj <- as.character(sub.data$subj)
metadata$subj <- as.character(metadata$subj)
sub.data <- left_join(sub.data,metadata)

# Merge in outliers
sub.data <- left_join(sub.data,select(sub.outliers,subj,outlier))

# Get data without nan
sub.data <- filter(sub.data,!is.na(wj_lwid_raw))
print(sprintf('%d SUBJECTS WITH WJ AND LDT SCORES',dim(sub.data)[1]))

# Remove subjects with very old scores
sub.data <- filter(sub.data,MonthsSinceTesting<12)
print(sprintf('%d SUBJECTS WITH WJ AND LDT SCORES within 12 months',dim(sub.data)[1]))

# Remove outliers based on RT data
print(cor(select(sub.data,pcor_all,pcor_real,pcor_pseudo,wj_lwid_raw,twre_swe_raw,
                 wj_wa_raw,twre_pde_raw,wasi_vocab_raw, wasi_mr_raw)))
print(cor(select(filter(sub.data,outlier==FALSE),pcor_all,pcor_real,pcor_pseudo,wj_lwid_raw,twre_swe_raw,
                 wj_wa_raw,twre_pde_raw,wasi_vocab_raw, wasi_mr_raw)))
print(sprintf('%d OUTLIERS BASED ON RT DATA',sum(sub.data$outlier==TRUE)))
sub.data <- filter(sub.data,outlier==FALSE) 
print(sprintf('%d SUBJECTS WHO ARE NOT OUTLIERS',dim(sub.data)[1]))


#### Moderation analysis for Months since Testing ####
M = sub.data$MonthsSinceTesting
fitMod <- lm (pcor_all ~ wj_lwid_raw + M+ wj_lwid_raw:M, data = sub.data)
summary(fitMod)

#### Moderation analysis for Age  ####
M = sub.data$agenow
fitMod <- lm (pcor_all ~ wj_lwid_raw + M+ wj_lwid_raw:M, data = sub.data)
summary(fitMod)

#### Moderation analysis for wj standard scores on a subset of children  ####
M = sub.data$wj_lwid_ss
fitMod <- lm (pcor_all ~ wj_lwid_raw + M+ wj_lwid_raw:M, data = sub.data)
summary(fitMod)

#### Mediation analysis for Months since Testing ####
M = sub.data$MonthsSinceTesting
# Mediation analysis (parametric)
fitM <- lm(M ~ wj_lwid_raw,     data=sub.data)
fitY <- lm(pcor_all ~ wj_lwid_raw + M, data=sub.data)
fitMed <- mediation::mediate(fitM, fitY, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMed)
plot(fitMed)
# Mediation analysis (nonparametric)
fitMedBoot <- mediation::mediate(fitM, fitY, boot=TRUE, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMedBoot)
plot(fitMedBoot)

#### Mediation analysis for Age ####
M = sub.data$agenow
# Mediation analysis (parametric)
fitM <- lm(M ~ wj_lwid_raw,     data=sub.data)
fitY <- lm(pcor_all ~ wj_lwid_raw + M, data=sub.data)
fitMed <- mediation::mediate(fitM, fitY, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMed)
plot(fitMed)
# Mediation analysis (nonparametric)
fitMedBoot <- mediation::mediate(fitM, fitY, boot=TRUE, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMedBoot)
plot(fitMedBoot)

#### Mediation analysis for wj standard scores on a subset of children ####
M = sub.data$wj_lwid_ss
# Mediation analysis (parametric)
fitM <- lm(M ~ wj_lwid_raw,     data=sub.data)
fitY <- lm(pcor_all ~ wj_lwid_raw + M, data=sub.data)
fitMed <- mediation::mediate(fitM, fitY, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMed)
plot(fitMed)
# Mediation analysis (nonparametric)
fitMedBoot <- mediation::mediate(fitM, fitY, boot=TRUE, sims=999, treat="wj_lwid_raw", mediator="M")
summary(fitMedBoot)
plot(fitMedBoot)

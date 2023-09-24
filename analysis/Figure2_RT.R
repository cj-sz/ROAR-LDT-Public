library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(psych)

# Load data
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/")
cur_dir<-'C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/'
#####

# These files are giving errors. There are files that exist with similar names, i.e. _newcodes, etc. but not sure which to use.
metadata <- read.csv(paste0(cur_dir,'metadata_all.csv'))
sub.data <- read.csv(paste0(cur_dir,'LDT_summarymeasures_wide.csv'))
#sub.data.rt <- read.csv(paste0(cur_dir,'LDT_summarymeasures_wide_RT.csv'))
sub.data.rt <- read.csv(paste0(cur_dir,'LDT_summarymeasures_wide_RT_max5_min_0.2_3IQR.csv'))
metadata <- rename(metadata,subj = record_id)
sub.outliers <- read.csv(paste0(cur_dir,'Subject_RT_Outliers.csv'))
sub.outliers$subj <- as.character(sub.outliers$subj)

# match data types
sub.data$subj <- as.character(sub.data$subj)
metadata$subj <- as.character(metadata$subj)
sub.data <- left_join(sub.data,metadata)

sub.data.rt$subj <- as.character(sub.data.rt$subj)
# Merge in RT data
sub.data <- left_join(sub.data,select(sub.data.rt,subj,median.logrt, mean.logrt,
                    median.rt, mean.rt, median.rt.real,mean.rt.real,median.rt.pseudo,mean.rt.pseudo, median.logrt.real,median.logrt.pseudo,
                    median.inv.rt,median.inv.rt.real,median.inv.rt.pseudo))
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
# sub.data <- filter(sub.data,outlier==FALSE) # OUTLIERS ARE REMOVED IN PLOTTING
print(sprintf('%d SUBJECTS WHO ARE NOT OUTLIERS',dim(sub.data)[1]))


## Make plots

# Set color limits
clims = c(80, 120)
sub.data$wj_clip <- sub.data$wj_lwid_ss
sub.data$wj_clip[sub.data$wj_clip<clims[1]]<-clims[1]
sub.data$wj_clip[sub.data$wj_clip>clims[2]]<-clims[2]
midcolor <- 'gray50'
lowcolor<-'dodgerblue4'
highcolor<-'firebrick'

# Percent correct versus WJ scatter plot (filtering outliers)
p1 <- ggplot(filter(sub.data, outlier==FALSE), aes(x=pcor_all, y=wj_lwid_raw)) +
  geom_point(aes(colour=wj_clip),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Percent Correct (full test)') +
  geom_point(data=filter(sub.data,outlier==TRUE), aes(x=pcor_all, y=wj_lwid_raw),size=3,color='gray30',shape=1)+
  scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor) + 
    labs(colour = "Standard\nScore") + theme(legend.position = "right")+
  geom_label(x=.5,y=65, label=sprintf('r = %.2f',cor(select(filter(sub.data, outlier==FALSE), pcor_all,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p1
#ggsave('Figure2_PercentCorrect.pdf',p1, width=3.5, height=3)
#ggsave('Figure2_PercentCorrect.png',p1, width=3.5, height=3,dpi=300)


# Plot median RT versus WJ scatter plot (filtering outliers)

# Get all columns with RT data
rt_columns<- names(sub.data[ , grepl( "median.logrt" , names(sub.data))])

# loop over RT columns to create and save scatterplots
for (var_ind in 1:length(rt_columns)) {
  var1 <- rt_columns[var_ind]
  # get r value and p value
  cor.data <- filter(sub.data, outlier==FALSE)
  cor_res   <- cor.test(cor.data[[var1]],cor.data$wj_lwid_raw,use='pairwise.complete.obs')
  
    p2 <- ggplot(filter(sub.data, outlier==FALSE), aes(x=get(var1), y=wj_lwid_raw)) +
    geom_point(aes(colour=wj_clip),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab(var1) +
    geom_point(data=filter(sub.data,outlier==TRUE), aes(x=get(var1), y=wj_lwid_raw),size=3,color='gray30',shape=1)+
    scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
    scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                          low = lowcolor, high = highcolor, mid = midcolor) + 
    labs(colour = "Standard\nScore") + theme(legend.position = "right")+
    geom_label(x=-0.4,y=65, label=sprintf('r = %.2f\np = %.3f',cor_res$estimate, cor_res$p.value),hjust=0, vjust=0,size=3)
  print(p2)
  
  figName <- paste0('Scatterplot_',var1, '_WJ.png')
  ggsave(figName,p2, width=3.5, height=3,dpi=300)
  }

# Inspect correlation between median RT and percent correct in the LDT task
cor_res<-cor.test(cor.data$median.logrt,cor.data$pcor_all,use='pairwise.complete.obs')
p3 <- ggplot(filter(sub.data, outlier==FALSE), aes(x=median.logrt, y=pcor_all)) +
  geom_point(aes(colour=wj_clip),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Median Log(RT)') +
  geom_point(data=filter(sub.data,outlier==TRUE), aes(x=median.logrt, y=pcor_all),size=3,color='gray30',shape=1)+
  scale_y_continuous('Percent Correct') +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor) + 
  labs(colour = "Standard\nScore") + theme(legend.position = "right")+
  geom_label(x=-0.4,y=0.9, label=sprintf('r = %.2f\np = %.3f',cor_res$estimate, cor_res$p.value),hjust=0, vjust=0,size=3)
p3

# Test correlation between RT and WJ, for subjects with high accuracy

# filter subjects by their accuracy
thresh_acc<-0.7 
cor.data <- filter(sub.data, outlier==FALSE, pcor_all > thresh_acc)

for (var_ind in 1:length(rt_columns)) {
  var1 <- rt_columns[var_ind]
  # calculate r value and p value
   cor_res   <- cor.test(cor.data[[var1]],cor.data$wj_lwid_raw,use='pairwise.complete.obs')
  
  p4 <- ggplot(cor.data, aes(x=get(var1), y=wj_lwid_raw)) +
    geom_point(aes(colour=wj_clip),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab(var1) +
    geom_point(data=cor.data, aes(x=get(var1), y=wj_lwid_raw),size=3,color='gray30',shape=1)+
    scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
    scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                          low = lowcolor, high = highcolor, mid = midcolor) + 
    labs(colour = "Standard\nScore") + theme(legend.position = "right")+
    geom_label(x=-0.2,y=50, label=sprintf('r = %.2f\np = %.3f',cor_res$estimate, cor_res$p.value),hjust=0, vjust=0,size=3)
  print(p4)
  
  figName <- paste0('Scatterplot_',var1, '_', thresh_acc, '_WJ.png')
   ggsave(figName,p2, width=3.5, height=3,dpi=300)
}

# Check if the correlation realwordRT-WJ is significantly different than the correlation pseudowordRT-WJ
# William's Test for dependent correlations
r1 <- cor(cor.data$median.logrt.real, cor.data$wj_lwid_raw)
r2 <- cor(cor.data$median.logrt.pseudo, cor.data$wj_lwid_raw)
r3 <- cor(cor.data$median.logrt.real, cor.data$median.logrt.pseudo)
n1 <- dim(cor.data)[1]
cor_diff<-r.test(n1, r1, r2, r3,pooled=TRUE, twotailed = TRUE)
print(sprintf('The difference between the two correlations: t = %.2f, p = %.7f ',cor_diff$t, cor_diff$p))

# Similarly, check if the correlation RT-WJ is significantly different than the correlation pcor_all-WJ
# William's Test for dependent correlations
r1 <- cor(cor.data$median.logrt, cor.data$wj_lwid_raw)
r2 <- cor(cor.data$pcor_all, cor.data$wj_lwid_raw)
r3 <- cor(cor.data$median.logrt, cor.data$pcor_all)
n1 <- dim(cor.data)[1]
cor_diff<-r.test(n1, r1, r2, r3,pooled=TRUE, twotailed = TRUE)
print(sprintf('The difference between the two correlations: t = %.2f, p = %.7f ',cor_diff$t, cor_diff$p))

# Calculate correlations for different Accuracy thresholds and save as csv
thresh_list<-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9)
res_table<-matrix(nrow=length(thresh_list),ncol=length(rt_columns)*2+2)
for (th_id in 1:length(thresh_list)) {
  cur_thresh<-thresh_list[th_id]
  cor.data <- filter(sub.data, outlier==FALSE, pcor_all > cur_thresh)
  for (var_ind in 1:length(rt_columns)) {
     var1 <- rt_columns[var_ind]
    # get r value and p value
    cor_res   <- cor.test(cor.data[[var1]],cor.data$wj_lwid_raw,use='pairwise.complete.obs')
    res_table[th_id,1]<-cur_thresh
    res_table[th_id,2]<-dim(cor.data)[1]
    res_table[th_id,var_ind*2+1]<-cor_res$estimate
    res_table[th_id,var_ind*2+2]<-cor_res$p.value
    
  } 
}
# Convert matrix to a data frame
res_df <- data.frame(res_table)
# Rename columns in dataframe
names(res_df)[1:2]<-c("AccThresh","N")
for (var_ind in 1:length(rt_columns)){
  names(res_df)[var_ind*2+1]<-paste0(rt_columns[var_ind],"_r")
  names(res_df)[var_ind*2+2]<-paste0(rt_columns[var_ind],"_p")
}

write.csv(res_df,'RT_Correlations_per_accThresh.csv')

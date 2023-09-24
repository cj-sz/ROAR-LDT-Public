
# Calculate RT values per subject and same in summarymeasures

library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(lme4)

# Load data
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/")
cur_dir<-'C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/'
#####

word.stats = read.csv('wordStatistics.csv')
word.stats$word <- as.character(word.stats$STRING)

#Read data
# Problem with these files as well. Similar names exist, which to use?
# Tried substituting newcodes for these and received the following error:
#Error in `chr_as_locations()`:
#  ! Can't rename columns that don't exist.
#✖ Column `record_id` doesn't exist.
Run `rlang::last_error()` to see where the error occurred.
sub.data <- read.csv('LDT_alldata_long.csv')
sub.data <- left_join(sub.data,word.stats)
metadata <- read.csv('metadata_all.csv')
metadata <- rename(metadata,subj = record_id)
sub.data <- left_join(sub.data,metadata)

## Reaction time measures

# remove incorrect responses
sub.data.c = filter(sub.data,acc==1)

## subject rt analysis
sub.rt <- summarize(group_by(sub.data,subj),median.rt=median(log10(rt)),mean.rt=mean(log10(rt)), sd.rt=sd(log10(rt)),pcor=sum(acc)/500,
                    median.raw.rt=median(rt), mean.raw.rt=mean(rt), sd.raw.rt=sd(rt),
                    Q1.rt=quantile(rt,0.25), Q3.rt=quantile(rt,0.75),IQR.rt=IQR(rt),
                    word.id.raw = mean(wj_lwid_raw), MonthsSinceTesting = mean(MonthsSinceTesting))
sub.rt <- cbind(sub.rt,rename(as.data.frame(scale(select(sub.rt,mean.rt,median.rt,sd.rt,pcor))),
                              mean.rt.z=mean.rt,median.rt.z=median.rt,sd.rt.z=sd.rt,pcor.z=pcor))
sub.rt <- left_join(sub.rt,summarize(group_by(filter(sub.data,acc==1),subj),median.rt.c=median(log10(rt)),mean.rt.c=mean(log10(rt))))
sub.rt$diff.rt <- sub.rt$median.rt - sub.rt$mean.rt.c
sub.rt$sumz <- sub.rt$median.rt - sub.rt$sd.rt.z
sub.rt$outlier <- sub.rt$median.rt.z < -3 & sub.rt$pcor.z < -1
sub.rt$mahal <- mahalanobis(select(sub.rt,median.rt,mean.rt,sd.rt),center=FALSE,cov=cov(select(sub.rt,median.rt,mean.rt,sd.rt)))

## Perform stats removing outliers
# make a new dataframe for this analysis
rt.data <- left_join(sub.data,select(sub.rt,subj,outlier))
rt.data <- filter(rt.data, outlier==FALSE)
rt.data$rt.log <- log(rt.data$rt)
m.rt <- mean(rt.data$rt.log)
sd.rt <- sd(rt.data$rt.log)
total_responses <- dim(rt.data)[1]

## Exclude RTs based on absolute value
# In LDT tasks in typical adults a timeout of 2.5s is often used.
# Since the current dataset includes children and adults with a wide range of reading ability, we use a more lenient threshold of 5s
 thresh_max <- 5
 print(sprintf('%.5f percent of trials have RT longer than %.0f sec',100*sum(rt.data$rt> thresh_max)/total_responses,thresh_max))
 rt.data<-filter(rt.data, rt < thresh_max)

 # Responses faster than 200ms are probably erroneous button presses
 thresh_min <- 0.2
 print(sprintf('%.5f percent of trials have RT shorter than %.1f sec',100*sum(rt.data$rt < thresh_min)/total_responses,thresh_min))
 rt.data<-filter(rt.data, rt > thresh_min)
 
# Exclude responses that deviate by more than 3IQR from Q1 or Q3
  thresh <- 3
  rt.data <- left_join(rt.data, select(sub.rt,subj,mean.rt,sd.rt, mean.raw.rt,sd.raw.rt, Q1.rt, Q3.rt, IQR.rt))
  
  count_outliers_high <- sum(rt.data$rt > (rt.data$Q3.rt + thresh*rt.data$IQR.rt))
  count_outliers_low<-sum(rt.data$rt < (rt.data$Q1.rt - thresh*rt.data$IQR.rt))

  print(sprintf('%.5f percent of trials have RTs longer than %.0f times the IQR from the subject Q3',100*(count_outliers_high/total_responses),thresh))
  print(sprintf('%.5f percent of trials have RTs shorter than %.0f times the IQR from the subject Q1' ,100*(count_outliers_low/total_responses),thresh))
  
  rt.data<-filter(rt.data, (rt < (Q3.rt + thresh*IQR.rt) & rt > (Q1.rt - thresh*IQR.rt)))
  print(sprintf('A total of %.5f percent of trials were removed' ,100*((total_responses-dim(rt.data)[1])/total_responses)))

# Keep only RTs for correct responses
  rt.data.c<-filter(rt.data,acc==1)
  sub.rt.c <- summarize(group_by(rt.data.c,subj),median.logrt=median(log10(rt)),mean.logrt=mean(log10(rt)), sd.logrt=sd(log10(rt)),pcor=sum(acc)/500,
                    word.id.raw = mean(wj_lwid_raw), MonthsSinceTesting = mean(MonthsSinceTesting),median.rt=median(rt),mean.rt=mean(rt), sd.rt=sd(rt),
                    mean.inv.rt = mean(-1/rt), median.inv.rt = median(-1/rt))

# Calculate means for real words and psedowords separately
sub.rt.real <- summarize(group_by(filter(rt.data.c,realpseudo=="real"),subj),median.rt.real=median(rt),mean.rt.real=mean(rt), sd.rt.real=sd(rt),
                                                                            median.logrt.real=median(log10(rt)), mean.logrt.real=mean(log10(rt)), sd.logrt.real=sd(log10(rt)),
                                                                            mean.inv.rt.real=mean(-1/rt),median.inv.rt.real=median(-1/rt))
sub.rt.pseudo <- summarize(group_by(filter(rt.data.c,realpseudo=="pseudo"),subj),median.rt.pseudo=median(rt),mean.rt.pseudo=mean(rt), sd.rt.pseudo=sd(rt),
                                                                            median.logrt.pseudo=median(log10(rt)), mean.logrt.pseudo=mean(log10(rt)), sd.logrt.pseudo=sd(log10(rt)),
                                                                            mean.inv.rt.pseudo=mean(-1/rt),median.inv.rt.pseudo=median(-1/rt))

# Combine with dataframe and save
  sub.rt.c<-left_join(sub.rt.c,sub.rt.real)
  sub.rt.c<-left_join(sub.rt.c,sub.rt.pseudo)
  
  sub.rt.c$subj <- as.character(sub.rt.c$subj)
  rt.filename <- paste0('LDT_summarymeasures_wide_RT_max',thresh_max, '_min_',thresh_min, '_',thresh,'IQR.csv')
  write.csv(sub.rt.c, rt.filename)


library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(lme4)
library(lmerTest)

# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/")

word.stats = read.csv('wordStatistics.csv')
word.stats$word <- as.character(word.stats$STRING)
sub.data <- read.csv('LDT_alldata_long_newcodes.csv')
sub.data <- left_join(sub.data,word.stats)
metadata <- read.csv('metadata_all_newcodes.csv')
sub.data <- left_join(sub.data,metadata)

# Scatter plot of corr values
h1a = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=log(UN2_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(5,12))+
  xlab('Log(Bigram frequency)')+
  ylab('Real words (count)')+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6),
        axis.title.y = element_text(size=8))
h1 = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=log(UN3_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-4,10))+
  xlab('Log(Trigram frequency)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h2 = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=LEN))+
  geom_histogram(color='gray20',fill='gray80',binwidth=1)+
  scale_x_continuous(limits = c(2,16))+
  xlab('Number of letters')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6))
h3 = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=Orth))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-1,25))+
  xlab('Orthographic neighbors')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h4 = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=log(Orth_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-3,10))+
  xlab('Log(Freq. of neighbors)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h4b = ggplot(filter(word.stats,!is.na(FREQ)),aes(x=log(FREQ)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-4,10))+
  xlab('Log(Lexical frequency)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h5a = ggplot(filter(word.stats,is.na(FREQ)),aes(x=log(UN2_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(5,12))+
  xlab('Log(Bigram frequency)')+
  ylab('Pseudo words (count)')+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6),
        axis.title.y = element_text(size=8))
h5 = ggplot(filter(word.stats,is.na(FREQ)),aes(x=log(UN3_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-4,10))+
  xlab('Log(Trigram frequency)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h6 = ggplot(filter(word.stats,is.na(FREQ)),aes(x=LEN))+
  geom_histogram(color='gray20',fill='gray80',binwidth=1)+
  scale_x_continuous(limits = c(2,16))+
  xlab('Number of letters')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h7 = ggplot(filter(word.stats,is.na(FREQ)),aes(x=Orth))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-1,25))+
  xlab('Orthographic neighbors')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))
h8 = ggplot(filter(word.stats,is.na(FREQ)),aes(x=log(Orth_F)))+
  geom_histogram(color='gray20',fill='gray80',bins=15)+
  scale_x_continuous(limits = c(-3,10))+
  xlab('Log(Freq. of neighbors)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=6), axis.text.x = element_text(size=6))

g <- arrangeGrob(h1a,h1,h2,h3,h4,h4b,h5a,h5,h6,h7,h8,nrow=2) 
ggsave('wordhists.png',g,width=7.5,height=3.5)
ggsave('wordhists.pdf',g,width=7.5,height=3.5)

## reaction time measures

# remove incorrect responses
sub.data.c = filter(sub.data,acc==1)
# Make a factor for length
sub.data.c$length.factor=cut(sub.data.c$wordLength,c(1,4,7,10,15))

g1 = ggplot(sub.data.c, aes(x=rt,linetype=realpseudo)) +
  geom_density()+
  xlim(c(0,2))+
  geom_vline(data=summarise(group_by(sub.data.c,realpseudo), mrt = median(rt)),
             aes(xintercept=mrt, linetype=realpseudo))

g2 = ggplot(filter(sub.data.c,realpseudo=='real'), aes(x=rt,color=length.factor)) +
  scale_color_discrete()+
  geom_density(linetype='dashed')+
  xlim(c(0,2))+
  geom_vline(data=summarise(group_by(filter(sub.data.c,realpseudo=='real'),length.factor), mrt = median(rt)),
             aes(xintercept=mrt, color=length.factor),linetype='dashed')

g3 = ggplot(filter(sub.data.c,realpseudo=='pseudo'), aes(x=rt,color=length.factor)) +
  scale_color_discrete()+
  geom_density(linetype='solid')+
  xlim(c(0,2))+
  geom_vline(data=summarise(group_by(filter(sub.data.c,realpseudo=='pseudo'),length.factor), mrt = median(rt)),
             aes(xintercept=mrt, color=length.factor),linetype='solid')

grid.arrange(g1,g2,g3)
g=arrangeGrob(g1,g2,g3)
ggsave('RTdistributionis.pdf',g,width=7.5,height=4)
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

# Plot mean and median rt
g1 <- ggplot(sub.rt, aes(x=mean.rt.z, y=median.rt.z, label=subj,color=outlier)) +
  geom_text()+
  theme(legend.position = 'none')

g2 <- ggplot(sub.rt, aes(x=mean.rt.z, y=sd.rt.z, label=subj,color=outlier)) +
  geom_text()+
  theme(legend.position = 'none')

g3 <- ggplot(sub.rt, aes(x=median.rt.z, y=sd.rt.z, label=subj,color=outlier)) +
  geom_text()+
  theme(legend.position = 'none')

g4 <- ggplot(sub.rt,aes(x=sumz,y=mahal, label=subj,color=outlier))+
  geom_text()+
  theme(legend.position = 'none')

g5 <- ggplot(sub.rt,aes(x=median.rt.z,y =pcor.z, label=subj,color=outlier))+
  geom_text()+
  theme(legend.position = 'none')

grid.arrange(g1,g2,g3,g4,g5,nrow=1)
g <- arrangeGrob(g1,g2,g3,g4,g5,nrow=1) 
ggsave('SubjectRt.png',g,width=7.5,height = 4)

sub.rt$subj <- as.character(sub.rt$subj)
write.csv(sub.rt,'Subject_RT_Outliers.csv')

## Perform stats removing outliers
sub.rt$subj <-as.integer(sub.rt$subj)
# make a new dataframe for this analysis
rt.data <- left_join(sub.data,select(sub.rt,subj,outlier))
rt.data <- filter(rt.data, outlier==FALSE)
rt.data$rt.log <- log(rt.data$rt)
m.rt <- mean(rt.data$rt.log)
sd.rt <- sd(rt.data$rt.log)
total_responses <- dim(rt.data)[1]

# Exclude Outliers
# In LDT tasks in typical adults a timeout of 2.5s is often used.
# Since the current dataset includes children and adults with a wide range of reading ability, we use a more lenient threshold of 5s
thresh_max <- 5
print(sprintf('%.5f percent of trials have RT longer than %.0f sec',100*sum(rt.data$rt> thresh_max)/total_responses,thresh_max))
rt.data<-filter(rt.data, rt < thresh_max)

# Responses faster than 200ms are probably erronous button presses
thresh_min <- 0.2
print(sprintf('%.5f percent of trials have RT shorter than %.1f sec',100*sum(rt.data$rt < thresh_min)/total_responses,thresh_min))
rt.data<-filter(rt.data, rt > thresh_min)

# Exclude responses that deviate by more than 2IQR from Q1 or Q3
thresh <- 3
rt.data <- left_join(rt.data, select(sub.rt,subj,mean.rt,sd.rt, mean.raw.rt,sd.raw.rt, Q1.rt, Q3.rt, IQR.rt))
count_outliers_high <- sum(rt.data$rt > (rt.data$Q3.rt + thresh*rt.data$IQR.rt))
count_outliers_low <- sum(rt.data$rt < (rt.data$Q1.rt - thresh*rt.data$IQR.rt))

print(sprintf('%.5f percent of trials have RTs longer than %.0f times the IQR from the subject Q3',100*(count_outliers_high/total_responses),thresh))
print(sprintf('%.5f percent of trials have RTs shorter than %.0f times the IQR from the subject Q1' ,100*(count_outliers_low/total_responses),thresh))

rt.data<-filter(rt.data, (rt < (Q3.rt + thresh*IQR.rt) & rt > (Q1.rt - thresh*IQR.rt)))
print(sprintf('A total of %.5f percent of trials were removed' ,100*((total_responses-dim(rt.data)[1])/total_responses)))

print(h1a)

rt.data.c<-filter(rt.data, acc==1)

# Stats

# real-pseudo
lme1 <- lmer(rt.log ~ realpseudo + (realpseudo | subj) + (1 | word), filter(rt.data,acc==1))
summary(lme1)
# Lexical frequency real
lme2 <- lmer(rt.log ~ log(FREQ) + (log(FREQ)  | subj) + (1 | word), filter(rt.data,acc==1 & realpseudo=='real'))
summary(lme2)
# Bigram frequency pseudo
lme3 <- lmer(rt.log ~ log(UN2_F) + (log(UN2_F) | subj) + (1 | word), filter(rt.data,acc==1 & realpseudo=='pseudo'))
summary(lme3)
# Word length effect
lme4 <- lmer(rt.log ~ scale(wordLength)  + (scale(wordLength) | subj) + (1 | word), filter(rt.data,acc==1))
summary(lme4)
# Word length effect interaction
lme5 <- lmer(rt.log ~ scale(wordLength) * scale(wj_lwid_raw) + (scale(wordLength) | subj) + (1 | word), filter(rt.data,acc==1))
summary(lme5)

# Bigram frequency real
lme6 <- lmer(rt.log ~ log(UN2_F) + (log(UN2_F) | subj) + (1 | word), filter(rt.data,acc==1 & realpseudo=='real'))
summary(lme6)

# Full model
lme.all <- lmer(rt.log ~ log(UN2_F) * realpseudo * scale(wordLength) + (log(UN2_F) + realpseudo | subj), filter(rt.data,acc==1))
summary(lme.all)

# Compute mean and sd of
rp <- summarize(group_by(rt.data,realpseudo,subj), mrt = median(rt), pcor = sum(acc)/250)

summarize(group_by(rp,realpseudo),mean.r = mean(mrt),sd.r = sd(mrt),mean.c = mean(pcor),sd.c = sd(pcor))

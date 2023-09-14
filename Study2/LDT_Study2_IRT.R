library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(psych)
library(viridis)
library(wesanderson)
library(lme4)
library(tidyr)
library(lmerTest)

setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public")

# Read data
dfv1 <- read.csv('data_allsubs/LDT_alldata_wide_newcodes.csv')
dfv2 <- read.csv('Study2/data/LDT_alldata_wide_v2_newcodes.csv')
df <- full_join(dfv1,dfv2)
metadata <- read.csv('Study2/data/metadata_all_roundeddates_newcodes.csv')
metadata <- select(metadata, subj, wj_brs,	wj_rf,	wj_lwid_raw,	wj_lwid_ss,	wj_wa_raw,	wj_wa_ss,	twre_index,	twre_swe_raw,	twre_swe_ss,	twre_pde_raw,	twre_pde_ss,	wasi_fs2,	ctopp_rapid,	wasi_vocab_raw,	wasi_vocab_ts,	wasi_mr_raw,	wasi_mr_ts,MonthsSinceTesting,agenow,	visit_age)
demographic <- read.csv('Study2/data/demographic_all_newcodes.csv')
metadata <- left_join(metadata, demographic)

# Fit IRT model and compute ability
m1 <- mirt(select(df,-subj), model = 1, itemtype = 'Rasch', guess=0.5, technical=list(NCYCLES=5000))
# Just for version 2
m2 <- mirt(select(dfv2,-subj), model = 1, itemtype = 'Rasch', guess=0.5, technical=list(NCYCLES=5000))

irt.estimates <- select(df,subj)
irt.estimates$theta1pl <- fscores(m1)
# Note whether they are V1 or V2
irt.estimates$version <- factor(is.na(df$throomba),levels = c(TRUE,FALSE), labels = c('v1','v2'))

# Compute percent correct
irt.estimates$ncor <- rowSums(select(df,-subj),na.rm=TRUE)

# Filter out V1 for the time being
irt.estimates <- filter(irt.estimates,version=='v2')

# Join metadata and irt estiates
sub.data <- left_join(irt.estimates, metadata)
print(sprintf('%d Subjects with ROAR-LDT V2 data',dim(sub.data)[1]))
# Get data without nan
sub.data <- filter(sub.data,!is.na(wj_lwid_raw))
print(sprintf('%d Subjects with wj_lwid_raw scores that are not NaN',dim(sub.data)[1]))
# Remove subjects with very old scores
sub.data <- filter(sub.data,MonthsSinceTesting<12)
print(sprintf('%d Subjects with wj_lwid_raw scores in the last 12 months',dim(sub.data)[1]))

# Look at RT data to detect subjects with odd response properties
sub.sum <- left_join(sub.data,select(read.csv('Study2/data/LDT_summarymeasures_wide_v2_newcodes.csv'), -date_ldt,-pcor_all))
rt.data <-read.csv('Study2/data/LDT_alldata_long_v2_newcodes.csv')
sub.sum<- left_join(sub.sum,summarize(group_by(rt.data,subj),median.rt=median(log10(rt)),mean.rt=mean(log10(rt)), sd.rt=sd(log10(rt)),median.raw.rt=median(rt), mean.raw.rt=mean(rt), sd.raw.rt=sd(rt),
                                      Q1.rt=quantile(rt,0.25), Q3.rt=quantile(rt,0.75),IQR.rt=IQR(rt)))

sub.sum <- filter(sub.sum, !is.na(length_real))
lm1 <-lm(wj_lwid_raw ~ theta1pl,sub.sum)
lm2 <-lm(wj_lwid_raw ~ theta1pl + realpseudo + length_real,sub.sum)
sub.sum$res <- lm1$residuals
sub.sum$res2<-sub.sum$res^2
sub.sum$outlier <- abs(scale(sub.sum$res)) > 1.8
summary(lm(res ~ poly(mean.rt,3) * scale(sd.rt),sub.sum))
ggplot(sub.sum,aes(x=mean.rt,y=res,color=outlier)) +
  geom_point()
ggplot(sub.sum,aes(x=scale(mean.rt),y=scale(median.rt),color=abs(res))) +
  geom_point()

df.long <- read.csv('Study2/data/LDT_alldata_long_v2_newcodes.csv')
df.lists <- summarise(group_by(df.long,subj,wordList,stimBlock),ncor=sum(acc))
sub.sum <- left_join(sub.sum,summarise(group_by(df.lists,subj),sd.ncor=sd(ncor),m.ncor=mean(ncor)),by='subj')
ggplot(sub.sum, aes(x=sd.ncor,y=res2))+
  geom_point()

## Figure
# Set color limits
clims = c(80, 120)
sub.data$wj_clip <- sub.data$wj_lwid_ss
sub.data$wj_clip[sub.data$wj_clip<clims[1]]<-clims[1]
sub.data$wj_clip[sub.data$wj_clip>clims[2]]<-clims[2]
agerange <- c(min(sub.data$visit_age/12),max(sub.data$visit_age/12))
p1 <- ggplot(sub.data, aes(x=ncor, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Number correct (out of 252)') +
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1')) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data$ncor),y=max(sub.data$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data, ncor,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p1
p1b <- ggplot(sub.data, aes(x=ncor, y=wj_lwid_raw)) +
  geom_point(size=2,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Number correct (out of 252)') +
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1')) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data$ncor),y=max(sub.data$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data, ncor,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p1b
p2 <- ggplot(sub.data, aes(x=theta1pl, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (1PL model)') +
  # scale_color_viridis(option='viridis',direction=-1)+
  # scale_color_gradientn(colours = wes_palette(n=5, name="Zissou1"),limits = agerange) + 
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1')) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data$theta1pl),y=max(sub.data$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data, theta1pl,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p2
grid.arrange(p1,p2,nrow=1)
g = arrangeGrob(p1,p2,nrow=1)
ggsave(sprintf('ROAR-LDT_v2%s.pdf',date()),g,width=6, height=3)

# Now make figures just for the young children
agemo <-95
agerange <- c(5.9,8)

sub.data.1 <- filter(sub.data,agenow<=agemo)
print(sprintf('%d Subjects below the age of %d Months',dim(sub.data.1)[1],agemo))

p3 <- ggplot(sub.data.1, aes(x=ncor/252, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=5,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + 
  xlab('Proportion correct (252 trials)') +
  ylab('Woodcock Johnson Word ID (raw)')+
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1'),limits = agerange) + 
  labs(colour = "Age") + theme(legend.position = 'right')+
  geom_label(x=min(sub.data.1$ncor/252),y=max(sub.data.1$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data.1, ncor,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p3
p4 <- ggplot(sub.data.1, aes(x=theta1pl, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (1PL model)') +
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1'),limits = agerange) + 
  #scale_color_gradientn(colours = wes_palette(n=5, name="Zissou1"),limits = agerange) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data.1$theta1pl),y=max(sub.data.1$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data.1, theta1pl,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p4
grid.arrange(p1,p2,p3,p4,nrow=2)
g = arrangeGrob(p1,p2,p3,p4,nrow=2)
ggsave(sprintf('ROAR-LDT_v2_2rows%s.pdf',date()),g,width=6, height=5)

## Analyze ELL data
sub.data.ell <- filter(sub.data, eng_age>=2)
ell1 <- ggplot(sub.data.ell, aes(x=ncor, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Number correct (out of 252)') +
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1'),limits = agerange) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data.ell$ncor),y=max(sub.data.ell$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data.ell, ncor,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
ell1
ell2 <- ggplot(sub.data.ell, aes(x=theta1pl, y=wj_lwid_raw)) +
  geom_point(aes(colour=agenow/12),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (1PL model)') +
  scale_color_gradientn(colours = c( 'dodgerblue1','firebrick1','goldenrod1'),limits = agerange) + 
  #scale_color_gradientn(colours = wes_palette(n=5, name="Zissou1"),limits = agerange) + 
  labs(colour = "Age") + theme(legend.position = "right")+
  geom_label(x=min(sub.data.ell$theta1pl),y=max(sub.data.ell$wj_lwid_raw)-1, label=sprintf('r = %.2f',cor(select(sub.data.ell, theta1pl,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
ell2
grid.arrange(ell1,ell2,nrow=1)
g = arrangeGrob(ell1,ell2,nrow=1)
ggsave(sprintf('ROAR-LDT_v2_ELL%s.pdf',date()),g,width=6, height=3)


## Analyze performance on Study 2 words

v2words = c('sit','listen', 'lunch', 'night', 'tis', 'stenil', 'nulch', 'ginth',
            'fun' ,'cold', 'bathroom','teacher','nuf', 'dolc', 'throomba', 'chareet',
            'hello', 'name', 'good', 'hungry', 'loleh', 'eamn', 'dogo', 'gurynh')
df.v2words <- select(df, all_of(c('subj',v2words)))
df.v2words.1st <- filter(df.v2words, subj %in% sub.data$subj)
print(colSums(df.v2words.1st)/dim(df.v2words.1st)[1])

# Make figre looking at item difficulty and item correlation with overal performance
item.cors = as.data.frame(cor(select(dfv2,-subj),rowSums(select(dfv2,-subj),dim=1)/(dim(dfv2)[2]-1)))
item.cors <- rename(item.cors,r.performance = V1)
item.cors$pcor <- colSums(select(dfv2,-subj))/(dim(dfv2)[1])
item.cors$items <- row.names(item.cors)
item.cors$v2newwords <- item.cors$items %in% v2words
# Scatter plot of corr values
gt1 = ggplot(item.cors,aes(x=r.performance,y=1-pcor,label=items,colour=v2newwords)) +
  geom_text(size=2,fontface='bold',alpha=0.7)+
  scale_color_manual(values = c('deepskyblue1','firebrick1'))+
  
  theme(legend.position='none') + 
  xlab('Item correlation with overall proportion correct') + ylab('Item difficulty (1 - proportion correct)')
gt1
ggsave('itemcors.pdf',gt1,width=5,height=5)

## Now try to better predict things
sub.sum <- left_join(sub.data,select(read.csv('Study2/data/LDT_summarymeasures_wide_v2_newcodes.csv'), -date_ldt,-pcor_all))
ICC(select(sub.sum,pcor_A,pcor_B,pcor_C))
sub.rt <- left_join(sub.data,read.csv('Study2/data/LDT_rtdata_wide_v2_newcodes.csv'))

# Look at list and order effects
df.long <- read.csv('Study2/data/LDT_alldata_long_v2_newcodes.csv')
df.long$corinc = factor(df.long$acc)
lmer1 <- glmer(corinc ~ scale(stimOrder) + (scale(stimOrder) | subj),data = df.long, family = binomial)
summary(lmer1)
df.lists <- summarise(group_by(df.long,subj,wordList,stimBlock),ncor=sum(acc))
df.lists$stimBlock <- factor(df.lists$stimBlock)
summary(lmer(ncor ~ wordList + (1|subj),df.lists))
summary(lmer(ncor ~ stimBlock + (1|subj),df.lists))
summary(lmer(ncor ~ wordList*stimBlock + (1|subj),df.lists))

cor(filter(df.lists, wordList=='A')$ncor,filter(df.lists, wordList=='B')$ncor)
cor(filter(df.lists, wordList=='A')$ncor,filter(df.lists, wordList=='C')$ncor)
cor(filter(df.lists, wordList=='B')$ncor,filter(df.lists, wordList=='C')$ncor)
df.lists.w <- pivot_wider(df.lists,id_cols=subj,names_from = wordList,values_from = ncor)
r1=ggplot(df.lists.w, aes(x=A,y=B))+
  geom_point()
r2=ggplot(df.lists.w, aes(x=A,y=C))+
  geom_point()
r3=ggplot(df.lists.w, aes(x=C,y=B))+
  geom_point()
plot_grid(r1,r2,r3)

# Make FIGURE 4
df.stimlist <- summarise(group_by(df.long, wordList, subj), pcor=sum(acc)/84)
df.stimlist <- summarise(group_by(df.stimlist, wordList), m=mean(pcor), se=sd(pcor)/sqrt(length(unique(subj))))
g1 <- ggplot(df.stimlist,aes(x=wordList,y=m,fill=wordList))+
  geom_bar(stat='identity',color='black',alpha=1)+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.2)+ theme(legend.position='none')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('List')+ylab('Proportion correct')+
  ylim(0,1)

df.long$stimNumCat <- cut(df.long$stimOrder, seq(0,252,21))
df.stimnum <-summarise(group_by(df.long,stimNumCat,subj),acc=sum(acc))
df.stimnum <- summarise(group_by(df.stimnum,stimNumCat), m=mean(acc),se=sd(acc)/sqrt(length(unique(subj))))
g2 <- ggplot(df.stimnum,aes(x=stimNumCat,y=m/21))+
  geom_bar(stat='identity',color='black')+
  geom_errorbar(aes(ymin=m/21-se/21, ymax=m/21+se/21), width=.2) +
  geom_hline(yintercept = mean(df.stimnum$m/21),linetype = "dashed",color='firebrick4')+ylab('Proportion correct')+xlab('Trial number')+
  ylim(0,1)
f4 <- plot_grid(p3,g1,g2,align = "h",rel_widths = c(3,1.2,3),nrow=1)
ggsave('Figure4.pdf',f4,width=8,height=3)


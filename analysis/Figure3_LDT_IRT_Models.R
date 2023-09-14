library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(psych)

# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs")

# Read data
df <- read.csv('LDT_alldata_wide_sorted_newcodes.csv')
rt <- read.csv('LDT_rt_wide_subset_newcodes.csv')
s <- read.csv('LDT_summarymeasures_wide_newcodes.csv')
word.stats = read.csv('wordStatistics.csv')
word.stats$items <- word.stats$STRING

# Remove outliers from data frame
sub.outliers <- read.csv('Subject_RT_Outliers.csv')
df <- left_join(df,select(sub.outliers,subj,outlier))
df <- filter(df,outlier==FALSE)
df <- select(df,-outlier)
write.csv(df,'LDT_alldata_wide_sorted_nooutliers.csv')

# Correlation between items and percent correct
item.cors = as.data.frame(cor(select(df,-subj),rowSums(select(df,-subj),dim=1)/(dim(df)[2]-1)))
item.cors <- rename(item.cors,rvaltest=V1)
item.cors$items <- row.names(item.cors)
# Change 2 items that got renamed by R
#item.cors$items <- str_replace(item.cors$items,'xIf','if')
#item.cors$items <- str_replace(item.cors$items,'in.','in')
item.cors <- left_join(item.cors,word.stats)
item.cors$realpseudo <- factor(!is.na(item.cors$FREQ),levels=c(TRUE,FALSE),labels=c('Real','Pseudo'))

# Item correlation with wj
#Load and join metadata
metadata <- read.csv('metadata_all_newcodes.csv')
# remove metadata that is more than 12 months old
metadata <- filter(metadata,MonthsSinceTesting<12)
# match data types
wj.data <- left_join(df,metadata)
# Compute item correlation with wj and put it table
item.cors$rvalwj <-cor(select(df,-subj),wj.data$wj_lwid_raw, use='pairwise.complete.obs')
print(filter(item.cors,items=='insows'))
#n.wj <- sum(!is.na(wj.data$wj_lwid_raw))
#n.test= sum(!is.na(wj.data$insows))
#r.test(n.wj,0.598751)
#r.test(n.test,0.6136101)

# Add threshold
cor.thresh.pc <- 0.1 # remove items that have low correlation with percent correct
cor.thresh.wj <- 0.1 # remove items that have low correlation with wj
item.cors$thresh <- as.numeric(item.cors$rvaltest>cor.thresh.pc &
  item.cors$rvalwj>cor.thresh.wj |
  is.na(item.cors$rvalwj))
item.cors$realpseudo.thresh = interaction(item.cors$realpseudo,item.cors$thresh)
# Scatter plot of corr values
gt1 = ggplot(item.cors,aes(x=rvaltest,y=rvalwj,label=items,colour=realpseudo.thresh)) +
  geom_text(size=2,fontface='bold',alpha=0.7)+
  scale_color_manual(values = c('firebrick1','firebrick4','deepskyblue1','deepskyblue4'))+
  theme(legend.position='none') +
  scale_x_continuous('Item correlation with LDT proportion correct',c(-.4,-.2,0,.2,.4,.6),limits=c(-.4,.7))+
scale_y_continuous('Item correlation with Woodcock Johnson Word ID (raw)',c(-.4,-.2,0,.2,.4,.6),limits=c(-.4,.7)) +
  geom_vline(xintercept=cor.thresh.pc,color='firebrick1',linetype ='dashed',alpha =0.2) +
  geom_hline(yintercept=cor.thresh.wj,color='firebrick1',linetype ='dashed',alpha=0.2)
gt1
ggsave('itemcors.pdf',gt1,width=5,height=5)
ggsave('itemcors.png',gt1,width=5,height=5)

# infit and outfit thresholds (https://www.rasch.org/rmt/rmt83b.htm and https://www.rasch.org/rmt/rmt34e.htm)
fit.thresh = c(.6, 1.4)
outliers <- TRUE
maxiter <- 20
iteration <- 0
df_goodRasch <- select(df, all_of(c('subj',item.cors$items[item.cors$thresh==1])))

# fit 1PL model and look at fit stats 
while (outliers > 0 & iteration<maxiter){
  iteration <- iteration + 1;
  start.dim <- dim(df_goodRasch)[2]-1
  mR <- mirt(select(df_goodRasch,-subj), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
  mR.itemfit = itemfit(mR, fit_stats = 'infit')
  good.items = as.character(mR.itemfit$item[mR.itemfit$infit>fit.thresh[1] & 
                            mR.itemfit$outfit>fit.thresh[1] &
                            mR.itemfit$infit<fit.thresh[2] &
                            mR.itemfit$outfit<fit.thresh[2]])
  outliers <- dim(df_goodRasch)[2] - length(good.items) -1 #-1 because subj was removed in fitting
  # Remove items with low or extreme slope and refit
  df_goodRasch <- select(df_goodRasch, all_of(c('subj',good.items)))
  end.dim <- dim(df_goodRasch)[2]-1
  print(sprintf('1PL ITERATION %d. STARTED WITH %d ITEMS. %d OUTLIERS REMOVED. %d ITEMS RETAINED.',iteration,start.dim,outliers,end.dim))
}
# Fit final model
mR <- mirt(select(df_goodRasch,-subj), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
mR.coef <- coef(mR,simplify=TRUE, IRTpars = TRUE) # Get coeeficients
mR.coef <- arrange(tibble::rownames_to_column(as.data.frame(mR.coef$items),'words'),b)
write.csv(mR.coef,'RaschModelWords.csv')

#  Fit 2 parameter IRT model with guess rate of 0.5
outliers <- TRUE
maxiter <- 20
iteration <- 0
df_good <- df_goodRasch
aminmax <- c(.7, Inf)
umin <- 0.9
# Model priors
mm = (
  'F = 1
PRIOR = (1-%d, a1, lnorm, .2, 1)'
)
while (outliers > 0 & iteration<maxiter){
  iteration <- iteration + 1;
  start.dim <- dim(df_good)[2]-1
  #mm = mirt.model(sprintf(mm,start.dim))
  mm=1
  m <- mirt(select(df_good,-subj), model = mm,itemtype = '2PL',guess=0.5,technical=list(NCYCLES=5000)) # 2AFC. Guess Rate = 0.5
  co <- coef(m,simplify=TRUE, IRTpars = TRUE) # Get coeeficients
  co <- tibble::rownames_to_column(as.data.frame(co$items),'words')
  ggplot(co, aes(a, b)) + geom_point(size=3)
  ggsave(sprintf('2PL-ModelParams_%d.png',iteration))
  # Remove items with low or extreme slope and refit
  df_good <- select(df_good,c(1, which(co$a>aminmax[1] & co$a<aminmax[2] & co$u>umin)+1)) # +1 to account for subj not being in fitting
  end.dim <- dim(df_good)[2]-1
  outliers <- sum(!(co$a>aminmax[1] & co$a<aminmax[2] & co$u>umin))
  print(sprintf('2PL ITERATION %d. STARTED WITH %d ITEMS. %d OUTLIERS REMOVED. %d ITEMS RETAINED.',iteration,start.dim,outliers,end.dim))
}

# Sort by difficulty
co <- arrange(co,b)
write.csv(co,'RaschModel_2PLWords.csv')
co <- left_join(co, rename(item.cors,words = items))

# figure out the words that were removed at each stage of IRT
df_preIRT <- select(df, all_of(c('subj',item.cors$items[item.cors$thresh==1])))
m.2PL <- mirt(select(df_preIRT,-subj), model = 1, itemtype = '2PL',guess=0.5, technical=list(NCYCLES=500)) 
m.2PL.coef <- coef(m.2PL,simplify=TRUE, IRTpars = TRUE) # Get coeeficients
m.2PL.coef <- tibble::rownames_to_column(as.data.frame(m.2PL.coef$items),'words')
# Mark which words from df_preIRT were removed during 1PL and 2PL clearning
m.2PL.coef$kept <- m.2PL.coef$words %in% co$words
# Joini real/pseudo
m.2PL.coef <- left_join(m.2PL.coef,rename(word.stats,words=items))
m.2PL.coef$realpseudo <- factor(!is.na(m.2PL.coef$FREQ),levels=c(TRUE,FALSE),labels=c('Real','Pseudo'))
m.2PL.coef$realpseudo.thresh = interaction(m.2PL.coef$realpseudo,m.2PL.coef$kept)

# Plot out parameters of 2PL model
gt2 = ggplot(m.2PL.coef,aes(x=b,y=a,label=words,color=realpseudo.thresh)) +
  geom_text(size=2,fontface='bold',alpha=0.7)+
  scale_color_manual(values = c('firebrick1','firebrick4','deepskyblue1','deepskyblue4'))+
  theme(legend.position='none') +scale_y_log10()+xlab('Item difficulty')+ylab('Item discriminability')
 
gt2
ggsave('itemparams2PL.pdf',gt1,width=5,height=5)
ggsave('itemparams2PL.png',gt1,width=5,height=5)
gt <- arrangeGrob(gt1,gt2,nrow=1) 
ggsave('itemanalysis.png',gt,width=7.5,height=4.08)
ggsave('itemanalysis.pdf',gt,width=7.5,height=4.08)

#  Fit 1 parameter IRT model with guess rate of 0.5 to these same data
m1 <- mirt(select(df_good,-subj), model = 1,itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
co.m1 <- coef(m1,simplify=TRUE, IRTpars = TRUE) # Get coeeficients
co.m1 <- arrange(tibble::rownames_to_column(as.data.frame(co.m1$items),'words'),b)
write.csv(co.m1,'RaschModel_2PLWordsRasch.csv')

# Histogram of slopes and difficulties
h1 <- ggplot(co, aes(x=a)) + 
  geom_histogram(color="black", fill="white")
h2 <- ggplot(co, aes(x=b)) + 
  geom_histogram(color="black", fill="white")
grid.arrange(h1, h2, nrow = 1)
g <- arrangeGrob(h1,h2, nrow=1) 
ggsave('2PL-ModelParamsHist.png',g)

##plots, see 'type' argument here, https://rdrr.io/cran/mirt/man/plot-method.html
p1 = plot(m,type="info") #test information
p2 = plot(m,type="trace",which.items=c(1,10,20,30)) #test information
p3 = plot(m,type="infotrace",which.items=c(1,10,20)) #test information
grid.arrange(p1,p2,p3, nrow = 1)
g <- arrangeGrob(p1,p2,p3, nrow=1) 
ggsave('2PL-testplots_randitems.png',g)

# Plot item functioms
png("itemfunctions.png")
plot(NULL,xlim=c(-4,4),ylim=c(.5,1),xlab='Theta',ylab='P(correct)')
cc<-coef(m)
th<-seq(-4,4,length.out=1000)
for (i in 1:(length(cc)-1)) {
  cc[[i]]->a
  k<-a[1]*(th-a[2])
  ek<-exp(k)
  yv<-a[3]+(1-a[3])*ek/(1+ek)
  lines(th,yv)
}
dev.off()

# Diagnostic plots
png("infoSE.png")
plot(m,type="infoSE") #test information and standard error
dev.off()

png("Precision.png")
plot(m,type="rxx") #test information and standard error
dev.off()

# Put theta estimates for each subject into a dataframe
sub.data <- select(df,subj)
# Compute and add percent correct
sub.data$pcor <- rowSums(select(df,-subj),dim=1)/(dim(df)[2]-1)
# Get subject thetas
sub.data$f1PL <- fscores(mR)
sub.data$f2PL1PL <- fscores(m1)
sub.data$f2PL <- fscores(m)
write.csv(sub.data,'SubjectThetaEstimates.csv')



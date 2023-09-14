library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(mirt)
library(psych)
library(viridis)
library(stringr)
library(lmerTest)

# Whether or not to generate new lists
generate.lists=FALSE

## Loading data

# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/")


df <- read.csv('LDT_alldata_wide_sorted_nooutliers.csv') # data with outliers removed
words.irt1 = read.csv('RaschModelWords.csv')
words.irt2 = read.csv('RaschModel_2PLWords.csv')
words.irt3 = read.csv('RaschModel_2PLWordsRasch.csv')
print(sprintf('Correlation between difficulty estimates based on 1PL vs 2PL: r=%.4f',
              cor(words.irt2$b,words.irt3$b)))
word.stats = read.csv('wordStatistics.csv')
word.stats$words <- word.stats$STRING # if and in get changed in R so we change them here
word.exclude <- read.csv('WordsToExclude.csv')

sub.data <- read.csv('SubjectThetaEstimates.csv')
# Load and join metadata
metadata <- read.csv('metadata_all_newcodes.csv')
#NaN metadata entries that are older than 12 months
metadata$wj_lwid_raw[metadata$MonthsSinceTesting>=12] <- NA
# match data types
sub.data$subj <- as.character(sub.data$subj)
metadata$subj <- as.character(metadata$subj)
sub.data <- left_join(sub.data,metadata)
# Load in WJ -> Grade equivalent Look Up Table
wj.lwid.grade <- read.csv('WJ-LWID-GradeEquiv.csv')
# Join in age and grade equivalent WJ scores
sub.data <- left_join(sub.data, wj.lwid.grade)

## Setting some parameters
theta.range <-quantile(sub.data$f1PL,c(0,1))

# Joining word stats
words.irt.stats1 <- left_join(words.irt1, word.stats)
words.irt.stats2 <- left_join(words.irt2, word.stats)
words.irt.stats3 <- left_join(words.irt3, word.stats)
write.csv(words.irt.stats1,'RaschModeWordsl_stats.csv')
write.csv(words.irt.stats2,'RaschModel_2PLWords_stats.csv')
write.csv(words.irt.stats3,'RaschModel_2PLWordsRasch_stats.csv')

sprintf('%d Pseudowords and %d Realswords',sum(is.na(words.irt.stats1$FREQ)),
        sum(!is.na(words.irt.stats1$FREQ)))
sprintf('%d Pseudowords and %d Realswords',sum(is.na(words.irt.stats2$FREQ)),
        sum(!is.na(words.irt.stats2$FREQ)))
sprintf('%d Pseudowords and %d Realswords',sum(is.na(words.irt.stats3$FREQ)),
        sum(!is.na(words.irt.stats3$FREQ)))

# Remove pseudowords that might confuse English Language Learners
words.irt.stats3 <- filter(words.irt.stats3,!is.element(words,word.exclude$WordsToExclude))
words.irt.stats3$realpseudo = factor(is.na(words.irt.stats3$FREQ),levels=c(FALSE,TRUE),labels = c('real','pseudo'))
sprintf('%d Pseudowords and %d Realswords left after final cleaning',sum(is.na(words.irt.stats3$FREQ)),
        sum(!is.na(words.irt.stats3$FREQ)))
g1 = ggplot(words.irt.stats3,aes(x=LEN,color=realpseudo,fill=realpseudo))+
  geom_histogram(binwidth = 1,position="dodge",alpha=0.5)

n.rw = 38 # 38 real words 
n.pw = 38 # 40 pseudoworsd
nl = 3 # 3 lists
thetavals = seq(min(theta.range),max(theta.range),.01)
# divide into real and pseudo word lists
rw <- filter(words.irt.stats3,!is.na(FREQ))
pw <- filter(words.irt.stats3,is.na(FREQ))
# rows to each list
rw.rows = floor(sum(!is.na(words.irt.stats3$FREQ))/nl)*nl
pw.rows = floor(sum(is.na(words.irt.stats3$FREQ))/nl)*nl

row2list.rw <- cbind(seq(1,rw.rows,3),seq(2,rw.rows,3),seq(3,rw.rows,3))
row2list.pw <- cbind(seq(1,pw.rows,3),seq(2,pw.rows,3),seq(3,pw.rows,3))

if (generate.lists){
  # Make 100 lists by randomly sampling rows
  nlists = 250
  # We want the ends of the continuum. Keep this many items at the top and bottom
  topkeep = 3
  bottomkeep = 5
  mse <- matrix(nrow=nlists,ncol=3)
  mi <- matrix(nrow=nlists,ncol=3)
  sdi <- matrix(nrow=nlists,ncol=3)
  rwj <- matrix(nrow=nlists,ncol=3)
  d.LH <- matrix(nrow=nlists,ncol=3)
  
  for (ii in 1:nlists){
    # Shuffle columns
    row2list.pw.ii <- row2list.pw[,sample(ncol(row2list.pw))]
    row2list.rw.ii <- row2list.rw[,sample(ncol(row2list.rw))]
    for (jj in 1:dim(row2list.pw)[1]){
      row2list.pw.ii[jj,] <- row2list.pw[jj,sample(ncol(row2list.pw))]}
    for (jj in 1:dim(row2list.rw)[1]){
      row2list.rw.ii[jj,] <- row2list.rw[jj,sample(ncol(row2list.rw))]}
    
    # Shuffle rows and extract desired number of words
    # First generate a random sample but leaving the first n and last n rows in plkace
    pwsample <- sample((bottomkeep+1):(nrow(row2list.pw.ii)-topkeep), size=(n.pw-bottomkeep-topkeep), replace = FALSE)
    rwsample <- sample((bottomkeep+1):(nrow(row2list.rw.ii)-topkeep), size=(n.rw-bottomkeep-topkeep), replace = FALSE)
    
    # Take the sample but keeping the desired number at the top and bottom
    row2list.pw.ii <- row2list.pw.ii[c(1:bottomkeep,pwsample,(nrow(row2list.pw.ii)-topkeep+1):nrow(row2list.pw.ii)),]
    row2list.rw.ii <- row2list.rw.ii[c(1:bottomkeep,rwsample,(nrow(row2list.rw.ii)-topkeep+1):nrow(row2list.rw.ii)),]
    
    # Make datafram with n lists
    word.lists = data.frame(listA = c(rw$words[row2list.rw.ii[,1]],pw$words[row2list.pw.ii[,1]]),
                            listB = c(rw$words[row2list.rw.ii[,2]],pw$words[row2list.pw.ii[,2]]),   
                            listC = c(rw$words[row2list.rw.ii[,3]],pw$words[row2list.pw.ii[,3]]))
    # Match word length histograms between rw and pw
    d.LH[ii,1] <- sum((hist(str_length(word.lists$listA[1:n.rw]), breaks=c(2: 16))$counts -
                hist(str_length(word.lists$listA[(n.rw+1):dim(word.lists)[2]]), breaks=c(2: 16))$counts)^2)
    d.LH[ii,2] <- sum((hist(str_length(word.lists$listB[1:n.rw]), breaks=c(2: 16))$counts -
                          hist(str_length(word.lists$listB[(n.rw+1):dim(word.lists)[2]]), breaks=c(2: 16))$counts)^2)
    d.LH[ii,3] <- sum((hist(str_length(word.lists$listC[1:n.rw]), breaks=c(2: 16))$counts -
                          hist(str_length(word.lists$listC[(n.rw+1):dim(word.lists)[2]]), breaks=c(2: 16))$counts)^2)
    m1 <- mirt(select(df,all_of(word.lists$listA)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
    m2 <- mirt(select(df,all_of(word.lists$listB)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
    m3 <- mirt(select(df,all_of(word.lists$listC)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
    ti1 <- testinfo(m1,thetavals)
    ti2 <- testinfo(m2,thetavals)
    ti3 <- testinfo(m3,thetavals)
    mse[ii,1] <- mean((ti1 - ti2)^2)
    mse[ii,2] <- mean((ti1 - ti3)^2)
    mse[ii,3] <- mean((ti2 - ti3)^2)
    mi[ii,1] <- mean(ti1)
    mi[ii,2] <- mean(ti2)
    mi[ii,3] <- mean(ti3)
    sdi[ii,1] <- sd(ti1)
    sdi[ii,2] <- sd(ti2)
    sdi[ii,3] <- sd(ti3)
    # Get theta values and check correlation with wj
    fs <- data.frame(subj=as.character(df$subj), fscores(m1))
    fs$F2 <-fscores(m2)
    fs$F3 <- fscores(m3)
    fs <- left_join(fs,metadata)
    rwj[ii,]<-cor(select(fs,F1,F2,F3),select(fs,wj_lwid_raw),use='pairwise.complete.obs')
    write.csv(word.lists,sprintf('wordlists/WordLists%d.csv',ii))
    print(sprintf('SAVING wordlists/WordLists%d.csv',ii))
    pdf(sprintf("wordlists/Precision%d.pdf",ii))
    plot(thetavals,ti1,col='red',lwd=1,tck=1)
    lines(thetavals,ti2,col='green',lwd=4)
    lines(thetavals,ti3,col='blue',lwd=4)
    dev.off()
  } 
  list.stats <- data.frame(mse=rowSums(mse),info=rowSums(mi), sd=rowSums(sdi),
                           r.wj=rwj,min.wj=apply(rwj,1,FUN=min),max.wj=apply(rwj,1,FUN=max),
                           mean.wj=rowMeans(rwj),length.diff=rowSums(d.LH))
  list.stats$listnum <-rownames(list.stats)
  list.stats <- arrange(list.stats,mse)
  
  p1 <- ggplot(list.stats, aes(mse, info, label = listnum,color=mean.wj)) + geom_text(alpha=.8)+theme(legend.position = 'none')+scale_color_viridis(option='plasma',direction=1)
  p2 <- ggplot(list.stats, aes(sd, info, label = listnum,color=mean.wj)) + geom_text(alpha=.8)+theme(legend.position = 'none')+scale_color_viridis(option='plasma',direction=1)
  p3 <- ggplot(list.stats, aes(sd, mse, label = listnum,color=mean.wj)) + geom_text(alpha=.8)+theme(legend.position = 'none')+scale_color_viridis(option='plasma',direction=1)
  p4 <- ggplot(list.stats, aes(min.wj, max.wj, label = listnum, color=info)) + geom_text(alpha=.8)+theme(legend.position = 'none')+scale_color_viridis(option='plasma',direction=1)
  p5 <- ggplot(list.stats, aes(length.diff, mean.wj, label = listnum,color=info)) + geom_text(alpha=.8)+theme(legend.position = 'none')+scale_color_viridis(option='plasma',direction=1)
  
  grid.arrange(p1,p2,p3, p4,p5, nrow =1)
  g <- arrangeGrob(p1,p2,p3,p4,p5, nrow=1) 
  ggsave('wordlists/ListStatsPlots.pdf',g,height = 3,width=10)
  g
  write.csv(list.stats,'wordlists/liststats.csv')
}else{
  list.stats = read.csv('wordlists/liststats.csv')
}

## Load up the prefered list and use it for figures
word.list.file = 'wordlists/WordLists144_finalized.csv' # swapped sain for med
print(sprintf('LOADING WORD LIST FILE: %s',word.list.file))
word.lists <- read.csv(word.list.file)

# Collect all the three lists into 1
all.words <- c(as.character(word.lists$listA), as.character(word.lists$listB),
              as.character(word.lists$listC))

# Plot the final list
m.A <- mirt(select(df,all_of(word.lists$listA)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
m.B <- mirt(select(df,all_of(word.lists$listB)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
m.C <- mirt(select(df,all_of(word.lists$listC)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
m.all <- mirt(select(df,all_of(all.words)), model = 1, itemtype = 'Rasch', guess=0.5) # 2AFC. Guess Rate = 0.5
# Compute model reliability
empirical_rxx(fscores(m.A, full.scores.SE = TRUE))
empirical_rxx(fscores(m.B, full.scores.SE = TRUE))
empirical_rxx(fscores(m.C, full.scores.SE = TRUE))
empirical_rxx(fscores(m.all, full.scores.SE = TRUE))

ti <- data.frame(A=testinfo(m.A,thetavals),B=testinfo(m.B,thetavals),C=testinfo(m.C,thetavals))
ti$thetavals <- thetavals
mse.final <- c(mean((ti$A - ti$B)^2), mean((ti$A - ti$C)^2), mean((ti$B - ti$C)^2))
mi.final<- c(mean(ti$A),mean(ti$B),mean(ti$C))

# Add theta estimates to dataframe
sub.data$ListAtheta <- fscores(m.A)
sub.data$ListBtheta <- fscores(m.B)
sub.data$ListCtheta <- fscores(m.C)
sub.data$ListAlltheta <- fscores(m.all)

# Fit linear model relating Theta to WJ grade equivalent
lm.T.WJ = lm(wj_lwid_raw~ ListAlltheta,sub.data)

# Plot test information
ti.long <- gather(ti,List,TestInfo,A,B,C)
p1 = ggplot(ti.long,aes(x=thetavals,y=TestInfo,group=List)) +
  geom_line(aes(colour=List),size=2,alpha=0.5) +
  xlab('Ability') + ylab('Test Information')+ 
  scale_y_continuous(limits=c(0,5.3),breaks=c(0,1,2,3,4,5))+
  theme(legend.direction = "horizontal", legend.position = c(0.55, 0.1),
        legend.background = element_rect(fill='white', size=.5, linetype="solid", colour="gray30"))+
  scale_x_continuous(breaks=c(-4, -2, 0,2,4),sec.axis = sec_axis(~ .*coef(lm.T.WJ)[2] + coef(lm.T.WJ)[1], 
                                         name= 'Woodcock Johnson Word ID (raw)',breaks=c(30,40,50,60,70))) +
  theme(axis.title = element_text(size = 10.5))+
  # Add grade level scale
  geom_vline(xintercept = (29-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],linetype='dashed',alpha=.3,color='indianred') +
  geom_vline(xintercept = (43-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],alpha=.3,color='indianred') +
  geom_text(x=(43-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],y=5.3,label='2nd',size=3,color='indianred')+
  
  geom_vline(xintercept = (50-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],linetype='dashed',alpha=.3,color='indianred') +
  geom_vline(xintercept = (55-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],alpha=.3,color='indianred') +
  geom_text(x=(55-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],y=5.3,label='4th',size=3,color='indianred')+

  geom_vline(xintercept = (58.5-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],linetype='dashed',alpha=.3,color='indianred') +

  geom_vline(xintercept = (61-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],alpha=.3,color='indianred') +
  geom_text(x=(61-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],y=5.3,label='6th',size=3,color='indianred') +

  geom_vline(xintercept = (63-coef(lm.T.WJ)[1])/coef(lm.T.WJ)[2],linetype='dashed',alpha=.3,color='indianred')
  
p1

p2 = ggplot(sub.data,aes(x=ListAtheta,y=ListBtheta)) + geom_point(alpha=0.7) +
  stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (List A)') + ylab('Ability estimate (List B)')+
  geom_label(x=-4.2,y=3, label=sprintf('r = %.2f',cor(sub.data$ListAtheta,sub.data$ListBtheta)),hjust=0, vjust=0,size=3)
p3 = ggplot(sub.data,aes(x=ListBtheta,y=ListCtheta)) + geom_point(alpha=0.7) +
  stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (List B)') + ylab('Ability estimate (List C)')+
  geom_label(x=-4.2,y=3, label=sprintf('r = %.2f',cor(sub.data$ListBtheta,sub.data$ListCtheta)),hjust=0, vjust=0,size=3)
p3b = ggplot(sub.data,aes(x=ListAtheta,y=ListCtheta)) + geom_point(alpha=0.7) +
  stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Ability estimate (List A)') + ylab('Ability estimate (List C)')+
  geom_label(x=-4.2,y=3, label=sprintf('r = %.2f',cor(sub.data$ListBtheta,sub.data$ListCtheta)),hjust=0, vjust=0,size=3)

grid.arrange(p1,p2,p3, nrow =1)
g <- arrangeGrob(p1,p2,p3,nrow=1) 
ggsave('TestInformation.pdf',g ,width=7.5, height=3)
ggsave('TestInformation.png',g,width=7.5, height=3)

g.b <- arrangeGrob(p2,p3,p3b,nrow=1) 
ggsave('ListABC_TestRetest.pdf',g.b ,width=7.5, height=3)
ggsave('ListABC_TestRetest.png',g.b,width=7.5, height=3)
## Plots relating to WJ scores

# Get data without nan
sub.data <- filter(sub.data,!is.na(wj_lwid_raw))

# Remove subjects with very old scores
sub.data <- filter(sub.data,MonthsSinceTesting<12)

# Plot
# Set color limits
clims = c(80, 120)
ylims = c(min(sub.data$wj_lwid_raw)-2, max(sub.data$wj_lwid_raw)+2)
sub.data$wj_clip <- sub.data$wj_lwid_ss
sub.data$wj_clip[sub.data$wj_clip<clims[1]]<-clims[1]
sub.data$wj_clip[sub.data$wj_clip>clims[2]]<-clims[2]
midcolor <- 'gray50'
lowcolor<-'dodgerblue4'
highcolor<-'firebrick'
s1 <- ggplot(sub.data, aes(x=ListAtheta, y=wj_lwid_raw)) +
  geom_point(size=3, aes(colour=wj_clip),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + 
  scale_x_continuous('Ability estimate (List A)',c(-4, -2 ,0, 2, 4),limits=c(-5.1,5.1)) +
  scale_y_continuous('Woodcock-Johnson Word ID (raw)', c(20,30,40,50,60,70),limits=c(18,75)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor)+ theme(legend.position='none') +
   geom_label(x=-4,y=70, label=sprintf('r = %.2f',cor(sub.data$ListAtheta,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=3)

s2 <- ggplot(sub.data, aes(x=ListBtheta, y=wj_lwid_raw)) +
  geom_point(size=3, aes(colour=wj_clip),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') +
  scale_x_continuous('Ability estimate (List B)',c(-4, -2 ,0, 2, 4),limits=c(-5.1,5.1)) +
  scale_y_continuous(element_blank(), c(20,30,40,50,60,70),limits=c(18,75)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor)+ theme(legend.position='none')+
  geom_label(x=-4,y=70, label=sprintf('r = %.2f',cor(sub.data$ListBtheta,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=3)

s3 <- ggplot(sub.data, aes(x=ListCtheta, y=wj_lwid_raw)) +
  geom_point(size=3, aes(colour=wj_clip),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') +
  scale_x_continuous('Ability estimate (List C)',c(-4, -2 ,0, 2, 4),limits=c(-5.1,5.1)) +
  scale_y_continuous(element_blank(), c(20,30,40,50,60,70),limits=c(18,75)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor)+ theme(legend.position='none') +
  geom_label(x=-4,y=70, label=sprintf('r = %.2f',cor(sub.data$ListCtheta,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=3)

  

grid.arrange(p1,p2,p3,s1,s2,s3, nrow =2)
g <- arrangeGrob(p1,p2,p3,s1,s2,s3,nrow=2) 
ggsave('Figure3_TestInformation.pdf',g ,width=7.5, height=6)
ggsave('Figure3_TestInformation.png',g,width=7.5, height=6,dpi=400)
ggsave('Figure3_TestInformation.tiff',g,width=7.5, height=6,dpi=400)


## Calculate reliability
ICC(as.matrix(select(sub.data,ListAtheta, ListBtheta, ListCtheta)))
cor(select(sub.data,ListAtheta, ListBtheta, ListCtheta))

# Save figure of the model with all the words
g <- ggplot(sub.data, aes(x=ListAlltheta, y=wj_lwid_raw)) +
  geom_point(size=3, aes(colour=wj_clip),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') +
  scale_x_continuous('Ability estimate (composite)',c(-4, -2 ,0, 2, 4)) +
  scale_y_continuous('Woodcock-Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor)+ theme(legend.position='none') +
  geom_label(x=-4.5,y=70, label=sprintf('r = %.2f',cor(sub.data$ListAlltheta,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=4)
ggsave('FullTest.pdf',g ,width=5.5, height=4.5)
ggsave('FullTest.png',g,width=5.5, height=4.5)
g


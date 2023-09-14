library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(psych)
library(CTT)

#Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/data_allsubs/")

# Function to Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Function to Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# sub.data <- read.csv('SubjectThetaEstimates.csv')
metadata <- read.csv('metadata_all_newcodes.csv')
sub.data <- read.csv('LDT_summarymeasures_wide_newcodes.csv')
sub.outliers <- read.csv('Subject_RT_Outliers.csv')
sub.outliers$subj <- as.character(sub.outliers$subj)

# match data types
sub.data$subj <- as.character(sub.data$subj)
metadata$subj <- as.character(metadata$subj)
sub.data <- left_join(sub.data,metadata)

response.data <- read.csv('LDT_alldata_wide_sorted_newcodes.csv')
# reliability values for tests
pcor_all.rxx = itemAnalysis(select(response.data,-subj))
test.reliability <-data.frame(pcor_all = pcor_all.rxx$alpha, wj_lwid_raw=0.94, wj_wa_ra = 0.90, twre_swe_raw=0.91, twre_pde_raw=0.92,
                              wasi_vocab_raw=0.90,wasi_matrix_raw=0.79)

# Histograms of subject charachteristics
print(mean(sub.data$agenow/12,na.rm=TRUE))
print(sd(sub.data$agenow/12,na.rm=TRUE))
print(mean(sub.data$wj_brs,na.rm=TRUE))
print(sd(sub.data$wj_brs,na.rm=TRUE))
print(mean(sub.data$twre_index,na.rm=TRUE))
print(sd(sub.data$twre_index,na.rm=TRUE))
print(sum(sub.data$wj_brs<85,na.rm=TRUE))/sum(!is.na(sub.data$wj_brs))
print(sum(sub.data$twre_index<85,na.rm=TRUE))/sum(!is.na(sub.data$twre_index))

print(summary(sub.data))

gg1 <- ggplot(sub.data,aes(x=agenow/12)) +
  geom_histogram(bins=20,color='gray20',fill='gray80')+
  xlab('Age (years)')+
  ylab('Count')+
  theme(axis.title.x = element_text(size=8),axis.title.y = element_text(size=8))

gg2 <- ggplot(sub.data,aes(x=wj_brs)) +
  geom_histogram(bins=20, color='gray20',fill='gray80')+
  xlab('WJ Basic Reading Skills (standard score)')+
  ylab(element_blank())+
  theme(axis.title.x = element_text(size=8),axis.title.y = element_text(size=8))

g <- arrangeGrob(gg1,gg2)
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
  geom_point(aes(colour=wj_clip),size=3,alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Proportion correct (full test)') +
  geom_point(data=filter(sub.data,outlier==TRUE), aes(x=pcor_all, y=wj_lwid_raw),size=3,color='gray30',shape=1)+
  scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor) + 
    labs(colour = "Standard\nScore") + theme(legend.position = "right",legend.title=element_text(size=10))+
  geom_label(x=.5,y=65, label=sprintf('r = %.2f',cor(select(filter(sub.data, outlier==FALSE), pcor_all,wj_lwid_raw))[1,2]),hjust=0, vjust=0,size=3)
p1
ggsave('Figure2_PercentCorrect.pdf',p1, width=3.5, height=3)
ggsave('Figure2_PercentCorrect.tiff',p1, width=3.5, height=3,dpi=400)

d.rxy = disattenuated.cor(r.xy=cor(select(filter(sub.data, outlier==FALSE),pcor_all,wj_lwid_raw))[1,2],
                  r.xx=c(test.reliability$pcor_all,test.reliability$wj_lwid_raw))
print(sprintf('Disatenuated correlation between LDT percent correct and WJ LWID RAW = %.3f',d.rxy))

# Percent correct versus WJ scatter plot with subject IDS
t1 <- ggplot(sub.data, aes(x=pcor_all, y=wj_lwid_raw,label=subj)) +
  geom_text( aes(colour=wj_clip),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray30') + xlab('Proportion correct (full test)') +
  scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
  scale_color_gradient2(midpoint=100, space ="Lab",limits=clims,
                        low = lowcolor, high = highcolor, mid = midcolor) + 
  labs(colour = "Standard\nScore") + theme(legend.position = "right")+
  geom_label(x=.5,y=65, label=sprintf('r = %.2f',cor(sub.data$pcor_all,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=3)
t1
ggsave('Figure2_PercentCorrect_sublabels.pdf',t1, width=3.5, height=3)

# Percent correct versus WJ scatter plot with subject IDS and color coded outliers based on RT
t2 <- ggplot(sub.data, aes(x=pcor_all, y=wj_lwid_raw,label=subj)) +
  geom_text( aes(colour=outlier),alpha=1) + stat_smooth(method="lm", se=TRUE, color='gray40') + xlab('Proportion correct (full test)') +
  scale_y_continuous('Woodcock Johnson Word ID (raw)', c(20,30,40,50,60,70)) +
  labs(colour = "Standard\nScore") + theme(legend.position = "right")+
  geom_label(x=.5,y=65, label=sprintf('r = %.2f',cor(sub.data$pcor_all,sub.data$wj_lwid_raw)),hjust=0, vjust=0,size=3)
t2
ggsave('Figure2_PercentCorrect_sublabels_outliers.pdf',t2, width=3.5, height=3)

## Corelations

# Select variables to be used in the correlation matrix EXCLUDING OUTLIERS
cor.data <- select(filter(sub.data, outlier==FALSE),pcor_all,pcor_real,pcor_pseudo,wj_lwid_raw,twre_swe_raw,
                   wj_wa_raw,twre_pde_raw,wasi_vocab_raw, wasi_mr_raw)
# Compute correlations
cormat <- cor(cor.data,use = "pairwise.complete.obs")
# Melt the correlation matrix
melted_cormat <- melt(get_upper_tri(cormat), na.rm = TRUE)
# Create a ggheatmap
c1 = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color='white')+
  scale_fill_gradient2(low = 'gray50', mid = "slateblue2", high = "sienna2",midpoint=0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  geom_text(aes(Var2, Var1, label = sprintf('%.2f',value)), color = "black", size = 2.8)+
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  size = 8, hjust = 1))+
  theme(axis.text.y = element_text(size = 8))+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.title=element_text(size=9),
    legend.text=element_text(size=7),
    legend.direction = "horizontal")+
  
  guides(fill = guide_colorbar(barwidth = 5, barheight = .8,
                               title.position = "top", title.hjust = 0.5))+
  scale_x_discrete(labels=c('LDT All','LDT Real', 'LDT Pseudo', 'WJ Word ID', 'TOWRE SWE', 'WJ WA', 'TOWRE PDE', 'WASI Vocab', 'WASI Matrix'))+
  scale_y_discrete(breaks = waiver(),labels=c('LDT All','LDT Real', 'LDT Pseudo', 'WJ Word ID', 'TOWRE SWE', 'WJ WA', 'TOWRE PDE', 'WASI Vocab', 'WASI Matrix'))+
  
  coord_fixed()
# Print the heatmap
ggsave('CorrelationMatrix.pdf',c1 ,width=5.5, height=4.5)
ggsave('CorrelationMatrix.tiff',c1,width=5.5, height=4.5,dpi=400)

g = plot_grid(p1,c1,nrow=1,rel_widths = c(3.3, 3))
ggsave('Figure1_PercentCorrect_CorrMat.pdf',g,width=7.5, height=3.5)
ggsave('Figure1_PercentCorrect_CorrMat.png',g,width=7.5, height=3.5,dpi=400)
ggsave('Figure1_PercentCorrect_CorrMat.tiff',g,width=7.5, height=3.5,dpi=400)


## Look at specific correlations
print(cor(select(filter(sub.data, outlier==FALSE),wj_lwid_raw,twre_swe_raw)))
sc <- cor(select(filter(sub.data, outlier==FALSE),wj_lwid_raw,twre_swe_raw,wj_wa_raw,twre_pde_raw))
summary(sc[lower.tri(sc)])
print(sc[lower.tri(sc)])

# WASI measures
sc <- cor(select(filter(sub.data, outlier==FALSE),pcor_all,wasi_vocab_raw,wasi_mr_raw), use="pairwise.complete.obs" )

# Word vs pseudoword
sc <- cor(select(filter(sub.data, outlier==FALSE),pcor_real,pcor_pseudo,wj_lwid_raw))
nwj = sum(!is.na(filter(sub.data, outlier==FALSE)$wj_lwid_raw))
r.test(n=nwj, r12=cor(select(filter(sub.data, outlier==FALSE),wj_lwid_raw, pcor_real)),
       r13=cor(select(filter(sub.data, outlier==FALSE), wj_lwid_raw, pcor_pseudo)),
       r23=cor(select(filter(sub.data, outlier==FALSE), pcor_real, pcor_pseudo)))


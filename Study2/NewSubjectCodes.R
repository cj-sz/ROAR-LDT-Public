library(dplyr)

# Read data
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public")

dfv1 <- read.csv('data_allsubs/LDT_alldata_wide.csv')
dfv2 <- read.csv('Study2/data/LDT_alldata_wide_v2.csv')
dfv1$subj <- dfv1$subj
dfv2$subj <- dfv2$subj

df <- rename(select(full_join(dfv1,dfv2),'subj'),record_id=subj)
df$subj <-1:dim(df)[1]
# Split the V1 and V2 lookup tables
df.1 <- df[1:dim(dfv1)[1],]
df.2 <- df[seq(dim(dfv1)[1]+1,dim(dfv1)[1]+dim(dfv2)[1]),]
# read in metadata and demographics
metadata.1 <- read.csv('data_allsubs/metadata_all.csv')
demographic.1 <- read.csv('data_allsubs/demographic_all.csv')
metadata.2 <- read.csv('Study2/data/metadata_all_roundeddates.csv')
demographic.2 <- read.csv('Study2/data/demographic_all.csv')
# Swap in new subj column
metadata.1 <- select(left_join(df.1,metadata.1),-record_id)
demographic.1 <- select(left_join(df.1,demographic.1),-record_id)
metadata.2 <- select(left_join(df.2,metadata.2),-record_id)
demographic.2 <- select(left_join(df.2,demographic.2),-record_id)
write.csv(metadata.1,'data_allsubs/metadata_all_newcodes.csv',row.names=FALSE)
write.csv(demographic.1, 'data_allsubs/demographic_all_newcodes.csv',row.names=FALSE)
write.csv(metadata.2,'Study2/data/metadata_all_roundeddates_newcodes.csv',row.names=FALSE)
write.csv(demographic.2, 'Study2/data/demographic_all_newcodes.csv',row.names=FALSE)

# rename subj columns
df.1 <- rename(df.1,subj=record_id,record_id=subj)
df.2 <- rename(df.2,subj=record_id,record_id=subj)

dfv1 <- rename(select(left_join(dfv1,df.1),-subj),subj=record_id)
dfv2<- rename(select(left_join(dfv2,df.2),-subj),subj=record_id)
write.csv(dfv1,'data_allsubs/LDT_alldata_wide_newcodes.csv',row.names=FALSE)
write.csv(dfv2,'Study2/data/LDT_alldata_wide_v2_newcodes.csv',row.names=FALSE)

# Loop over other v1 files
fnames = c('LDT_alldata_long',
           'LDT_alldata_wide_sorted',
           'LDT_alldata_wide_sorted_nooutliers',
           'LDT_alldata_wide_subset',
           'LDT_data_all_long',
           'LDT_rt_wide_subset',
           'LDT_rtdata_wide',
           'LDT_summarymeasures_wide',
           'LDT_summarymeasures_wide_RT')

for (ii in 1:length(fnames)){
  d <- read.csv(sprintf('data_allsubs/%s.csv',fnames[ii]))
  d <- left_join(d,df.1)
  d <- rename(select(d,-subj), subj=record_id)
  #print(head(d))
  write.csv(d,sprintf('data_allsubs/%s_newcodes.csv',fnames[ii]),row.names=FALSE)
  }
  
# Loop over other v2 files
fnames = c('LDT_alldata_long_v2', 'LDT_rtdata_wide_v2',
           'LDT_alldata_wide_sorted_v2',
           'LDT_alldata_wide_subset_v2',	'LDT_summarymeasures_wide_v2')

for (ii in 1:length(fnames)){
  d <- read.csv(sprintf('Study2/data/%s.csv',fnames[ii]))
  d <- left_join(d,df.2)
  d <- rename(select(d,-subj), subj=record_id)
  #print(head(d))
  write.csv(d,sprintf('Study2/data/%s_newcodes.csv',fnames[ii]), row.names=FALSE)
}

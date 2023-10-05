library(plyr) 
library(readr)
library(stringr)
library(dplyr)
library(data.table)

# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/ntr")

forR_input <- read_csv("forR_input.csv",col_types = cols(.default = "c")) #CRITICAL: read all columns as characters, or "t"'s may be read as TRUE!
forR_input <- mutate_all(forR_input,.funs=tolower)

#if there are non-linear mappings for E to schwa, as in ABLE (the written order is L-E but the phonological order is E-L)
myindices <- which(forR_input == "ə+_",arr.ind=T) #find where there are non-linear Es (as in "able")
for (i in 1:nrow(myindices)) {
  forR_input[myindices[i,1],myindices[i,2]-1] <- "_" #replace the blanks, which come from Excel, with an underscore, just as a placeholder...note that these need to go in the column to the left of the "ə+_" mappings
}

#if there are non-linear mappings for E to ʌ (as in ONE, where O --> /w/, N--> /n/, and E --> ʌ but out of order):
myindices <- which(forR_input == "ʌ+_",arr.ind=T) #find where there are non-linear Es (as in "once")
for (i in 1:nrow(myindices)) {
  forR_input[myindices[i,1],myindices[i,2]-1] <- "_" #replace the blanks, which come from Excel, with an underscore, just as a placeholder...note that these need to go in the column to the left of the "ə+_" mappings
}

#if there are non-linear mappings for E to rhotic (as in THEATRE):
myindices <- which(forR_input == "ɚ+_",arr.ind=T) #find where there are non-linear Es (as in "acquire")
for (i in 1:nrow(myindices)) {
  forR_input[myindices[i,1],myindices[i,2]-1] <- "_" #replace the blanks, which come from Excel, with an underscore, just as a placeholder...note that these need to go in the column to the left of the "ə+_" mappings
}

#note: the syllables are mapped in these columns in the CSV:#
#first syllable: [,c(2:19)]
#second syllable: [,c(20:37)]
#third syllable: [,c(38:52)]
#fourth syllable: [,c(53:64)]
#fifth syllable: [,c(65:76)]
#sixth syllable: [,c(77:88)]



##############################
####WORD-INITIAL MEASURES#####
##############################

wordinitial_list <- list()
wordinitial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

##word-initial for loop:
for(i in 1:length(forR_input$STRING)){
  wordinitial_temp <- t(matrix(cbind(colnames(forR_input)[c(2:19)][!is.na(forR_input[i,c(2:19)])],forR_input[i,c(2:19)][!is.na(forR_input[i,c(2:19)])])[1:3,2]))
  wordinitial_list[[length(wordinitial_list)+1]] <- wordinitial_temp
}


##word initial data frame:
wordinitial_df <- do.call(rbind,wordinitial_list)
colnames(wordinitial_df) <- wordinitial_df[1,]
wordinitial_df <- as.data.frame(wordinitial_df[-1,])
wordinitial_df$G <-str_extract(wordinitial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme
  
##word initial P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
wordinitial_PG_prop <- prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=1)

##word initial G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display)
wordinitial_GP_prop <- t(prop.table(table(wordinitial_df$P,wordinitial_df$G),margin=2))

##word initial FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
wordinitial_FREQ <- t(table(wordinitial_df$P,wordinitial_df$G))

#example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
wordinitial_PG_prop[rownames(wordinitial_PG_prop)=="i",][wordinitial_PG_prop[rownames(wordinitial_PG_prop)=="i",]>0]

#example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
wordinitial_GP_prop[rownames(wordinitial_GP_prop)=="c",][wordinitial_GP_prop[rownames(wordinitial_GP_prop)=="c",]>0]








##############################
####SYLLABLE-INITIAL MEASURES#####
##############################

syllableinitial_list <- list()
syllableinitial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

##syllable-initial for loop for the SECOND through SIXTH syllables only!:
##special note: must exclude WORD-FINAL mappings, they will be treated as word final even if they are also syllable-initial (e.g, the Y in joe-Y)

#create a new corpus where you turn into NA the final mappings from each word so they'll end up being excluded from the syllable-initial lists:
newcorpus <- forR_input
for( i in 1:nrow(newcorpus)){
  mycols <-max.col(!is.na(newcorpus[i,]),ties.method="last")  #figure out which columns will be the final mappings
  newcorpus[i,c((mycols-2):mycols)] <- "NA"
}

for(i in 1:length(newcorpus$STRING)){
  
  #second syllable
  syllableinitial_temp <- t(matrix(tryCatch(
                                  cbind(colnames(newcorpus)[c(20:37)][!is.na(newcorpus[i,c(20:37)])],newcorpus[i,c(20:37)][!is.na(newcorpus[i,c(20:37)])])[1:3,2],
                                  error=function(err) NA)))
  syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
  
  #third syllable
  syllableinitial_temp <- t(matrix(tryCatch(
                                  cbind(colnames(newcorpus)[c(38:52)][!is.na(newcorpus[i,c(38:52)])],newcorpus[i,c(38:52)][!is.na(newcorpus[i,c(38:52)])])[1:3,2],
                                  error=function(err) NA)))
  syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
  
  #fourth syllable
  syllableinitial_temp <- t(matrix(tryCatch(
                                  cbind(colnames(newcorpus)[c(53:64)][!is.na(newcorpus[i,c(53:64)])],newcorpus[i,c(53:64)][!is.na(newcorpus[i,c(53:64)])])[1:3,2],
                                  error=function(err) NA)))
  syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
  
  #fifth syllable
  syllableinitial_temp <- t(matrix(tryCatch(
                                  cbind(colnames(newcorpus)[c(65:76)][!is.na(newcorpus[i,c(65:76)])],newcorpus[i,c(65:76)][!is.na(newcorpus[i,c(65:76)])])[1:3,2],
                                  error=function(err) NA)))
  syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
  
  #sixth syllable
  syllableinitial_temp <- t(matrix(tryCatch(
                                  cbind(colnames(newcorpus)[c(77:88)][!is.na(newcorpus[i,c(77:88)])],newcorpus[i,c(77:88)][!is.na(newcorpus[i,c(77:88)])])[1:3,2],
                                  error=function(err) NA)))
  syllableinitial_list[[length(syllableinitial_list)+1]] <- syllableinitial_temp
  
}

#replace empty matrices with NAs of 1x3 so that they can be bound with the non-empty matrices
for(i in 1:length(syllableinitial_list)){
  ifelse(all(is.na(syllableinitial_list[[i]])),syllableinitial_list[[i]] <- matrix(NA,nrow=1,ncol=3),syllableinitial_list[[i]]<-syllableinitial_list[[i]])
}

##syllable initial data frame:
syllableinitial_df <- do.call(rbind,syllableinitial_list)
colnames(syllableinitial_df) <- syllableinitial_df[1,]
syllableinitial_df <- as.data.frame(syllableinitial_df[-1,])
syllableinitial_df$G <-str_extract(syllableinitial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

##syllable initial P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
syllableinitial_PG_prop <- prop.table(table(syllableinitial_df$P,syllableinitial_df$G),margin=1)

##syllable initial G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
syllableinitial_GP_prop <- t(prop.table(table(syllableinitial_df$P,syllableinitial_df$G),margin=2))

##syllable initial FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
syllableinitial_FREQ <- t(table(syllableinitial_df$P,syllableinitial_df$G))

#example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)=="i",][syllableinitial_PG_prop[rownames(syllableinitial_PG_prop)=="i",]>0]

#example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)=="c",][syllableinitial_GP_prop[rownames(syllableinitial_GP_prop)=="c",]>0]









##############################
#####WORD- and SYLLABLE-FINAL MEASURES##
##############################
wordfinal_list <- list()
wordfinal_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

syllablefinal_list <- list()
syllablefinal_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

##syllable-final for loop, begins with 1st syllable, appending mappings...then 2nd syllable, etc. before going on to the ith+1 word
for(i in 1:length(forR_input$STRING)){ 

#first syllable
syllable_length1 <- length(cbind(colnames(forR_input)[c(2:19)][!is.na(forR_input[i,c(2:19)])],
                                  forR_input[i,c(2:19)][!is.na(forR_input[i,c(2:19)])])[-c(1:3),2])
#second syllable
syllable_length2 <- length(cbind(colnames(forR_input)[c(20:37)][!is.na(forR_input[i,c(20:37)])],
                                 forR_input[i,c(20:37)][!is.na(forR_input[i,c(20:37)])])[,2])
#third syllable
syllable_length3 <- length(cbind(colnames(forR_input)[c(38:52)][!is.na(forR_input[i,c(38:52)])],
                                 forR_input[i,c(38:52)][!is.na(forR_input[i,c(38:52)])])[,2])
#fourth syllable
syllable_length4 <- length(cbind(colnames(forR_input)[c(53:64)][!is.na(forR_input[i,c(53:64)])],
                                 forR_input[i,c(53:64)][!is.na(forR_input[i,c(53:64)])])[,2])
#fifth syllable
syllable_length5 <- length(cbind(colnames(forR_input)[c(65:76)][!is.na(forR_input[i,c(65:76)])],
                                 forR_input[i,c(65:76)][!is.na(forR_input[i,c(65:76)])])[,2])
#sixth syllable
syllable_length6 <- length(cbind(colnames(forR_input)[c(77:88)][!is.na(forR_input[i,c(77:88)])],
                                 forR_input[i,c(77:88)][!is.na(forR_input[i,c(77:88)])])[,2])

holdword<-rbind(
cbind(colnames(forR_input)[c(2:19)][!is.na(forR_input[i,c(2:19)])],
                                                            forR_input[i,c(2:19)][!is.na(forR_input[i,c(2:19)])])[-c(1:3),2][(syllable_length1-2):syllable_length1],
cbind(colnames(forR_input)[c(20:37)][!is.na(forR_input[i,c(20:37)])],
                                                           forR_input[i,c(20:37)][!is.na(forR_input[i,c(20:37)])])[,2][(syllable_length2-2):syllable_length2],
cbind(colnames(forR_input)[c(38:52)][!is.na(forR_input[i,c(38:52)])],
                                                           forR_input[i,c(38:52)][!is.na(forR_input[i,c(38:52)])])[,2][(syllable_length3-2):syllable_length3],
cbind(colnames(forR_input)[c(53:64)][!is.na(forR_input[i,c(53:64)])],
                                                           forR_input[i,c(53:64)][!is.na(forR_input[i,c(53:64)])])[,2][(syllable_length4-2):syllable_length4],
cbind(colnames(forR_input)[c(65:76)][!is.na(forR_input[i,c(65:76)])],
                                                           forR_input[i,c(65:76)][!is.na(forR_input[i,c(65:76)])])[,2][(syllable_length5-2):syllable_length5],
cbind(colnames(forR_input)[c(77:88)][!is.na(forR_input[i,c(77:88)])],
                                                           forR_input[i,c(77:88)][!is.na(forR_input[i,c(77:88)])])[,2][(syllable_length6-2):syllable_length6]
)

syllablefinal_list[[length(syllablefinal_list)+1]] <-   as.data.frame(head(holdword,-1)) #pull out non-word-final
wordfinal_list[[length(wordfinal_list)+1]] <-   as.data.frame(tail(holdword,1)) #pull out just the word-final
                                                            
}


##syllable final data frame:
syllablefinal_df <- do.call(rbind,syllablefinal_list)
colnames(syllablefinal_df) <- syllablefinal_df[1,]
syllablefinal_df <- as.data.frame(syllablefinal_df[-1,])
syllablefinal_df$G <-str_extract(syllablefinal_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

##syllable final P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
syllablefinal_PG_prop <- prop.table(table(syllablefinal_df$P,syllablefinal_df$G),margin=1)

##syllable final G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
syllablefinal_GP_prop <- t(prop.table(table(syllablefinal_df$P,syllablefinal_df$G),margin=2))

##syllable final FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
syllablefinal_FREQ <- t(table(syllablefinal_df$P,syllablefinal_df$G))

#example of pulling out one P->G mapping odds (just replace the "k" with the IPA symbol for the desired sound#
syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)=="k",][syllablefinal_PG_prop[rownames(syllablefinal_PG_prop)=="k",]>0]

#example of pulling out one G->P mapping odds (just replace the "k" with the desired graphemes#
syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)=="c",][syllablefinal_GP_prop[rownames(syllablefinal_GP_prop)=="c",]>0]



##word final data frame:
wordfinal_df <- do.call(rbind,wordfinal_list)
colnames(wordfinal_df) <- wordfinal_df[1,]
wordfinal_df <- as.data.frame(wordfinal_df[-1,])
wordfinal_df$G <-str_extract(wordfinal_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

##word final P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
wordfinal_PG_prop <- prop.table(table(wordfinal_df$P,wordfinal_df$G),margin=1)

##word final G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
wordfinal_GP_prop <- t(prop.table(table(wordfinal_df$P,wordfinal_df$G),margin=2))

##word final FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
wordfinal_FREQ <- t(table(wordfinal_df$P,wordfinal_df$G))

#example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
wordfinal_PG_prop[rownames(wordfinal_PG_prop)=="i",][wordfinal_PG_prop[rownames(wordfinal_PG_prop)=="i",]>0]

#example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
wordfinal_GP_prop[rownames(wordfinal_GP_prop)=="c",][wordfinal_GP_prop[rownames(wordfinal_GP_prop)=="c",]>0]




##############################
#####SYLLABLE-MEDIAL MEASURES#
##############################
syllablemedial_list <- list()
syllablemedial_list[[1]] <- matrix(c("P","G","totalPG"),ncol=3)

for(i in 1:length(forR_input$STRING)){ 
#first syllable
syllable_length <- length(cbind(colnames(forR_input)[c(2:19)][!is.na(forR_input[i,c(2:19)])],
                                forR_input[i,c(2:19)][!is.na(forR_input[i,c(2:19)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)


medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(2:19)][!is.na(forR_input[i,c(2:19)])],forR_input[i,c(2:19)][!is.na(forR_input[i,c(2:19)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))


syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp

#second syllable
syllable_length <- length(cbind(colnames(forR_input)[c(20:37)][!is.na(forR_input[i,c(20:37)])],
                                forR_input[i,c(20:37)][!is.na(forR_input[i,c(20:37)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)

medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(20:37)][!is.na(forR_input[i,c(20:37)])],forR_input[i,c(20:37)][!is.na(forR_input[i,c(20:37)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))


syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp

#third syllable
syllable_length <- length(cbind(colnames(forR_input)[c(38:52)][!is.na(forR_input[i,c(38:52)])],
                                forR_input[i,c(38:52)][!is.na(forR_input[i,c(38:52)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)

medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(38:52)][!is.na(forR_input[i,c(38:52)])],forR_input[i,c(38:52)][!is.na(forR_input[i,c(38:52)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))

syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp

#fourth syllable
syllable_length <- length(cbind(colnames(forR_input)[c(53:64)][!is.na(forR_input[i,c(53:64)])],
                                forR_input[i,c(53:64)][!is.na(forR_input[i,c(53:64)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)

medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(53:64)][!is.na(forR_input[i,c(53:64)])],forR_input[i,c(53:64)][!is.na(forR_input[i,c(53:64)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))

syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp

#fifth syllable
syllable_length <- length(cbind(colnames(forR_input)[c(65:76)][!is.na(forR_input[i,c(65:76)])],
                                forR_input[i,c(65:76)][!is.na(forR_input[i,c(65:76)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)

medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(65:76)][!is.na(forR_input[i,c(65:76)])],forR_input[i,c(65:76)][!is.na(forR_input[i,c(65:76)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))

syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp

#sixth syllable
syllable_length <- length(cbind(colnames(forR_input)[c(77:88)][!is.na(forR_input[i,c(77:88)])],
                                forR_input[i,c(77:88)][!is.na(forR_input[i,c(77:88)])])[-c(1:3),2])
myncol <- ifelse(syllable_length <=6, 1, (syllable_length-3)/3)

medial_temp<- t(matrix(tryCatch(
  cbind(colnames(forR_input)[c(77:88)][!is.na(forR_input[i,c(77:88)])],forR_input[i,c(77:88)][!is.na(forR_input[i,c(77:88)])])[-c(1:3),2][((syllable_length-3)%%1):(syllable_length-3)],
  error=function(err) NA),ncol=myncol))

syllablemedial_list[[(length(syllablemedial_list)+1)]] <- medial_temp
}

#replace empty matrices with NAs of 1x3 so that they can be bound with the non-empty matrices
for(i in 1:length(syllablemedial_list)){
  ifelse(all(is.na(syllablemedial_list[[i]])),syllablemedial_list[[i]] <- matrix(NA,nrow=1,ncol=3),syllablemedial_list[[i]]<-syllablemedial_list[[i]])
}

##syllable medial data frame:
syllablemedial_df <- do.call(rbind,syllablemedial_list)
colnames(syllablemedial_df) <- syllablemedial_df[1,]
syllablemedial_df <- as.data.frame(syllablemedial_df[-1,])
syllablemedial_df$G <-str_extract(syllablemedial_df$totalPG, '\\b\\w+$') #CRITICAL! this is needed to include the silent E's as part of the grapheme

##syllable medial P->G proportion table (note: will have many zero cells, but that can be removed later for ease of display)
syllablemedial_PG_prop <- prop.table(table(syllablemedial_df$P,syllablemedial_df$G),margin=1)

##syllable medial G->P proportion table (note: will have many zero cells, but that can be removed later for ease of display
syllablemedial_GP_prop <- t(prop.table(table(syllablemedial_df$P,syllablemedial_df$G),margin=2))

##syllable medial FREQ table (note: will have many zero cells, but that can be removed later for ease of display)
syllablemedial_FREQ <- t(table(syllablemedial_df$P,syllablemedial_df$G))

#example of pulling out one P->G mapping odds (just replace the "i" with the IPA symbol for the desired sound#
syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)=="i",][syllablemedial_PG_prop[rownames(syllablemedial_PG_prop)=="i",]>0]

#example of pulling out one G->P mapping odds (just replace the "c" with the desired graphemes#
syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)=="c",][syllablemedial_GP_prop[rownames(syllablemedial_GP_prop)=="c",]>0]






####### TO IDENTIFY WORDS CAUSING A PROBLEM BECAUSE THEY WERE MIS-CODED #######
##e.g., if a two-syllable word was mis-coded as having a first and a THIRD syllable, this will lead to errors##

what<-matrix(NA)
for (i in 1:length(syllablemedial_list)){what[i]<-(ncol(syllablemedial_list[[i]]))}
summary(what) #this should come back as all 3's, if not, something is "off"!
which.max(what)


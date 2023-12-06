###TO PROCESS WORDS
# DESKTOP
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public/ntr/automated_toolkit")
#first: load the pg mapping libraries:
require(readxl)

# Different categorizations of words.
word_initial_mappings <- read_excel("word initial mappings.xlsx")
word_final_mappings <- read_excel("word final mappings.xlsx")
syllable_medial_mappings <- read_excel("syllable medial mappings.xlsx")
syllable_final_mappings <- read_excel("syllable final mappings.xlsx")
syllable_initial_mappings <- read_excel("syllable initial mappings.xlsx")

#second: load word list
library(readr)

# List of (currently) ~10700 words
wordlist_v1_0 <- read_csv("wordlist_v1_0.csv")

# third: load functions

# TODO LATER: Explore this algorithm. Also interested in number of words that
# correspond to the various ambiguities present.
# Maps phoneme and grapheme data for a given spelling and pronunciation
map_PG <- function(spelling,pronunciation,map_progress=FALSE){
  
  #required packages
  require(stringr)
  require(stringi)
  
  #list vowels and consonants per in-house code
  vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^")
  
  consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                  "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                  "r", "s", "S", "t", "v", "w", "z")
  
  ####################################
  #write internal functions
  ####################################
  
  #prepare word for processing function
  prep_word <- function(word_index){
    
    #get parsed syllables
    hold0 <- as.data.frame(as.vector(sapply(pronunciation[word_index],parse_syllables))) #hold on to the list of syllables
    hold0 <- hold0[hold0!="",] #this will be able to identify if its a word like /ko ko/ or /ha ha/ that repeats the same syllable twice
    parsed_syll <- matrix(strsplit(hold0,"   ")[[1]])
    
    #get letter string, make sure its all lower case
    letter_str <- tolower(spelling[word_index])
    
    #replace empty syllable cells with NA, but don't overwrite this...so store it separately
    syll_count <- parsed_syll 
    tryCatch({syll_count[syll_count=="",] <- NA}, #tryCatch needed because this fails on monosyllabic
             error=function(e) {syll_count <<- 1})
    
    #get syllable count
    syll_count <- length(na.omit(syll_count))
    
    ##create 12345 structure to capture phoneme position w/in syllables (1 = word-initial, 2 = syllable-initial, 3 = medial, 4 = syllable-final, 5 = word-final)
    syll_struc <- parsed_syll
    
    #make all word-initial phonemes 1:
    tryCatch({syll_struc[1,] <- sub('.','1',syll_struc[1,])},
             error=function(e) {syll_struc <<- sub('.','1',syll_struc)} )  
    
    #make all syll-initial phonemes 2:
    for (i in 2:10){
      tryCatch(syll_struc[i,] <- {sub('.','2',syll_struc[i,])},
               error=function(e) NA)}
    
    #make all syll-medial phonemes 3:
    my.index <- matrix(NA,nrow=10) #get # of phonemes per syllable...
    for (i in 1:10){
      tryCatch({my.index[i] <- nchar(syll_struc[i,])},
               error=function(e) NA)} #will replace from 2nd to (length-1)th with 3's...
    
    #to get the index for monosyllabic words:
    my.index[1,] <- nchar(syll_struc[1]) 
    
    for (i in 1:10){tryCatch({
      substring(syll_struc[i,],2,my.index[i]-1) <- paste(rep("3",my.index[i]-2),collapse="")},
      error=function(e) NA)}
    
    #to do it for monosyllabic words:
    if(max(nchar(syll_struc)==1 & syll_count==1)) { 
      syll_struc <- 1
    } else {
      ifelse(syll_count==1, substring(syll_struc[1],2,my.index[1,]-1) <- paste(rep("3",my.index[1,]-2),collapse=""), syll_struc<-syll_struc)
    }
    
    #make all syll-final phonemes 4:
    for (i in 1:10){ #update my.index so that it only has the number of final char, and only if those are not also the first char (i.e., the maximum from 
      #previous my.index, excluding when that max is 1)
      ifelse (my.index[i] > 1, my.index[i] <- my.index[i], my.index[i] <- 0)}
    for (i in 1:10){tryCatch({
      substring(syll_struc[i,],my.index[i],my.index[i]) <- "4"},
      error=function(e) NA)}
    
    #make all word-final phonemes 5
    #this is easiest by IDing the last 4 and making it a 5...because that will avoide the monophonemic issue
    #e.g., EYE has just a 1, word-initial, per the current Toolkit schema...this won't overwrite that
    #syll_count has already IDed the last syllable, so:
    monophonemic <- FALSE
    ifelse (nchar(syll_struc[1])==1 & syll_count==1,monophonemic <- TRUE,monophonemic <- FALSE)
    ifelse (monophonemic==FALSE & syll_count >1, str_sub(syll_struc[syll_count,],-1,-1) <- "5",syll_struc<-syll_struc)
    #finally, get monosyllabic finished:
    ifelse (monophonemic==FALSE & syll_count ==1, str_sub(syll_struc[syll_count],-1,-1) <- "5",syll_struc<-syll_struc)
    
    #these need to be set to FALSE before trying to map a new word!
    map_j <- FALSE
    map_j_wi <- FALSE
    
    #make syll_struc just a string as well as the other strings
    #the 0 indicates nothing has been mapped yet
    syll_struc0 <- paste(syll_struc,collapse="")
    parsed_syll0 <- paste(parsed_syll,collapse="")
    letter_str0 <- letter_str
    
    return(matrix(c(syll_struc0,parsed_syll0,letter_str0)))
  }
  
  #Maximum Onset Principle phonological parse function
  parse_syllables <- function(pronunciation,word_index) {
    library(stringr)
    
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
                "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^")
    
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                    "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                    "r", "s", "S", "t", "v", "w", "z")
    
    syll_init_cons <- c("spl", "spr", "str", "skr", "skw", "pl", "pr", "G", "tr", "C", "tw", "kl", "kr", "kw", "bl", "br", "dr", "dw", "gl", "gr", "fl", "fr", "Tr", "Sr", "sl", "st", "sp", "sk", "sm", "sn", "sf", "bj", "fj", "vj","mj","kj","hj","nj","pj","tj","Cj","dj","Tj","Gj","gj","Sj","sj","gw")
    
    all_phonemes <- c(vowels, syll_init_cons, consonants)
    
    phonemes <- c()
    remaining <- pronunciation[word_index]
    vowel_count <- 0
    
    while(nchar(remaining) > 0) {
      matched <- FALSE
      
      if (length(phonemes) == 0 || (length(phonemes) > 0 && phonemes[length(phonemes)] %in% vowels)) {
        for (p in syll_init_cons) {
          if (startsWith(remaining, p)) {
            phonemes <- c(phonemes, p)
            remaining <- substring(remaining, nchar(p) + 1)
            matched <- TRUE
            break
          }
        }
      }
      
      if (!matched) {
        for (p in all_phonemes) {
          if (startsWith(remaining, p)) {
            phonemes <- c(phonemes, p)
            remaining <- substring(remaining, nchar(p) + 1)
            matched <- TRUE
            if (p %in% vowels) {
              vowel_count <- vowel_count + 1
            }
            break
          }
        }
      }
      
      if (!matched) {
        break
      }
    }
    
    pattern <- paste(ifelse(phonemes %in% vowels, "V", "C"), collapse = "")
    
    if (vowel_count < 2) {
      return(pronunciation[word_index])
    }
    
    rules <- list(
      "CCCVCC" = 5,
      "CCVCC" = 4,
      "CVCCC" = 4,
      "CCCVC" = 4,
      "CVCC" = 3,
      "CCVC" = 3,
      "CVC" = 2,
      "CVV" = 2,
      "VCC" = 2,
      "VCV" = 1,
      "VV" = 1
    )
    
    split_index <- 0
    
    for (rule in names(rules)) {
      if (startsWith(pattern, rule)) {
        split_index <- rules[[rule]]
        break
      }
    }
    
    if (split_index >= 1) {
      left <- paste(phonemes[1:split_index], collapse = "")
      right <- paste(phonemes[(split_index + 1):length(phonemes)], collapse = "")
      
      first_three_right <- substr(right, 1, 3)
      first_two_right <- substr(right, 1, 2)
      first_one_right <- substr(right, 1, 1)
      
      special_case <- FALSE
      
      if (first_one_right == "N") {
        left <- paste0(left, first_one_right)
        right <- substr(right, 2, nchar(right))
        special_case <- TRUE
      }
      
      if (!special_case) {
        if (all(substr(first_two_right, 1, 1) %in% consonants, 
                substr(first_two_right, 2, 2) %in% consonants)) {
          
          if (!(first_two_right %in% syll_init_cons)) {
            left <- paste0(left, substr(right, 1, 1))
            right <- substr(right, 2, nchar(right))
          }
        }
      }
      
      result <- paste(left, " ", right)
      
      if (vowel_count > 2) {
        right_split <- parse_syllables(right)
        result <- paste(left, " ", right_split)
      }
      
      return(result)
      
    }
    
    return(pronunciation[word_index])
  }
  
  #word-final function
  map_wf <- function(p_wf,syll_struc0,parsed_syll0,letter_str0){
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_wf must be overwritten with  p_wf_jointPRIOR
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_wf =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll0,-2,-1)=="ju")
                 p_wf<- str_sub(parsed_syll0,-2,-1)
               else
                 if(str_sub(parsed_syll0,-3,-2)=="ju")
                   p_wf<- str_sub(parsed_syll0,-3,-2),
               p_wf <- p_wf)
      },    error = function(e) NA)
    
    
    #look at the phoneme in front of the p_wf, to check if its a vowel. If it is, then keep /z/ --> ES, /s/ --> ES, and /d/ --> ED from being an option
    p_wf_prior <- str_sub(parsed_syll0,-2,-2)
    no_z_es <- FALSE
    no_d_ed <- FALSE
    no_s_es <- FALSE
    if(p_wf_prior %in% vowels & p_wf == "d" | p_wf_prior %in% vowels & p_wf == "z" | p_wf_prior %in% vowels & p_wf == "s") {
      no_z_es <- TRUE
      no_d_ed <- TRUE
      no_s_es <- TRUE
    }else {
      no_z_es <- no_z_es
      no_s_es <- no_s_es
      no_d_ed <- no_d_ed
    }
    
    #don't allow /z/ --> DS if p_wf_prior is actually a /d/
    no_z_ds <- FALSE
    if(p_wf_prior == "d" & p_wf == "z")
      no_z_ds <- TRUE
    
    #don't allow /d/ --> LD if p_wf_prior is actually a /l/
    no_d_ld <- FALSE
    if(p_wf_prior == "l" & p_wf == "d")
      no_d_ld <- TRUE
    
    #don't allow /t/ --> CT or --> BT if p_wf_prior is actually a /k/ or /b/
    no_t_ct <- FALSE
    no_t_bt <- FALSE
    if(p_wf_prior == "k" & p_wf == "t") {no_t_ct <- TRUE}
    if(p_wf_prior == "b" & p_wf == "t") {no_t_bt <- TRUE}
    
    #don't allow /e/[schwa] --> IA or EA if p_wf_prior is actually a /j/ or /i/, or /2/ [/oi/]
    no_e_ia <- FALSE
    no_e_ea <- FALSE
    if(p_wf_prior == "j" & p_wf == "e" | p_wf_prior == "i" & p_wf == "e" | p_wf_prior == "2" & p_wf == "e") {
      no_e_ia <- TRUE
      no_e_ea <- TRUE
    }
    
    #don't allow /k/ --> LK if p_wf_prior is actually a /l/
    no_k_lk <- FALSE
    if(p_wf_prior == "l" & p_wf == "k") {
      no_k_lk <- TRUE
    }
    
    #don't allow /f/ --> LF if p_wf_prior is actually a /l/
    no_f_lf <- FALSE
    if(p_wf_prior == "l" & p_wf == "f") {
      no_f_lf <- TRUE
    }
    
    #don't allow /m/ --> LM if p_wf_prior is actually a /l/
    no_m_lm <- FALSE
    if(p_wf_prior == "l" & p_wf == "m") {
      no_m_lm <- TRUE
    }
    
    #don't allow /s/ --> TZ if p_wf_prior is actually a /t/
    no_s_tz <- FALSE
    if(p_wf_prior == "t" & p_wf == "s") {
      no_s_tz <- TRUE
    }
    
    #don't allow /v/ --> LV if p_wf_prior is actually a /l/
    no_v_lv <- FALSE
    if(p_wf_prior == "l" & p_wf == "v") {
      no_v_lv <- TRUE
    }
    
    #look at the 2 phonemes in front of the p_wf, to check if they are "3r", like in ACRES "8k3rz". If they are, then keep /z/ --> ES and /d/ --> ED from being an option
    p_wf_2prior <- str_sub(parsed_syll0,-3,-2)
    if(p_wf_2prior == "3r"){
      no_z_es <- TRUE
      no_d_ed <- TRUE
    }
    
    #extra block of /z/ --> ES: for ONES and such, which need to map the /^/ to _e
    ifelse(p_wf_2prior == "^n" & p_wf == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    
    #look at the 2 phonemes in front of the p_wf, to check if they are "e+Consonant", like in TABLES "t8belz". If they are, then keep /z/ --> ES and /d/ --> ED from being an option
    p_wf_anteprior <- str_sub(parsed_syll0,-3,-3)
    if(p_wf_anteprior == "e" & p_wf_prior %in% consonants){
      no_z_es <- TRUE
      no_d_ed <- TRUE
    }else {
      no_z_es <- no_z_es
      no_d_ed <- no_d_ed
    }
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_wf_jointprior <- str_sub(parsed_syll0,-2,-1)
    wf_x <- str_sub(letter_str0,-1,-1)
    map_x = FALSE
    ifelse (wf_x=="x", ifelse(p_wf_jointprior=="ks",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    wf_xe <- str_sub(letter_str0,-2,-1)
    map_xe = FALSE
    ifelse (wf_xe=="xe", ifelse(p_wf_jointprior=="ks",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #find the options:
    g_opt <- word_final_mappings[word_final_mappings$phoneme==p_wf,]
    
    #remove the following as needed
    ifelse(p_wf_prior == "p" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme=="pt",], g_opt <- g_opt)
    ifelse(p_wf_prior == "b" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme=="bt",], g_opt <- g_opt)
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_s_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_z_ds == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ds",], g_opt <- g_opt)
    ifelse(no_d_ed == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    ifelse(no_d_ld == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ld",], g_opt <- g_opt)
    ifelse(no_e_ia == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    ifelse(no_e_ea == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ea",], g_opt <- g_opt)
    ifelse(no_t_ct == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ct",], g_opt <- g_opt)
    ifelse(no_t_bt == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="bt",], g_opt <- g_opt)
    ifelse(no_k_lk == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lk",], g_opt <- g_opt)
    ifelse(no_f_lf == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lf",], g_opt <- g_opt)
    ifelse(no_m_lm == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lm",], g_opt <- g_opt)
    ifelse(no_s_tz == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="tz",], g_opt <- g_opt)
    ifelse(no_v_lv == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lv",], g_opt <- g_opt)
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- word_final_mappings[word_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- word_final_mappings[word_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str0))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str0)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str0)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_wf <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_wf <- g_opt$grapheme[g_wf]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_wf <- "FAILED", g_wf <- g_wf)
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str0),stri_reverse(g_wf),"_"))
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, the X scenario must trigger removing /ks/
    parsed_syll.latest <- str_sub(parsed_syll0,1,-(nchar(p_wf)+1))
    
    #to handle /ks/ for X...the flag map_x should trigger 1 more phoneme to be removed
    ifelse (map_x, parsed_syll.latest <- str_sub(parsed_syll0,1,-3), parsed_syll.latest <- parsed_syll.latest)
    
    #to handle /ks/ for XE...the flag map_xe should trigger 1 more phoneme to be removed, 
    ifelse (map_xe, parsed_syll.latest <- str_sub(parsed_syll0,1,-3), parsed_syll.latest <- parsed_syll.latest)
    
    #update p.wf to correctly reflect that the /k/ was added for the X, i.e., make it /ks/ not just /s/
    ifelse (map_x, p_wf <- p_wf_jointprior, p_wf <- p_wf)
    ifelse (map_xe, p_wf <- p_wf_jointprior, p_wf <- p_wf)
    
    
    #finally, have the latest syll_struc updated...if the X flag is set to remove an additional final syllable
    syll_struc.latest <- str_sub(syll_struc0,1,-(nchar(p_wf)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_wf,g_wf,"5",syll_struc0,syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
  }
  
  #syllable-medial function
  map_m <- function(p_m,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_m must be overwritten with the p_m_jointPRIOR
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_m =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="ju")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="ju")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "je" instead of "ju"
    if(p_m =="e")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="je")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="je")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "jU" instead of "ju"
    if(p_m =="U")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="jU")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="jU")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "j3" instead of "j3"
    if(p_m =="3")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="j3")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="j3")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #look at the phoneme in front of the p_m, to check if its a vowel. If it is, then keep /z/ --> ES from being an option
    #if its /c/ or /3/ and current p_m is /r/, don't allow /r/ --> OR ... or /d/ --> ED
    p_m_prior <- str_sub(parsed_syll.latest,-2,-2)
    no_z_es <- FALSE
    no_r_or <- FALSE
    no_d_ed <- FALSE
    no_s_tz <- FALSE
    ifelse(p_m_prior %in% vowels & p_m == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_m_prior %in% vowels & p_m == "d", no_d_ed <- TRUE, no_z_es <- no_d_ed)
    ifelse(p_m_prior == "c" & p_m == "r" | p_m_prior == "3" & p_m == "r", no_r_or <- TRUE, no_r_or <- no_r_or)
    ifelse(p_m_prior == "t" & p_m == "s" , no_s_tz <- TRUE, no_s_tz <- no_s_tz)
    
    #don't allow /e/[schwa] --> IA or EA or IE if p_wf_prior is actually a /j/ or /i/
    no_e_ia <- FALSE
    no_e_ea <- FALSE
    no_e_ie <- FALSE
    if(p_m_prior == "j" & p_m == "e" | p_m_prior == "i" & p_m == "e") {
      no_e_ia <- TRUE
      no_e_ea <- TRUE
      no_e_ie <- TRUE
    }
    
    #don't allow /e/[schwa] --> UI if it's after /w/ like PENGUIN
    no_e_ui <- FALSE
    if(p_m_prior == "w" & p_m == "e") { no_e_ui <- TRUE}
    
    #if its /w/ , don't allow ^ to be o_e because it needs to be just _e as in ANYONE (i.e., it will map it as ^ to O_E leaving /w/ without a mapping)
    #and don't allow /1/ to be ui because it's probably a word like PENGUIN
    no_o_e <- FALSE
    no_1_ui <- FALSE
    ifelse(p_m_prior == "w" & p_m == "^", no_o_e <- TRUE, no_o_e <- no_o_e)
    ifelse(p_m_prior == "w" & p_m == "1", no_1_ui <- TRUE, no_1_ui <- no_1_ui)
    
    #don't allow /3/ to map to HE medially unless its after /p/, because currently only SHEPHERD does th is
    no_3_he <- FALSE
    ifelse(p_m_prior != "p" & p_m == "3", no_3_he <- TRUE, no_3_he <- no_3_he)
    
    #don't allow /m/ --> LM if preceded by /l/
    no_m_lm <- FALSE
    ifelse(p_m_prior == "l" & p_m == "m", no_m_lm <- TRUE, no_m_lm <- no_m_lm)
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_m_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    m_x <- str_sub(letter_str.latest,-1,-1)
    map_x = FALSE
    ifelse (m_x=="x", ifelse(p_m_jointprior=="ks",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    m_xe <- str_sub(letter_str.latest,-2,-1)
    map_xe = FALSE
    ifelse (m_xe=="xe", ifelse(p_m_jointprior=="ks",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #check 2prior, if it's /3r/ then keep the z_es and d_ed blocks:
    p_m_2prior <- str_sub(parsed_syll.latest,-3,-2)
    if(p_m_2prior == "3r"){
      ifelse(p_m == "z",no_z_es <- TRUE, no_z_es <- no_z_es)
      ifelse(p_m == "d",no_d_ed <- TRUE, no_d_ed <- no_d_ed)
    }
    
    #find the options:
    g_opt <- syllable_medial_mappings[syllable_medial_mappings$phoneme==p_m,]
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #currently the code thinks this may be because a silent H was assigned, so this should BLOCK that from happening
    if(flag.index == my.index){
      if(p_m == "3"){g_opt <- g_opt[-which(g_opt$grapheme=="he"),]} #removes the row from g_opt that has "he" as an option for /3/
    }
    
    #or it could be there is an X coming up an so the syllabic structure is problematic at an earlier point, which happens with EXHAUST
    #to check that, look for an X and remap the whole structure and then restart
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    
    #the following lines remove things as needed
    ifelse(no_z_es==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_d_ed==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    ifelse(no_r_or==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="or",], g_opt <- g_opt)
    ifelse(no_o_e==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="o_e",], g_opt <- g_opt)
    ifelse(no_3_he==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="he",], g_opt <- g_opt)
    ifelse(no_e_ie==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ie",], g_opt <- g_opt)
    ifelse(no_e_ea==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ea",], g_opt <- g_opt)
    ifelse(no_e_ia==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    ifelse(no_1_ui==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ui",], g_opt <- g_opt)
    ifelse(no_m_lm==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lm",], g_opt <- g_opt)
    ifelse(no_e_ui==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ui",], g_opt <- g_opt)
    ifelse(no_s_tz==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="tz",], g_opt <- g_opt)
    
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme=="x",], g_opt <- g_opt)
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_m <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_m <- g_opt$grapheme[g_m]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_m <- "FAILED", g_m <- g_m)
    
    ##NOW: if it failed because it couldn't make a /j/ that should have been /ju/ or some other biphone like that, catch that now
    p_m_jointpost <- stri_sub(parsed_syll0,-my.index,-my.index+1) #combine the current p_m with the one that was most recently mapped
    map_j = FALSE
    ifelse(p_m_jointpost == "ju" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /je/ as in BINOCULAR /be na kje l3r/
    ifelse(p_m_jointpost == "je" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /jU/ as in BINOCULAR /be na kjU l3r/
    ifelse(p_m_jointpost == "jU" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /j3/ as in FAILURE /f8l j3r/
    ifelse(p_m_jointpost == "j3" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /nj/ as in FAILURE /le za nje/--NOTE its p_m_jointPRIOR
    ifelse(p_m_jointprior == "nj" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    if(map_j == TRUE) {
      get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2] #this retrieves the syll_struc that was mapped up to this point
      #CAREFUL! don't change the syllabic structure if this /j/ should be a /nj/:
      if(stri_sub(letter_str.latest,-1,-1)=="n" & p_m_prior=="n" & stri_sub(parsed_syll0,-my.index+1,-my.index+1) !="u"){ #if the letter string up to here ends N and the p_m_prior == "n", then this is probably a rare /nj/ mapping as in lasaGNa or seNor--unless its NJU like NUCLEAR
        flag.nj<<-TRUE
        get_tomap_position <- stri_sub(syll_struc0,-my.index-1,-my.index-1) #get position that the /j/ should be, which should match whatever the /n/ is in front of it
        stri_sub(syll_struc.latest,-1,-1) <- get_tomap_position #update that syllabic position
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #make the new syll_struc0
      } else {
        get_mapped_position <- stri_sub(syll_struc0,-my.index+1,-my.index+1) #get position that the /j/ should be, which should match whatever the mapped vowel was
        stri_sub(syll_struc.latest,-1,-1) <- get_mapped_position #update that syllabic position
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped)} #make the new syll_struc0
    }
    
    if (map_j == TRUE) {
      map_j <<- TRUE
      if(flag.nj==TRUE){
        flag.index_j <<- my.index #note the index will be the current one if the problem was /nj/ as opposed to /ju/ because the latter is encountered earlier (u is after the /j/, n is before it...)
      }else{
        flag.index_j <<- my.index-1} #this is critical! it must be the case that the map_j flag is only taking effect with the flag.index matches...which is one earlier than it was originally caught
      
      flag.restart <<- TRUE
    } #NOTE: earlier in the code there is a p_m_jointpost overwriting of p_m if the map_j flag was set as true the first time around, critically needed!
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_m),"_"))
    
    #if there is an _ underscore followed by something OTHER than "e", this is a FAIL! set the flag.index and restart...
    if( str_sub(letter_str.latest,-2,-2)=="_"){
      if(str_sub(letter_str.latest,-1,-1)!="e"){
        flag.restart <<- TRUE
        if (p_m=="j") { flag.index_j <<- my.index-1
        map_j <<- TRUE
        }else{
          flag.index <<- my.index-1
        }}}
    
    #and same issue but if the _ is even more out of place, i.e, if it's not the next-to-last...
    if( tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest)-1 | tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest) | tail(unlist(gregexpr('_', letter_str.latest)),1) == -1) { #if there is an _ anywhere but final or next to final...tail() is needed because there could be two __
      flag.restart <- flag.restart
    }else{
      flag.restart <<- TRUE
      if (p_m_jointprior=="nj") {
        flag.index_j <<- my.index #index will be the current one if the problem is /nj/
        map_j <<- TRUE
        flag.nj <<- TRUE
        stri_sub(syll_struc0,-flag.index_j,-flag.index_j)<-"2" #NOTE: the index just set should have the syllabic position become a 2, because the /nj/ must be that
        syll_struc0 <<- syll_struc0
      }else{
        if(p_m=="j") {
          flag.index_j <<- my.index-1 #index will be for previous pass if the problem is /ju/
          map_j <<- TRUE
        }
        flag.index <<- my.index-1
      }}
    
    #if the replacement with _ procedure has left the last 2 characters as_E,  keep it that way so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-2,-1)=="_e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #make sure there aren't TWO underscores, which will happen when there are clusters, like in ACQUIESCENCE (the N and the C cluster before the final E)
    letter_str.latest <- gsub("__","_",letter_str.latest)
    
    #update p.wf to correctly reflect that the /k/ was added for the X, i.e., make it /ks/ not just /s/
    ifelse (map_x, p_m <- p_m_jointprior, p_m <- p_m)
    ifelse (map_xe, p_m <- p_m_jointprior, p_m <- p_m)
    
    #now also update the parsed_syll to reflect what has been mapped. make sure it removes extra if there was a biphone like /ks/
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_m)+1))
    
    #finally, have the latest syll_struc updated...removes more than one if the phoneme is longer as in /ks/
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_m)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_m,g_m,"3",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #syllable-initial function
  map_si <- function(p_si,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    #if map_j is TRUE and flag.nj is set, then this should pick up that a p_si == "j" needs to become "nj"
    if(map_j == TRUE & flag.nj == TRUE){
      if(str_sub(parsed_syll.latest,-2,-1) == "nj"){
        flag.nj <<- FALSE #reset
        map_j <<- FALSE #reset
        p_si <- str_sub(parsed_syll.latest,-2,-1)
      }} #this will make p_si "nj" so long as the flag.nj was raised and there actually IS an "nj" to map in the right position
    
    #look at the phoneme in front of the p_m, to check if its a vowel. If it is, then keep /z/ --> ES from being an option
    #if its /k/ or /b/ and the current p_is /t/, then block the BT and CT options for /t/
    #and block /e/ [schwa] from being "ia" if p_si_prior is a vowel..and block /m/ --> LM if /l/ precedes
    p_si_prior <- str_sub(parsed_syll.latest,-2,-2)
    no_z_es <- FALSE
    no_t_bt <- FALSE
    no_t_ct <- FALSE
    no_e_ia <- FALSE
    no_m_lm <- FALSE
    no_m_thm <- FALSE
    no_s_tz  <- FALSE
    no_n_gn <- FALSE
    no_n_kn <- FALSE
    no_n_nn <- FALSE
    no_d_dd <- FALSE
    no_t_tt <- FALSE
    no_k_lk <- FALSE
    no_j_ill <- FALSE
    no_l_sl <- FALSE
    no_r_rr <- FALSE
    ifelse(p_si_prior %in% vowels, no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_si_prior %in% vowels & p_si == "e", no_e_ia <- TRUE, no_e_ia <- no_e_ia)
    ifelse(p_si_prior %in% vowels & p_si == "e" & stri_sub(letter_str.latest,-3,-3)=="i", no_e_ia <- FALSE, no_e_ia <- no_e_ia) #except to no /e/ [schwa] --> IA: if the letter string has ANOTHER i, as it does in Hawaiian
    ifelse(p_si_prior == "b" & p_si == "t", no_t_bt <- TRUE, no_t_bt <- no_t_bt)
    ifelse(p_si_prior == "k" & p_si == "t", no_t_ct <- TRUE, no_t_ct <- no_t_ct)
    ifelse(p_si_prior == "l" & p_si == "m", no_m_lm <- TRUE, no_m_lm <- no_m_lm)
    ifelse(p_si_prior == "T" & p_si == "m" |p_si_prior == "D" & p_si == "m", no_m_thm <- TRUE, no_m_thm <- no_m_thm)
    ifelse(p_si_prior == "t" & p_si == "s", no_s_tz <- TRUE, no_s_tz <- no_s_tz)
    ifelse(p_si_prior == "g" & p_si == "n", no_n_gn <- TRUE, no_n_gn <- no_n_gn)
    ifelse(p_si_prior == "k" & p_si == "n", no_n_kn <- TRUE, no_n_kn <- no_n_kn)
    ifelse(p_si_prior == "n" & p_si == "n", no_n_nn <- TRUE, no_n_nn <- no_n_nn)
    ifelse(p_si_prior == "d" & p_si == "d", no_d_dd <- TRUE, no_d_dd <- no_d_dd)
    ifelse(p_si_prior == "t" & p_si == "t", no_t_tt <- TRUE, no_t_tt <- no_t_tt)
    ifelse(p_si_prior == "l" & p_si == "k", no_k_lk <- TRUE, no_k_lk <- no_k_lk)
    ifelse(p_si_prior == "i" & p_si == "j", no_j_ill <- TRUE, no_j_ill <- no_j_ill)
    ifelse(p_si_prior == "s" & p_si == "l" | p_si_prior == "z" & p_si == "l", no_l_sl <- TRUE, no_l_sl <- no_l_sl)
    ifelse(p_si_prior == "r" & p_si == "r", no_r_rr <- TRUE, no_r_rr <- no_r_rr)
    
    #look at the final two phonemes jointly to see if they are /ks/ or /gz/ or /kS/ AND whether the last grapheme is X -OR!- XE
    p_si_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    si_x <- str_sub(letter_str.latest,-1,-1)
    if(map_x == FALSE){
      if(si_x=="x" | si_x=="xh"){ #"xh" added as an option because of words like EXHAUST ... 
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_x <<- TRUE
          flag.index <<- my.index
        }}}
    
    #now check if its ending in XE...
    si_xe <- str_sub(letter_str.latest,-2,-1)
    if(map_xe == FALSE){
      if (si_xe=="xe") {
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_xe <<- TRUE
          flag.index <<- my.index
        }}}
    
    #must also check p_si_jointprior against a possible XI as in OBNOXIOUS, which means it would be "xi" not "x" or "xe"...creates a new map_xi flag
    si_xi <- str_sub(letter_str.latest,-2,-1)
    if(map_xi == FALSE) {
      if(si_xi=="xi"){
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_xi <<- TRUE
          flag.index <<- my.index
        }}}
    
    #if map_j is true and flag.index_j == my.index, we need to update the p_si to be p_si_jointprior if it's /ju/ or a related one like /je/
    if( flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "ju" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "jU" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "j3" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "je")
      p_si <- p_si_jointprior
    
    #find the options:
    g_opt <- syllable_initial_mappings[syllable_initial_mappings$phoneme==p_si,]
    
    #the following line removes /z/ ES as an option when its after a vowel
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    
    #the following line removes /e/ IA as an option when its after a vowel
    ifelse(no_e_ia == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    
    #remove these as needed:
    if( no_t_ct == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ct",]}
    if( no_t_bt == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="bt",]}
    if( no_m_lm == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="lm",]}
    if( no_m_thm == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="thm",]}
    if( no_s_tz == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tz",]}
    if( no_n_gn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="gn",]}
    if( no_n_kn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="kn",]}
    if( no_n_nn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="nn",]}
    if( no_d_dd == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="dd",]}
    if( no_t_tt == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tt",]}
    if( no_k_lk == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="lk",]}
    if( no_j_ill == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ill",]}
    if( no_l_sl == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="sl",]}
    if( no_r_rr == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="rr",]}
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #currently the code thinks this may be because /l/ was mapped to SL when the S needed to be left behind for a /s/ or /z/
    if(flag.index == my.index){
      if(p_si == "l"){g_opt <- g_opt[-which(g_opt$grapheme=="sl"),]} #removes the row from g_opt that has "sl" as an option for /l/
    }
    
    #or it could be there is an X coming up and so the syllabic structure is problematic at an earlier point, which happens with EXHAUST
    #to check that, look for an X and remap the whole structure and then restart
    #NOTE::DO NOT do this if any map_x flag is already set to TRUE
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){ #this only takes place if the map_x flag wasn't ALREADY set
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    #MAJOR NOTE: X cannot be in syllable initial, in fact! so if this flag has been set, it's actually an indication that
    #the X and its phonemes, /ks/ or whatever, should instead be syllable final (of the preceding syllable)
    #this requires a major fix: it  means the previously-assigned PG MAY be wrong, AND, this PG actually is
    #going to need to be syllable-final. So the easiest thing to do is use this flag to change the initial syll_struc and then
    #COMPLETELY restart the mapping. In essence, this flag should catch the issue and force a restart of the entire mapping...
    #THIS SHOUDL ONLY HAPPEN IF A FLAG WAS SET!
    if(map_x == TRUE | map_xe == TRUE | map_xi == TRUE) {
      if(flag.index == my.index){
        get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2] #this retrieves the syll_struc that was mapped up to this point
        ifelse(get_mapped!="5",stri_sub(get_mapped,1,1) <- "2",get_mapped<-get_mapped) #this makes the new syll_struc for that later part of the word correctly show what is syll-initial -UNLESS- it's actually the end of the word, like GALAXY (so in that case a 5)
        stri_sub(syll_struc.latest,-1,-1) <- "4" #this changes the part that was currently being mapped to correctly show its syll-final
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #finally, make the new syll_struc0
        flag.restart <<- TRUE #you actually do need to enforce a restart here because it IS possible for it to not fail at this step--this happens for EXAMINATION because the /z/ of the /gz/ gets mapped to X, which leaves it to get stuck trying to then do the /g/ with only an E leftover
      }}
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_si <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_si <- g_opt$grapheme[g_si]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_si <- "FAILED", g_si <- g_si)
    
    ##NOW: if it failed because it couldn't make a /j/ that should have been /ju/ or some other biphone like that, catch that now
    p_si_jointpost <- stri_sub(parsed_syll0,-my.index,-my.index+1) #combine the current p_si with the one that was most recently mapped
    if(p_si_jointpost == "ju" & g_si == "FAILED" | p_si_jointpost == "jU" & g_si == "FAILED" | p_si_jointpost == "j3" & g_si == "FAILED" | p_si_jointpost == "je" & g_si == "FAILED"){
      map_j <<- TRUE #if this gets set as true, it means we need to remap...
      flag.index_j <<- my.index-1
      #the current position should match the upcoming one UNLESS! it was a 5:
      if(stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) != "5"){
        stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) <- stri_sub(syll_struc0,nchar(syll_struc.latest),nchar(syll_struc.latest))
      }else{
        stri_sub(syll_struc0,nchar(syll_struc.latest),nchar(syll_struc.latest)) <- stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) #swapped order if word final...
      }
      flag.restart <<- TRUE
    }
    syll_struc0 <<- syll_struc0
    
    #THE FOLLOWING BLOCK OF CODE IS COMMENTED OUT BECAUSE IT SEEMS TO BE WRONG
    #if(map_j == TRUE & my.index == flag.index_j) { #note: only do this if you actually just mapped a /j/!
    #  get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2] #this retrieves the syll_struc that was mapped up to this point
    #  stri_sub(get_mapped,1,1) <- "2" #it should start as a 2, because this is assuming that we already failed on a /u/ but are now looking at a p_si that is /j/
    #  stri_sub(syll_struc.latest,-1,-1)  <- get_mapped #update that syllabic position
    #  syll_struc0 <<- paste0(syll_struc.latest,get_mapped)} #make the new syll_struc0
    
    #now catch instances where this may have failed because the previously mapped phoneme was too "greedy" OR maybe not greedy enough!
    #for example, in TOUGHER, it will have assigned the /3/ to HE, which means the /f/ is left to be a G which it cannot!
    #in CHRYSANTHEMUM, it will have assigned /e/ [schwa] to HE, and so /T/ will be unmappable with only the T left...
    #in EXHIBITION, it fails because /schwa/ maps to just the I, and then /s/ fails on EXH--this is tricky because the syllabic structure needs to be corrected so that the schwa is syll_initial and /ks/is final...
    if (g_si == "FAILED") {
      if(map_j == FALSE)
        if(p_si_jointprior == "ks" | p_si_jointprior == "kS" | p_si_jointprior == "gz"){
          get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2]
          ifelse(get_mapped!="5",stri_sub(get_mapped,1,1) <- "2",get_mapped<-get_mapped) #it should start as a 2, because the current 2 is about to become a 4...UNLESS it was word final, like GALAXY. NB: this code may be redundant with earlier...
          stri_sub(syll_struc.latest,-1,-1) <- "4" #make the current a 4...
          syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #make the new syll_struc0
          flag.restart <<- TRUE
        }else{
          flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed if it wasn't an X issue
          flag.restart <<- TRUE
        }else{
          flag.restart <<- TRUE
        }}
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_si),"_"))
    
    #if the replacement with _ makes the last character now the _, simply remove it
    if( str_sub(letter_str.latest,-1,-1)=="_"){letter_str.latest <- str_sub(letter_str.latest, end = -2)}
    
    #if there is an _ underscore followed by something OTHER than "e", this is a FAIL! set the flag.index and restart...
    if(tail(unlist(gregexpr("_", letter_str.latest)), n=1) != -1){ #this will be FALSE if the underscore was cleared, in which case nothing will be done
      if(stri_sub(letter_str.latest,tail(unlist(gregexpr("_", letter_str.latest)), n=1)+1,tail(unlist(gregexpr("_", letter_str.latest)), n=1)+1) != "e"){ #it had better be 'e', otherwise:
        #need to now repeat the work to check if actually this could be a map_j situation
        if(p_si_jointpost %in% c("ju","jU","j3","je")){
          flag.restart <<- TRUE
          map_j <<- TRUE
          flag.index_j <<- my.index-1
        }else{
          flag.restart <<- TRUE
          flag.index <<- my.index
        }}}
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_si)+1))
    
    #finally, have the latest syll_struc updated
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_si)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_si,g_si,"2",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #syllable-final function
  map_sf <- function(p_sf,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    ##CRITICAL: words that have an internal final E will break the syllabic structure--this flag will catch that to restart with an updated syll_struc0
    #example is BASEMENT, parsed by MOP as /b8/ + /sment/. The script will fail when the p_sf is a vowel but there is a final E, e.g.,
    #BASEMENT will be left with BA_E searching for p_sf = 8 but that's not allowed (by definition _E cannot be syllable final)
    if(str_sub(letter_str.latest,-2,-1)=="_e"){ 
      to_map <- "342" #this finds the current problematic sequence, which is a "423", and makes it the needed "342"
      stri_sub(syll_struc0,nchar(syll_struc.latest),-(nchar(syll_struc0)-nchar(syll_struc.latest)-1)) <- to_map
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
    } 
    
    #look at the phoneme in front of the p_sf, to check if its a vowel. If it is, then keep /z/ --> ES from being an option or /d/ --> ED
    p_sf_prior <- str_sub(parsed_syll.latest,-2,-2)
    no_z_es <- FALSE
    no_d_ed <- FALSE
    ifelse(p_sf_prior %in% vowels & p_sf == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_sf_prior %in% vowels & p_sf == "d", no_d_ed <- TRUE, no_d_ed <- no_d_ed)
    
    #look at the phoneme in front of the p_sf, to check if its /h/ or /D/ or /T/. If it is, then don't allow silent H options if the current p_sf is a vowel
    no_silent_H <- FALSE
    ifelse(p_sf_prior == "h" & p_sf %in% vowels | p_sf_prior == "D" & p_sf %in% vowels | p_sf_prior == "T" & p_sf %in% vowels, no_silent_H <- TRUE, no_silent_H <- no_silent_H)
    
    #look at the phoneme in front of the p_sf, to check if its /3/. If it is, then don't allow r to ro if the current p_sf is /r/ (because it's probably IRON or something like that)
    no_r_ro <- FALSE
    ifelse(p_sf_prior == "3" & p_sf == "r", no_r_ro <- TRUE, no_r_ro <- no_r_ro)
    
    #more specific case: don't do it if the sound is /f/ that may be a PH or GH
    ifelse(p_sf_prior == "f" & p_sf %in% vowels & stri_sub(letter_str.latest,-2,-2) == "h", no_silent_H <- TRUE, no_silent_H <- no_silent_H)
    
    #look at the phoneme in front of the p_sf, to check if its /w/. If it is, then don't allow /e/ [schwa] --> UA 
    no_e_ua <- FALSE
    ifelse(p_sf_prior == "w" & p_sf == "e", no_e_ua <- TRUE, no_e_ua <- no_e_ua)
    
    #look at the phoneme in front of the p_sf, to check if its one that needs to block HO
    no_ho <- FALSE
    ifelse(p_sf_prior %in% c("w","C","S","k","t") , no_ho <- TRUE, no_ho <- no_ho)
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_sf_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    sf_x <- str_sub(letter_str.latest,-1,-1)
    map_x = FALSE
    ifelse (sf_x=="x", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    sf_xe <- str_sub(letter_str.latest,-2,-1)
    map_xe = FALSE
    ifelse (sf_xe=="xe", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #now check if its ending in XI...
    sf_xi <- str_sub(letter_str.latest,-2,-1)
    map_xi = FALSE
    ifelse (sf_xi=="xi", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz",map_xi <- TRUE, map_xi <- FALSE), map_xi <- FALSE)
    
    #catch if its an 3rd like tired, in which case block d_ed...ALSO block /ju/ to EU if we have here /iju/ like in REUSE
    p_sf_jointprior2 <- str_sub(parsed_syll.latest,-3,-1)
    if(p_sf_jointprior2=="3rd")
      no_d_ed <- TRUE
    no_ju_eu <- FALSE
    if(p_sf_jointprior2=="iju" & p_sf == "ju" | p_sf_jointprior2=="iju" & p_sf == "u")
      no_ju_eu <- TRUE
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_m must be overwritten with the p_m_jointPRIOR
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_sf =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="ju")
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="ju")
                   p_sf<- str_sub(parsed_syll.latest,-3,-2),
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #do the same but if its "je" instead of "ju"
    if(p_sf =="e")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="je")
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="je")
                   p_sf<- str_sub(parsed_syll.latest,-3,-2),
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #do the same but if its "jU" instead of "ju"
    if(p_sf =="U")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="jU")
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="jU")
                   p_sf<- str_sub(parsed_syll.latest,-3,-2),
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #or if flag.ts is set, try that biphone!
    if(flag.index == my.index & flag.ts == TRUE){p_sf <- "ts"}
    
    #find the options:
    g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,]
    
    #the following line removes /z/ ES as an option when its after a vowel
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    
    #the following line removes /d/ ED as an option when its after a vowel
    ifelse(no_d_ed == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    
    #remove options as needed
    ifelse(no_silent_H, g_opt <- g_opt[str_sub(g_opt$grapheme,1,1)!="h",], g_opt <- g_opt)
    ifelse(no_e_ua, g_opt <- g_opt[g_opt$grapheme!="ua",], g_opt <- g_opt)
    ifelse(no_r_ro, g_opt <- g_opt[g_opt$grapheme!="ro",], g_opt <- g_opt)
    ifelse(no_ju_eu, g_opt <- g_opt[g_opt$grapheme!="eu",], g_opt <- g_opt)
    ifelse(no_ho, g_opt <- g_opt[g_opt$grapheme!="ho",], g_opt <- g_opt)
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #and rewrite the phoneme to be the jointprior one if its /ks/ and if the X flag is true
    if (map_x == TRUE & p_sf_jointprior == "ks" | map_x == TRUE & p_sf_jointprior == "kS" | map_x == TRUE & p_sf_jointprior == "gz"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="x",], g_opt <- g_opt)
    if (map_xe == TRUE & p_sf_jointprior == "ks" | map_xe == TRUE & p_sf_jointprior == "kS" | map_xe == TRUE & p_sf_jointprior == "gz"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #do similar if the map_xi flag is TRUE
    ifelse(map_xi, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="xi",], g_opt <- g_opt)
    if (map_xi == TRUE & p_sf_jointprior == "ks" | map_xi == TRUE & p_sf_jointprior == "kS" | map_xi == TRUE & p_sf_jointprior == "gz"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #first check if its an X issue... ###BUT DON'T DO THIS if we've already raised the map_x flag! as shown by SIXTEEN
    #to check that, look for an X and remap the whole structure and then restart
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+3,unlist(gregexpr('gz', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+3,unlist(gregexpr('gz', parsed_syll.latest))+3) <- "2" #make it syll-initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+3,unlist(gregexpr('kS', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+3,unlist(gregexpr('kS', parsed_syll.latest))+3) <- "2" #make it initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+3,unlist(gregexpr('ks', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+3,unlist(gregexpr('ks', parsed_syll.latest))+3) <- "2" #make it initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    #if it wasn't an X issue, then currently the code thinks this may be because a silent H was assigned, so this should BLOCK that from happening
    if(flag.index == my.index & p_sf == "3"){g_opt <- g_opt[!stri_sub(g_opt$grapheme,1,1)=="h",]}
    if(flag.index == my.index & p_sf == "e"){g_opt <- g_opt[!stri_sub(g_opt$grapheme,1,1)=="h",]}
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_sf <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_sf <- g_opt$grapheme[g_sf]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_sf <- "FAILED", g_sf <- g_sf)
    
    #now catch instances where this may have failed because the previously mapped phoneme was too "greedy" OR potentially not greedy enough
    #for example, in CLOTHESLINE, it will have assigned the /l/ to SL, which means the /z/ is left to map from just CLOTHE (no S left!)
    #another example: EXHAUST is first parsed IG-ZOST and so the /c/ is mapped to just AU, and then it fails b/c /z/ can't map to XH
    if (g_sf == "FAILED") {
      if( paste0(p_sf,PGlist[[length(PGlist)-5]]) == "ts") { ##try to "nip in the bud" a potential /ts/ biphone...if the current p_sf = s is coming before trying to map "t"
        stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) <- "4" #will make the previously mapped 's' a p_sf to join the 't'
        syll_struc0 <<- syll_struc0
        flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed
        flag.ts <<- TRUE
        flag.restart <<- TRUE 
      }else{
        flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed
        flag.restart <<- TRUE
      }} 
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_sf),"_"))
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, note that it removes more than 1 if the p_sf is a biphone 
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_sf)+1))
    
    #and update syll_struc if the X flag is set, so that it becomes sf-sf:
    ifelse(map_x, str_sub(syll_struc.latest,-2,-1) <- "44", syll_struc.latest <- syll_struc.latest)
    ifelse(map_xe, str_sub(syll_struc.latest,-2,-1) <- "44", syll_struc.latest <- syll_struc.latest)
    
    #finally, have the latest syll_struc updated..note that TWO are removed if the p_sf is longer than 1 (as it is with /ks/)
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_sf)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_sf,g_sf,"4",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #word-initial function
  map_wi <- function(p_wi,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    tryCatch({
      ifelse(map_j_wi == TRUE, p_wi <- str_sub(parsed_syll0,1,2), p_wi <- p_wi) #if the map_j_wi flag was set the first time through, this will be sure to map the /ju/ or /je/ or /jU/
    },    error = function(e) NA)
    
    
    #find the options:
    g_opt <- word_initial_mappings[word_initial_mappings$phoneme==p_wi,]
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_wi <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_wi <- g_opt$grapheme[g_wi]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_wi <- "FAILED", g_wi <- g_wi)
    
    #now check for word-initial /ju/ or /je/ or /jU/
    map_j_wi = FALSE
    
    #check to see if this is a word beginning with /ju/, etc. (as opposed to /j/ + /u/)
    p_wi_jointpost <- paste0(p_wi,stri_sub(parsed_syll0,-(my.index-1),-(my.index-1))) #this pastes the correct phoneme_wi with the previously mapped phoneme, whatever it was
    ifelse(p_wi_jointpost == "ju" & g_wi == "FAILED" |p_wi_jointpost == "je" & g_wi == "FAILED" |p_wi_jointpost == "jU" & g_wi == "FAILED" , map_j_wi <- TRUE, map_j_wi <- FALSE) #if this gets set as true, it means we need to remap...
    
    #now do what's needed to restart the whole thing if we have /ju/ and the mapping failed
    if(map_j_wi == TRUE) {
      flag.restart <<- TRUE
      map_j_wi <<- map_j_wi
    } else { flag.restart <<- FALSE }
    
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_wi),"_"))
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #catch the FAIL if we're NOT now left with ""
    ifelse(letter_str.latest !="", g_wi <- "FAILED", g_wi <- g_wi)
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, the X scenario must trigger removing /ks/
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_wi)+1))
    
    #finally, have the latest syll_struc updated
    syll_struc.latest <- str_sub(syll_struc.latest,1,-2)
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_wi,g_wi,"1",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    #and finally, set the flag to indicate the whole word was mapped
    ifelse(flag.restart==TRUE,mapped_wi <<- FALSE, mapped_wi <<- TRUE)
    
    return(PGlist)
    print(PGlist)
  }
  
  #formatting data for output function
  generate_custom_sequence <- function(length) {
    sequence <- c()
    count <- 1
    skip <- 0
    
    while (length(sequence) < length) {
      if (skip < 3) {
        sequence <- c(sequence, count)
        count <- count + 1
      } else {
        count <- count + 3  # Skip three numbers
        skip <- -1  # Reset skip
      }
      
      skip <- skip + 1
    }
    
    return(sequence)
  }
  
  ####################################
  #process words and create outputs (main function)
  ####################################
  
  all_words <- list()
  cleared <- matrix(NA,nrow=length(spelling))
  
  if(map_progress==TRUE) #only show progress bar if map_progress == TRUE; defaults to NO
  pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  for (i in 1:length(spelling)){
    word_index <- i
    prepped_word <- prep_word(word_index) #get the 3-row matrix from prep_word (syll_struc0,parsed_syll0, and parsed_syll0 in that order)
    
    syll_struc0 <- prepped_word[1,]
    parsed_syll0 <- prepped_word[2,]
    letter_str0 <- prepped_word[3,]
    
    mapped_wi <- FALSE 
    map_j_wi <- FALSE 
    map_j <- FALSE
    my.index <- 0 
    flag.index <- 0
    flag.index_j <- 0
    flag.nj <- 0
    flag.ts <- 0
    map_x <- FALSE
    map_xe <- FALSE
    map_xi <- FALSE
    count.restarts <- 0
    
    while (mapped_wi == FALSE & count.restarts < 10){
      
      flag.restart <- FALSE
      
      if(map_j_wi==TRUE){
        str_sub(syll_struc0,1,2) <- "1" #the syll_struc should be 1 at the front instead of 13, so that /ju/ is encountered as word initial as soon as the /u/ is encountered
      } else { syll_struc0 <- syll_struc0}
      
      #a matrix that has the current (last) phoneme and its position
      to_map <- matrix(NA,nrow=1,ncol=2)
      to_map[1,1] <- str_sub(parsed_syll0,-1,-1)
      to_map[1,2] <- str_sub(syll_struc0,-1,-1)
      
      if(nchar(syll_struc0)==1){
        
        p_wi <- to_map[1,1]
        my.index <- 1 
        PGlist <- matrix(map_wi(p_wi = p_wi, syll_struc.latest = syll_struc0, parsed_syll.latest = parsed_syll0, letter_str.latest = letter_str0))
      } else {
        if (to_map[1,2] == 5) {
          p_wf <- to_map[1,1]
          my.index <- 1 #p_wf will always be the first thing to try to map
          PGlist <- map_wf(p_wf = p_wf,syll_struc0 = syll_struc0,parsed_syll0 = parsed_syll0,letter_str0 = letter_str0)
        } }
      
      ###WORD-FINAL should now be assigned. the while loop will go through the remaining phonemes until none are left
      
      while (PGlist[[length(PGlist)]] != "") {
        
        if(flag.restart == TRUE){
          count.restarts <- count.restarts+1
          break}
        to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
        to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
        syll_struc.latest <- PGlist[[length(PGlist)-2]]
        parsed_syll.latest <- PGlist[[length(PGlist)-1]]
        letter_str.latest <- PGlist[[length(PGlist)]]
        
        if (to_map[1,2] == 3) {
          p_m <- to_map[1,1]
          my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
          PGlist <- append(PGlist,map_m(p_m = p_m, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
        } 
        
        if(flag.restart == TRUE){
          count.restarts <- count.restarts+1
          break}
        to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
        to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
        syll_struc.latest <- PGlist[[length(PGlist)-2]]
        parsed_syll.latest <- PGlist[[length(PGlist)-1]]
        letter_str.latest <- PGlist[[length(PGlist)]]
        
        if (to_map[1,2] == 2) {
          p_si <- to_map[1,1]
          my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
          PGlist <- append(PGlist,map_si(p_si = p_si, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
        } 
        
        if(flag.restart == TRUE){
          count.restarts <- count.restarts+1
          break}
        to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
        to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
        syll_struc.latest <- PGlist[[length(PGlist)-2]]
        parsed_syll.latest <- PGlist[[length(PGlist)-1]]
        letter_str.latest <- PGlist[[length(PGlist)]]
        
        if (to_map[1,2] == 4) {
          p_sf <- to_map[1,1]
          my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
          PGlist <- append(PGlist,map_sf(p_sf = p_sf, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
        } 
        
        if(flag.restart == TRUE){
          count.restarts <- count.restarts+1
          break}
        to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
        to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
        syll_struc.latest <- PGlist[[length(PGlist)-2]]
        parsed_syll.latest <- PGlist[[length(PGlist)-1]]
        letter_str.latest <- PGlist[[length(PGlist)]]
        
        if (to_map[1,2] == 1) {
          p_wi <- to_map[1,1]
          my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
          PGlist <- append(PGlist,map_wi(p_wi = p_wi, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
        }
        if(flag.restart == TRUE){
          count.restarts <- count.restarts+1
          break}
        if(mapped_wi == TRUE) break
      }
    }
    
    my.format <- generate_custom_sequence(nchar(syll_struc0)*3)
    my.format <- my.format + 1
    my.format[1:3]<-my.format[1:3]-1
    all_words[[i]] <- rev(data.frame(matrix(unlist(PGlist)[my.format],nrow=3)))
    all_words[[i]] <- all_words[[i]][,!is.na(all_words[[i]][1,])]
    all_words[[i]] <- data.frame(all_words[[i]])
    cleared[i] <- mapped_wi
    
    if(map_progress==TRUE) #only show progress bar if map_progress == TRUE; defaults to NO
    setTxtProgressBar(pb,i)
  }
  
  return(list(all_words,cleared))
}

# Maps onset and rime data.
map_OR <- function(spelling, pronunciation,map_progress=FALSE){
  
  library(stringi)
  
  #list vowels per in-house code
  vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
  
  #first run all the words through map_PG
  hold <- map_PG(spelling,pronunciation, map_progress = map_progress)
  
  #only show progress bar if map_progress == TRUE; defaults to NO
  if(map_progress==TRUE) 
    pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  for(i in 1:length(spelling)){
    tryCatch(
      {if(which(is.na(hold[[1]][[i]][1,])) > 0)
        hold[[1]][[i]] <- hold[[1]][[i]][,-which(is.na(hold[[1]][[i]][1,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
      }, error=function(e) NA)
    
    hold[[1]][[i]][4,] <- hold[[1]][[i]][1,]
    hold[[1]][[i]][4,][hold[[1]][[i]][1,] %in% vowels] <- "V"
    hold[[1]][[i]][4,][!hold[[1]][[i]][1,] %in% vowels] <- "C"
    
    #label columns with alphabetic indices for later cbinding
    colnames(hold[[1]][[i]]) <- c(letters,LETTERS)[1:ncol(hold[[1]][[i]])]
    
    #get complete CV string
    CVstring <- paste0(hold[[1]][[i]][4,],collapse="")
    
    #get complete position string
    posstring <- paste0(hold[[1]][[i]][3,],collapse="")
    
    #get column indices for eventual pasting
    colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
    
    #find the positions of 'V' characters
    split_positions1 <- gregexpr("V", CVstring)[[1]]
    
    #insert spaces before 'V' characters
    CVstring <- gsub("V", " V", CVstring)
    
    #make the posstring and colstring match the regrouped CVstring
    posstring <- paste0(substring(posstring,c(1,split_positions1),c(split_positions1-1,nchar(posstring))),collapse=" ")
    colstring <- paste0(substring(colstring,c(1,split_positions1),c(split_positions1-1,nchar(colstring))),collapse=" ")
    
    #find the positions of '2' characters
    split_positions2 <- gregexpr("2", posstring)[[1]]
    
    #insert spaces before '2' characters
    posstring <- gsub("2", " 2", posstring)
    
    #make the CVstring and colstring match the regroups posstring
    CVstring <- paste0(substring(CVstring,c(1,split_positions2),c(split_positions2-1,nchar(CVstring))),collapse=" ")
    colstring <- paste0(substring(colstring,c(1,split_positions2),c(split_positions2-1,nchar(colstring))),collapse=" ")
    
    #convert colstring to matrix of column indices to paste together:
    colstring <- as.matrix(read.table(text=colstring))
    
    #make a matrix to hold the remapping
    result <- matrix(NA,ncol=length(colstring),nrow=4)
    
    #now loop through each to-be-combined element of colstring to fill in result matrix
    for (j in 1:length(colstring)) {
      cols_to_paste <- unlist(strsplit(colstring[j], ""))
      result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
    }
    result <- as.data.frame(result)
    
    #format the new OR matrix including removing medial 3's from the position info, keeping only the first remaining character (e.g., a 14 rhyme like in AB-STAIN should be just 1) and dropping the CV markers
    hold[[1]][[i]] <- result
    hold[[1]][[i]][3,] <- gsub("3","",hold[[1]][[i]][3,]) 
    
    #run through each position code in row 3-- if row 4 starts with V, then the maximum is needed, if its C then the minimum is needed (e.g., 14 as the start in UPDO should become a 4)
    for (j in 1:ncol(hold[[1]][[i]])){
      ifelse(substring(hold[[1]][[i]][4,j],1,1) == "V", hold[[1]][[i]][3,j] <- max(as.numeric(strsplit(hold[[1]][[i]][3,j],"")[[1]])),hold[[1]][[i]][3,j] <- min(as.numeric(strsplit(hold[[1]][[i]][3,j],"")[[1]])))
    }
    
    hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4,]) #remove VC structural
    
    if(map_progress == TRUE) #only do progress bar if requested
    setTxtProgressBar(pb,i)
  }
  
  return(hold)
}

# After mapping, computes various relevant information; i.e. how many words
# correspond to a certain mapping criteria.
make_tables <- function(mapped_words){
  
  library(dplyr)
  library(tidyr)
  
  #create a dummy word to ensure place holders for all syllabic positions:
  dummy_word <- map_PG("hardy","hardi")
  dummy_word[[1]][[1]][,1]<-c(7,7,1)
  dummy_word[[1]][[1]][,2]<-c(7,7,3)
  dummy_word[[1]][[1]][,3]<-c(7,7,4)
  dummy_word[[1]][[1]][,4]<-c(7,7,2)
  dummy_word[[1]][[1]][,5]<-c(7,7,5)
  
  convert_matrix <- function(mat) {
    df <- as.data.frame(t(mat))
    colnames(df) <- c("phoneme", "grapheme", "position")
    separator <- data.frame(phoneme = "----------", grapheme = "----------", position = "----------")
    return(rbind(df, separator))
  }
  
  
  all_words_df <- do.call(rbind, lapply(mapped_words[[1]], convert_matrix))
  all_words_df <- rbind(all_words_df, convert_matrix(dummy_word[[1]][[1]])) #tack on the dummy word to ensure all 5 positions are present
  all_words_df <- all_words_df[1:(nrow(all_words_df) - 1), ]
  rownames(all_words_df) <- NULL
  
  filtered_df <- all_words_df %>%
    filter(phoneme != "----------" & grapheme != "----------")
  
  phoneme_grapheme_frequency <- filtered_df %>%
    group_by(position, phoneme, grapheme) %>%
    summarize(freq = n()) %>%
    ungroup()
  
  result_df <- phoneme_grapheme_frequency %>%
    spread(key = position, value = freq, fill = 0)
  
  pg <- result_df %>%
    group_by(phoneme) %>%
    mutate(across(`1`:`5`, 
                  ~ round((. / sum(.)), 4), 
                  .names = "prob_{.col}")) %>%
    ungroup() %>%
    transmute(phoneme = paste(phoneme), grapheme = paste(grapheme),
              wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, wf = prob_5) %>%
    mutate(across(c(wi, si, sm, sf, wf), 
                  ~ as.character(ifelse(is.na(.), "0", .))))
  
  gp <- result_df %>%
    group_by(grapheme) %>%
    mutate(across(`1`:`5`, 
                  ~ round((. / sum(.)), 4), 
                  .names = "prob_{.col}")) %>%
    ungroup() %>%
    transmute(phoneme = paste(phoneme), grapheme = paste(grapheme),
              wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, wf = prob_5) %>%
    mutate(across(c(wi, si, sm, sf, wf), 
                  ~ as.character(ifelse(is.na(.), "0", .))))
  
  result_df <- result_df %>%
    rename(wi = `1`, si = `2`, sm = `3`, sf = `4`, wf = `5`)
  
  phoneme_freq <- result_df %>%
    group_by(phoneme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  grapheme_freq <- result_df %>%
    group_by(grapheme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  phoneme_grapheme_freq <- result_df %>%
    group_by(phoneme, grapheme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  #now drop the 7's from the dummy word
  pg <- pg[pg$phoneme!="7",]
  gp <- gp[gp$grapheme!="7",]
  phoneme_freq <- phoneme_freq[phoneme_freq$phoneme!="7",]
  grapheme_freq <- grapheme_freq[grapheme_freq$grapheme!="7",]
  phoneme_grapheme_freq <- phoneme_grapheme_freq[phoneme_grapheme_freq$grapheme!="7",]
  
  return(list(pg=pg,gp=gp,p_freq=phoneme_freq,g_freq=grapheme_freq,pg_freq=phoneme_grapheme_freq))
}

## Useful Helper Functions ##

# Takes in a list of spellings and pronunciations, and a specific level of
# evaluation (currently supports phoneme/grapheme and onset/rime, in accordance
# with the above functions), and the tables from above. Applies either map_PG
# or map_OR depending on what is specified. "tables" corresponds to the relevant
# table (should be the result of calling map_PG or map_OR, respectively,
# on all of the words in the corpus.)
map_value <- function(spelling, pronunciation, level, tables) {
  
  pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  mylist <- list()
  
  position_mapping <- c("1" = "wi", "2" = "si", "3" = "sm", "4" = "sf", "5" = "wf")
  
  for( i in 1:length(spelling)) {
    
    if(level == "PG"){
      value <- map_PG(spelling[i], pronunciation[i], map_progress = FALSE)
    } else {   
      value <- map_OR(spelling[i], pronunciation[i], map_progress = FALSE)
    }
    
    
    df <- as.data.frame(value[[1]][[1]])
    
    while(all(is.na(df[,1]))) {
      df <- df[,-1]
    }
    
    rownames(df) <- c("phoneme", "grapheme", "position")
    
    df["position", ] <- position_mapping[unlist(df["position", ])]
    
    df[c("PG", "GP", "PG_freq", "P_freq", "G_freq"), ] <- NA
    
    tryCatch({
      for (col in colnames(df)) {
        phoneme_val <- df["phoneme", col]
        grapheme_val <- df["grapheme", col]
        position_val <- df["position", col]
        
        df["PG", col] <- subset(tables$pg, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["GP", col] <- subset(tables$gp, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["PG_freq", col] <- subset(tables$pg_freq, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["P_freq", col] <- subset(tables$p_freq, phoneme == phoneme_val)[,position_val]
        df["G_freq", col] <- subset(tables$g_freq, grapheme == grapheme_val)[,position_val]
      }}, error=function(e) {NA})
    
    df["spelling", 1] <- spelling[i]
    df["spelling", -1] <- ""
    
    df["pronunciation", 1] <- pronunciation[i]
    df["pronunciation", -1] <- ""
    
    df["PG_accuracy", 1] <- value[[2]] #encode if the word actually was not mappable, i.e., was "wrong"
    df["PG_accuracy", -1] <- ""
    
    mylist[[i]] <- df
    names(mylist[[i]]) <- spelling[i]
    
    setTxtProgressBar(pb,i)
  }
  
  return(mylist)
}

# Reutrns a list of words that correspond to a certain (or any) phoneme and
# grapheme in a given position. Requires a list of words that have been returned
# from map_value.
word_pattern <- function(mapped_words, phoneme, grapheme, position) {
  
  #all for one thing to be "any" -- set up scenarios for what to do based on indices, e.g., any phoneme == scenario 1 . scenario 4 is for fully specified requests
  scenario <- ifelse(phoneme == "any",1,
                     ifelse(grapheme == "any",2,
                            ifelse(position == "any",3,
                                   4)))
  
  matching_words <- c()
  
  for (i in seq_along(mapped_words)) {
    df <- mapped_words[[i]]
    if (!("spelling" %in% rownames(df))) {
      next 
    }
    
    word_values <- as.character(df["spelling", ])
    
    word <- word_values[which.max(nchar(word_values))]
    
    if (nchar(word) == 0) next  
    
    #FULLY SPECIFIED:
    if (scenario == 4)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["grapheme", j] == grapheme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY POSITION:
    if (scenario == 3)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["grapheme", j] == grapheme
        ) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY GRAPHEME:
    if (scenario == 2)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY PHONEME:
    if (scenario == 1)
      for (j in 1:ncol(df)) {
        if (df["grapheme", j] == grapheme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
  }
  
  return(matrix(matching_words))
}

# Returns phoneme/grapheme probabilities for a list of words.
summarize_words <- function(mapped_words,parameter){
  
  mystats <- matrix(nrow=length(mapped_words),ncol=7)
  
  for(i in 1:length(mapped_words)){
    data <- as.numeric(mapped_words[[i]][parameter,])
    mystats[i,1] <- mapped_words[[i]]["spelling",1]
    mystats[i,2] <- mapped_words[[i]]["pronunciation",1]
    if(mapped_words[[i]]["PG_accuracy",][1] == TRUE){ #if PG_accuracy is TRUE, meaning the word was mapable, then fill in its values...
      mystats[i,3] <- mean(data)
      mystats[i,4] <- median(data)
      mystats[i,5] <- max(data)
      mystats[i,6] <- min(data) 
      mystats[i,7] <- sd(data)
    } else { #otherwise put in 0's
      mystats[i,3] <- NA
      mystats[i,4] <- NA
      mystats[i,5] <- NA
      mystats[i,6] <- NA
      mystats[i,7] <- NA
    }
  }
  
  mystats <- as.data.frame(mystats)
  colnames(mystats) <- c('spelling','pronunciation','mean','median','max','min','sd')
  mystats$mean <- as.numeric(mystats$mean)
  mystats$median <- as.numeric(mystats$median)
  mystats$max <- as.numeric(mystats$max)
  mystats$min <- as.numeric(mystats$min)
  mystats$sd <- as.numeric(mystats$sd)
  
  return(mystats)
}

## SCRIPTING ##

#fourth: map all the words at both levels
# NOTE: The below operations take a long time. Only necessary if need to create
# entirely new sets for a new corpus. The results of these already exist in the
# directory.
all_words_PG <- map_PG(spelling=wordlist_v1_0$spelling,pronunciation = wordlist_v1_0$pronunciation, map_progress = TRUE)
all_words_OR <- map_OR(spelling=wordlist_v1_0$spelling,pronunciation = wordlist_v1_0$pronunciation, map_progress = TRUE)

#fifth: create PG and OR tables, given those mapped words
all_tables_PG <- make_tables(all_words_PG)
all_tables_OR <- make_tables(all_words_OR)

#sixth: score all the words, given the mappings and tables
scored_words_PG <- map_value(spelling = wordlist_v1_0$spelling, pronunciation = wordlist_v1_0$pronunciation, level = "PG", tables = all_tables_PG)
scored_words_OR <- map_value(spelling = wordlist_v1_0$spelling, pronunciation = wordlist_v1_0$pronunciation, level = "OR", tables = all_tables_OR)

#examples of extracting information
summarize_words(mapped_words = scored_words_PG[which(wordlist_v1_0$spelling=="penguin")], parameter = "PG")

all_words_PG_probability <- summarize_words(mapped_words = scored_words_PG, parameter = "PG")
head(all_words_PG_probability)

all_words_OR_GP_probability <- summarize_words(mapped_words = scored_words_OR, parameter = "GP")
head(all_words_OR_GP_probability)

word_pattern(scored_words_PG,"any","ho","wi")

word_pattern(scored_words_OR,"8n","any","wf")

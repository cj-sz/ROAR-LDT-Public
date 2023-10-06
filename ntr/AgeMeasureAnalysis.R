library(tidyverse)
library(dplyr)

# Set directory to the root directory of the project folder
# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public")

# Laptop
# setwd("C:/Users/cjsol/Documents/GitHub/ROAR-LDT-Public")

# Load in original required data
metadata = read.csv("data_allsubs/metadata_all_newcodes.csv")
long_newcodes_data = read.csv("data_allsubs/LDT_alldata_long_newcodes.csv")
wide_newcodes_data = read.csv("data_allsubs/LDT_alldata_wide_newcodes.csv")


# Rounding function
# Takes in a list of elements, and a number of trailing decimal places to be
# specified. Rounds all values in the list accordingly, and removes extra
# whitespace in the elements of the list.
round_list <- function(list, digits) {
    rounded_list <- lapply(list, function(elt) {
        trimws(format(round(elt, digits), nsmall = digits))
    })
    return(rounded_list)
}

# Changing metadata to reflect age in years
updated_metadata <- filter(metadata, !is.na(visit_age))
updated_metadata[2] <- updated_metadata[2]/12
# USE THIS FOR ROUNDED AGE DATA IN PLACE OF ABOVE LINE
# updated_metadata[2] <- round_list(updated_metadata[2]/12, 2)

# Get the max and min age from the data we have
min_age <- min(as.numeric(updated_metadata$visit_age))
max_age <- max(as.numeric(updated_metadata$visit_age))

# Remove all entries from long_newcodes_data associated with subjects whose
# ages are unknown (i.e. only keeps rows in updated_long_newcodes whose "subj"
# entry is also present in the updated_metadata)
updated_long_newcodes <- long_newcodes_data %>%
  filter(subj %in% updated_metadata$subj)

# For wide_newcodes_data, first prepend a column with all subject IDs (the
# data is already sorted in ascending subject ID order). Then filter.
subj <- 1:120
updated_wide_newcodes <- data.frame(subj, wide_newcodes_data) %>%
    filter(subj %in% updated_metadata$subj)

# Add age data to the updated_long_newcodes next to each subject
updated_long_newcodes <- merge(updated_long_newcodes, 
    updated_metadata[, c("subj", "visit_age")], by = "subj", all.x = TRUE)

# Saves this data to an output file
# write.csv(updated_long_newcodes, "updated_long_newcodes.csv")

# Create a data frame of data frames, based on the range of ages. Each sub
# data frame consists of all subjects in that age group, and the words they
# were tested on along with their accuracy.
# First convert the min and max ages from the updated_metadata to their
# integer values, and iterate through these ages
# The data recorded is based on the updated_long_newcodes dataframe.
# Data is exported to csv's for each age bin. Named based on lower bound
# (i.e. bin_7.csv refers to ages (7:8])
dir.create("age_data")

# The below should be wrapped into a function returning a list of data frames, representing
# each age bin. The resultant list should then be passed into get_word_age_accuracies() below
# whenever it is called. 
for (age in seq(from = ceiling(min_age), to = ceiling(max_age), by = 1)) {
    print("age")
    print(age)
    temp_df <- data.frame()
    out_path <- paste0("age_data/bin_", age - 1, ".csv")
    # Iterate through all subjects
    for (subject in updated_metadata$subj) {
            # If that subject is within the desired range
            if (!is.na(updated_metadata[subject, "visit_age"]) &&
                as.numeric(updated_metadata[subject, "visit_age"]) > (age - 1) && 
                as.numeric(updated_metadata[subject, "visit_age"]) <= age) {
                # Add all of their data to the data frame
                temp_df <- rbind(temp_df, subset(updated_long_newcodes, 
                    visit_age == updated_metadata[subject, "visit_age"]))
            }
    }
    
    # Write each set of age data to its own csv
    write.csv(temp_df, file = out_path)
}

# Next, we can compute the averages for each word for each age bin, 
# based on all of the data across age bins

# Read in the word statistics
word_statistics <- read.csv("data_allsubs/wordStatistics.csv")

# This function returns a data frame whose rows are words/pseudowords and whose columns are different age bins,
# and whose cells correspond to the average accuracy for that word/pseudoword 
# within the age bin. If there is no data for a word-bin pairing, is filled with
# NA. # Takes in a data frame of word statistics, the minimum age for a bin, 
# and the maximum age for bins.
get_word_age_accuracies <- function(word_statistics, min_age, max_age) {
    # Create a vector of age bins from 7 to 28
    age_bins <- (ceiling(min_age)-1):(ceiling(max_age)-1)
    
    # Create an empty data frame with words as rows and age bins as columns
    word_age_accuracies <- data.frame(word = word_statistics$STRING, 
        stringsAsFactors = FALSE)
    
    # Add columns for each age bin and initialize them with empty values
    for (age in age_bins) {
        bin_col <- paste("bin_", age, sep = "")
        word_age_accuracies[, bin_col] <- ""
    }
    
    # Iterate through all of the words, now in word_age_accuracies, and, 
    # iterating through all instances of that word in a given age bin, sum the 
    # accuracies (0/1) and ultimately divide by the number of occurrences
    # to obtain the average for that word for that age bin.
    for word 
}

# make a list of accuracy averages for each age bin for that word
for word in word_statistics (would need to get this)
    compute that accuracy by summing all of the 1s/0s for accuracy for that word in the age bin, and divide by the number of them that there are
    can do something similar for response times
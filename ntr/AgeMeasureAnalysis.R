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

### DEFINE USEFUL FUNCTIONS USED IN THE SCRIPT FILE BELOW ###

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

# Returns a data frame of data in the "long" format for the given age bin,
# where the data returned corresponds to ages in range (age_bin, age_bin + 1]
# Age data is retrieved from the passed in metadata file data frame.
get_long_bin_data <- function(metadata, long_data, age_bin) {
    # Create the data frame
    bin = data.frame()

    # Go through all subjects in the metadata
    for (subject in metadata$subj) {
        # If that subject is within the desired range
        if (!is.na(metadata[subject, "visit_age"]) &&
            as.numeric(metadata[subject, "visit_age"]) > (age_bin) && 
            as.numeric(metadata[subject, "visit_age"]) <= age_bin + 1) {
            # Add all of their data to the data frame
            bin <- rbind(bin, subset(long_data, 
                visit_age == metadata[subject, "visit_age"]))
        }
    }

    # Return the age bin data
    return(bin)
}

# Takes in age data given by get_long_bin_data and the min and max age in
# the metadata-dataset. Outputs csv's with one-year age bin data within that
# range to the ~/age_data folder, named repsectively according to the convention
# above.
output_long_bin_data <- function(long_bin_data, metadata, min_age, max_age) {
    # Create a directory for storing the age data
    if (!file.exists("ntr/age_data")) {
        dir.create("ntr/age_data")
    }

    # For each age bin in the range passed in
    for (age in seq(from = ceiling(min_age), to = ceiling(max_age), by = 1)) {
        # Get the relevant data for that age bin
        temp_df <- get_long_bin_data(metadata, long_bin_data, age - 1)

        # Set the write path to be named corresponding to the age bin
        out_path <- paste0("ntr/age_data/bin_", age - 1, ".csv")

        # Write the data to the file
        write.csv(temp_df, file = out_path)
    }
}

# Returns a data frame whose words are rows/pseudowords and whose columns are
# different age bins. Each entry is the average for the respective statistic
# (as determined by the value passed to "data_type").
# Takes in data in the "long" format, metadata for subject ages, a data frame
# of word statistics based on the wordStatistics csv where each word is
# listed once, and the minimum and maximum age of the subjects in the data set.
# The first parameter denotes the type of data to be returned; right now,
# accuracy and response time are supported (denoted "acc" and "rt" in accordance
# with the names of the columns.
get_avg_word_age_data <- function(data_type, long_data, metadata, word_statistics, min_age, max_age) {
    # Create a vector of age bins the min_age to the max_age of the dataset
    age_bins <- (ceiling(min_age)-1):(ceiling(max_age)-1)
    
    # Create an empty data frame with words as rows and age bins as columns
    word_age_averages <- data.frame(word = word_statistics$STRING, 
        stringsAsFactors = FALSE)
    
    # Add columns for each age bin and initialize them with empty values
    for (age in age_bins) {
        bin_col <- as.character(age)
        word_age_averages[, bin_col] <- NA
    }
    
    # Progress counters:
    completed <- 1
    word_count <- nrow(word_statistics)

    # Iterate through all of the words, now in word_age_averages, and, 
    # iterating through all instances of that word in a given age bin, sum the 
    # accuracies (0/1) and ultimately divide by the number of occurrences
    # to obtain the average for that word for that age bin.
    for (word in word_age_averages$word) {
        for (age in age_bins) {
            # Get the data for that age
            age_data <- get_long_bin_data(metadata, long_data, age)

            # Only operate if there is data to operate on (for efficiency)
            # TODO: Allow for any data to be gleaned from the set as long as
            # the passed in column name (to the data_type parameter) is valid,
            # and its entries are numbers.
            # For now, only accuracy and response time are supported.
            if (nrow(age_data) != 0) {
                # Get all the relevant statistics for the given word
                if (data_type == "acc") {
                    temp <- age_data$acc[age_data$word == word]
                } else if (data_type == "rt") {
                    temp <- age_data$rt[age_data$word == word]
                }

                # Set the average in word_age_averages based on the bin
                # This will throw a warning when an element of accs is NA, so 
                # ignore it
                suppressWarnings({
                    word_age_averages[word_age_averages$word == word, as.character(age)] <- mean(temp)
                })
            }

            # Print progress at the end of each age bin:
            message <- sprintf("IN PROGRESS: Age bin %3d of %3d in word %3d of %3d.\r",
                    age - 5, ncol(word_age_averages) - 1, completed, word_count)
            cat(message)
        }

        # Update number of words completed
        completed <- completed + 1
    }
    
    # Progress result
    cat("\n")
    cat("Completed ", completed - 1, " of ", word_count, " words.\n")

    # Ultimately return the averages
    return(word_age_averages)
}

### SCRIPTING ###

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
# Note that there are only 120 (non-outlier) subjects in the data.
subj <- 1:120
updated_wide_newcodes <- data.frame(subj, wide_newcodes_data) %>%
    filter(subj %in% updated_metadata$subj)

# Add age data to the updated_long_newcodes next to each subject
updated_long_newcodes <- merge(updated_long_newcodes, 
    updated_metadata[, c("subj", "visit_age")], by = "subj", all.x = TRUE)

# Output all of the long age bin data to some files, named by the convention
# ~/age_data/bin_<age_bin>.csv
output_long_bin_data(updated_long_newcodes, updated_metadata, min_age, max_age)

# Next, we can compute the averages for each word for each age bin, 
# based on all of the data across age bins

# Read in the word statistics
word_statistics <- read.csv("data_allsubs/wordStatistics.csv")

# Get the accuracies for each word and age group.
average_word_age_accuracies <- get_avg_word_age_data("acc", updated_long_newcodes, updated_metadata, word_statistics, min_age, max_age)

# Get the average response times for each word and age group
average_word_age_rt <- get_avg_word_age_data("rt", updated_long_newcodes, updated_metadata, word_statistics, min_age, max_age)

# Write them both to a csv
write.csv(average_word_age_accuracies, "ntr/age_data/average_word_age_accuracies.csv")
write.csv(average_word_age_rt, "ntr/age_data/average_word_age_rt.csv")

# Next we are interested in getting the average accuracy and response time data
# for the different age bins for all words corresponding to a certain
# phoneme/grapheme in a position, onset/rime, etc. Use PROCESSING SCRIPTS.R from
# ntr/automated_toolkit directory.
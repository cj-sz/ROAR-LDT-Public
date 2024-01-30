library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(openxlsx)

# Set directory to the root directory of the project folder
# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public")
# Laptop
setwd("C:/Users/cjsol/Documents/Github/ROAR-LDT-public")


# Grab the automated pg toolkit functions
load("ntr/automated-toolkit/version 1.1/automated toolkit/PG Toolkit v1.1.RData")

# Old versions
# source("C:/Users/Caleb Solomon/Documents/Github/ROAR-LDT-Public/ntr/automated-toolkit/automated-toolkit-OLD/PROCESSING-SCRIPTS.R")

# Laptop
# setwd("C:/Users/cjsol/Documents/GitHub/ROAR-LDT-Public")

# Load in original required data
metadata <- read.csv("data_allsubs/metadata_all_newcodes.csv")
long_newcodes_data <- read.csv("data_allsubs/LDT_alldata_long_newcodes.csv")
wide_newcodes_data <- read.csv("data_allsubs/LDT_alldata_wide_newcodes.csv")

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
    bin <- data.frame()

    # Go through all subjects in the metadata
    for (subject in metadata$subj) {
        # If that subject is within the desired range
        if (!is.na(metadata[subject, "visit_age"]) &&
            as.numeric(metadata[subject, "visit_age"]) > (age_bin) && 
            as.numeric(metadata[subject, "visit_age"]) <= age_bin + 1) {
            # Add all of their data to the data frame
            bin <- rbind(bin, subset(long_data, 
                visit_age == metadata[subject, "visit_age"])) # nolint
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

# TODO merge this and the function below it to save time (maybe can reverse order of for loops?)

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
    # Create a vector of age bins from the min_age to the max_age of the dataset
    age_bins <- (ceiling(min_age) - 1):(ceiling(max_age) - 1)

    # Create an empty data frame with words as rows and age bins as columns
    word_age_averages <- data.frame(
        word = word_statistics$STRING,
        stringsAsFactors = FALSE
    )

    # TODO remove this statement
    View(word_age_averages)

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
            message <- sprintf(
                "IN PROGRESS: Age bin %3d of %3d in word %3d of %3d.\r",
                age - 5, ncol(word_age_averages) - 1, completed, word_count
            )
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


# Takes in a pre-processed scored list of words (processed by map_values in the
# PG Toolkit) and a list of words to grab (a list of strings). Outputs, in list
# format, the data frames from scored_words (in the same pre-processed format)
# corresponding to the input list of desired words.
# Returns a list of data frames in the same format as processed by map_values in
# the PG toolkit.
#
# Requires the PG Toolkit's functions to be loaded.
get_scored_words <- function(scored_words, words) {
    ret <- list()
    for (i in 1:length(scored_words)) {
        if (scored_words[[i]]["spelling", 1] %in% words) {
            ret <- append(ret, list(scored_words[[i]]))
        }
    }
    return(ret)
}

# Takes in a scored list of words pre-processed by the PG Toolkit's map_values
# function, a desired phoneme, grapheme, and position, a list of words to summarize (a list of strings),
# and a desired trait to summarize. (Expand on this documentation)
# Calls the summarize_words function from the PG toolkit on all words present in
# both the PG Toolkit's corpus as well as the ROAR's dataset, at the desired level
# of evaluation (the passed in trait)
#
# Required the PG Toolkit's functions to be loaded.
# Requires age data to exist in csv's in the proper location, ~/ntr/age_data
summarize_word_list <- function(scored_words, phoneme, grapheme, position, input_list, trait, min_age, max_age) {
    # Takes in a list of scored words, a phoneme, a grapheme, and a position for them
    # Outputs all words in the ROAR corpus that have been processed by the PG Toolkit
    # Outputs corresponding ROAR data to a csv located in ~/ntr/summarized_ROAR_words
    # containing all trials with the respective words matching the input criteria that
    # are also in the ROAR set, in the given range of ages [min_age, max_age)
    match <- word_pattern(scored_words, phoneme, grapheme, position)
    if (length(match) == 0) {
        print("No words in the corpus match the provided pattern.")
    } else {
        wordlist <- match[match %in% input_list]
        if (length(wordlist) == 0) {
            print("No matching words with the given inputs are present in the input list.")
        } else {
            summarize_words(get_scored_words(scored_words, wordlist), trait)
            # Output the csv's
            if (!file.exists("ntr/summarized_ROAR_words")) {
                dir.create("ntr/summarized_ROAR_words")
            }
            temp <- data.frame()
            for (i in floor(min_age):ceiling(max_age)) {
                fn <- paste("ntr/age_data/bin_", i, ".csv", sep = "")
                if (file.exists(fn)) {
                    tryCatch(
                        {
                            data <- read.csv(fn)
                            if (nrow(data) != 0 && ncol(data) != 0) {
                                filtered_data <- data[data$word %in% wordlist, ]
                                if (nrow(filtered_data) != 0 && ncol(filtered_data) != 0) {
                                    temp <- rbind(temp, filtered_data)
                                }
                            }
                        },
                        error = function(e) {
                            print(paste("Age bin ", i, " is empty. Skipping...", sep = ""))
                        }
                    )
                } else {
                    print("File does not exist")
                }
            }
            temp <- temp[, -1]
            if (nrow(temp) != 0 && ncol(temp) != 0) {
                colnames(temp) <- c("subj", "word", "rt", "acc", "wordLength", "realpseudo", "visit_age")
            }
            out_path <- paste0("ntr/summarized_ROAR_words/phoneme-", phoneme, "_grapheme-", grapheme, "_position-", position, ".csv", sep = "")
            # Write the data to the file
            write.csv(temp, file = out_path)
        }
    }
}

# Requires a set of scored preprocessed words from the PG Toolkit to be passed in.
# Takes in list of words in the ROAR corpus and a desired phoneme-grapheme mapping and position.
# Outputs the subset of those ROAR words corresponding to the phoneme/grapheme mapping.
# (Works with any input list but primary functionality is for ROAR words)
# Requires PG Toolkit R Data to be loaded
# Note that when ROAR words/pseudowords are passed in, pseudowords will never
# be present in the output list, as they are not part of the PG Toolkit's set
# of pre-processed words.
words_with_mapping <- function(scored_words, inputlist, phoneme, grapheme, position) {
    match <- word_pattern(scored_words, phoneme, grapheme, position)
    return(match[match %in% inputlist])
}

# Takes in a desired phoneme grapheme mapping and position.
# Outputs a histogram with the number of trials for those words satisfying
# the input parameters across all age bins in the ROAR corpus.
# Note that the min_age and the max_age (with range [min_age, max_age)) also
# need to be provided as integers.
# A list of words in the ROAR corpus also needs to be provided. (providing the entire thing
# makes the plot way too large to be useful)
# A set of scored words from the PG Toolkit further needs to be provided.
# If desired, can provide an accuracy (either "any", 1, or 0) and a response time
# ("any" or a floating point threshold) to further filter data.
# It is assumed that all age bins are alreday generated to ntr/age_data and
# have not been artificially renamed.
# TODO: Ouput the plots somewhere.
roar_hist <- function(scored_words, input, phoneme, grapheme, position, min_age, max_age, acc, rt) {
    keptacc <- acc
    keptrt <- rt
    # TODO: Add a quiet to hide this message if desired
    print("Note that when providing an accuracy, the only data points counted will be those where the accuracy is precisely the input given (1 for accurate, 0 for inaccurate).")
    print("Note that when providing a response time, the only data points counted will be those where the response time is less than or equal to the provided input.")
    if (!(is.character(acc) && acc == "any") && !(is.numeric(acc) && acc == 0 || acc == 1)) {
        rt = -1
        print("Invalid accuracy provided. Must be 'any', 1, or 0. Proceeding with 'any' accuracy.")
    }
    if (!(is.character(rt) && rt == "any") && !(is.double(rt) && rt > 0)) {
        acc = -1
        print("Invalid response time provided. Must be 'any' or a positive decimal number. Proceeding with 'any' response time.")
    }
    if (acc == "any") {
        acc = -1
    }
    if (rt == "any") {
        rt = -1
    }
    if (is.character(input)) {
        wordlist <- words_with_mapping(scored_words, input, phoneme, grapheme, position)
        # First add dummy empty columns to the data frame for all ages in the range.
        df <- data.frame(matrix(nrow = 0, ncol = 2))
        colnames(df) <- c(age = numeric(), entries = numeric(), stringsAsFactors = FALSE)
        for (i in min_age:(max_age - 1)) {
            df <- rbind(df, data.frame(age = i, entries = 0))
        }
        for (w in wordlist) {
            # go through all of the age bins for all words
            # data frame with two columns: ages and counts for the word.
            for (i in floor(min_age):(ceiling(max_age) - 1)) {
                fn <- paste("ntr/age_data/bin_", i, ".csv", sep = "")
                # The file having one line means that it is empty (has no data) based on
                # the way the age bins are generated in an earlier function.
                if (file.exists(fn) && length(readLines(fn, n = -1)) != 1) {
                    data <- read.csv(fn)
                    if (acc == -1 && rt == -1) {
                        df[i - min_age + 1, "entries"] <- df[i - min_age + 1, "entries"] + length(which(data$word == w))
                    } else if (acc == -1) {
                        # Only the response time is specific
                        count <- 0
                        for (j in 1:nrow(data)) {
                            if (data[j, "word"] == w && data[j, "rt"] <= acc) {
                                count <- count + 1
                            }
                        }
                        df[i - min_age + 1, "entries"] <- df[i - min_age + 1, "entries"] + count
                    } else if (rt == -1) {
                        # Only the accuracy is specific
                        count <- 0
                        for (j in 1:nrow(data)) {
                            if (data[j, "word"] == input && data[j, "acc"] == acc) {
                                count <- count + 1
                            }
                        }
                        df[i - min_age + 1, "entries"] <- df[i - min_age + 1, "entries"] + count
                    } else {
                        count <- 0
                        for (j in 1:nrow(data)) {
                            if (data[j, "word"] == w && data[j, "acc"] == acc && data[j, "rt"] <= rt) {
                                count <- count + 1
                            }
                        }
                        df[i - min_age + 1, "entries"] <- df[i - min_age + 1, "entries"] + length(which(data$word == w))
                    }
                } else {
                    print(paste("File does not exist, or there are no entries for age bin ", i, ". Replacing with 0.", sep = ""))
                }
            }
        }
        if (length(wordlist) > 0) {
            print("Matching words:")
            print(paste(wordlist, sep = " "))
            ggplot(df, aes(x = age, y = entries, fill = age)) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(aes(label=entries), position=position_dodge(width=0.9), vjust=-0.25) +
                scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$entries) + 10)) +
                scale_x_continuous(expand = c(0, 0), limits = c(5.5, 28.5), breaks = seq(6,29,1)) +
                labs(
                    x = "Age (1-Year Bins) (inclusive lower end, exclusive upper end)",
                    y = "Trials for matching words",
                    title = paste("Data points for phoneme: ", phoneme, ", grapheme: ", grapheme, ", position: ", position, ", acc = ", keptacc, ", rt <= ", keptrt, ", matching words: ", length(wordlist), sep = "")
                )
        } else {
            print("No words matched the specified inputs. Nothing to plot.")
        }
    } else {
        print("Invalid input. Must be either a valid integer age (present among the ROAR age bins)or a word in the provided ROAR corpus.")
    }
}

# Fixed consolidated version. All ph/gr/po inputs assumed to be the same length. Type, "low_to_high" or
# "high_to_low" determines the output directory.
# TODO: Need to fix issue where columns of different lengths are not allowed. Then can send.
output_gp_trajectories <- function(scored_words, phonemes, graphemes, positions, type) {
    columns <- list()
    wordlists <- list()
    out <- createWorkbook()
    if (!file.exists("ntr/age_data/gp_trajectory_outputs")) {
        dir.create("ntr/age_data/gp_trajectory_outputs")
    }
    for (i in 1:length(phonemes)) {
        columns <- append(columns, paste(phonemes[i], "+", graphemes[i], "_", positions[i], sep = ""))
        wordlists <- c(wordlists, list(word_pattern(scored_words, phonemes[i], graphemes[i], positions[i])))
    }
    maxlistsize <- length(wordlists[[1]])
    for (i in 2:(length(wordlists))) {
        if (length(wordlists[[i]]) > maxlistsize) {
            maxlistsize <- length(wordlists[[i]])
        }
    }
    df <- data.frame(matrix(NA, nrow = maxlistsize, ncol = length(phonemes)))

    for (i in 1:length(wordlists)) {
        if (length(wordlists[[i]]) > 0) {
            col <- c(wordlists[[i]], rep(NA, maxlistsize - length(wordlists[[i]])))
            df[, i] <- col
        }
    }

    colnames(df) <- columns
    addWorksheet(out, sheetName = type)
    writeDataTable(out, sheet = 1, x = df)

    # Might include this later, and if so need to parse output of scored words.
    # Need to fix up the return type of get_scored_words somehow if this is implemented. 
    # for (i in 2:(1+length(phonemes))) {
    #     print(i)
    #     addWorksheet(out, sheetName = columns[i - 1])
    #     writeDataTable(out, sheet = i, x = get_scored_words(scored_words, wordlists[i - 1]))
    # }

    saveWorkbook(out, paste("ntr/age_data/gp_trajectory_outputs/", type, ".xlsx", sep = ""), overwrite = TRUE)
}

#################
### SCRIPTING ###
#################

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
# Changed to a doubble forward slash on radian
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
# phoneme/grapheme in a position, onset/rime, etc.

# Example of getting a single word corresponding to a given phoneme, grapheme, and position,
# from the PG Toolkit
ret <- word_pattern(scored_words_OR, phoneme = "any", grapheme = "int", position = "wf")

# Summarize all of the words present in the ROAR data that also correspond to the
# desired input mappings.
#
# The only thing to do now is to replace this with the desired phoneme/grapheme/position mappings and the proper input scored word list and trait!
summarize_word_list(scored_words = scored_words_OR, phoneme = "any", grapheme = "en", position = "wf", input_list = word_statistics$STRING, trait = "PG", min_age, max_age)
summarize_word_list(scored_words = scored_words_OR, phoneme = "any", grapheme = "a_ek", position = "wf", input_list = word_statistics$STRING, trait = "PG", min_age, max_age)
summarize_word_list(scored_words = scored_words_OR, phoneme = "8", grapheme = "any", position = "sf", input_list = word_statistics$STRING, trait = "PG", min_age, max_age)

# Get all ROAR words with a corresponding mapping
templist <- words_with_mapping(scored_words = scored_words_OR, inputlist = word_statistics$STRING, phoneme = "any", grapheme = "a_ek", position = "wf")
templist
# Get a plot of all ROAR words with a specified mapping corresponding to some desired criteria.
roar_hist(scored_words_OR, word_statistics$STRING, phoneme = "any", grapheme = "a_ek", position = "wf", min_age = floor(min_age), max_age = ceiling(max_age), acc = "any", rt = "any")
roar_hist(scored_words_OR, word_statistics$STRING, phoneme = "any", grapheme = "a_ek", position = "wf", min_age = floor(min_age), max_age = ceiling(max_age), acc = 0, rt = "any")
roar_hist(scored_words_OR, word_statistics$STRING, phoneme = "any", grapheme = "a_ek", position = "wf", min_age = floor(min_age), max_age = ceiling(max_age), acc = 1, rt = "any")

# Getting list of words for five high-to-low and five low-to-high mappings according to what Jeremy
# and Michaela sent. Utilize xlsx package to write different outputs to multiple sheets.
# 5 low to high: 
# ɪ+e_wi (ɪ is encoded as "1" in the toolkit)
# k+c_wf
# i+ie_wf
# oʊ+oe_wf (oʊ with the little omega is encoded as "o" in the toolkit)
# u+u_wf
# 5 high to low: 
# u+ough_wf
# ju+u_wf 
# eɪ+ae_wf (note eɪ is encoded as "8" in the toolkit)
# aʊ+ho_wi (note aʊ is encoded as "O" in the toolkit)
# u+ou_wf'
# These last five have a lot fewer entries, so less likely they have intersection with the toolkit 

# Required structures
l2hph <- c("1", "k", "i", "o", "u")
l2hg <- c("e", "c", "ie", "oe", "u")
l2hpo <- c("wi", "wf", "wf", "wf", "wf")
h2lph <- c("u", "ju", "8", "O", "u")
h2lg <- c("ough", "u", "ae", "ho", "ou")
h2lpo <- c("wf", "wf", "wf", "wi", "wf")

# From PG, and the intersection:
output_gp_trajectories(scored_words_PG, l2hph, l2hg, l2hpo, "low_to_high_Toolkit")
output_gp_trajectories(scored_words_PG, h2lph, h2lg, h2lpo, "high_to_low_Toolkit")
output_gp_trajectories(get_scored_words(scored_words_PG, word_statistics$STRING), l2hph, l2hg, l2hpo, "low_to_high_ROAR")
output_gp_trajectories(get_scored_words(scored_words_PG, word_statistics$STRING), h2lph, h2lg, h2lpo, "high_to_low_ROAR")

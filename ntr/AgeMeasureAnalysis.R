library(tidyverse)
library(dplyr)

# Desktop
setwd("C:/Users/Caleb Solomon/Documents/GitHub/ROAR-LDT-Public")

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

# First, remove all subjects whose ages are unknown from the metadata, and 
# convert remaining ages to age in years. Rounding to 2 decimal places for the 
# age
updated_metadata <- filter(metadata, !is.na(visit_age))
updated_metadata[2] <- round_list(updated_metadata[2]/12, 2)
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

# Create a data frame of data frames, based on the range of ages. Each sub
# data frame consists of all subjects in that age group, and the words they
# were tested on along with their accuracy.
# First convert the min and max ages from the updated_metadata to their
# integer values, and iterate through these ages
# The data recorded is based on the updated_long_newcodes dataframe.
age_bin_data <- list()
for (age in seq(from = ceiling(min_age), to = ceiling(max_age), by = 1)) {
  temp_df <- data.frame()
  # Iterate through all subjects
  for (subj in updated_metadata$subj) {
    # If that subject is within the desired range
    if (!is.na(updated_metadata[subj, "visit_age"]) &&
               as.numeric(updated_metadata[subj, "visit_age"]) > (age - 1) && 
               as.numeric(updated_metadata[subj, "visit_age"]) <= age) {
      # Add all of their data to the data frame
      temp_df <- rbind(temp_df, subset(updated_long_newcodes, subj == subj))
    }
  }
  age_bin_data <- append(age_bin_data, temp_df)
}

# Finally, convert the age bin data to a data frame itself. Note that the
# age bin column represents the ceiling of the age bin (i.e. "8" represents
# all subject data where the subject's age is from 7 to 8, exclude-7 and
# include-8). The I() function allows this.
# NOTE: the below does not currently work. Likely requires usage of data.table library. currently researching this.
age_bin_data <- data.frame(Bin = ceiling(min_age), to = ceiling(max_age), Data = I(age_bin_data))





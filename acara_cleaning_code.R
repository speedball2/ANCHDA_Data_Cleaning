# Title: ACARA data for ANCHDA
# Description: This script reads the ACARA data from the excel documents and reorganise it in separate CSVs - one per indicator.
# Author: Claire Boulange
# Date: completed on March 24, 2023

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)


#set working directory and options
acara_folder <- "Z:/CDA/Claire WD/anchda/ACARA"
path_out = "Z:/CDA/Claire WD/indicators_outputs/temp/"

options(timeout = 600) 
setwd(acara_folder)


##------------------------------------------------------------------------ begin analysis on naplan results ---------------------------------------------------------------------------------------###

# Get list of NAPLAN files
naplan_files <- list.files(pattern = "NAPLAN Results.*.xlsx")

# Read each file into a list of data frames
naplan_results <- lapply(naplan_files, read_excel, sheet = 2)

# Remove spaces and file extension from file names, and convert to lower case
names_datasets <- tolower(gsub("\\.xlsx", "", gsub(" ", "_", naplan_files)))
names(naplan_results) <- names_datasets

# Error handling loop to identify NULL data frames
naplan_results <- lapply(naplan_results, function(df) {
  if (is.null(df)) {
    return(data.frame())
  } else {
    return(df)
  }
})


# Run a function to extract columns, filter, and reshape the data
naplan_results <- lapply(naplan_results, function(x) {
  x[, c(2, 4, 5, 6, 9)] %>%
    filter(Domain %in% c("Reading", "Writing", "Spelling", "Grammar and Punctuation", "Numeracy"))
})

# Split the filtered tibbles into separate tables based on domain, student grade level
list_of_tables <- lapply(naplan_results, function(x) {
  split(x, list(Name = paste(x$Domain, x$`Student Grade Level`, sep = "_")))
})


#rename the columns to VISER standards
list_of_tables <- lapply(list_of_tables, function(y) {
  lapply(y, function(x) {
    setNames(x, c(paste0(names(x)[1], "_code16"), "year", 
                  paste0(names(x)[3], "_acara"), "student_grade_level", "naplan_score"))
  })
})


# Save all the tibbles as individual CSV files, sort the files in sub-folders based on geographies 
# and clean the path names to all lower case and no space
lapply(names(list_of_tables), function(outer_name) {
  lapply(names(list_of_tables[[outer_name]]), function(inner_name) {
    x <- list_of_tables[[outer_name]][[inner_name]]
    sa_code <- gsub("naplan_results_by_", "", outer_name)
    subject_grade <- gsub(" ", "_", inner_name)
    subject_code <- switch(gsub("_.*", "", tolower(subject_grade)), 
                           "reading" = "451",
                           "writing" = "452",
                           "spelling" = "453",
                           "grammar" = "454",
                           "numeracy" = "455",
                           "grammar_and_punctuation" = "454")
    grade <- gsub(".* ", "", subject_grade)
    folder_name <- paste0(path_out, "/", sa_code)
    dir.create(folder_name, showWarnings = FALSE)
    filename <- paste0(folder_name, "/acara_", tolower(subject_code), "_naplan_results_", tolower(grade), "_", tolower(sa_code), ".csv")
    write.csv(x, filename, row.names = FALSE)
  })
})







##------------------------------------------------------------------------ begin analysis on school attendance ---------------------------------------------------------------------------------------###

# note - on March 24 2023 - the data has been requested - the version that was delivered is not suitable for the analysis - pausing the work here

# R script: aedc_qa_cell_suppress
# Author: claire boulange
# Date: 2023 04 28
# Description: This script reads the cleaned data prepared on aedc, it then checks for formatting errors and prints all outputs to a txt file
# it then applies a cell suppression function - note here the cell suppression rule is n<5 and population < 15 are suppressed

# Libraries required
library(tidyverse)


# Set the path for the output text file - where you print findings of the QA
# Open a connection to the output file
output_file_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc/output_review_aedc.txt"

sink(output_file_path)

# Set the working directory - folder with data cleaned ready for QA
setwd("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc/")

# Define a list of acceptable values for the first column
first_col_check <- list("STE_CODE16", "SA2_CODE16", "SA3_CODE16", "SA4_CODE16", "LGA_CODE16", "Australia")


# Get a list of CSV files in the working directory
csv_files <- list.files(pattern="^aedc.*\\.csv$")
cat("Number of files in folder:", length(csv_files), "\n") # Print the number of files in the folder and their names
cat(paste(csv_files, collapse = "\n"), "\n")


df_list <- list()  # initialize an empty list to store data frame

# Read each CSV file into a data frame and store it in the df_list
for (file in csv_files) {
  if (endsWith(file, ".csv")) {  # only read in CSV files
    df <- read.csv(file)
    df_list[[file]] <- df  # append the data frame to the list using the file name as the key
  }
}


# Define a function to check if column names are in snake case format
is_snake_case <- function(x) {
  all(grepl("^[a-z]+(_[a-z]+)*$", x))
}

# Define a regex pattern to check age_group format
age_group_regex <- "^\\d{1,2}-\\d{1,2}$|^\\d{1,2}-\\d{2}$|^\\d{2}-\\d{2}$"

# Define a list of acceptable values for the first column
first_col_check <- list("STE_CODE16", "SA2_CODE16", "SA3_CODE16", "SA4_CODE16", "LGA_CODE16", "Australia")

# Iterate through each data frame in df_list and perform checks
for (df_name in names(df_list)) {
  df <- df_list[[df_name]]
  col_names <- names(df)
  first_col <- colnames(df)[1]
  
  # check if age_group and sex columns exist
  if (!("age_group" %in% col_names)) {
    cat("Error: age_group column not found in", df_name, "\n")
  }
  if (!("sex" %in% col_names)) {
    cat("Error: sex column not found in", df_name, "\n")
  }
  
  # check if age_group values are in the correct format
  if ("age_group" %in% col_names) {
    age_group_values <- df$age_group
    if (!all(grepl(age_group_regex, age_group_values))) {
      cat("Error: age_group values in", df_name, "are not in the correct format (expected format: \\d-\\d, \\d-\\d{2}, or \\d{2}-\\d{2})\n")
    }
  }
  
  # check if all column names except the first column are in snake case
  if (!is_snake_case(col_names[-1])) {
    cat("Error: column names in", df_name, "are not in snake case format (lowercase words separated by underscores)\n")
  }
  
  # check if geography column is one of the acceptable values
  if (!(first_col %in% first_col_check)) {
    cat("Error: geography column is not one of the acceptable values (", paste(first_col_check, collapse=", "), ") in", df_name, "\n")
  }
  
  # print unique values of age_group, calendar_year, and sex
  cat("Unique values in age_group for", df_name, ":\n")
  cat(unique(df$age_group), "\n\n")
  
  cat("Unique values in calendar_year for", df_name, ":\n")
  cat(unique(df$calendar_year), "\n\n")
  
  cat("Unique values in sex for", df_name, ":\n")
  cat(unique(df$sex), "\n\n")
}

# -------------------------------------------------------------------------------------------------------------------------------
#
#                                 HECK OUTPUTS NOW - GO BACK TO YOUR CODE IF ANY FORMATTING ERRORS DETECTED
#
#---------------------------------------------cell suppression-------------------------------------------------------------------
# Define the input directory path
input_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc"

# Define the output directory path for the cleaned data + cell suppression applied
output_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc/cell_suppressed/"

# Create the output directory if it doesn't already exist
dir.create(output_dir, showWarnings = FALSE)

# Get a list of all CSV files in the input directory
csv_files <- list.files(input_dir, pattern = ".csv$", full.names = TRUE)

# Define the function to replace invalid values with suppression
replace_invalid_vals <- function(df, threshold = 15, suppression = "9999999") {
  valid_cols <- names(df)[endsWith(names(df), "_valid")]
  for (col in valid_cols) {
    invalid_cells <- which(df[[col]] < threshold)
    df[invalid_cells, -(1:4)] <- suppression
  }
  
  n_cols <- names(df)[startsWith(names(df), "n_")]
  for (col in n_cols) {
    invalid_cells <- which(df[[col]] %in% 1:4)
    df[invalid_cells, -(1:4)] <- suppression
  }
  
  return(df)
}


# Loop through each CSV file and apply cell suppression
for (file in csv_files) {
  
  # Read in the CSV file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Apply cell suppression to the data frame
  df <- replace_invalid_vals(df)
  
  # Write the modified data frame to a new CSV file in the output directory
  out_file <- file.path(output_dir, basename(file))
  write.csv(df, out_file, row.names = FALSE)
  
  # Print the name and path of the file created
  cat(sprintf("File created: %s\n", out_file))
}



# Close the connection to the output file
sink()

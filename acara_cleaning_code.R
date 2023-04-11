# Title: ACARA data for ANCHDA
# Description: This script reads the ACARA data from the excel documents and reorganise it in separate CSVs - one per indicator.
# Author: Claire Boulange
# Date: completed on March 24, 2023

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

#set working directory and options
acara_folder <- "Z:/CDA/Claire WD/anchda/ACARA/ACARA/"
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


# Rename the columns to VISER standards and convert to lower case, and add age_group column
list_of_tables <- lapply(naplan_results, function(x) {
  x <- x %>% 
    mutate(age_group = case_when(
      `Student Grade Level` == "Year 3" ~ "8-9",
      `Student Grade Level` == "Year 5" ~ "10-11",
      `Student Grade Level` == "Year 7" ~ "12-13",
      `Student Grade Level` == "Year 9" ~ "14-15",
      TRUE ~ NA_character_
    ))
  setNames(x, c(paste0(toupper(names(x)[1]), "_CODE16"), "calendar_year", 
                tolower(c(names(x)[3], "Student Grade Level", "NAPLAN Score", "age_group"))))
})

# Define output directory
path_out <- "Z:/CDA/Claire WD/indicators_outputs/temp/acara_naplan_results/"

# Create output directory if it doesn't exist
if (!dir.exists(path_out)) {
  dir.create(path_out)
}

# Loop through each tibble in list_of_tables
for (i in seq_along(list_of_tables)) {
  
  # Clean column names
  colnames(list_of_tables[[i]])[1] <- toupper(colnames(list_of_tables[[i]])[1])
  colnames(list_of_tables[[i]]) <- gsub(" ", "_", tolower(colnames(list_of_tables[[i]])))
  colnames(list_of_tables[[i]])[2] <- "year"
  colnames(list_of_tables[[i]])[3] <- paste0(colnames(list_of_tables[[i]])[3], "_acara")
  
  # Extract suffix from tibble name
  suffix <- substr(names(list_of_tables)[i], nchar("naplan_results_by_") + 1, nchar(names(list_of_tables)[i]))
  
  # Create subdirectory based on suffix
  dir_path <- file.path(path_out, gsub("_", "", suffix))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  # Define file path
  file_path <- file.path(dir_path, paste0("acara_naplan_results_", suffix, ".csv"))
  
  # Write tibble to CSV
  write.csv(list_of_tables[[i]], file_path, row.names = FALSE)
  
}



##------------------------------------------------------------------------ begin analysis on school attendance ---------------------------------------------------------------------------------------###

# note - alternative data here - new data request sent to acara 


# Get list of Student Attendance files, ignoring temp files
student_attendance <- list.files(pattern = "Student Attendance.*\\.xlsx", ignore.case = TRUE)
student_attendance <- student_attendance[!grepl("~$", student_attendance)]

# Read each file into a list of data frames
student_attendance_df <- lapply(student_attendance, read_excel, sheet = 2)


# Remove spaces and file extension from file names, and convert to lower case
names_datasets <- tolower(gsub("\\.xlsx", "", gsub(" ", "_", student_attendance)))
names(student_attendance_df) <- names_datasets

# Error handling loop to identify NULL data frames
student_attendance_df <- lapply(student_attendance_df, function(df) {
  if (is.null(df)) {
    return(data.frame())
  } else {
    return(df)
  }
})

# Loop through each tibble in the list and select the required columns
for (i in seq_along(student_attendance_df)) {
  student_attendance_df[[i]] <- student_attendance_df[[i]][, c(2, 4, 6)]
}

# Define output directory
path_out <- "Z:/CDA/Claire WD/indicators_outputs/temp/acara_student_attendance"

# Create output directory if it doesn't exist
if (!dir.exists(path_out)) {
  dir.create(path_out)
}


# Loop through each tibble in student_attendance_df
for (i in seq_along(student_attendance_df)) {
  # Rename columns
  colnames(student_attendance_df[[i]])[1] <- paste0(colnames(student_attendance_df[[i]])[1], "_code16")
  colnames(student_attendance_df[[i]]) <- gsub(" ", "_", tolower(colnames(student_attendance_df[[i]])))
  colnames(student_attendance_df[[i]])[3] <- paste0(colnames(student_attendance_df[[i]])[3], "_acara")
  colnames(student_attendance_df[[i]])[2] <- "year"
  
  # Extract suffix from tibble name
  suffix <- substr(names(student_attendance_df)[i], nchar("student_attendance_by_") + 1, nchar(names(student_attendance_df)[i]))
  
  # Create subdirectory based on suffix
  dir_path <- file.path(path_out, gsub("_", "", suffix))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  # Define file path
  file_path <- file.path(dir_path, paste0("acara_student_attendance_rate_", suffix, ".csv"))
  
  # Write tibble to CSV
  write.csv(student_attendance_df[[i]], file_path, row.names = FALSE)
} 



#end of script#--------------------------------------------------------------------------------------------------------------------------------





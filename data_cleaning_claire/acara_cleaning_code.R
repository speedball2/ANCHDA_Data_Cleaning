# Title: ACARA data for ANCHDA
# Description: This script reads the ACARA data from the excel documents
# this script creates indicators for naplan results
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
    filter(Domain %in% c("Reading", "Writing", "Spelling", "Grammar and Punctuation", "Numeracy")) %>%
    filter(`Average NAPLAN Score` >= 0)
})


list_of_tables <- lapply(naplan_results, function(x) {
  x <- x %>%
    rename_with(~paste0(.,"_CODE16"), 1) %>% # Add suffix _CODE16 to the first column name
    rename(student_grade_level = `Student Grade Level`, # Rename column 4 to "student_grade_level"
           naplan_score_acara = `Average NAPLAN Score`, # Rename column 5 to "naplan_score_acara"
           calendar_year = `Calendar Year`, # Rename column 6 to "calendar_year"
           domain = Domain # Rename column 7 to "domain"
    ) %>% 
    mutate(age_group = case_when(
      student_grade_level == "Year 3" ~ "8-8",
      student_grade_level == "Year 5" ~ "10-10",
      student_grade_level == "Year 7" ~ "12-12",
      student_grade_level == "Year 9" ~ "14-14",
      TRUE ~ NA_character_
    ),
    sex = "all" # Add a new column called sex and set the value to "all" for all rows
    ) %>%
    select(1, sex, age_group, student_grade_level, calendar_year, domain, naplan_score_acara) %>%
    setNames(c(paste0(toupper(names(x)[1]), "_CODE16"), "sex", "age_group", 
               "student_grade_level", "calendar_year", "domain", "naplan_score_acara"))
})


# Split the filtered tibbles into separate tables based on domain, student grade level
list_of_tables <- lapply(list_of_tables, function(x) {
  split(x, list(Name = paste(x$domain, x$student_grade_level, sep = "_")))
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





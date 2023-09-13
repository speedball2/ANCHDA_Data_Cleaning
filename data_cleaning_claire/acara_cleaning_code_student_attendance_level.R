# Title: ACARA data for ANCHDA
# Description: This script reads the ACARA data from the excel documents and reorganise it in separate CSVs - one per indicator.
# Author: Claire Boulange
# Date: completed on 

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

#set working directory and options
acara_folder <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/acara/"
path_out = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/acara/outputs"

acara_folder <- "./data/ACARA"
path_out = "./output/ACARA/attendance_level"

# Create output directory if it doesn't exist
if (!dir.exists(path_out)) {
  dir.create(path_out)
}

options(timeout = 600) 
setwd(acara_folder)


##----------------------------------------------------------------------------attendance level---------------------------------------------------------###


# Get list of Student Attendance files, ignoring temp files
student_attendance <- list.files(pattern = "Student Attendance by.*\\.xlsx", ignore.case = TRUE)
student_attendance <- student_attendance[!grepl("~$", student_attendance)]

# Read each file into a list of data frames
student_attendance_df <- lapply(student_attendance, read_excel, sheet = 2)


# Remove spaces and file extension from file names, and convert to lower case
names_datasets <- tolower(gsub("\\.xlsx", "", gsub(" ", "_", student_attendance)))
names(student_attendance_df) <- names_datasets





# Define a function to clean the data for each tibble in the list
clean_data <- function(df) {
  
  # Filter rows where Term is "Semester 1"
  df <- df[df$`Term Semester` == "Semester 1", ]
  # Select the required columns
  df <- df[, c(2, 4, 5, 6, 11)] # take attendance level
  
  # Rename columns
  
  colnames(df)[1] <- paste0(colnames(df)[1], "_code16")
  colnames(df) <- gsub(" ", "_", tolower(colnames(df)))
  colnames(df)[2] <- "calendar_year"
  colnames(df)[1] <- toupper(colnames(df)[1])  # Change the column name at index 1 to uppercase
  code_name <-  colnames(df)[1]
  # Recode the values in school_sector
  df <- df %>%
    mutate(school_sector = case_when(
      school_sector == "Government" ~ "government",
      school_sector %in% c("Catholic", "Independent") ~ "non_government"
    ))
  
  # Recode the values in school_type
  df <- df %>%
    mutate(school_type = case_when(
      school_type == "Primary" ~ "primary",
      school_type == "Secondary" ~ "secondary",
      school_type == "Combined" ~ "combined"
    ))
  
  # Create age_group based on the values in school_type
  df <- df %>% 
    mutate(age_group = case_when(
      school_type == "primary" ~ "6-11",
      school_type == "secondary" ~ "12-15",
      school_type == "combined" ~ "6-15"
    )) %>%
    
    # Add a column for sex
    mutate(sex = "all") %>%
    
    # Select columns in the required order
    select(1, 7, 6, 2, 3, 4, 5)
  
  inicator_name <- colnames(df)[7]
  
  #as the non_government consist with two other sectors c("Catholic", "Independent") we have to take the average across that variable
  df <- df %>% group_by(eval(parse(text =  colnames(df)[1])) ,sex, age_group, school_sector, calendar_year, school_sector, school_type) %>% summarise(new_val = mean(eval(parse(text = names(df)[7]))))
  
  names(df)[1] <- code_name
  
  names(df)[which(names(df) == "new_val")] <- inicator_name
  # Return the cleaned dataframe
  return(df)
}

# Use lapply to clean each tibble in the list
student_attendance_df_clean <- lapply(student_attendance_df, clean_data)



# Filter tibbles based on their common pattern in names
lga_tables <- student_attendance_df_clean[grep("student_attendance_by_lga_", names(student_attendance_df_clean))]
sa2_tables <- student_attendance_df_clean[grep("student_attendance_by_sa2_", names(student_attendance_df_clean))]
sa3_tables <- student_attendance_df_clean[grep("student_attendance_by_sa3_", names(student_attendance_df_clean))]

# Combine tibbles with similar names using rbind
lga_combined <- do.call(rbind, lga_tables)
sa2_combined <- do.call(rbind, sa2_tables)
sa3_combined <- do.call(rbind, sa3_tables)


##----------------------------------------------------------------------------------------------------------------------##
## calculate the total attendance rate by geo-area


sa3_total <- sa3_combined %>%
  select(-c(school_sector, school_type, age_group)) %>%  # drop school_sector and school_type columns
  group_by(SA3_CODE16, sex, calendar_year) %>%
  summarize(attendance_level = mean(attendance_level))%>%
  mutate(age_group = "6-15", attendance_level = round(attendance_level, 2)) %>%  
  select(SA3_CODE16, sex, age_group, calendar_year, attendance_level)

sa2_total <- sa2_combined %>%
  select(-c(school_sector, school_type, age_group)) %>%  # drop school_sector and school_type columns
  group_by(SA2_CODE16, sex, calendar_year) %>%
  summarize(attendance_level = mean(attendance_level))%>%
  mutate(age_group = "6-15", attendance_level = round(attendance_level, 2)) %>%   
  select(SA2_CODE16, sex, age_group, calendar_year, attendance_level)

lga_total <- lga_combined %>%
  select(-c(school_sector, school_type, age_group)) %>%  # drop school_sector and school_type columns
  group_by(LGA_CODE16, sex, calendar_year) %>%
  summarize(attendance_level = mean(attendance_level))%>%
  mutate(age_group = "6-15", attendance_level = round(attendance_level, 2)) %>% 
  select(LGA_CODE16, sex, age_group, calendar_year, attendance_level)


file_name <- "acara_473_attendance_level_total_SA3.csv"
file_path <- paste(path_out, file_name, sep = "/")
write.csv(sa3_total, file_path, row.names = FALSE)

file_name <- "acara_473_attendance_level_total_SA2.csv"
file_path <- paste(path_out, file_name, sep = "/")
write.csv(sa2_total, file_path, row.names = FALSE)

file_name <- "acara_473_attendance_level_total_LGA.csv"
file_path <- paste(path_out, file_name, sep = "/")
write.csv(lga_total, file_path, row.names = FALSE)

##----------------------------------------------------------------------------------------------------------------------##
# Split the combined tibbles into separate tables based on school sector and school type
lga_combined <- split(lga_combined, list(lga_combined$school_sector, lga_combined$school_type))

sa2_combined <- split(sa2_combined, list(sa2_combined$school_sector, sa2_combined$school_type))

sa3_combined <- split(sa3_combined, list(sa3_combined$school_sector, sa3_combined$school_type))


# Export tibbles in lga_combined list to CSV files
for (i in seq_along(lga_combined)) {
  # Get unique values for school_sector and school_type
  #sector_type <- unique(lga_combined[[i]] %>% select(school_sector, school_type))
  
  school_type <- unique(lga_combined[[i]]$school_type)
  school_sector <- unique(lga_combined[[i]]$school_sector)
  # Construct file name
  file_name <- paste0("acara_473_attendance_level_", school_type, "_", school_sector, "_LGA.csv")
  
  # Export to CSV file
  write.csv(lga_combined[[i]], file.path(path_out, file_name), row.names = FALSE)
}

# Export tibbles in sa2_combined list to CSV files
for (i in seq_along(sa2_combined)) {
  # Get unique values for school_sector and school_type
  #sector_type <- unique(sa2_combined[[i]] %>% select(school_sector, school_type))
  school_type <- unique(lga_combined[[i]]$school_type)
  school_sector <- unique(lga_combined[[i]]$school_sector)
  # Construct file name
  file_name <- paste0("acara_473_attendance_level_", school_type, "_", school_sector, "_SA2.csv")
  
  # Export to CSV file
  write.csv(sa2_combined[[i]], file.path(path_out, file_name), row.names = FALSE)
}

# Export tibbles in sa3_combined list to CSV files
for (i in seq_along(sa3_combined)) {
  # Get unique values for school_sector and school_type
  #sector_type <- unique(sa3_combined[[i]] %>% select(school_sector, school_type))
  
  school_type <- unique(lga_combined[[i]]$school_type)
  school_sector <- unique(lga_combined[[i]]$school_sector)
  # Construct file name
  file_name <- paste0("acara_473_attendance_level_", school_type, "_", school_sector, "_SA3.csv")
  
  # Export to CSV file
  write.csv(sa3_combined[[i]], file.path(path_out, file_name), row.names = FALSE)
}



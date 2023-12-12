# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Read the Excel file and remove unwanted rows
data <- read_excel("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", sheet = 1, col_names = FALSE) #mothers = page 2
data <- data[-c(1, 3, 4), ]



# Step 2: Extract the first row and identify non-NA strings
first_row <- data[1, ]
non_na_strings <- na.omit(unlist(first_row))




# Step 3: Save non-NA strings in a list and their positions in another list
strings_list <- as.list(non_na_strings)
positions_list <- as.list(which(!is.na(first_row)))


clean_string <- function(str) {
  gsub("\\s*\\(.*\\)", "", str)
}

# Clean the strings_list
cleaned_strings_list <- lapply(strings_list, clean_string)

# Create a list to store the individual data frames
dataframes_list <- list()

# Get the total number of data columns
num_columns <- ncol(data)

# Loop through the cleaned strings_list to create the data frames
for (i in seq_along(positions_list)) {
  # Determine the start and end positions for each data frame
  start_pos <- positions_list[[i]]
  end_pos <- ifelse(i == length(positions_list), num_columns, positions_list[[i + 1]] - 1)
  
  # Extract the subset of columns for the current data frame
  current_df <- data[, start_pos:end_pos]
  
  # Assign a cleaned name to the data frame based on the corresponding string
  df_name <- cleaned_strings_list[[i]]
  names(current_df) <- paste("Column_", start_pos:end_pos, sep = "")
  
  # Add the current data frame to the list with the appropriate name
  dataframes_list[[df_name]] <- current_df
}


# Loop through each tibble in dataframes_list
for (df_name in names(dataframes_list)) {
  # Get the current tibble
  current_df <- dataframes_list[[df_name]]
  
  # Add column 1 and 2 from 'data' at positions 1 and 2 in the current tibble
  current_df <- cbind(data[, 1:2], current_df)
  
  # Update the tibble in dataframes_list with the modified one
  dataframes_list[[df_name]] <- current_df
}

# Loop through each tibble in dataframes_list
for (df_name in names(dataframes_list)) {
  # Get the current tibble
  current_df <- dataframes_list[[df_name]]
  
  # Use row 2 as column names
  col_names <- as.character(current_df[2, ])
  col_names[is.na(col_names)] <- "" # Replace NA values with empty strings
  colnames(current_df) <- col_names
  
  # Remove row 1 and row 2 from the current tibble
  current_df <- current_df[-c(1, 2), ]
  
  # Update the tibble in dataframes_list with the modified one
  dataframes_list[[df_name]] <- current_df
}


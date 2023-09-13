# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Step 1: Read the Excel file and remove unwanted rows
data <- read_excel("iadatasheet_physical.xlsx", sheet = 1, col_names = FALSE)
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




###########
# Function to rename columns
rename_columns <- function(data) {
  col_names <- colnames(data)
  new_col_names <- col_names
  for (i in seq_along(col_names)) {
    if (grepl("^\\d{4} to \\d{4}$", col_names[i])) {
      series_name <- col_names[i]
      new_col_names[i + 1] <- paste(series_name, "count", sep = " ")
      new_col_names[i + 2] <- paste(series_name, "pop", sep = " ")
    }
  }
  colnames(data) <- new_col_names
  return(data)
}


# Usage: Call the function with your data frame
df <- rename_columns(df)

# Convert the wide table to a long table using tidyr::pivot_longer
long_df <- pivot_longer(
  df,
  cols = starts_with("20"),   # Choose the columns representing the years
  names_to = "year_range",    # Name of the new column for year range
  values_to = "indicator",    # Name of the new column for indicator values
  names_prefix = "",          # Remove any prefix from year_range column
  values_drop_na = TRUE       # Drop rows with NA values (optional)
)

# Extract 'count' and 'pop' values from the 'indicator' column
long_df$count <- NA
long_df$pop <- NA
long_df$count[grepl("count", long_df$year_range)] <- long_df$indicator[grepl("count", long_df$year_range)]
long_df$pop[grepl("pop", long_df$year_range)] <- long_df$indicator[grepl("pop", long_df$year_range)]

# Remove the " count" and " pop" suffix from the 'year_range' column
long_df$year_range <- sub(" count| pop", "", long_df$year_range)

# Remove the 'indicator' column
long_df <- long_df[, -which(names(long_df) == "indicator")]

# Group by 'Codes', 'Names', and 'year_range', and combine rows
long_df <- long_df %>% 
  group_by(Codes, Names, year_range) %>% 
  summarize_all(funs(first(na.omit(.))))

# Calculate the indicator as count/pop * 10000
long_df$indicator <- as.numeric(long_df$count) / as.numeric(long_df$pop) * 10000

# Convert the "year_range" column to the desired format
long_df$year_range <- gsub("^(\\d{2})(\\d{2}) to (\\d{2})(\\d{2})$", "\\1\\2 - \\3\\4", long_df$year_range)



################################################################################


# Function to rename columns
rename_columns <- function(data) {
  col_names <- colnames(data)
  new_col_names <- col_names
  for (i in seq_along(col_names)) {
    if (grepl("^\\d{4} to \\d{4}$", col_names[i])) {
      series_name <- col_names[i]
      new_col_names[i + 1] <- paste(series_name, "count", sep = " ")
      new_col_names[i + 2] <- paste(series_name, "pop", sep = " ")
    }
  }
  colnames(data) <- new_col_names
  return(data)
}

# Function to process each data frame
process_dataframe <- function(df) {
  
  
  # Rename columns
  df <- rename_columns(df)
  
  # Convert the wide table to a long table using tidyr::pivot_longer
  long_df <- pivot_longer(
    df,
    cols = starts_with("20"),   # Choose the columns representing the years
    names_to = "year_range",    # Name of the new column for year range
    values_to = "indicator",    # Name of the new column for indicator values
    names_prefix = "",          # Remove any prefix from year_range column
    values_drop_na = TRUE       # Drop rows with NA values (optional)
  )
  
  # Extract 'count' and 'pop' values from the 'indicator' column
  long_df$count <- NA
  long_df$pop <- NA
  long_df$count[grepl("count", long_df$year_range)] <- long_df$indicator[grepl("count", long_df$year_range)]
  long_df$pop[grepl("pop", long_df$year_range)] <- long_df$indicator[grepl("pop", long_df$year_range)]
  
  # Remove the " count" and " pop" suffix from the 'year_range' column
  long_df$year_range <- sub(" count| pop", "", long_df$year_range)
  
  # Remove the 'indicator' column
  long_df <- long_df[, -which(names(long_df) == "indicator")]
  
  # Group by 'Codes', 'Names', and 'year_range', and combine rows
  long_df <- long_df %>% 
    group_by(Codes, Names, year_range) %>% 
    summarize_all(funs(first(na.omit(.))))
  
  # Calculate the indicator as count/pop * 10000
  long_df$indicator <- round(as.numeric(long_df$count) / as.numeric(long_df$pop) * 10000, 2)
  
  # Convert the "year_range" column to the desired format
  long_df$year_range <- gsub("^(\\d{2})(\\d{2}) to (\\d{2})(\\d{2})$", "\\1\\2 - \\3\\4", long_df$year_range)
  
  return(long_df)
}

# Apply the process_dataframe function to each data frame in dataframes_list
processed_dataframes_list <- lapply(dataframes_list, process_dataframe)

for (i in seq_along(processed_dataframes_list)) {
  # Replace '*' in the "count" column with 9999999
  processed_dataframes_list[[i]]$count[processed_dataframes_list[[i]]$count == '*'] <- 9999999
  
  # Replace '*' in the "pop" column with 9999999
  processed_dataframes_list[[i]]$pop[processed_dataframes_list[[i]]$pop == '*'] <- 9999999
  
  # Convert "count" column to numeric
  processed_dataframes_list[[i]]$count <- as.numeric(processed_dataframes_list[[i]]$count)
  
  # Convert "pop" column to numeric
  processed_dataframes_list[[i]]$pop <- as.numeric(processed_dataframes_list[[i]]$pop)
  
  # Identify rows where "count" column has 9999999 or 0
  rows_to_replace <- processed_dataframes_list[[i]]$count == 9999999 | processed_dataframes_list[[i]]$count == 0
  
  # Replace values in those rows (except for columns 1, 2, and 3) with 9999999
  processed_dataframes_list[[i]][rows_to_replace, -(1:3)] <- 9999999
}



for (i in seq_along(processed_dataframes_list)) {
  # Add a new column "age_group" in the 2nd position with values "0-18"
  processed_dataframes_list[[i]] <- dplyr::mutate(processed_dataframes_list[[i]], age_group = "0-18")
  
  # Add a new column "sex" in the 3rd position with values "all"
  processed_dataframes_list[[i]] <- dplyr::mutate(processed_dataframes_list[[i]], sex = "all")
  
  # Rename the column "Codes" to "SA2_CODE16"
  processed_dataframes_list[[i]] <- dplyr::rename(processed_dataframes_list[[i]], SA2_CODE16 = Codes)
  
  # Reorganize columns in the specified order
  processed_dataframes_list[[i]] <- processed_dataframes_list[[i]][c("SA2_CODE16", "age_group", "sex", "year_range", "count", "pop", "indicator")]
}


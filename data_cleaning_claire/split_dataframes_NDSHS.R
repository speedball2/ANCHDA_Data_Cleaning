# Set the directory path
directory_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NDSHS/cell_suppressed"

# List all files in the directory
file_list <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames <- list()

# Loop through each CSV file and read them into separate data frames
for (file_path in file_list) {
  # Extract the file name (without extension) as the data frame name
  data_frame_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the CSV file into a data frame
  data_frames[[data_frame_name]] <- read.csv(file_path)
}

# Initialize an empty list to store the resulting data frames
split_data_frames <- list()

# Loop through each data frame in data_frames
for (df_name in names(data_frames)) {
  df <- data_frames[[df_name]]
  
  # Extract the first 4 columns
  first_4_columns <- df[, 1:4]
  
  # Extract columns with "n_" prefix
  n_prefix_columns <- df[, grepl("^n_", colnames(df))]
  
  # Extract columns with "p_" prefix
  p_prefix_columns <- df[, grepl("^p_", colnames(df))]
  
  # Create tables combining first 4 and n_ prefix columns
  n_combined_table <- cbind(first_4_columns, n_prefix_columns)
  
  # Create tables combining first 4 and p_ prefix columns
  p_combined_table <- cbind(first_4_columns, p_prefix_columns)
  
  # Store the combined tables in split_data_frames
  split_data_frames[[paste(df_name, "_n", sep = "")]] <- n_combined_table
  split_data_frames[[paste(df_name, "_p", sep = "")]] <- p_combined_table
}


# Specify the output directory
output_directory <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NDSHS/cell_suppressed/split_csv"

# Create the output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Loop through each split data frame and save it as a CSV
for (df_name in names(split_data_frames)) {
  df <- split_data_frames[[df_name]]
  
  # Construct the output file path
  output_file <- file.path(output_directory, paste0(df_name, ".csv"))
  
  # Save the data frame as a CSV file
  write.csv(df, file = output_file, row.names = FALSE)
}

# Verify that the CSV files have been saved to the specified directory
cat("CSV files saved to:", output_directory, "\n")

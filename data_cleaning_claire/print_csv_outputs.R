# Specify the directory path
directory_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA"

# Get a list of all CSV files in the directory and its subdirectories
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Extract only the names of the CSV files
csv_file_names <- basename(csv_files)

# Print the names of the CSV files
print(csv_file_names)


# Create a data frame with the names of the CSV files
csv_file_names_df <- data.frame(File_Name = csv_file_names)

# Print the data frame
print(csv_file_names_df)

# Alternatively, you can export the data frame to a CSV file
write.csv(csv_file_names_df, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/csv_file_names_list.csv", row.names = FALSE)

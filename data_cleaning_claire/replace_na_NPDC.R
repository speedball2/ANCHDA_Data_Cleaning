# Set the directory path
directory_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NPDC"

# List all CSV files in the directory
file_list <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames <- list()

# Read each CSV file into a data frame and replace NA with 9999999
for (file_path in file_list) {
  # Read the CSV file into a data frame
  df <- read.csv(file_path)
  
  # Replace NA with 9999999 in the data frame
  df[is.na(df)] <- 9999999
  
  # Store the data frame in the list
  data_frames[[basename(file_path)]] <- df
}

# Now you have a list of data frames with NA replaced by 9999999


# Specify the output directory
output_directory <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NPDC/Na_replaced_csv"

# Create the output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Loop through each split data frame and save it as a CSV
for (df_name in names(data_frames)) {
  df <- data_frames[[df_name]]
  
  # Construct the output file path
  output_file <- file.path(output_directory, paste0(df_name, ".csv"))
  
  # Save the data frame as a CSV file
  write.csv(df, file = output_file, row.names = FALSE)
}

# Verify that the CSV files have been saved to the specified directory
cat("CSV files saved to:", output_directory, "\n")

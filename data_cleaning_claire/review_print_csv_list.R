library(tidyverse)

# Specify the directory
directory <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA"

# List all subdirectories
subdirectories <- list.dirs(directory, full.names = TRUE, recursive = TRUE)

# Initialize an empty data frame to store the results
result_table <- data.frame(Subfolder = character(), CSV_File = character(), stringsAsFactors = FALSE)

# Loop through each subdirectory
for (subdir in subdirectories) {
  # List CSV files in the current subdirectory
  csv_files <- list.files(subdir, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if there are CSV files in the current subdirectory
  if (length(csv_files) > 0) {
    # Create a data frame with relative subdirectory path and CSV file names
    subdirectory_table <- data.frame(Subfolder = gsub(paste0("^", directory), "", subdir),
                                     CSV_File = basename(csv_files),
                                     stringsAsFactors = FALSE)
    
    # Append the data frame to the result_table
    result_table <- bind_rows(result_table, subdirectory_table)
  }
}

# Print the result_table
print(result_table)


write.csv(result_table, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/list_csv_ready.csv", row.names = FALSE)

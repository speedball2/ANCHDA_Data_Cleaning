# R script: ccdsvu_qa_cell_suppress
# Author: claire boulange
# Date: 2023 04 28
# Description: This script reads the cleaned data prepared on ccdsvu, it then checks for formatting errors and prints all outputs to a txt file
# it then applies a cell suppression function - note here the cell suppression rule is n<5 

#---------------------------------------------cell suppression-------------------------------------------------------------------

# Define the input directory path
input_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/ccdsvu/"

# Define the output directory path
output_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/ccdsvu//cell_suppressed/"

# Create the output directory if it doesn't already exist
dir.create(output_dir, showWarnings = FALSE)

# Get a list of all CSV files in the input directory
csv_files <- list.files(input_dir, pattern = ".csv$", full.names = TRUE)

# Loop through each CSV file and apply cell suppression
for (file in csv_files) {
  
  # Read in the CSV file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Apply cell suppression
  df[df[, 5] < 5, -c(1:4)] <- 9999999
  
  # Write the modified data frame to a new CSV file in the output directory
  write.csv(df, file.path(output_dir, basename(file)), row.names = FALSE)
  
}

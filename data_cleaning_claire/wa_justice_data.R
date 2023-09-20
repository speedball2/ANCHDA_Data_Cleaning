# Load the readxl package if not already loaded
library(readxl)

file_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/cda_update/SPRA-8939 Telethon Child Development Atlas Response.xlsx"

# Load the Excel workbook
wb <- excel_sheets(file_path)


# Initialize a list to store data frames from sheets with "Table" in their names
table_data <- list()

# Loop through each sheet and read data into a data frame
for (sheet_name in wb) {
  sheet_data <- read_excel(file_path, sheet = sheet_name)
  if (grepl("Table", sheet_name, ignore.case = TRUE)) {
    table_data[[sheet_name]] <- sheet_data
  }
}

# Remove the first object from the list
table_data <- table_data[-1]

# Loop through each table in table_data
for (table_name in names(table_data)) {
  # Set row 2 as column names
  colnames(table_data[[table_name]]) <- table_data[[table_name]][2, ]
  
  # Remove the first two rows (including the header row)
  table_data[[table_name]] <- table_data[[table_name]][-c(1, 2), ]
}



library(readxl)
library(writexl)
# Set the path to the Excel workbook
excel_file <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/phidu_data_lga_aust.xls"
output_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/PHIDU_single_files"

# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)

# Read the Excel file
sheets <- excel_sheets(excel_file)

# Iterate over each sheet and export as separate Excel file
for (sheet in sheets) {
  sheet_data <- read_excel(excel_file, sheet = sheet)
  output_file <- paste0(output_dir, "/", sheet, ".xlsx")
  write_xlsx(sheet_data, output_file)
  
  cat("Sheet", sheet, "exported to", output_file, "\n")
}

cat("Export completed successfully!\n")

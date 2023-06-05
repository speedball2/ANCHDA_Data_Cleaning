# Set the path where you want to save the output file
output_file_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/output_review_NHS.txt"
# Open a connection to the output file
sink(output_file_path)
setwd("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/nhs_qa/")

#-------------------------------------------------------
first_col_check <- list("STE_CODE16", "SA2_CODE16", "SA3_CODE16", "SA4_CODE16", "LGA_CODE16", "Australia")
csv_files <- list.files("./output/" ,"*.csv", full.names=TRUE)  
cat("Number of files in folder:", length(csv_files), "\n")
cat(paste(csv_files, collapse = "\n"), "\n")


df_list <- list()  # initialize an empty list to store data frames

for (file in csv_files) {
  if (endsWith(file, ".csv")) {  # only read in CSV files
    df <- read.csv(file)
    df_list[[file]] <- df  # append the data frame to the list using the file name as the key
  }
}

for (file in names(df_list)) {
  # Get the column names of the current data frame
  col_names <- names(df_list[[file]])
  
  # Remove the first 4 columns from the list of column names
  #col_names <- col_names[-c(1:3)]
  
  # Print the remaining column names
  cat(paste("Columns in", file, ":\n"))
  cat(paste(col_names, collapse = "\n"), "\n\n")
}



# Close the connection to the output file
sink()





# print the unique values of age_group for each data frame in df_list
for (df in df_list) {
  
  df_name <- names(df)
  cat("Unique values of age_group in",df_name, ":\n")
  print(unique(df$age_group))
  cat("\n")
}


# print the unique values in year_range for each data frame in df_list
for (df_name in names(df_list)) {
  
  #df_name <- names(df)
  cat("Unique values in year_range for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$year_range), "\n\n")
}

# print the unique values in sex for each data frame in df_list
for (df_name in names(df_list)) {
  cat("Unique values in sex for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$sex), "\n\n")

}


for (df_name in names(df_list)) {
  cat("Unique values in STE_CODE16 for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$SA2_CODE16), "\n\n")
  
}

for (df_name in names(df_list)) {
  cat("Unique values in SA3_CODE16 for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$SA3_CODE16), "\n\n")
  
}
for (file in names(df_list)) {
  # Get the unique values in the 5th column of the current data frame
  unique_vals <- unique(df_list[[file]][[5]])
  
  # Print the unique values
  cat(paste("Unique values in the 5th column of", file, ":\n"))
  cat(paste(unique_vals, collapse = "\n"), "\n\n")
}



for (file in csv_files) {
  
  # read the CSV file
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # check that geography column is first column
  if (!names(data)[1] %in% first_col_check) {
    cat("Warning: First column name (", names(data)[1], ") in", file, "is not in the reference list:", paste(first_col_check, collapse = ", "), "\n")
  }
  
  # check that all column names except the first column are snake case
  non_snake_case_cols <- names(data)[-1][!grepl("^[a-z_]+$", names(data)[-1])]
  if (length(non_snake_case_cols) > 0) {
    cat("Warning: Column name(s) (", paste(non_snake_case_cols, collapse = ", "), ") in", file, "are not in snake case.\n")
  }
  
  # check that values in age_group are in the format of \d-\d, \d-\d{2}, or \d{2}-\d{2}
  if (any(!is.na(data$age_group) & !grepl("^\\d-\\d$|^\\d-\\d{2}$|^\\d{2}-\\d{2}$", data$age_group))) {
    cat("Warning: age_group values should be in the format of \\d-\\d, \\d-\\d{2}, or \\d{2}-\\d{2} in", file, "\n")
  }
  
  # check that the sex and age_group columns exist
  if (!("sex" %in% names(data))) {
    cat("Error: 'sex' column is missing in", file, "\n")
  }
  
  if (!("age_group" %in% names(data))) {
    cat("Error: 'age_group' column is missing in", file, "\n")
  }
  
  # print the names of all columns except the first three
  cat("Column names in", file, ":", paste(names(data)[2:length(names(data))], collapse = ", "), "\n")
  
# 
#   if (("age_group" %in% names(data)) & ("sex" %in% names(data))){
#     output_summary <- data %>%
#       count(eval(parse ( text = names(data)[1])), sex, age_group, sort = TRUE) 
#     names(output_summary)[1] <- names(data)[1]
#     print (output_summary)
#   }
  
  
  # print a blank line to separate the output for each file
  cat("\n")
}

setwd("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/nhs_qa/")

csv_files <- list.files() 
cat("Number of files in folder:", length(csv_files), "\n")
cat(paste(csv_files, collapse = "\n"), "\n")


df_list <- list()  # initialize an empty list to store data frames

for (file in csv_files) {
  if (endsWith(file, ".csv")) {  # only read in CSV files
    df <- read.csv(file)
    df_list[[file]] <- df  # append the data frame to the list using the file name as the key
  }
}

# print the unique values of age_group for each data frame in df_list
for (df in df_list) {
  cat("Unique values of age_group in", names(df), ":\n")
  print(unique(df$age_group))
  cat("\n")
}


# print the unique values in year_range for each data frame in df_list
for (df_name in names(df_list)) {
  cat("Unique values in year_range for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$year_range), "\n\n")
}

# print the unique values in sex for each data frame in df_list
for (df_name in names(df_list)) {
  cat("Unique values in sex for", df_name, ":\n")
  cat(unique(df_list[[df_name]]$sex), "\n\n")

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
  cat("Column names in", file, ":", paste(names(data)[4:length(names(data))], collapse = ", "), "\n")
  
  # print a blank line to separate the output for each file
  cat("\n")
}

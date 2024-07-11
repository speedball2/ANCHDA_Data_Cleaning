# Load the readxl package
library(readxl)

# Specify the file path
file_path <- "Z:/CDA/Data/Ngala/Ngala_IA.xlsx"

# Specify the sheet name
sheet_name <- "ngala"  # Change it to the actual sheet name

#extract following indicators:
#total calls
#average call length
#average age in weeks

# ------------------------------------------------------------------------------------------------- Total Calls ---------------------------
# Read the specified range (B1:BD36) from the Excel file
total_calls <- readxl::read_excel(file_path, sheet = sheet_name, range = "B1:T36")

# Extract the row with years from the data
years_row <- as.character(total_calls[1, -1])

# Rename the columns
colnames(total_calls) <- c("SA3_CODE16", years_row)
# Remove the first row
total_calls <- total_calls[-1, ]


# Pivot longer and add columns
total_calls_long <- total_calls %>%
  pivot_longer(cols = -SA3_CODE16, names_to = "calendar_year", values_to = "total_calls") %>%
  mutate(sex = "all", age_group = "0-7") %>%
  select(SA3_CODE16, age_group, sex, calendar_year, total_calls)

# ------------------------------------------------------------------------------------------------- average_call_length ---------------------------
# Read the "SA3_CODE16" column from the first column (B1:B36)
sa3_code <- readxl::read_excel(file_path, sheet = sheet_name, range = "B1:B36")

# Read the specified range (U1:AL36) from the Excel file
average_call_length <- readxl::read_excel(file_path, sheet = sheet_name, range = "U1:AL36")

# Combine "SA3_CODE16" and the rest of the data
average_call_length <- cbind(SA3_CODE16 = sa3_code, average_call_length)



# Extract the row with years from the data
years_row <- as.character(average_call_length[1, -1])

# Rename the columns
colnames(average_call_length) <- c("SA3_CODE16", years_row)
# Remove the first row
average_call_length <- average_call_length[-1, ]


# Pivot longer and add columns
average_call_length_long <- average_call_length %>%
  pivot_longer(cols = -SA3_CODE16, names_to = "calendar_year", values_to = "average_call_length") %>%
  mutate(sex = "all", age_group = "0-7") %>%
  select(SA3_CODE16, age_group, sex, calendar_year, average_call_length)


# ------------------------------------------------------------------------------------------------- average_age_in_weeks ---------------------------
# Read the "SA3_CODE16" column from the first column (B1:B36)
sa3_code <- readxl::read_excel(file_path, sheet = sheet_name, range = "B1:B36")

# Read the specified range (U1:AL36) from the Excel file
average_age_in_weeks <- readxl::read_excel(file_path, sheet = sheet_name, range = "AM1:BD36")

# Combine "SA3_CODE16" and the rest of the data
average_age_in_weeks <- cbind(SA3_CODE16 = sa3_code, average_age_in_weeks)



# Extract the row with years from the data
years_row <- as.character(average_age_in_weeks[1, -1])

# Rename the columns
colnames(average_age_in_weeks) <- c("SA3_CODE16", years_row)
# Remove the first row
average_age_in_weeks <- average_age_in_weeks[-1, ]


# Pivot longer and add columns
average_age_in_weeks_long <- average_age_in_weeks %>%
  pivot_longer(cols = -SA3_CODE16, names_to = "calendar_year", values_to = "average_age_in_weeks") %>%
  mutate(sex = "all", age_group = "0-7") %>%
  select(SA3_CODE16, age_group, sex, calendar_year, average_age_in_weeks)


#---------------------------------------------------------------------------------------------------- save files

# Save average_age_in_weeks_long
write.csv(average_age_in_weeks_long, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/ngala/CDA_1501_ngala_average_age_in_weeks_SA3.csv", row.names = FALSE)

# Save average_call_length_long
write.csv(average_call_length_long, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/ngala/CDA_1501_ngala_average_call_length_SA3.csv", row.names = FALSE)

# Save total_calls_long
write.csv(total_calls_long, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/ngala/CDA_1501_ngala_total_calls_SA3.csv", row.names = FALSE)


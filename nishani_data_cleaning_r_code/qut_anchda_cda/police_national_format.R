# this script is about preparing police data for national atlas
# 

# Libraries
library(openxlsx)
library(dplyr)
library(purrr)

# # Define file paths
# data_folder <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW"
# ref_geo_sa_file <- file.path(data_folder, "public_data/ASGS2016_SA2_SA3_SA4_code_name_matching_ref_csv/SA2_2016_AUST_no_geom.csv")
# ref_geo_lga_file <- file.path(data_folder, "public_data/ASGS2016_SA2_SA3_SA4_code_name_matching_ref_csv/lga_2016_no_geom.csv")
# erp_lga_file <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/ERP/ABS_ERP_181_ERP_LGA.csv"
# erp_sa2_file <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/ERP/ABS_ERP_181_ERP_SA2.csv"
# erp_sa3_file <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/ERP/ABS_ERP_181_ERP_SA3.csv"
# response_file <- file.path(data_folder, "cda_data/SPRA-8939 Telethon Child Development Atlas Response.xlsx")

# Define file paths
#data_folder <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW"
ref_geo_sa_file <- "./data/SA2_2016_AUST_no_geom.csv"
ref_geo_lga_file <- "./data/lga_2016_no_geom.csv"
erp_lga_file <- "./data/ABS_ERP_181_ERP_LGA.csv"
erp_sa2_file <- "./data/ABS_ERP_181_ERP_SA2.csv"
erp_sa3_file <- "./data/ABS_ERP_181_ERP_SA3.csv"
response_file <- "./data/SPRA-8939 Telethon Child Development Atlas Response.xlsx"




# Function to load data and rename columns
load_and_rename_data <- function(file, sheet_name) {
  data <- read.xlsx(file, sheet = sheet_name)
  colnames(data) <- data[1, ]
  data <- data[-1, ]
  return(data)
}


# Function to remove age category column
remove_age_category_column <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>% select(-`Age Category`)
    return(df)
  })
  return(cleaned_data_list)
}

# Load reference data
ref_geo_sa <- read.csv(ref_geo_sa_file)
ref_geo_lga <- read.csv(ref_geo_lga_file)

# Prepare tables for join
ref_geo_sa2 <- ref_geo_sa[, c("SA2_MAINCODE_2016", "SA2_NAME_2016")]
ref_geo_sa3 <- ref_geo_sa[, c("SA3_CODE_2016", "SA3_NAME_2016")]
ref_geo_sa3 <- distinct(ref_geo_sa3)
ref_geo_lga <- ref_geo_lga[, c("LGA_CODE16", "LGA_NAME16")]

# Remove specified suffixes from LGA_NAME16
ref_geo_lga$LGA_NAME16 <- gsub("\\s*\\(C\\)|\\s*\\(A\\)|\\s*\\(R\\)|\\s*\\(S\\)|\\s*\\(DC\\)|\\s*\\(M\\)|\\s*\\(SA\\)|\\s*\\(Qld\\)|\\s*\\(T\\)|\\s*\\(OT\\)|\\s*\\(Tas\\.\\)|\\s*\\(ACT\\)|\\s*\\(NT\\)|\\s*\\(Vic\\.\\)|\\s*\\(AC\\)|\\s*\\(RC\\)|\\s*\\(NSW\\)|\\s*\\(WA\\)", "", ref_geo_lga$LGA_NAME16)


# Load ERP data
erp_lga <- read.csv(erp_lga_file) %>% select(-estimated_regional_population_uncertainty_correspondence)
erp_sa2 <- read.csv(erp_sa2_file)
erp_sa3 <- read.csv(erp_sa3_file)

# Data loading for response data
sheet_names <- getSheetNames(response_file)
data_LGA <- list(
  Table4 = load_and_rename_data(response_file, "Table 4"),
  Table7 = load_and_rename_data(response_file, "Table 7")
)
data_SA2 <- list(
  Table6 = load_and_rename_data(response_file, "Table 6"),
  Table9 = load_and_rename_data(response_file, "Table 9")
)
data_SA3 <- list(
  Table5 = load_and_rename_data(response_file, "Table 5"),
  Table8 = load_and_rename_data(response_file, "Table 8")
)


##------------------------------------------------------------------------------

# Define a function to process ERP data
process_erp_data <- function(erp_data, code_column, age_column) {
  
  names(erp_data)[1] <- "geo"
  erp_data <- erp_data %>%
    mutate(age = sub("-.*", "", age_group)) %>%
    select(-age_group) %>%
    rename(age_group = age) %>%
    group_by(geo, calendar_year, sex) %>%
    summarize(
      estimated_resident_population = sum(estimated_regional_population[age_group >= 10 & age_group <= 24]),
      age_group = "10-24"
    )
  
   names(erp_data)[1] <- code_column
  
  
  return(erp_data)
}

# Process ERP data for different datasets
erp_lga <- process_erp_data(erp_lga, "LGA_CODE16", age_group)
erp_sa2 <- process_erp_data(erp_sa2, "SA2_CODE16", age_group)
erp_sa3 <- process_erp_data(erp_sa3, "SA3_CODE16", age_group)


# Function to join data frames by LGA and a reference data frame
join_data_LGA <- function(data_LGA, ref_geo_lga) {
  joined_data_LGA <- lapply(data_LGA, function(df) {
    left_join(df, ref_geo_lga, by = c("LGA" = "LGA_NAME16"))
  })
  return(joined_data_LGA)
}

# Function to reorganize and clean the joined data
reorganize_data_LGA <- function(joined_data_LGA) {
  reorganized_data_LGA <- lapply(joined_data_LGA, function(df) {
    df <- df %>%
      select(-LGA) %>%  # Remove the "LGA" column
      mutate(age_group = ifelse(`Age Category` == "Young Offender", "10-24", NA)) %>%  # Create age_group column
      filter(!is.na(age_group)) %>%  # Remove rows where age_group is NA
      mutate(sex = "all") %>%  # Add a new "sex" column with the value "all"
      rename(calendar_year = `Year Offence occurred`) %>%  # Rename Year Offence occurred to calendar_year
      select(LGA_CODE16, calendar_year, age_group, sex, everything())  # Reorganize the columns
    return(df)
  })
  return(reorganized_data_LGA)
}

# Function to remove the "Age Category" column
remove_age_category_column <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      select(-`Age Category`)
    return(df)
  })
  return(cleaned_data_list)
}

# Join and reorganize data
joined_data_LGA <- join_data_LGA(data_LGA, ref_geo_lga)
reorganized_data_LGA <- reorganize_data_LGA(joined_data_LGA)
reorganized_data_LGA <- remove_age_category_column(reorganized_data_LGA)

# List to store the joined data frames with ERP information
joined_data_LGA_erp <- list()

# Iterate through the list of reorganized data frames
for (table_name in names(reorganized_data_LGA)) {
  # Get the data frame by name
  table_data <- reorganized_data_LGA[[table_name]]
  
  #table_data[, "LGA_CODE16"] <- as.character(table_data[, "LGA_CODE16"])
  # Convert "calendar_year" to an integer in the reorganized data frame
  table_data <- table_data %>%
    mutate(calendar_year = as.integer(calendar_year))
  
  # Join with erp_lga based on LGA_CODE16, sex, age_group, and calendar_year
  joined_data <- table_data %>%
    left_join(erp_lga, by = c("LGA_CODE16", "sex", "age_group", "calendar_year"))
  
  # Store the joined data frame in the list
  joined_data_LGA_erp[[table_name]] <- joined_data
}




### sa2

# Function to join data frames by SA2 and a reference data frame
join_data_SA2 <- function(data_SA2, ref_geo_sa2) {
  joined_data_SA2 <- lapply(data_SA2, function(df) {
    left_join(df, ref_geo_sa2, by = c("Location:SA2" = "SA2_NAME_2016"))
  })
  return(joined_data_SA2)
}

# Function to reorganize and clean the joined data
reorganize_data_SA2 <- function(joined_data_SA2) {
  reorganized_data_SA2 <- lapply(joined_data_SA2, function(df) {
    df <- df %>%
      select(-`Location:SA2`) %>%  # Remove the "Location:SA2" column
      mutate(age_group = ifelse(`Age Category` == "Young Offender", "10-24", NA)) %>%  # Create age_group column
      filter(!is.na(age_group)) %>%  # Remove rows where age_group is NA
      mutate(sex = "all") %>%  # Add a new "sex" column with the value "all")
      rename(calendar_year = `Year Offence occurred`, SA2_CODE16 = SA2_MAINCODE_2016) %>%  # Rename Year Offence occurred to calendar_year and SA2_MAINCODE_2016 to SA2_CODE16
      select(SA2_CODE16, calendar_year, age_group, sex, everything())  # Reorganize the columns
    return(df)
  })
  return(reorganized_data_SA2)
}

# Function to remove the "Age Category" column
remove_age_category_column <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      select(-`Age Category`)
    return(df)
  })
  return(cleaned_data_list)
}

# Join and reorganize data
joined_data_SA2 <- join_data_SA2(data_SA2, ref_geo_sa2)
reorganized_data_SA2 <- reorganize_data_SA2(joined_data_SA2)
reorganized_data_SA2 <- remove_age_category_column(reorganized_data_SA2)

# List to store the joined data frames with ERP information
joined_data_SA2_erp <- list()

# Iterate through the list of reorganized data frames
for (table_name in names(reorganized_data_SA2)) {
  # Get the data frame by name
  table_data <- reorganized_data_SA2[[table_name]]
  
  #table_data[, "SA2_CODE16"] <- as.character(table_data[, "SA2_CODE16"])
  # Convert "calendar_year" to an integer in the reorganized data frame
  table_data <- table_data %>%
    mutate(calendar_year = as.integer(calendar_year))
  
  # Join with erp_SA2 based on SA2_CODE16, sex, age_group, and calendar_year
  joined_data <- table_data %>%
    left_join(erp_sa2, by = c("SA2_CODE16", "sex", "age_group", "calendar_year"))
  
  # Store the joined data frame in the list
  joined_data_SA2_erp[[table_name]] <- joined_data
}



### sa3

# Function to join data frames by SA3 and a reference data frame
join_data_SA3 <- function(data_SA3, ref_geo_sa3) {
  joined_data_SA3 <- lapply(data_SA3, function(df) {
    left_join(df, ref_geo_sa3, by = c("Location:SA3" = "SA3_NAME_2016"))
  })
  return(joined_data_SA3)
}

# Function to reorganize and clean the joined data
reorganize_data_SA3 <- function(joined_data_SA3) {
  reorganized_data_SA3 <- lapply(joined_data_SA3, function(df) {
    df <- df %>%
      select(-`Location:SA3`) %>%  # Remove the "Location:SA3" column
      mutate(age_group = ifelse(`Age Category` == "Young Offender", "10-24", NA)) %>%  # Create age_group column
      filter(!is.na(age_group)) %>%  # Remove rows where age_group is NA
      mutate(sex = "all") %>%  # Add a new "sex" column with the value "all")
      rename(calendar_year = `Year Offence occurred`, SA3_CODE16 = SA3_CODE_2016) %>%  # Rename Year Offence occurred to calendar_year and SA3_MAINCODE_2016 to SA3_CODE16
      select(SA3_CODE16, calendar_year, age_group, sex, everything())  # Reorganize the columns
    return(df)
  })
  return(reorganized_data_SA3)
}

# Function to remove the "Age Category" column
remove_age_category_column <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      select(-`Age Category`)
    return(df)
  })
  return(cleaned_data_list)
}

# Join and reorganize data
joined_data_SA3 <- join_data_SA3(data_SA3, ref_geo_sa3)
reorganized_data_SA3 <- reorganize_data_SA3(joined_data_SA3)
reorganized_data_SA3 <- remove_age_category_column(reorganized_data_SA3)

# List to store the joined data frames with ERP information
joined_data_SA3_erp <- list()

# Iterate through the list of reorganized data frames
for (table_name in names(reorganized_data_SA3)) {
  # Get the data frame by name
  table_data <- reorganized_data_SA3[[table_name]]
  
  #table_data[,"SA3_CODE16"] <- as.character(table_data[,"SA3_CODE16"])
  # Convert "calendar_year" to an integer in the reorganized data frame
  table_data <- table_data %>%
    mutate(calendar_year = as.integer(calendar_year))
  
  # Join with erp_SA3 based on SA3_CODE16, sex, age_group, and calendar_year
  joined_data <- table_data %>%
    left_join(erp_sa3, by = c("SA3_CODE16", "sex", "age_group", "calendar_year"))
  
  # Store the joined data frame in the list
  joined_data_SA3_erp[[table_name]] <- joined_data
}




# Function to remove rows with calendar_year equal to 2005 or 2022
remove_rows_by_year <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      filter(!(calendar_year %in% c(2005, 2022)))
    return(df)
  })
  return(cleaned_data_list)
}

# Apply the function to your data frame lists
joined_data_LGA_erp <- remove_rows_by_year(joined_data_LGA_erp)
joined_data_SA2_erp <- remove_rows_by_year(joined_data_SA2_erp)
joined_data_SA3_erp <- remove_rows_by_year(joined_data_SA3_erp)



# Function to convert the 7th and 8th columns to numeric in a data frame
convert_columns7_and_8_to_numeric <- function(data_list) {
  transformed_data_list <- lapply(data_list, function(df) {
    df[[7]] <- as.numeric(df[[7]])  # Convert the 7th column to numeric
    df[[8]] <- as.numeric(df[[8]])  # Convert the 8th column to numeric
    return(df)
  })
  return(transformed_data_list)
}

# Apply the function to your data frame list to convert the 7th and 8th columns to numeric
joined_data_LGA_erp <- convert_columns7_and_8_to_numeric(joined_data_LGA_erp)
joined_data_SA2_erp <- convert_columns7_and_8_to_numeric(joined_data_SA2_erp)
joined_data_SA3_erp <- convert_columns7_and_8_to_numeric(joined_data_SA3_erp)


###-----------------






















# Function to perform the conversion, create the "Proportion of" variable, and rename column 10
convert_and_create_proportion <- function(data_list) {
  transformed_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      #mutate(across(7:8, as.numeric)) %>%  # Convert the 8th and 9th columns to numeric
      mutate(proportion = (.[, 7] / .[, 8]) * 10000)  # Create the "Proportion of" per 10,000 persons
    # Rename column 10
    names(df)[9] <- paste0("Proportion per 10000 ", sub("Number of ", "", names(df)[7]))
    return(df)
  })
  return(transformed_data_list)
}

# Apply the function to each of the data frame lists
joined_data_SA3_erp_prop <- convert_and_create_proportion(joined_data_SA3_erp)
joined_data_SA2_erp_prop <- convert_and_create_proportion(joined_data_SA2_erp)
joined_data_LGA_erp_prop <- convert_and_create_proportion(joined_data_LGA_erp)




# Function to convert column names to snake case
convert_column_names <- function(data_list) {
  transformed_data_list <- lapply(data_list, function(df) {
    # Convert column names except the first one
    names(df)[-1] <- tolower(gsub(" ", "_", names(df)[-1]))
    return(df)
  })
  return(transformed_data_list)
}

# Apply the function to each of the data frame lists
joined_data_SA3_erp_prop <- convert_column_names(joined_data_SA3_erp_prop)
joined_data_SA2_erp_prop <- convert_column_names(joined_data_SA2_erp_prop)
joined_data_LGA_erp_prop <- convert_column_names(joined_data_LGA_erp_prop)


# Function to remove rows with calendar_year equal to 2005 or 2022
remove_rows_by_year <- function(data_list) {
  cleaned_data_list <- lapply(data_list, function(df) {
    df <- df %>%
      filter(!(calendar_year %in% c(2005, 2022)))
    return(df)
  })
  return(cleaned_data_list)
}

# Apply the function to your data frame lists
joined_data_LGA_erp_prop <- remove_rows_by_year(joined_data_LGA_erp_prop)
joined_data_SA2_erp_prop <- remove_rows_by_year(joined_data_SA2_erp_prop)
joined_data_SA3_erp_prop <- remove_rows_by_year(joined_data_SA3_erp_prop)

######### here I need to append new rows, create case for Offence = all, group by col[1:4] return sum for col[7] and unique val in col[8]

creating_all_offence <-function(data){
  
  geo <- names(data)[1]
  indicator <- names(data)[7]
  indicator_prop <- names(data)[9]
  names(data)[1] <- "geo_name"
  names(data)[7] <- "indicator"
  names(data)[9] <- "indicator_prop"
  
  new_rows <- data %>% group_by(geo_name, calendar_year, age_group, sex) %>%
    summarize(
      offence = "all",
      indicator = sum(as.numeric(indicator)),
      estimated_resident_population = unique(as.numeric(estimated_resident_population)),
      indicator_prop = indicator /estimated_resident_population * 10000
    )
  
  new_rows$offence_category <- "all"
  data <- rbind(data, new_rows)
  names(data)[1] <- geo
  names(data)[which(names(data) == "indicator")] <- indicator
  names(data)[which(names(data) == "indicator_prop")] <- indicator_prop
  return(data)
}

new_joined_data_LGA_erp_prop <- lapply(joined_data_LGA_erp_prop, function(x)creating_all_offence(x))
new_joined_data_SA2_erp_prop <- lapply(joined_data_SA2_erp_prop, function(x)creating_all_offence(x))
new_joined_data_SA3_erp_prop <- lapply(joined_data_SA3_erp_prop, function(x)creating_all_offence(x))




############


joined_data_LGA_erp_prop <- lapply(joined_data_LGA_erp_prop, function(df) {
  df %>%
    mutate(across(7:9, ~ifelse(df[[7]] <= 4, 9999999, .)))
})





joined_data_SA2_erp_prop <- lapply(joined_data_SA2_erp_prop, function(df) {
  df %>%
    mutate(across(7:9, ~ifelse(df[[7]] <= 4, 9999999, .)))
})



joined_data_SA3_erp_prop <- lapply(joined_data_SA3_erp_prop, function(df) {
  df %>%
    mutate(across(7:9, ~ifelse(df[[7]] <= 4, 9999999, .)))
})




# For joined_data_LGA_erp_prop
joined_data_LGA_erp_prop <- lapply(joined_data_LGA_erp_prop, function(df) {
  df %>%
    mutate(across(9, ~ifelse(is.infinite(.), 9999999, .)))
})

# For joined_data_SA2_erp_prop
joined_data_SA2_erp_prop <- lapply(joined_data_SA2_erp_prop, function(df) {
  df %>%
    mutate(across(9, ~ifelse(is.infinite(.), 9999999, .)))
})

# For joined_data_SA3_erp_prop
joined_data_SA3_erp_prop <- lapply(joined_data_SA3_erp_prop, function(df) {
  df %>%
    mutate(across(9, ~ifelse(is.infinite(.), 9999999, .)))
})












base_folder_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/cda_police"

# Function to save a table as CSV with specific columns
save_table_as_csv <- function(df, folder_path, geography_name, suffix) {
  # Select specific columns
  if (suffix == "7") {
    selected_columns <- df[, c(1, 2, 3, 4, 5, 6, 7)]
    col_name <- names(df)[7]
  } else if (suffix == "9") {
    selected_columns <- df[, c(1, 2, 3, 4, 5, 6, 9)]
    col_name <- names(df)[9]
  }
  
  # Create a file name based on geography name and col_name
  file_name <- file.path(folder_path, paste0("cda_data_",geography_name, "_", col_name, ".csv"))
  
  # Save the selected columns to a CSV file
  write.csv(selected_columns, file_name, row.names = FALSE)
}

# Save the tables for joined_data_SA3_erp
for (i in seq_along(joined_data_SA3_erp_prop)) {
  df <- joined_data_SA3_erp_prop[[i]]
  geography_name <- sub("_CODE16$", "", names(df)[1])
  
  # Save table with suffix "8"
  save_table_as_csv(df, base_folder_path, geography_name, "7")
  
  # Save table with suffix "10"
  save_table_as_csv(df, base_folder_path, geography_name, "9")
}

# Save the tables for joined_data_SA2_erp
for (i in seq_along(joined_data_SA2_erp_prop)) {
  df <- joined_data_SA2_erp_prop[[i]]
  geography_name <- sub("_CODE16$", "", names(df)[1])
  
  # Save table with suffix "8"
  save_table_as_csv(df, base_folder_path, geography_name, "7")
  
  # Save table with suffix "10"
  save_table_as_csv(df, base_folder_path, geography_name, "9")
}

# Save the tables for joined_data_LGA_erp
for (i in seq_along(joined_data_LGA_erp_prop)) {
  df <- joined_data_LGA_erp_prop[[i]]
  geography_name <- sub("_CODE16$", "", names(df)[1])
  
  # Save table with suffix "7"
  save_table_as_csv(df, base_folder_path, geography_name, "7")
  
  # Save table with suffix "9"
  save_table_as_csv(df, base_folder_path, geography_name, "9")
}









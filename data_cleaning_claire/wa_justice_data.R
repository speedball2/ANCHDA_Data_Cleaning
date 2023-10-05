# Load the required libraries
library(sf)
library(dplyr)
library(readxl)
library(purrr)

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


# Define the file path to the shapefile
shp_file <- "Z:\\CDA\\cda_update\\gis\\SA3_2021_AUST_SHP_GDA2020\\SA3_2021_AUST_GDA2020.shp"

# Read the shapefile
sa3_shapefile <- st_read(shp_file)
# Filter the shapefile
sa3_wa <- sa3_shapefile %>%
  filter(STE_NAME21 == "Western Australia")

# Extract only the desired columns from sa3_wa
sa3_table <- sa3_wa %>%
  select(SA3_CODE21, SA3_NAME21)

# Display the first few rows of the resulting table
head(sa3_table)

# List of data frames in table_data (assuming you only want to join tables 2, 5, and 8)
data_frames_list <- list(table_data$`Table 5`, table_data$`Table 8`)

# Function to join a data frame based on its name
join_data_with_sa3 <- function(data_frame) {
  data_frame_name <- deparse(substitute(data_frame))
  
  # Join the data frame with the sa3_table
  joined_data <- data_frame %>%
    left_join(sa3_table, by = c("Location:SA3" = "SA3_NAME21"))
  
  return(joined_data)
}

# Use purrr::map to apply the join function to the selected data frames
joined_data_list <- map(data_frames_list, join_data_with_sa3)

# Display the first few rows of the joined data frames (one at a time)

head(joined_data_list[[1]])  # For Table 2 probably can get rid of cause no age groups
head(joined_data_list[[2]])  # For Table 5


#7844

# Assuming your tibble is stored in joined_data_list[[2]]
data <- joined_data_list[[2]]

# Create an sf object using the existing 'geometry' column
sf_data <- st_as_sf(data, crs = 7844)

# Write the sf object to a shapefile
st_write(sf_data, "sa3_offences.shp")


# Assuming your tibble is stored in joined_data_list[[2]]
data <- joined_data_list[[2]]

# Create an sf object using the existing 'geometry' column
sf_data <- st_as_sf(data)

# Write the sf object to a GeoJSON file
st_write(sf_data, "sa3_offences.geojson", driver = "GeoJSON")
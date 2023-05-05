# Title: child care deserts study data
# Description: clean data from VU on child care deserts
# Author: Claire Boulange
# Date: completed on april 23

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
#set working directory and options
setwd("Z:/CDA/Claire WD/anchda")
path_out = "Z:/CDA/Claire WD/indicators_outputs/temp/"

options(timeout = 600) 

# Read the file into a data frame
file_to_clean <- "SA3 data Childcare accessibility.xlsx"
childcare_accessibility <- read_excel(file_to_clean)

#clean table - slice by geo and rename indicators:

#Pop_living_where_chidcare_insufficient_n 
#Pop_living_where_chidcare_insufficient_p
#Number_places_per_child_average
#Number_child_per_place_average



df3 <- childcare_accessibility %>%
  select(SA3_CODE16 = sa3_code_2016, 
         pop_living_where_childcare_insufficient_n = Pop_living_in_desert, 
         pop_living_where_childcare_insufficient_p = Pop_desert_percent, 
         n_places_per_child_average = Ave_places_per_child, 
         n_child_per_place_average = Ave_child_per_place) %>%
  mutate(across(where(is.numeric), 
                function(x) ifelse(round(x, 1) %% 1 == 0.5, 
                                   ceiling(x * 10) / 10, round(x, 2))))


df3_1 <- df3 %>% select(SA3_CODE16, pop_living_where_childcare_insufficient_n)
df3_2 <- df3 %>% select(SA3_CODE16, pop_living_where_childcare_insufficient_p)
df3_3 <- df3 %>% select(SA3_CODE16, number_places_per_child_average)
df3_4 <- df3 %>% select(SA3_CODE16, number_child_per_place_average)




write.csv(df3, file = file.path(path_out, "CCDSVU_413_child_care_availability_sa3.csv"), row.names = FALSE)

#---------------------------------------------cell suppression-------------------------------------------------------------------
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


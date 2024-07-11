# ----------------------------------------------- #
# --- SA2 Preschool Data Age - Demo for HREA  --- #
# ----------------------------------------------- #

# ----------- #
# --- SWD --- #
# ----------- #

# set working directory 

setwd("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_CLEAN/Data_cleaning_code")

# ----------------- #
# --- Libraries --- #
# ----------------- #

library(readxl)
library(tidyr)


# ----------------=----------------------------------------------------------------------------------------- #
# ---SA2 Preschool Data Age - Demo data for HREA (all years), 3,4,5,6 year olds who attended pre school  --- #
# ---------------------------------------------------------------------------------------------------------- #

temp_df <- cbind(
  read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/Preschool_age_SA2/Preschool_Attendance_SA2_Age_2014.xlsx",
             sheet = sheet_index,
             range = "C8:C2692", col_names = FALSE),
  read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
             sheet = sheet_index,
             range = "O8:P2692", col_names = FALSE),
  read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
             sheet = sheet_index,
             range = "Y8:Z2692", col_names = FALSE),
  read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
             sheet = sheet_index,
             range = "AI8:AJ2692", col_names = FALSE))

temp_df <- cbind(
  read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/Preschool_age_SA2/Preschool_Attendance_SA3_age_2013.xlsx",
             sheet = 1,
             range = "B11:E363", col_names = FALSE), 
  read_excel()
)


a <- attended_preschool
t <- "3_yr_olds"

colnames(temp_df) <- c(paste0(t,a))










temp_df
# Temporal concordance function for census data - drafting/testing


library(tidyverse) #for tidyr::fill()
library(readxl)


# Load correspondence pipeline


source("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/census_temporal_correspondence_cleaning_fn_v2.R")


#===================================================
# Applying 

#===================================================
# LGA

# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
GEO_TO <- "LGA_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
GEO_TYPE <- "LGA"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "LGA"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#===================================================
# SA2


# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
GEO_TO <- "SA2_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
GEO_TYPE <- "SA2"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "SLA"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#===================================================
# SA3

# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
GEO_TO <- "SA3_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
GEO_TYPE <- "SA3"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "SSD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)



#===================================================
# SA4

# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
  VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
  GEO_TO <- "SA4_CODE_2016"                                       # Target geography column
  FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
  GEO_TYPE <- "SA4"                                               # Type of target geometry (used for looping over list of correspondence files)
  GEO_TYPE_2006 <- "SD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)
  
temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#==================================================


# STATE

# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
GEO_TO <- "State"                                       # Target geography column
FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
GEO_TYPE <- "STE"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_COL_FINAL <- "STE_CODE16"

state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006, GEO_COL_FINAL = GEO_COL_FINAL)



#---------


# NATIONAL


# User inputs:
data_file_base <- "census_tenure_type"                               # Base file name for datasets e.g. census_tenure_type
VAR_NAME <- "tenure_type_dwelling"                                  # Name of the input variable column.
GEO_TO <- "Australia"                                       # Target geography column
FILTER_VARS <- c("tend_tenure_type")                            # Name of original data set filter variable(s).
GEO_TYPE <- "national"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_COL_FINAL <- "Australia"


state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006, GEO_COL_FINAL = GEO_COL_FINAL)





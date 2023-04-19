# Cleaning code for TableBuilder Data - CENSUS - Internet Connection

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


#-----------


# Load cleaning functions

source("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Initial_Cleaning/TB_Census_cleaning_fn_v2.R")


#-------------------

# APPLY

#2006

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_internet_connection", data_item_name ="internet_connection_dwelling", calendar_year=2006,code_or_name = "code", census_tag = "NEDD", census_filter_col_name = "nedd_type_of_internet_connection")

#------


#2011

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_internet_connection", data_item_name ="internet_connection_dwelling", calendar_year=2011,code_or_name = "code", census_tag = "NEDD", census_filter_col_name = "nedd_type_of_internet_connection")


#-----


#2016

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_internet_connection", data_item_name ="internet_connection_dwelling", calendar_year=2016,code_or_name = "code", census_tag = "NEDD", census_filter_col_name = "nedd_type_of_internet_connection")


#------


#2021 - NO DATA AVAILABLE



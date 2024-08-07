# Cleaning code for TableBuilder Data - CENSUS - Internet Connection

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


#----------


# Load cleaning function

source("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Initial_Cleaning/TB_Census_cleaning_fn_v1.R")




#-------------------

# APPLY

#2006

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn(data_file_base = "census_core_activity_need_assistance", data_item_name ="core_activity_need_for_assistance", calendar_year=2006,code_or_name = "code")

#------


#2011

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn(data_file_base = "census_core_activity_need_assistance", data_item_name ="core_activity_need_for_assistance", calendar_year=2011,code_or_name = "code")


#-----


#2016

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn(data_file_base = "census_core_activity_need_assistance", data_item_name ="core_activity_need_for_assistance", calendar_year=2016,code_or_name = "code")


#------


#2021

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

TB_Census_cleaning_fn(data_file_base = "census_core_activity_need_assistance", data_item_name ="core_activity_need_for_assistance", calendar_year=2021,code_or_name = "code")





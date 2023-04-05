# Cleaning code for TableBuilder Data - CENSUS - english_not_well

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


# Set WD to Census year folder
#setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")


# Load cleaning function

source("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Initial_Cleaning/TB_Census_cleaning_fn_v1.R")




#-------------------

# APPLY to english_not_well

#2006 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn(data_file_base = "census_english_notwell", data_item_name ="english_not_well", calendar_year=2006,code_or_name = "name")



#---------

#2011 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn(data_file_base = "census_english_notwell", data_item_name ="english_not_well", calendar_year=2011,code_or_name = "name")

#--------
#2016 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn(data_file_base = "census_english_notwell", data_item_name ="english_not_well", calendar_year=2016,code_or_name = "name")

#-----------

#2021 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

TB_Census_cleaning_fn(data_file_base = "census_english_notwell", data_item_name ="english_not_well", calendar_year=2021,code_or_name = "name")

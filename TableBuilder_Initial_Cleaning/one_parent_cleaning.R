# Cleaning code for TableBuilder Data - CENSUS - Religion

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item



# Load cleaning function

source("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Initial_Cleaning/TB_Census_cleaning_fn_v2.R")




#-------------------

# APPLY

#2006 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_one_parent", data_item_name ="one_parent_children", calendar_year=2006,code_or_name = "code", census_tag = "FMCF", census_filter_col_name = "fmcf_family_composition", claire_redownload = T)


#---------

#2011 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_one_parent", data_item_name ="one_parent_children", calendar_year=2011,code_or_name = "code", census_tag = "FMCF", census_filter_col_name = "fmcf_family_composition", claire_redownload = T)

#--------
#2016 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_one_parent", data_item_name ="one_parent_children", calendar_year=2016,code_or_name = "code", census_tag = "FMCF", census_filter_col_name = "fmcf_family_composition", claire_redownload = T)

#-----------

#2021 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

TB_Census_cleaning_fn_dwelling(data_file_base = "census_one_parent", data_item_name ="one_parent_children", calendar_year=2021,code_or_name = "code", census_tag = "FMCF", census_filter_col_name = "fmcf_family_composition", claire_redownload = T)

# Cleaning code for TableBuilder Data - CENSUS - Religion

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item



# Load cleaning functions

source("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Initial_Cleaning/TB_Census_cleaning_fn_v2.R")




#-------------------

# APPLY

#2006 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn_individual(data_file_base = "census_english_notwell", data_item_name ="n_children_and_young_people_who_speak_english_not_well_or_not_at_all", calendar_year=2006,code_or_name = "name", census_tag = "ENGP", census_filter_col_name = "engp_proficiency_in_spoken_english", claire_redownload = F)


#---------

#2011 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn_individual(data_file_base = "census_english_notwell", data_item_name ="n_children_and_young_people_who_speak_english_not_well_or_not_at_all", calendar_year=2011,code_or_name = "name", census_tag = "ENGP", census_filter_col_name = "engp_proficiency_in_spoken_english", claire_redownload = F)

#--------
#2016 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn_individual(data_file_base = "census_english_notwell", data_item_name ="n_children_and_young_people_who_speak_english_not_well_or_not_at_all", calendar_year=2016,code_or_name = "name", census_tag = "ENGP", census_filter_col_name = "engp_proficiency_in_spoken_english", claire_redownload = F)

#-----------

#2021 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

TB_Census_cleaning_fn_individual(data_file_base = "census_english_notwell", data_item_name ="n_children_and_young_people_who_speak_english_not_well_or_not_at_all", calendar_year=2021,code_or_name = "name", census_tag = "ENGLP", census_filter_col_name = "engp_proficiency_in_spoken_english", claire_redownload = F)

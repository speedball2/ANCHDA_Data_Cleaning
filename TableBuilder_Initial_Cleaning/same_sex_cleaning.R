# Cleaning code for TableBuilder Data - CENSUS - Internet Connection

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


#-----------


same_sex_cleaning_fn <- function(data_file_base, data_item_name, calendar_year,code_or_name,census_tag_1, census_filter_col_name_1, census_tag_2, census_filter_col_name_2, claire_redownload = T){
  
  # data_file_base = stem for file name e.g. for completed year 12 the files are named like "census_year12_LGA_2011.csv" so the base is "census_year12" (string)
  # data_item_name = name for variable in cleaned datasheet e.g. "Completed_Year12" (string)
  # calendar_year = one of 2006,2011,2016,2021 (numeric)
  # code_or_name = if geographies are exported as name, then "name" will get them converted to codes below (string)
  # census_tag = 4-letter code for census variable e.g. "FMCF" = family composition
  # census_filter_col_name = desired formatting for column name of census variable e.g. "fmcf_family_composition"
  # claire_redownload = if FALSE, year after geography in file name, if TRUE, year before geography
  
  
  if(calendar_year == 2006){
    geog_list <- c("LGA","SLA","SSD","SD","STE","national")
  }else{
    geog_list <- c("LGA","SA2","SA3","SA4","STE","national")
  }
  
  for(i in 1:6){
    
    if(claire_redownload == T){
      
      if(calendar_year == 2006){
        geog_list <- c("LGA","SLA","SSD","SD","STE","National")
      }else{
        geog_list <- c("LGA","SA2","SA3","SA4","STE","National")
      }
      
      data_temp_name <- paste0(data_file_base,"_",calendar_year,"_",geog_list[i],".csv")
    }else{
      data_temp_name <- paste0(data_file_base,"_",geog_list[i],"_",calendar_year,".csv")
    }
    
    data_temp <- read.csv(data_temp_name,skip=8,row.names=NULL,na.strings=c("","NA"), check.names=FALSE)  
    #Skips first 10 ("8") lines -- what read.csv sees as the size of the  header from TableBuilder export.
    #row.names=NULL was due to an error I was getting - due to mix of tab/comma delimitation in outputs... workaround below
    #na.strings to call empty cells NA so they can be filled down with tidyr::fill()
    #check.names=FALSE stops read.csv from editing the column names, leaves them unchanged from TableBuilder export
    
    #issue with header formatting tabs vs commas... need following lines to fix up colnames
    colnames(data_temp) <- colnames(data_temp)[2:ncol(data_temp)] #fix colnames issue
    data_temp <- data_temp[,1:(ncol(data_temp)-1)] #drop added NA column
    
    colnames(data_temp)[ncol(data_temp)] <- data_item_name #rename variable column with specified data_item_name
    
    # delete trailing crap rows
    trailing_rows <- which(is.na(data_temp[,ncol(data_temp)])) #identify NA's in final column (variable of interest)
    data_temp <- data_temp[-trailing_rows,] #deleting trailing rows (4)
    
    
    # fill down variables
    data_temp <- data_temp %>% fill(everything(),.direction="down")
    
    # Fix column names (FOR DWELLINGS / FAMILIES - ASGC for 2006)
    # Geography
    if(calendar_year==2006){
      if(i==1){names(data_temp)[grepl("Local Government Areas", names(data_temp)[])] <- paste0(geog_list[i],"_CODE_",calendar_year)} #change col name for geography up to state
      if(i>1 & i<5){names(data_temp)[grepl("ASGC", names(data_temp)[])] <- paste0(geog_list[i],"_CODE_",calendar_year)} #change col name for geography up to state
      if(i==5){names(data_temp)[grepl("ASGC", names(data_temp)[])] <- "State"} #change col name for geography up to state
      if(i==6){names(data_temp)[grepl("ASGC", names(data_temp)[])] <- "Australia"} #change col name for geography up to state
    }
    else{
      if(i<5){names(data_temp)[grepl(geog_list[i], names(data_temp)[])] <- paste0(geog_list[i],"_CODE_",calendar_year)} #change col name for geography up to state
      if(i==5){names(data_temp)[grepl("tate|TATE", names(data_temp)[])] <- "State"} #change col name for geography up to state
      if(i==6){names(data_temp)[grepl("tralia", names(data_temp)[])] <- "Australia"} #change col name for geography up to state
    }
    
    
    # filters age, sex
    # names(data_temp)[grepl("Sex", names(data_temp)[])] <- "sex"
    # names(data_temp)[grepl("Age", names(data_temp)[])] <- "age_group"
    
    # variable filter levels
    names(data_temp)[grepl(census_tag_1, names(data_temp)[])] <- census_filter_col_name_1
    # extra census filter for same sex parents
    
    if(calendar_year==2021 | calendar_year==2016){
      names(data_temp)[grepl("CACF", names(data_temp)[])] <- census_filter_col_name_2 #kludge fix for 2021
      print(paste0(census_tag_2, " not present, used CACF instead"))
    } else{
      names(data_temp)[grepl(census_tag_2, names(data_temp)[])] <- census_filter_col_name_2
    }
    
    # AGREE ON STANDARD FILTER NAMES - for now leave as what comes from TableBuilder
    
    # ADD HERE - Filter LEVELS
    
    
    #---------
    
    
    # If geog type = name, then  sub in code for name...
    
    #1. find geo type - geog_list[i]
    #2. find geo column 
    if(code_or_name == "name" & geog_list[i]!="national"){ #skip national - no code
      
      current_geog <- ifelse(geog_list[i]=="STE","STATE",geog_list[i]) #STE name only for 2006 - naming "STATE" for 2011 onwards
      
      relevant_name_file <- read.csv(paste0("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_name.csv"),skip=9,check.names=FALSE)
      relevant_names <- c(row.names(relevant_name_file)) #get list of relevant geography names
      
      relevant_code_file <- read.csv(paste0("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_code.csv"),skip=9,check.names=FALSE)
      relevant_codes <- c(row.names(relevant_code_file)) #get list of relevant geography codes
      
      
      geo_column_index <- which(grepl(current_geog,names(data_temp))) #What column is the relevant geography?
      
      #replace geography column with matching codes instead of names
      data_temp[,geo_column_index] <- relevant_codes[match(data_temp[,geo_column_index],relevant_names)] 
      
      
      #end of if(){} section for geography names -> codes
    }
    
    
    # save clean csv
    # path to destination (interim cleaned data folder)
    
    
    # Strip "LGA" string from front of LGA code
    if(geog_list[i]=="LGA"){
      data_temp[,grepl(geog_list[i], names(data_temp)[])] <- data_temp[,grepl(geog_list[i], names(data_temp)[])] %>% stringr::str_replace("^[A-Z]*", "")
    }
    
    
    interim_folder <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/"
    write.csv(data_temp,file=paste0(interim_folder,data_file_base,"_",geog_list[i],"_",calendar_year,"_INTERIM.csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
  }
  
}



#-------------------

# APPLY

#2006

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

same_sex_cleaning_fn(data_file_base = "census_same_sex_couple", data_item_name ="same_sex_couple_children", calendar_year=2006,code_or_name = "code", census_tag_1 = "SSCF", census_filter_col_name_1 = "sscf_same_sex_couple_indicator", census_tag_2 = "CDCF", census_filter_col_name_2 = "cdcf_count_of_dependent_children_in_family")

#------


#2011

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

same_sex_cleaning_fn(data_file_base = "census_same_sex_couple", data_item_name ="same_sex_couple_children", calendar_year=2011,code_or_name = "name", census_tag_1 = "SSCF", census_filter_col_name_1 = "sscf_same_sex_couple_indicator", census_tag_2 = "CDCF", census_filter_col_name_2 = "cdcf_count_of_dependent_children_in_family")


#-----


#2016

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

same_sex_cleaning_fn(data_file_base = "census_same_sex_couple", data_item_name ="same_sex_couple_children", calendar_year=2016,code_or_name = "code", census_tag_1 = "SSCF", census_filter_col_name_1 = "sscf_same_sex_couple_indicator", census_tag_2 = "CDCF", census_filter_col_name_2 = "cdcf_count_of_dependent_children_in_family")


#------


#2021

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

same_sex_cleaning_fn(data_file_base = "census_same_sex_couple", data_item_name ="same_sex_couple_children", calendar_year=2021,code_or_name = "code", census_tag_1 = "SSCF", census_filter_col_name_1 = "sscf_same_sex_couple_indicator", census_tag_2 = "CDCF", census_filter_col_name_2 = "cdcf_count_of_dependent_children_in_family")

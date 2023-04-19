# Cleaning code for TableBuilder Data - CENSUS - unemployment

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


# Set WD to Census year folder
#setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")


# Load cleaning function

unemployment_cleaning_fn <- function(data_file_base, data_item_name, calendar_year,code_or_name){
  
  # data_file_base = stem for file name e.g. for completed year 12 the files are named like "census_year12_LGA_2011.csv" so the base is "census_year12" (string)
  # data_item_name = name for variable in cleaned datasheet e.g. "Completed_Year12" (string)
  # calendar_year = one of 2006,2011,2016,2021 (numeric)
  # code_or_name = if geographies are exported as name, then "name" will get them converted to codes below (string)
  
  if(calendar_year == 2006){
    geog_list <- c("LGA","SLA","SSD","SD","STE","national")
  }else{
    geog_list <- c("LGA","SA2","SA3","SA4","STE","national")
  }
  
  for(i in 1:6){
    
    data_temp_name <- paste0(data_file_base,"_",geog_list[i],"_",calendar_year,".csv")
    
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
    
    # Fix column names
    if(i<5){names(data_temp)[grepl(geog_list[i], names(data_temp)[])] <- paste0(geog_list[i],"_CODE_",calendar_year)} #change col name for geography up to state
    if(i==5){names(data_temp)[grepl("tate|TATE", names(data_temp)[])] <- "State"} #change col name for geography up to state
    if(i==6){names(data_temp)[grepl("tralia", names(data_temp)[])] <- "National"} #change col name for geography up to state
    
    names(data_temp)[grepl("Sex", names(data_temp)[])] <- "sex"
    names(data_temp)[grepl("Age", names(data_temp)[])] <- "age_group"
    
    
    
    #---------
    
    
    # If geog type = name, then  sub in code for name...
    
    #1. find geo type - geog_list[i]
    #2. find geo column 
    if(code_or_name == "name" & geog_list[i]!="national"){ #skip national - no code
      
      current_geog <- ifelse(geog_list[i]=="STE","STATE",geog_list[i]) #STE name only for 2006 - naming "STATE" for 2011 onwards
      
      relevant_name_file <- read.csv(paste0("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_name.csv"),skip=9,check.names=FALSE)
      relevant_names <- c(row.names(relevant_name_file)) #get list of relevant geography names
      
      relevant_code_file <- read.csv(paste0("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_code.csv"),skip=9,check.names=FALSE)
      relevant_codes <- c(row.names(relevant_code_file)) #get list of relevant geography codes
      
      
      geo_column_index <- which(grepl(current_geog,names(data_temp))) #What column is the relevant geography?
      
      #replace geography column with matching codes instead of names
      data_temp[,geo_column_index] <- relevant_codes[match(data_temp[,geo_column_index],relevant_names)] 
      
      
      #end of if(){} section for geography names -> codes
    }
    
    
    
    #---------
    
    #Unemployment - pivot wider
    
    # 
    # if(calendar_year=2006){
    # data_temp <- data_temp %>% 
    #   group_by(`Labour Force Status - 2006 (LFS06P)`) %>%
    #   mutate(row = row_number()) %>%
    #   pivot_wider(names_from = `Labour Force Status - 2006 (LFS06P)`, values_from = unemployment) %>%
    #   select(-row)
    # } else if(calendar_year = 2011) {
    #   data_temp <- data_temp %>% 
    #     group_by(`Labour Force Status and Hours Worked Not Stated (LFHRP)`) %>%
    #     mutate(row = row_number()) %>%
    #     pivot_wider(names_from = `Labour Force Status and Hours Worked Not Stated (LFHRP)`, values_from = unemployment) %>%
    #     select(-row)
    # }else {
    #   data_temp <- data_temp %>% 
    #     group_by(`LFHRP Labour Force Status and Hours Worked Not Stated`) %>%
    #     mutate(row = row_number()) %>%
    #     pivot_wider(names_from = `LFHRP Labour Force Status and Hours Worked Not Stated`, values_from = unemployment) %>%
    #     select(-row)
    # }
    # 
    
    
    #data_temp <- data_temp %>% pivot_wider(names_from = 2, values_from = 5)

    
    if(geog_list[i]=="LGA"){
      data_temp[,grepl(geog_list[i], names(data_temp)[])] <- data_temp[,grepl(geog_list[i], names(data_temp)[])] %>% stringr::str_replace("^[A-Z]*", "")
    }
    
    # save clean csv
    # path to destination (interim cleaned data folder)
    
    interim_folder <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/"
    write.csv(data_temp,file=paste0(interim_folder,data_file_base,"_",geog_list[i],"_",calendar_year,"_INTERIM.csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
  }
  
}



#-------------------

# APPLY to unemployment

#2006 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

unemployment_cleaning_fn(data_file_base = "census_unemployment", data_item_name ="unemployment", calendar_year=2006,code_or_name = "code")



#---------

#2011 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

unemployment_cleaning_fn(data_file_base = "census_unemployment", data_item_name ="unemployment", calendar_year=2011,code_or_name = "code")

#--------
#2016 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

unemployment_cleaning_fn(data_file_base = "census_unemployment", data_item_name ="unemployment", calendar_year=2016,code_or_name = "code")

#-----------

#2021 - CODE

setwd("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

unemployment_cleaning_fn(data_file_base = "census_unemployment", data_item_name ="unemployment", calendar_year=2021,code_or_name = "code")

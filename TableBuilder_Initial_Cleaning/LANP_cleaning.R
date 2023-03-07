# Cleaning code for TableBuilder Data - CENSUS


# To use on your system, make sure you update file paths to the relevant local directory for OneDrive - find/replace "/Users/Current/" below

library(tidyverse) #for tidyr::fill()

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


# Set WD to Census year folder
#setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

LANP_Census_cleaning_fn <- function(data_file_base, data_item_name, calendar_year,code_or_name){
  
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
    
    data_temp_name <- paste0(data_file_base,"_",calendar_year,"_",geog_list[i],".csv")
    
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
    
    # check column names
    names(data_temp)[1] <- "Language Spoken at Home (LANP) - 2 Digit"
    
    # AGREE ON STANDARD FILTER NAMES - for now leave as what comes from TableBuilder
    
    # ADD HERE - Filter LEVELS
    
    
    #---------
    
    
    # If geog type = name, then  sub in code for name...
    
    #1. find geo type - geog_list[i]
    #2. find geo column 
    if(code_or_name == "name" & geog_list[i]!="national"){ #skip national - no code
      
      current_geog <- ifelse(geog_list[i]=="STE","STATE",geog_list[i]) #STE name only for 2006 - naming "STATE" for 2011 onwards
      
      relevant_name_file <- read.csv(paste0("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_name.csv"),skip=9,check.names=FALSE)
      relevant_names <- c(row.names(relevant_name_file)) #get list of relevant geography names
      
      relevant_code_file <- read.csv(paste0("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/ASGS_Codes_Names/",calendar_year,"_",geog_list[i],"_code.csv"),skip=9,check.names=FALSE)
      relevant_codes <- c(row.names(relevant_code_file)) #get list of relevant geography codes
      
      
      geo_column_index <- which(grepl(current_geog,names(data_temp))) #What column is the relevant geography?
      
      #replace geography column with matching codes instead of names
      data_temp[,geo_column_index] <- relevant_codes[match(data_temp[,geo_column_index],relevant_names)] 
      
      
      #end of if(){} section for geography names -> codes
    }
    
    
    
    # FOR LANP --> sum non-English/Not stated languages, add on
    
    
    #geog_names <- c("Local Government Area (LGA)","Statistical Local Area (SLA)","Statistical Subdivision (SSD)","Statistical Division (SD)","State/Territory (STE)","Australia")
    #if(calendar_year==2006){
    data_temp_sumlanguage <- data_temp %>% 
      #group_by(geog_names[i],`Age 5 Year Age Groups (AGEP)`,`Sex Male/Female (SEXP)`) %>% 
      group_by_at(c(2,3,4)) %>% 
      filter (`Language Spoken at Home (LANP) - 2 Digit` != "Not stated" & `Language Spoken at Home (LANP) - 2 Digit` != "English") %>% 
      mutate(Other = sum(language_spoken_at_home)) %>%  #count sum of not "English" or "Not stated" 
      select(-c(`Language Spoken at Home (LANP) - 2 Digit`,`language_spoken_at_home`)) %>% #drop first column
      ungroup() %>% #undo grouping
      distinct() %>% 
      pivot_longer(cols = Other, names_to = "Language Spoken at Home (LANP) - 2 Digit", values_to = "language_spoken_at_home")#keep unique rows
    
    
    data_temp_english <- data_temp %>% 
      filter (`Language Spoken at Home (LANP) - 2 Digit` == "Not stated" | `Language Spoken at Home (LANP) - 2 Digit` == "English")
    
    data_temp2 <- rbind(data_temp_english,data_temp_sumlanguage)
    # }else{
    #   data_temp_sumlanguage <- data_temp %>% 
    #     #group_by(geog_names[i],`Age 5 Year Age Groups (AGEP)`,`Sex Male/Female (SEXP)`) %>% 
    #     group_by_at(c(2,3,4)) %>% 
    #     filter (`LANP - 2 Digit Level` != "Not stated" & `LANP - 2 Digit Level` != "English") %>% 
    #     mutate(Other = sum(language_spoken_at_home)) %>%  #count sum of not "English" or "Not stated" 
    #     select(-c(`LANP - 2 Digit Level`,`language_spoken_at_home`)) %>% #drop first column
    #     ungroup() %>% #undo grouping
    #     distinct() %>% 
    #     pivot_longer(cols = Other, names_to = "LANP - 2 Digit Level", values_to = "language_spoken_at_home")#keep unique rows
    #   
    #   
    #   data_temp_english <- data_temp %>% 
    #     filter (`LANP - 2 Digit Level` == "Not stated" | `LANP - 2 Digit Level` == "English")
    #   
    #   data_temp2 <- rbind(data_temp_english,data_temp_sumlanguage)
    #   
    #   
    # }
    
    
    
    
    # save clean csv
    # path to destination (interim cleaned data folder)
    
    
    
    
    
    
    interim_folder <- "/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/"
    write.csv(data_temp2,file=paste0(interim_folder,data_file_base,"_",geog_list[i],"_",calendar_year,"_INTERIM.csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
  }
  
}


# TEST

## Set WD to raw data files

#setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

#TB_Census_cleaning_fn(data_file_base = "census_year12", data_item_name ="Completed_Year12", calendar_year=2006)


#--------

# "LANP Language Spoken at Home" # - English, Other, Not Stated


# APPLY to LANP Language Spoken at Home


#---------------
#2006 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

LANP_Census_cleaning_fn(data_file_base = "census_LANP", data_item_name ="language_spoken_at_home", calendar_year=2006,code_or_name = "name")

#---------------
#2011 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

LANP_Census_cleaning_fn(data_file_base = "census_LANP", data_item_name ="language_spoken_at_home", calendar_year=2011,code_or_name = "name")

#---------------
#2016 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

LANP_Census_cleaning_fn(data_file_base = "census_LANP", data_item_name ="language_spoken_at_home", calendar_year=2016,code_or_name = "name")

#---------------
#2021 - NAME

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

LANP_Census_cleaning_fn(data_file_base = "census_LANP", data_item_name ="language_spoken_at_home", calendar_year=2021,code_or_name = "name")


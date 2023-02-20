# Cleaning code for TableBuilder Data - CENSUS - Tenure Type

library(tidyverse)
library(readr)

# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


# Set WD to Census year folder
#setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn <- function(data_file_base, data_item_name, calendar_year,code_or_name){
  
  if(calendar_year == 2006){
    geog_list <- c("LGA","SLA","SSD","SD","STE","national")
  }else{
    geog_list <- c("LGA","SA2","SA3","SA4","STE","national")
  }
  
  for(i in 1:length(geog_list)){ #loop over the 6 geography types
    
    data_temp_name <- paste0(data_file_base,"_",geog_list[i],"_",calendar_year,".csv")
    data_temp <- read.csv(data_temp_name,skip=8,row.names=NULL,na.strings=c("","NA"), check.names=FALSE)  
    #Skips first 10 ("8") lines -- what read.csv sees as the size of the  header from TableBuilder export.
    #row.names=NULL was due to an error I was getting - due to mix of tab/comma delimitation in outputs... workaround below
    #na.strings to call empty cells NA so they can be filled down with tidyr::fill()
    #check.names=FALSE stops read.csv from editing the column names, leaves them unchanged from TableBuilder export
    
    #issue with header formatting tabs vs commas... need following lines to fix up colnames
    colnames(data_temp) <- colnames(data_temp)[2:ncol(data_temp)] #fix colnames issue
    data_temp <- data_temp[,1:(ncol(data_temp)-1)] #drop added NA column
    colnames(data_temp)[ncol(data_temp)] <- data_item_name
    
    
    # delete trailing crap rows
    trailing_rows <- which(is.na(data_temp[,ncol(data_temp)]))
    data_temp <- data_temp[-trailing_rows,] #deleting trailing rows (4)
    
    
    # fill down vars
    
    data_temp <- data_temp %>% fill(everything(),.direction="down")
    
    # check column names
    
    names(data_temp)
    
    
    
    # If geog type = name, then  sub in code for name
    
    #1. find geo type - geog_list[i]
    #2. find geo column 
    if(code_or_name == "name"){
      geo_column_index <- which(grepl(geog_list[i],names(data_temp))) #What column is the relevant geom?
      #data_temp[,geo_column_index] <- XXXXYYYYZZZ_FINISH_THIS_FUNCTION
    }
    
    
    # save clean csv
    # path to destination (interim cleaned data folder)
    
    interim_folder <- "/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Correspondence/"
    write.csv(data_temp,file=paste0(interim_folder,data_file_base,"_",geog_list[i],"_",calendar_year,"_INTERIM.csv"),row.names=FALSE)
  }
  
}




#-------------------

# APPLY to Religion

#2006

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2006")

TB_Census_cleaning_fn(data_file_base = "census_tenure_type", data_item_name ="Dwelling_Tenure_Type", calendar_year=2006,code_or_name = "code")

#------


#2011

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2011")

TB_Census_cleaning_fn(data_file_base = "census_tenure_type", data_item_name ="Dwelling_Tenure_Type", calendar_year=2011,code_or_name = "code")


#-----


#2016

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2016")

TB_Census_cleaning_fn(data_file_base = "census_tenure_type", data_item_name ="Dwelling_Tenure_Type", calendar_year=2016,code_or_name = "code")


#------


#2021

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/TableBuilder_Data/Census/Census_2021")

TB_Census_cleaning_fn(data_file_base = "census_tenure_type", data_item_name ="Dwelling_Tenure_Type", calendar_year=2021,code_or_name = "code")





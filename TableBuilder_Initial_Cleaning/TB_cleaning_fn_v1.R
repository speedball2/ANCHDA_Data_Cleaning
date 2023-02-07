# Cleaning code for TableBuilder Data


# function (data_item(s), calendar_year, geography)

# Run ONCE per census year for each data item


# Set WD to Census year folder
#setwd("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/Census/Census_2006")

TB_cleaning_fn <- function(data_file_base, data_item, calendar_year, geography, code_or_name){
  
  if(calendar_year == 2006){
    geog_list <- c("LGA","SLA","SSD","SD","STE","national")
  }else{
    geog_list <- c("LGA","SA2","SA3","SA4","STE","national")
  }
  
  for(i in 1:length(geog_list)){ #loop over the 6 geography types
    data_temp_name <- paste0(data_file_base,"_",geog_list[i],"_",calendar_year,".csv")
    data_temp <- read.csv(data_temp_name)
  }
  
}


# TEST

#setwd("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/public_data/Census/Census_2006")
TB_cleaning_fn(data_file_base = "census_year12")


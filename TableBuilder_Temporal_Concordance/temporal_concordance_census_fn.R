# Temporal concordance function for census data - drafting/testing


library(tidyverse) #for tidyr::fill()
library(readxl)

#################
### 2006 Files ##
#################

SLA_2006_SA2_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F3858", col_names = TRUE)

SLA_2006_SA2_2016 <-  SLA_2006_SA2_2016[-1,] #remove empty first row


SLA_2006_SA2_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2286", col_names = TRUE)
  
SLA_2006_SA2_2016_quality <-  SLA_2006_SA2_2016_quality[-1,] #remove empty first row

#------------


SSD_2006_SA3_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F644", col_names = TRUE)

SSD_2006_SA3_2016 <-  SSD_2006_SA3_2016[-1,] #remove empty first row


SSD_2006_SA3_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 3,
                                range = "A6:C346", col_names = TRUE)

SSD_2006_SA3_2016_quality <-  SSD_2006_SA3_2016_quality[-1,] #remove empty first row



#-------------

SD_2006_SA4_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F161", col_names = TRUE)

SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row


SD_2006_SA4_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 3,
                               range = "A6:C96", col_names = TRUE)

SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row


#------------

LGA_2006_LGA_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                 sheet = 4,
                                 range = "A6:F906", col_names = TRUE)

LGA_2006_LGA_2016 <-  LGA_2006_LGA_2016[-1,] #remove empty first row  



LGA_2006_LGA_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                sheet = 3,
                                range = "A6:C552", col_names = TRUE)

LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016[-1,] #remove empty first row  


  
#################
### 2011 Files ##
#################


SA2_2011_SA2_2016 <- 
SA3_2011_SA3_2016 <- 
SA4_2011_SA4_2016 <- 
LGA_2011_LGA_2016 <- 
  
SA2_2021_SA2_2016 <- 
SA3_2021_SA3_2016 <- 
SA4_2021_SA4_2016 <- 
LGA_2021_LGA_2016 <- 

# FOR ONE VARIABLE
# Read in all census years for one geography

# Read in correspondence files for that geography

# Convert each year 
# 2006 --> 2016
# 2011 --> 2016
# 2016 <-- 2021

# Stack them together in combined data frame

temporal_concordance_census_fn <- function(data_file_base, data_item_name, original_geography_year){
  
  
  
  
  # Read in correspondence files for that geography
  
  if (geog_list[i]) = 
    
    
    
    
  
}
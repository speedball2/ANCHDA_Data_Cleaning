# Temporal concordance function for census data - drafting/testing


library(tidyverse) #for tidyr::fill()
library(readxl)


#########################################################################################################################################################


#################
### 2006 Files ##
#################

SLA_2006_SA2_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F3858", col_names = TRUE)

SLA_2006_SA2_2016 <-  SLA_2006_SA2_2016[-1,] #remove empty first row

names(SLA_2006_SA2_2016) <- c("SLA_CODE_2006", "SLA_NAME_2006",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",            "PERCENTAGE")


SLA_2006_SA2_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2286", col_names = TRUE)
  
SLA_2006_SA2_2016_quality <-  SLA_2006_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SLA_2006_SA2_2016 <-  dplyr::left_join(SLA_2006_SA2_2016,SLA_2006_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SSD_2006_SA3_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F644", col_names = TRUE)

SSD_2006_SA3_2016 <-  SSD_2006_SA3_2016[-1,] #remove empty first row


SSD_2006_SA3_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 3,
                                range = "A6:C346", col_names = TRUE)

SSD_2006_SA3_2016_quality <-  SSD_2006_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SSD_2006_SA3_2016 <-  dplyr::left_join(SSD_2006_SA3_2016,SSD_2006_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SD_2006_SA4_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F161", col_names = TRUE)

SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row


SD_2006_SA4_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 3,
                               range = "A6:C96", col_names = TRUE)

SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SD_2006_SA4_2016 <-  dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2006_LGA_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                 sheet = 4,
                                 range = "A6:F906", col_names = TRUE)

LGA_2006_LGA_2016 <-  LGA_2006_LGA_2016[-1,] #remove empty first row  



LGA_2006_LGA_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                sheet = 3,
                                range = "A6:C552", col_names = TRUE)

LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016_quality[-1,] #remove empty first row  

# Join quality info onto correspondence sheet
LGA_2006_LGA_2016 <-  dplyr::left_join(LGA_2006_LGA_2016,LGA_2006_LGA_2016_quality,by="LGA_CODE_2016") %>% as.data.frame()



#########################################################################################################################################################
  
#################
### 2011 Files ##
#################


SA2_2011_SA2_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F2433", col_names = TRUE)


names(SA2_2011_SA2_2016) <- c("SA2_CODE_2011", "SA2_NAME_2011",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",           "PERCENTAGE")       

SA2_2011_SA2_2016 <-  SA2_2011_SA2_2016[-1,] #remove empty first row


SA2_2011_SA2_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                sheet = 3,
                                        range = "A6:C2298", col_names = TRUE)

SA2_2011_SA2_2016_quality <-  SA2_2011_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA2_2011_SA2_2016 <-  dplyr::left_join(SA2_2011_SA2_2016,SA2_2011_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SA3_2011_SA3_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F376", col_names = TRUE)

SA3_2011_SA3_2016 <-  SA3_2011_SA3_2016[-1,] #remove empty first row


SA3_2011_SA3_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SA3_2011_SA3_2016_quality <-  SA3_2011_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA3_2011_SA3_2016 <-  dplyr::left_join(SA3_2011_SA3_2016,SA3_2011_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SA4_2011_SA4_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F117", col_names = TRUE)

SA4_2011_SA4_2016 <-  SA4_2011_SA4_2016[-1,] #remove empty first row


SA4_2011_SA4_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                       sheet = 3,
                                       range = "A6:C96", col_names = TRUE)

SA4_2011_SA4_2016_quality <-  SA4_2011_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SA4_2011_SA4_2016 <-  dplyr::left_join(SA4_2011_SA4_2016,SA4_2011_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2011_LGA_2016 <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_LGA_2011_LGA_2016.csv")


# quality info already included



#########################################################################################################################################################
  
################
## 2021 Files ##
################

SA2_2021_SA2_2016 <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA2_2016_SA2_2021.csv")
SA3_2021_SA3_2016 <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA3_2016_SA3_2021.csv")
SA4_2021_SA4_2016 <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA4_2016_SA4_2021.csv")
LGA_2021_LGA_2016 <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_LGA_2016_LGA_2021.csv")



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
# Create named list of correspondence DFs to select from in loop below

corr_year_from = c(rep(c(2006,2011,2021),each=4))
corr_geo_to = c(rep(c("SA2","SA3", "SA4", "LGA"),4))
select_correspondence_indices <- paste0(corr_year_from,corr_geo_to)
correspondence_sheet_list = list(SLA_2006_SA2_2016,SSD_2006_SA3_2016,SD_2006_SA4_2016,LGA_2006_LGA_2016,
                                  SA2_2011_SA2_2016,SA3_2011_SA3_2016,SA4_2011_SA4_2016,LGA_2011_LGA_2016,
                                  SA2_2021_SA2_2016,SA3_2021_SA3_2016,SA4_2021_SA4_2016,LGA_2021_LGA_2016)
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # FOR ONE VARIABLE
  # Read in all census years for one geography
  
  # Read in correspondence files for that geography

# Convert each year 
# 2006 --> 2016
# 2011 --> 2016
# 2016 <-- 2021

# Stack them together in combined data frame

#-------------

# TESTING NEW APPROACH 03/04/23
# 
# Pseudocode steps for correspondence calculation:
#   
#   1. New variable in original dataframe filter_cols_combo = paste (filter cols)
#       --> new DF has to have same original geom code column name as correspondence sheet (fixed in original cleaning steps)
# 2. Also add new column of "calendar year" in original data frame
#   --> If LGA, strip "LGA" substring off start of code column in original DF (fixed in original cleaning steps)
# 3. OUTER JOIN original df and correspondence df (keeps instances where multiple original geoms correspond to one target geom, and vice versa)
#   --> like this: merge(df, df.cor, by = "SD_CODE_2006",all=T)
# 4. new_values = multiply values by ratio column
#      --> If calendar_year = 2021, ratio needs to be INVERSE (1/ratio)
# 5. Group by 1) filter_cols_combo and 2) new geometry (e.g. SA4_CODE_2016)
# 6. sum(new_values)
# 7. Select only needed columns

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


origin_folder_path_base <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/"

destination_folder_path_base <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_READY_FOR_QA/"

#-------------

# User inputs:
data_file_base <- #"census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- #"volunteering"                                  # Name of the input variable column.
GEO_TO <- #"SA4_CODE_2016"                                       # Target geography column
FILTER_VARS <- #c("age_group", "sex")                            # Name of original data set filter variable(s).
GEO_TYPE <- #"SA4"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- #"SD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

#--------

# Rather than writing to loop over geogs, run separately for each geography type

temporal_concordance_census_fn <- function(origin_folder_path_base,destination_folder_path_base,data_file_base,VAR_NAME,GEO_TO,FILTER_VARS,GEO_TYPE,GEO_TYPE_2006){
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Loop for 2006 - different names for SA2 = SLA, SA3 = SSD, SA4 = SD
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Select appropriate correspondence sheet for 2006
  df_corr_2006 <- correspondence_sheet_list[[match(paste0(2006,GEO_TYPE),select_correspondence_indices)]]
  
  # Read in 2006 data
  df_2006_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE_2006,"_2006_INTERIM.csv")
  
  df_2006 <- read.csv(df_2006_name,na.strings=c("","NA"), check.names=FALSE)  

  
  
  df_2006 <- df_2006 %>% as.data.frame()
  
  df_2006$filters_combo <- paste(df_2006[,FILTER_VARS[1]],df_2006[,FILTER_VARS[2]],df_2006[,FILTER_VARS[3]],sep="_")
  
  df_2006$calendar_year <- rep(2006,length(df_2006$age_group))
  
  # Outer join
  merging_df_2006 <- merge(df_2006, df_corr_2006, by = {{paste0(GEO_TYPE_2006,"_CODE_2006")}},all=T)
  
  merging_df_2006$new_vals_raw <- merging_df_2006[,VAR_NAME] * merging_df_2006$RATIO
  
  # group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
  merging_df_2006 <- merging_df_2006 %>% 
    group_by(filters_combo,.data[[GEO_TO]]) %>% mutate(new_vals = round(sum(new_vals_raw)))
  
  
  
  # Rename uncertainty indicator
  
  uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
  merging_df_2006[[uncertainty_colname]] <- merging_df_2006$QI_INDICATOR
  
  
  # ungroup
  merging_df_2006 <- merging_df_2006 %>% ungroup()
  
  # Keep only necessary columns
  #out_df_2006 <- merging_df_2006 %>% select(age_group, sex, .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  # If there is an additional filter column in the data, use the following line instead
  out_df_2006 <- merging_df_2006 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  
  #rename values column after temporal correspondence
  out_df_2006 <- rename(out_df_2006, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  out_df_2006 <- distinct(out_df_2006, .keep_all = T) %>% drop_na(.data[[GEO_TO]])
  
  
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2011 - nothing special
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
  # Select appropriate correspondence sheet for 2011
  df_corr_2011 <- correspondence_sheet_list[[match(paste0(2011,GEO_TYPE),select_correspondence_indices)]]
  
  # Read in 2011 data
  df_2011_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2011_INTERIM.csv")
  
  df_2011 <- read.csv(df_2011_name,na.strings=c("","NA"), check.names=FALSE)  
  
  
  
  df_2011 <- df_2011 %>% as.data.frame()
  
  df_2011$filters_combo <- paste(df_2011[,FILTER_VARS[1]],df_2011[,FILTER_VARS[2]],df_2011[,FILTER_VARS[3]],sep="_")
  
  df_2011$calendar_year <- rep(2011,length(df_2011$age_group))
  
  # Outer join
  merging_df_2011 <- merge(df_2011, df_corr_2011, by = {{paste0(GEO_TYPE,"_CODE_2011")}},all=T)
  
  merging_df_2011$new_vals_raw <- merging_df_2011[,VAR_NAME] * merging_df_2011$RATIO
  
  # group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
  merging_df_2011 <- merging_df_2011 %>% 
    group_by(filters_combo,.data[[GEO_TO]]) %>% mutate(new_vals = round(sum(new_vals_raw)))
  
  
  
  # Rename uncertainty indicator
  
  uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
  merging_df_2011[[uncertainty_colname]] <- merging_df_2011$QI_INDICATOR
  
  
  # ungroup
  merging_df_2011 <- merging_df_2011 %>% ungroup()
  
  # Keep only necessary columns
  #out_df_2011 <- merging_df_2011 %>% select(age_group, sex, .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  # If there is an additional filter column in the data, use the following line instead
  out_df_2011 <- merging_df_2011 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  
  #rename values column after temporal correspondence
  out_df_2011 <- rename(out_df_2011, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  out_df_2011 <- distinct(out_df_2011, .keep_all = T) %>% drop_na(.data[[GEO_TO]])
  
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2016 - no concordance steps, just check out df names
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2016_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2016_INTERIM.csv")
  
  df_2016 <- read.csv(df_2016_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2016[[uncertainty_colname]] <- rep(NA, length(df_2016$age_group)) # create uncertainty column (empty for 2016)
  
  df_2016$calendar_year <- rep(2016, length(df_2016$age_group))
  
  out_df_2016 <- df_2016 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2021 - need to use inverse of from/to ratio
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Select appropriate correspondence sheet for 2021
  df_corr_2021 <- correspondence_sheet_list[[match(paste0(2021,GEO_TYPE),select_correspondence_indices)]]
  
  # Read in 2021 data
  df_2021_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2021_INTERIM.csv")
  
  df_2021 <- read.csv(df_2021_name,na.strings=c("","NA"), check.names=FALSE)  
  
  
  
  df_2021 <- df_2021 %>% as.data.frame()
  
  df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],df_2021[,FILTER_VARS[3]],sep="_")
  
  df_2021$calendar_year <- rep(2021,length(df_2021$age_group))
  
  # Outer join
  merging_df_2021 <- merge(df_2021, df_corr_2021, by = {{paste0(GEO_TYPE,"_CODE_2021")}},all=T)
  
  # NB!!!!! USING INVERSE OF RATIO HERE (FROM/TO RATIO NEEDS TO BE TO/FROM RATIO FOR using 2016-->2021 correspondence files to get 2016 values FROM 2021 values)
  merging_df_2021$new_vals_raw <- merging_df_2021[,VAR_NAME] * 1/merging_df_2021$RATIO
  
  # group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
  merging_df_2021 <- merging_df_2021 %>% 
    group_by(filters_combo,.data[[GEO_TO]]) %>% mutate(new_vals = round(sum(new_vals_raw)))
  
  
  
  # Rename uncertainty indicator
  
  uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
  merging_df_2021[[uncertainty_colname]] <- merging_df_2021$QI_INDICATOR
  
  
  # ungroup
  merging_df_2021 <- merging_df_2021 %>% ungroup()
  
  # Keep only necessary columns
  #out_df_2021 <- merging_df_2021 %>% select(age_group, sex, .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  # If there is an additional filter column in the data, use the following line instead
  out_df_2021 <- merging_df_2021 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  
  #rename values column after temporal correspondence
  out_df_2021 <- rename(out_df_2021, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  out_df_2021 <- distinct(out_df_2021, .keep_all = T) %>% drop_na(.data[[GEO_TO]])
    
      
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # cbind out_df from each year
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)

  write.csv(out_df_all_years,file=paste0(destination_folder_path_base,data_file_base,"_",GEO_TYPE,".csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
}


#===================================================
# Applying - Volunteering

#===================================================
# LGA

# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- "volunteering"                                  # Name of the input variable column.
GEO_TO <- "LGA_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
GEO_TYPE <- "LGA"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "LGA"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#===================================================
# SA2


# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- "volunteering"                                  # Name of the input variable column.
GEO_TO <- "SA2_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
GEO_TYPE <- "SA2"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "SLA"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#===================================================
# SA3

# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- "volunteering"                                  # Name of the input variable column.
GEO_TO <- "SA3_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
GEO_TYPE <- "SA3"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "SSD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)

temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)



#===================================================
# SA4

# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
  VAR_NAME <- "volunteering"                                  # Name of the input variable column.
  GEO_TO <- "SA4_CODE_2016"                                       # Target geography column
  FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
  GEO_TYPE <- "SA4"                                               # Type of target geometry (used for looping over list of correspondence files)
  GEO_TYPE_2006 <- "SD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)
  
temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)


#==================================================




# STATE


state_stack_fn <- function(origin_folder_path_base,destination_folder_path_base,data_file_base,VAR_NAME,GEO_TO,FILTER_VARS,GEO_TYPE,GEO_TYPE_2006){
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2006
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2006_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2006_INTERIM.csv")
  
  df_2006 <- read.csv(df_2006_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2006$calendar_year <- rep(2006, length(df_2006$age_group))
  
  out_df_2006 <- df_2006 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  

  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2011
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2011_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2011_INTERIM.csv")
  
  df_2011 <- read.csv(df_2011_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2011$calendar_year <- rep(2011, length(df_2011$age_group))
  
  out_df_2011 <- df_2011 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2016
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2016_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2016_INTERIM.csv")
  
  df_2016 <- read.csv(df_2016_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2016$calendar_year <- rep(2016, length(df_2016$age_group))
  
  out_df_2016 <- df_2016 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2021
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2021_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2021_INTERIM.csv")
  
  df_2021 <- read.csv(df_2021_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2021$calendar_year <- rep(2021, length(df_2021$age_group))
  
  out_df_2021 <- df_2021 %>% select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # cbind out_df from each year
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) 
  
  if(GEO_TYPE == "STE"){out_df_all_years <- out_df_all_years %>% mutate(State = recode(State, "1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
                                                                 "New South Wales" = 1, "Victoria" = 2, "Queensland" = 3, "South Australia" = 4, 
                                                                 "Western Australia" = 5, "Tasmania" = 6, "Northern Territory" = 7, 
                                                                 "Australian Capital Territory" = 8, "Other Territories" = 9))
  }
  
  out_df_all_years <- out_df_all_years %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)
  
  write.csv(out_df_all_years,file=paste0(destination_folder_path_base,data_file_base,"_",GEO_TO,".csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
}

# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- "volunteering"                                  # Name of the input variable column.
GEO_TO <- "State"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
GEO_TYPE <- "STE"                                               # Type of target geometry (used for looping over list of correspondence files)


state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)



#---------


# NATIONAL


# User inputs:
data_file_base <- "census_volunteer"                               # Base file name for datasets e.g. census_volunteer
VAR_NAME <- "volunteering"                                  # Name of the input variable column.
GEO_TO <- "National"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex","VOLWP Voluntary Work for an Organisation or Group")                            # Name of original data set filter variable(s).
GEO_TYPE <- "national"                                               # Type of target geometry (used for looping over list of correspondence files)


state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)





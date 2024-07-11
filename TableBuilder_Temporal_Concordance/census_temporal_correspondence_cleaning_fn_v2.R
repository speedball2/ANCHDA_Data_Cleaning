# Temporal concordance function for census data - drafting/testing

# Changes at 19/05/23:
# - move order of "calendar_year" column before variable column in output df's for each function
# 

library(tidyverse) #for tidyr::fill()
library(readxl)


#########################################################################################################################################################


#################
### 2006 Files ##
#################

SLA_2006_SA2_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F3858", col_names = TRUE)

SLA_2006_SA2_2016 <-  SLA_2006_SA2_2016[-1,] #remove empty first row

names(SLA_2006_SA2_2016) <- c("SLA_CODE_2006", "SLA_NAME_2006",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",            "PERCENTAGE")


SLA_2006_SA2_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2286", col_names = TRUE)

SLA_2006_SA2_2016_quality <-  SLA_2006_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SLA_2006_SA2_2016 <-  dplyr::left_join(SLA_2006_SA2_2016,SLA_2006_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SSD_2006_SA3_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F644", col_names = TRUE)

SSD_2006_SA3_2016 <-  SSD_2006_SA3_2016[-1,] #remove empty first row


SSD_2006_SA3_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                        sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SSD_2006_SA3_2016_quality <-  SSD_2006_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SSD_2006_SA3_2016 <-  dplyr::left_join(SSD_2006_SA3_2016,SSD_2006_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SD_2006_SA4_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F161", col_names = TRUE)

SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row


SD_2006_SA4_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                                       sheet = 3,
                                       range = "A6:C96", col_names = TRUE)

SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SD_2006_SA4_2016 <-  dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2006_LGA_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                sheet = 4,
                                range = "A6:F906", col_names = TRUE)

LGA_2006_LGA_2016 <-  LGA_2006_LGA_2016[-1,] #remove empty first row  



LGA_2006_LGA_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                        sheet = 3,
                                        range = "A6:C552", col_names = TRUE)

LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016_quality[-1,] #remove empty first row  

# Join quality info onto correspondence sheet
LGA_2006_LGA_2016 <-  dplyr::left_join(LGA_2006_LGA_2016,LGA_2006_LGA_2016_quality,by="LGA_CODE_2016") %>% as.data.frame()



#########################################################################################################################################################

#################
### 2011 Files ##
#################


SA2_2011_SA2_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F2433", col_names = TRUE)


names(SA2_2011_SA2_2016) <- c("SA2_CODE_2011", "SA2_NAME_2011",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",           "PERCENTAGE")       

SA2_2011_SA2_2016 <-  SA2_2011_SA2_2016[-1,] #remove empty first row


SA2_2011_SA2_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2298", col_names = TRUE)

SA2_2011_SA2_2016_quality <-  SA2_2011_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA2_2011_SA2_2016 <-  dplyr::left_join(SA2_2011_SA2_2016,SA2_2011_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SA3_2011_SA3_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F376", col_names = TRUE)

SA3_2011_SA3_2016 <-  SA3_2011_SA3_2016[-1,] #remove empty first row


SA3_2011_SA3_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                        sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SA3_2011_SA3_2016_quality <-  SA3_2011_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA3_2011_SA3_2016 <-  dplyr::left_join(SA3_2011_SA3_2016,SA3_2011_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SA4_2011_SA4_2016 <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                sheet = 4,
                                range = "A6:F117", col_names = TRUE)

SA4_2011_SA4_2016 <-  SA4_2011_SA4_2016[-1,] #remove empty first row


SA4_2011_SA4_2016_quality <- read_excel("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                        sheet = 3,
                                        range = "A6:C96", col_names = TRUE)

SA4_2011_SA4_2016_quality <-  SA4_2011_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SA4_2011_SA4_2016 <-  dplyr::left_join(SA4_2011_SA4_2016,SA4_2011_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2011_LGA_2016 <- read.csv("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_LGA_2011_LGA_2016.csv")


# quality info already included



#########################################################################################################################################################

################
## 2021 Files ##
################

SA2_2021_SA2_2016 <- read.csv("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA2_2016_SA2_2021.csv")
SA3_2021_SA3_2016 <- read.csv("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA3_2016_SA3_2021.csv")
SA4_2021_SA4_2016 <- read.csv("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA4_2016_SA4_2021.csv")
LGA_2021_LGA_2016 <- read.csv("/Users/Current/Desktop/ANCHDA_RA/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_LGA_2016_LGA_2021.csv")


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

destination_folder_path_base <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/after_correspondence_before_name_fixes/"

#-------------

# User inputs:
data_file_base <- #"census_LANP"                               # Base file name for datasets e.g. census_LANP
  VAR_NAME <- #"language_spoken_at_home"                                  # Name of the input variable column.
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
  
  # filters combo
  if(length(FILTER_VARS) == 1){
    df_2006$filters_combo <- df_2006[,FILTER_VARS[1]]
  } else if(length(FILTER_VARS) == 2){
    df_2006$filters_combo <- paste(df_2006[,FILTER_VARS[1]],df_2006[,FILTER_VARS[2]],sep="_")
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    df_2006$filters_combo <- paste(df_2006[,FILTER_VARS[1]],df_2006[,FILTER_VARS[2]],df_2006[,FILTER_VARS[3]],sep="_")
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    df_2006$filters_combo <- paste(df_2006[,FILTER_VARS[1]],df_2006[,FILTER_VARS[2]],df_2006[,FILTER_VARS[3]],df_2006[,FILTER_VARS[4]],sep="_")
  } else {print("check number of filter variables in FILTER_VARS argument")}
  
  
  df_2006$calendar_year <- rep(2006,length(df_2006[,1]))
  
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
  if(length(FILTER_VARS) == 1){
    out_df_2006 <- merging_df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2006 <- merging_df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
    } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
  out_df_2006 <- merging_df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
    }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
      out_df_2006 <- merging_df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
    } else {print("check number of filter variables in FILTER_VARS argument")}
 
  
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
  
  # filters combo
  if(length(FILTER_VARS) == 1){
    df_2011$filters_combo <- df_2011[,FILTER_VARS[1]]
  } else if(length(FILTER_VARS) == 2){
    df_2011$filters_combo <- paste(df_2011[,FILTER_VARS[1]],df_2011[,FILTER_VARS[2]],sep="_")
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    df_2011$filters_combo <- paste(df_2011[,FILTER_VARS[1]],df_2011[,FILTER_VARS[2]],df_2011[,FILTER_VARS[3]],sep="_")
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    df_2011$filters_combo <- paste(df_2011[,FILTER_VARS[1]],df_2011[,FILTER_VARS[2]],df_2011[,FILTER_VARS[3]],df_2011[,FILTER_VARS[4]],sep="_")
  } else {print("check number of filter variables in FILTER_VARS argument")}
  
  
  df_2011$calendar_year <- rep(2011,length(df_2011[,1]))
  
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
  if(length(FILTER_VARS) == 1){
    out_df_2011 <- merging_df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2011 <- merging_df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2011 <- merging_df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2011 <- merging_df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument")}
  
  
  #rename values column after temporal correspondence
  out_df_2011 <- rename(out_df_2011, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  out_df_2011 <- distinct(out_df_2011, .keep_all = T) %>% drop_na(.data[[GEO_TO]])
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Loop for 2016 - no concordance steps, just check out df names
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2016_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2016_INTERIM.csv")
  
  df_2016 <- read.csv(df_2016_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2016[[uncertainty_colname]] <- rep(NA, length(df_2016[,1])) # create uncertainty column (empty for 2016)
  
  df_2016$calendar_year <- rep(2016, length(df_2016[,1]))
  
  if(length(FILTER_VARS) == 1){
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]],.data[[GEO_TO]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], .data[[GEO_TO]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument - writing out_df_2016")}
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Loop for 2021 - need to use inverse of from/to ratio
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Select appropriate correspondence sheet for 2021
  df_corr_2021 <- correspondence_sheet_list[[match(paste0(2021,GEO_TYPE),select_correspondence_indices)]]
  
  # Read in 2021 data
  df_2021_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2021_INTERIM.csv")
  
  df_2021 <- read.csv(df_2021_name,na.strings=c("","NA"), check.names=FALSE)  
  
  
  
  df_2021 <- df_2021 %>% as.data.frame()
  
  # filters combo
  if(length(FILTER_VARS) == 1){
    df_2021$filters_combo <- df_2021[,FILTER_VARS[1]]
  } else if(length(FILTER_VARS) == 2){
    df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],sep="_")
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],df_2021[,FILTER_VARS[3]],sep="_")
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],df_2021[,FILTER_VARS[3]],df_2021[,FILTER_VARS[4]],sep="_")
  } else {print("check number of filter variables in FILTER_VARS argument")}
  
  
  df_2021$calendar_year <- rep(2021,length(df_2021[,1]))
  
  # Outer join
  merging_df_2021 <- merge(df_2021, df_corr_2021, by = {{paste0(GEO_TYPE,"_CODE_2021")}},all=T)
  
  
  # Fix to 2021 merge
  # Remove BELOW MINIMUM OUTPUT SIZE rows and other BMOS flags (see documentation on correspondence from ABS)
  merging_df_2021 <- merging_df_2021[-which(merging_df_2021$BMOS_NULL_FLAG != 0),]
  
  
  # NOTE at 12/05/23
  # For duplicate 2021 geographies - this step will cause inflated estimates for a few areas with duplicate recipient geographies (2021 areas) - e.g. for SA2, Brisbane Airport, or Willow Vale-Pimpama, or Taylor 801041117 
  
  # group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
  merging_df_2021 <- merging_df_2021 %>% 
    group_by(filters_combo,.data[[GEO_TO]]) %>% mutate(new_vals = round(sum(.data[[VAR_NAME]])))
  
  
  
  # Rename uncertainty indicator
  
  uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
  merging_df_2021[[uncertainty_colname]] <- merging_df_2021$QI_INDICATOR
  
  
  # ungroup
  merging_df_2021 <- merging_df_2021 %>% ungroup()
  
  # Keep only necessary columns
  if(length(FILTER_VARS) == 1){
    out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument")}
  
  #rename values column after temporal correspondence
  out_df_2021 <- rename(out_df_2021, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  out_df_2021 <- distinct(out_df_2021, .keep_all = T) %>% drop_na(.data[[GEO_TO]])
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # cbind out_df from each year
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  #out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)
  
  if(length(FILTER_VARS) <= 2){
    out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]])
  } else if(length(FILTER_VARS) >= 3){
    out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)
  } else {print("check number of filter variables in FILTER_VARS argument - arranging out_df_all_years")}
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # FORMATTING CONSISTENCY CHECKS ON FINAL ASSEMBLED DATASET
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #1. put geography column first, then age_group, sex, then other filter vars, then values column
  
  #out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], age_group, sex, .data[[FILTER_VARS[3]]], .data[[VAR_NAME]], .data[[uncertainty_colname]], calendar_year)
  
  if(length(FILTER_VARS) == 1){
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], calendar_year, .data[[VAR_NAME]], .data[[uncertainty_colname]])
  } else if(length(FILTER_VARS) == 2){
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], calendar_year, .data[[VAR_NAME]], .data[[uncertainty_colname]])
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], calendar_year, .data[[VAR_NAME]], .data[[uncertainty_colname]])
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], calendar_year, .data[[VAR_NAME]], .data[[uncertainty_colname]])
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_all_years")}
  
  
  #2. format geography column to match standard _CODE16 format
  names(out_df_all_years)[1] <- paste0(GEO_TYPE,"_CODE16")
  
  #3. Correct all colnames to snake case (except geography column)
  
  # Function from https://github.com/sbha/dfnames/blob/master/R/sc_names.R
  to_snake_case <- function(names){
    x <- trimws(names)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    x <- gsub("[[:punct:] ]", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x <- make.unique(x, sep = "_")
    x <- tolower(x)
    x
  }

  # Snake case all column names except geography
  names(out_df_all_years)[-1] <- to_snake_case(names(out_df_all_years)[-1])
  
  # remove "total" rows
  out_df_all_years <- out_df_all_years[rowSums(sapply(out_df_all_years, grepl, pattern = 'Total')) == 0, ]
  
  # make sex lowercase
  if("sex" %in% names(out_df_all_years)){
    out_df_all_years$sex <- tolower(out_df_all_years$sex)
  }
  
  # remove " years" from age_group so it's just number-number e.g. "0-4"
  if("age_group" %in% names(out_df_all_years)){
    # remove " years" from age_group
    out_df_all_years$age_group <- gsub(' years','',out_df_all_years$age_group)
  }
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # SAVE OUTPUT TO CSV
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  write.csv(out_df_all_years,file=paste0(destination_folder_path_base,data_file_base,"_",GEO_TYPE,".csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
}




# TEST

# # User inputs:
# data_file_base <- "census_year12"                               # Base file name for datasets e.g. census_year12
#   VAR_NAME <- "completed_year12"                                  # Name of the input variable column.
#   GEO_TO <- "SA4_CODE_2016"                                       # Target geography column
#   FILTER_VARS <- c("age_group", "sex")                            # Name of original data set filter variable(s).
#   GEO_TYPE <- "SA4"                                               # Type of target geometry (used for looping over list of correspondence files)
#   GEO_TYPE_2006 <- "SD"                                           # 2006 Type of geometry (SLA, SSD, SD, LGA)
#   
# temporal_concordance_census_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)








#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Function for STATE and NATIONAL geographies
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


state_stack_fn <- function(origin_folder_path_base,destination_folder_path_base,data_file_base,VAR_NAME,GEO_TO,FILTER_VARS,GEO_TYPE,GEO_TYPE_2006, GEO_COL_FINAL){
  

  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2006
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2006_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2006_INTERIM.csv")
  
  df_2006 <- read.csv(df_2006_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2006$calendar_year <- rep(2006, length(df_2006[,1]))
  
  #out_df_2006 <- df_2006 %>% dplyr::select(age_group, sex, .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  
  
  if(length(FILTER_VARS) == 1){
    out_df_2006 <- df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2006 <- df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2006 <- df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2006 <- df_2006 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[FILTER_VARS[4]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_2006")}
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2011
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2011_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2011_INTERIM.csv")
  
  df_2011 <- read.csv(df_2011_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2011$calendar_year <- rep(2011, length(df_2011[,1]))
  
  
  if(length(FILTER_VARS) == 1){
    out_df_2011 <- df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2011 <- df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2011 <- df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2011 <- df_2011 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[FILTER_VARS[4]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_2011")}
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2016
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2016_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2016_INTERIM.csv")
  
  df_2016 <- read.csv(df_2016_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2016$calendar_year <- rep(2016, length(df_2016[,1]))
  
  
  if(length(FILTER_VARS) == 1){
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2016 <- df_2016 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[FILTER_VARS[4]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_2016")}
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Import 2021
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  df_2021_name <- paste0(origin_folder_path_base,data_file_base,"_",GEO_TYPE,"_2021_INTERIM.csv")
  
  df_2021 <- read.csv(df_2021_name,na.strings=c("","NA"), check.names=FALSE)  
  
  df_2021$calendar_year <- rep(2021, length(df_2021[,1]))
  
  
  if(length(FILTER_VARS) == 1){
    out_df_2021 <- df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 2){
    out_df_2021 <- df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_2021 <- df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_2021 <- df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[FILTER_VARS[4]]], .data[[GEO_TO]], .data[[VAR_NAME]], calendar_year)
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_2021")}
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # cbind out_df from each year
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  
  if(length(FILTER_VARS) <= 2){
    out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]])
  } else if(length(FILTER_VARS) >= 3){
    out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)
  } else {print("check number of filter variables in FILTER_VARS argument - arranging out_df_all_years")}  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # FORMATTING CONSISTENCY CHECKS ON FINAL ASSEMBLED DATASET
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # fix state/national names
  
  
  if(GEO_TYPE == "STE"){out_df_all_years <- out_df_all_years %>% mutate(State = recode(State, "1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
                                                                                       "New South Wales" = 1, "Victoria" = 2, "Queensland" = 3, "South Australia" = 4, 
                                                                                       "Western Australia" = 5, "Tasmania" = 6, "Northern Territory" = 7, 
                                                                                       "Australian Capital Territory" = 8, "Other Territories" = 9))
  }
  
  
  if(GEO_TYPE == "national"){out_df_all_years <- out_df_all_years %>% mutate(Australia = recode(Australia, "0"="0","Australia"="0"))
  }
  
  
  #1. put geography column first, then age_group, sex, then other filter vars, then values column
  
  if(length(FILTER_VARS) == 1){
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], calendar_year, .data[[VAR_NAME]])
  } else if(length(FILTER_VARS) == 2){
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], calendar_year, .data[[VAR_NAME]])
  } else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], calendar_year, .data[[VAR_NAME]])
  }else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
    out_df_all_years <- out_df_all_years %>% dplyr::select(.data[[GEO_TO]], .data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], calendar_year, .data[[VAR_NAME]])
  } else {print("check number of filter variables in FILTER_VARS argument - selecting cols for out_df_all_years")}
  
  
  #2. format geography column to match standard _CODE16 format
  names(out_df_all_years)[1] <- GEO_COL_FINAL
  
  #3. Correct all colnames to snake case
  
  
  # Function from https://github.com/sbha/dfnames/blob/master/R/sc_names.R
  to_snake_case <- function(names){
    x <- trimws(names)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    x <- gsub("[[:punct:] ]", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x <- make.unique(x, sep = "_")
    x <- tolower(x)
    x
  }
  
  # Snake case all column names except geography
  names(out_df_all_years)[-1] <- to_snake_case(names(out_df_all_years)[-1])
  
  # remove "total" rows
  out_df_all_years <- out_df_all_years[rowSums(sapply(out_df_all_years, grepl, pattern = 'Total')) == 0, ]
  
  # make sex lowercase
  if("sex" %in% names(out_df_all_years)){
    out_df_all_years$sex <- tolower(out_df_all_years$sex)
  }
  
  if("age_group" %in% names(out_df_all_years)){
  # remove " years" from age_group
  out_df_all_years$age_group <- gsub(' years','',out_df_all_years$age_group)
  }
  
  
  write.csv(out_df_all_years,file=paste0(destination_folder_path_base,data_file_base,"_",ifelse(GEO_TYPE == "STE", "STE", "Australia"),".csv"),row.names=FALSE) #row.names=FALSE -- don't save indices in first column
}

# TEST EXAMPLE

# 
# 
# # User inputs:
# data_file_base <- "census_religion"                               # Base file name for datasets e.g. census_religion
# VAR_NAME <- "religion"                                  # Name of the input variable column.
# GEO_TO <- "State"                                       # Target geography column
# FILTER_VARS <- c("age_group", "sex","RELP Religious Affiliation - 1-digit level")                            # Name of original data set filter variable(s).
# GEO_TYPE <- "STE"                                               # Type of target geometry (used for looping over list of correspondence files)
# GEO_COL_FINAL <- "State"
# 
# state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)



#---------

# 
# # NATIONAL
# 
# 
# # User inputs:
# data_file_base <- "census_religion"                               # Base file name for datasets e.g. census_religion
# VAR_NAME <- "religion"                                  # Name of the input variable column.
# GEO_TO <- "National"                                       # Target geography column
# FILTER_VARS <- c("age_group", "sex","RELP Religious Affiliation - 1-digit level")                            # Name of original data set filter variable(s).
# GEO_TYPE <- "national"                                               # Type of target geometry (used for looping over list of correspondence files)
# GEO_COL_FINAL <- "Australia"

# state_stack_fn(origin_folder_path_base = origin_folder_path_base,destination_folder_path_base = destination_folder_path_base,data_file_base = data_file_base,VAR_NAME = VAR_NAME, GEO_TO = GEO_TO, FILTER_VARS = FILTER_VARS, GEO_TYPE = GEO_TYPE, GEO_TYPE_2006 = GEO_TYPE_2006)




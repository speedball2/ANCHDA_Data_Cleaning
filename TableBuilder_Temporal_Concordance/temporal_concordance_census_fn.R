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

LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016[-1,] #remove empty first row  

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
#       --> new DF has to have same original geom code column name as correspondence sheet (in original cleaning steps)
# 2. Also add new column of "calendar year" in original data frame
#   --> If LGA, strip "LGA" substring off start of code column in original DF (in original cleaning steps)
# 3. outer join original df and correspondence df (keeps instances where multiple original geoms correspond to one target geom, and vice versa)
# like this: merge(df, df.cor, by = "SD_CODE_2006",all=T)
# 4. new_values = multiply values by ratio column
#      --> If calendar_year = 2021, ratio needs to be INVERSE (1/ratio)
# 5. Group by 1) filter_cols_combo and 2) new geometry (e.g. SA4_CODE_2016)
# 6. sum(new_values)
# 7. Select only needed columns

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# TESTING 


df.cor <- dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

names(df.cor)

df <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/census_year12_SD_2006_INTERIM.csv")

names(df)

# OF: IF LGA - strip "LGA" string off front
#df$LGA..UR. <- stringr::str_sub(df$LGA..UR.,4,-1)

# --> new DF has to have same original geom code column name as correpsondence sheet

#names(df) <- c("age_group", "sex", "HSCP.Highest.Year.of.School.Completed", "SD_CODE_2006", "completed_year12") # Fix names

df <- df %>% rename({{GEO_FROM}} := {{OG_GEO_FROM}}) %>% 
  rename(age_group := {{OG_AGE_COL}}) %>%  
  rename(sex := {{OG_SEX_COL}}) %>% 
  

VAR_NAME <- "completed_year12"

df <- df %>% as.data.frame()
# OF: IF 2021 -- need to take inverse of from:to ratio


df$filters_combo <- paste(df$age_group,df$sex,sep="_")

df$calendar_year <- rep("2006",length(df$age_group))

merging_df <- merge(df, df.cor, by = "SD_CODE_2006",all=T)

merging_df$new_vals_raw <- merging_df[,VAR_NAME] * merging_df$RATIO

# group by filter variables and new_geography, find sum over multiple entries for target geom
merging_df <- merging_df %>% 
  group_by(filters_combo,SA4_CODE_2016) %>% mutate(new_vals = round(sum(new_vals_raw)))



# Rename uncertainty indicator

uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
merging_df[[uncertainty_colname]] <- merging_df[,QI_INDICATOR]


# ungroup
merging_df <- merging_df %>% ungroup()

# Keep only necessary columns
kept_df_2006 <- merging_df %>% select({{GEO_TO}}, age_group, sex, new_vals, {{uncertainty_colname}}, calendar_year)

#rename values column after temporal correspondence
kept_df_2006 <- rename(kept_df_2006, {{VAR_NAME}} := new_vals)

# Remove duplicate rows and NA for geom
kept_df_2006 <- distinct(kept_df_2006, .keep_all = T) %>% drop_na({{GEO_TO}})




origin_folder_path_base <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/"

destination_folder_path_base <- "/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_READY_FOR_QA/"

#-------------

# User inputs:
data_file_base <- "census_year12"                               # Base file name for datasets e.g. census_year12
VAR_NAME <- "completed_year12"                                  # Name of the input variable column.
OG_GEO_FROM_2006 <- "Statistical.Division..SD."                      # Original name of FROM geography column
OG_GEO_FROM_2006 <- "Statistical.Division..SD."                      # Original name of FROM geography column
OG_AGE_COL <- "AGEP.Age..5.Year.Groups."                        # Original name of "age_group" filter column
OG_SEX_COL <- "SEXP.Sex"                                        # Original name of "sex" filter column
GEO_FROM <- "SD_CODE_2006"                                      # Original data set geography column (name matched with correspondence sheet)
GEO_TO <- "SA4_CODE_2016"                                       # Target geography column
FILTER_VARS <- c("age_group", "sex")                            # Name of original data set filter variable(s).
GEO_TYPE <- "SA4"                                               # Type of target geometry (used for looping over list of correspondence files)
GEO_TYPE_2006 <- "SD"                                           # 2006 Type of target geometry (SLA, SSD, SD, LGA)

#--------

# Rather than writing to loop over geogs, run separately for each geography type

temporal_concordance_census_fn <- function(data_file_base,VAR_NAME,OG_GEO_FROM,OG_AGE_COL,OG_SEX_COL,GEO_FROM,GEO_TO,FILTER_VARS,GEO_TYPE,GEO_TYPE_2006){
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Loop for 2006 - different names for SA2 = SLA, SA3 = SSD, SA4 = SD
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Select appropriate correspondence sheet for 2006
  df_corr_2006 <- correspondence_sheet_list[[match(paste0(2006,GEO_TYPE),select_correspondence_indices)]]
  
  # Read in 2006 data
  df_2006_name <- paste0(folder_path_base,data_file_base,"_",GEO_TYPE_2006,"_2006_INTERIM.csv")
  
  df_2006 <- read.csv(df_2006_name,na.strings=c("","NA"), check.names=FALSE)  

  df_2006 <- df_2006 %>% rename({{GEO_FROM}} := {{OG_GEO_FROM}}) %>% 
    rename(age_group := {{OG_AGE_COL}}) %>%  
    rename(sex := {{OG_SEX_COL}}) %>% 
    
    
    VAR_NAME <- "completed_year12"
  
  df <- df %>% as.data.frame()
  # OF: IF 2021 -- need to take inverse of from:to ratio
  
  
  df$filters_combo <- paste(df$age_group,df$sex,sep="_")
  
  df$calendar_year <- rep("2006",length(df$age_group))
  
  merging_df <- merge(df, df.cor, by = "SD_CODE_2006",all=T)
  
  merging_df$new_vals_raw <- merging_df[,VAR_NAME] * merging_df$RATIO
  
  # group by filter variables and new_geography, find sum over multiple entries for target geom
  merging_df <- merging_df %>% 
    group_by(filters_combo,SA4_CODE_2016) %>% mutate(new_vals = round(sum(new_vals_raw)))
  
  
  
  # Rename uncertainty indicator
  
  uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
  merging_df[[uncertainty_colname]] <- merging_df[,QI_INDICATOR]
  
  
  # ungroup
  merging_df <- merging_df %>% ungroup()
  
  # Keep only necessary columns
  kept_df_2006 <- merging_df %>% select({{GEO_TO}}, age_group, sex, new_vals, {{uncertainty_colname}}, calendar_year)
  
  #rename values column after temporal correspondence
  kept_df_2006 <- rename(kept_df_2006, {{VAR_NAME}} := new_vals)
  
  # Remove duplicate rows and NA for geom
  kept_df_2006 <- distinct(kept_df_2006, .keep_all = T) %>% drop_na({{GEO_TO}})
  
  
  
  
  
  merging_df_2006 <- 
    
  out_df_2006 <- 
    
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2011 - nothing special
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2016 - no concordance steps, just check out df names
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # Loop for 2021 - need to use inverse of from/to ratio
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
    
      
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # cbind out_df from each year
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  }

    
    
    
    
  
}
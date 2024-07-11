# Working example for temporal concordance - extending into function pipeline



library(tidyverse) #for tidyr::fill()
library(readxl)


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

# TESTING 


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


df.cor <- SD_2006_SA4_2016
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


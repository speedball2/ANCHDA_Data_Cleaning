
# --------------------------- #
# --- Cleaning AEDC data  --- #
# --------------------------- #

# ----------- #
# --- SWD --- #
# ----------- #

# set working directory 

setwd("~/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_CLEAN/Data_cleaning_code")

# ----------------- #
# --- Libraries --- #
# ----------------- #

library(readxl)
library(tidyverse)



# ----------------=--------------------------- #
# --- functionalised cleaning code - 2009  --- #
# -------------------------------------------- #

AEDC_stripping_function <- function(domain_name,sheet_index){
  
  
  temp_df <- cbind(
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "C8:C2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "I8:J2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "S8:T2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AC8:AD2692", col_names = FALSE))
  
  
  colnames(temp_df) <- c("SA3_NAME16",paste0(domain_name,"_Nontrack_2009"),paste0(domain_name,"_Pontrack_2009"),paste0(domain_name,"_Natrisk_2009"),paste0(domain_name,"_Patrisk_2009"),paste0(domain_name,"_Nvul_2009"),paste0(domain_name,"_Pvul_2009"))
  
  
  return(temp_df)
}



AEDC_2009_health <- AEDC_stripping_function("health",2)
AEDC_2009_social <- AEDC_stripping_function("social",3)
AEDC_2009_emotional <- AEDC_stripping_function("emotional",4)
AEDC_2009_language <- AEDC_stripping_function("language",5)

#combining all of the data frames - for 2009 -  together 


AEDC_data_2009 <- cbind(AEDC_2009_health,AEDC_2009_social,AEDC_2009_emotional,AEDC_2009_language)
AEDC_data_2009 <- AEDC_data_2009[,-c(8,15,22,29)] #removing multiples of "SA3_NAME16"


# ----------------=--------------------------- #
# --- functionalised cleaning code - 2012  --- #
# -------------------------------------------- #

AEDC_stripping_function <- function(domain_name,sheet_index){
  
  
  temp_df <- cbind(
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "C8:C2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "K8:L2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "U8:V2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AE8:AF2692", col_names = FALSE))
  
  
  colnames(temp_df) <- c("SA3_NAME16",paste0(domain_name,"_Nontrack_2012"),paste0(domain_name,"_Pontrack_2012"),paste0(domain_name,"_Natrisk_2012"),paste0(domain_name,"_Patrisk_2012"),paste0(domain_name,"_Nvul_2012"),paste0(domain_name,"_Pvul_2012"))
  
  
  return(temp_df)
}

#using owen's function 


AEDC_2012_health <- AEDC_stripping_function("health",2)
AEDC_2012_social <- AEDC_stripping_function("social",3)
AEDC_2012_emotional <- AEDC_stripping_function("emotional",4)
AEDC_2012_language <- AEDC_stripping_function("language",5)


#combining all of the data frames - for 2012 -  together 


AEDC_data_2012 <- cbind(AEDC_2012_health,AEDC_2012_social,AEDC_2012_emotional,AEDC_2012_language)
AEDC_data_2012 <- AEDC_data_2012[,-c(1,8,15,22,29)] #removing multiples of "SA3_NAME16"


# ----------------=--------------------------- #
# --- functionalised cleaning code - 2015  --- #
# -------------------------------------------- #

AEDC_stripping_function <- function(domain_name,sheet_index){
  
  
  temp_df <- cbind(
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "C8:C2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "M8:N2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "W8:X2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AG8:AH2692", col_names = FALSE))
  
  
  colnames(temp_df) <- c("SA3_NAME16",paste0(domain_name,"_Nontrack_2015"),paste0(domain_name,"_Pontrack_2015"),paste0(domain_name,"_Natrisk_2015"),paste0(domain_name,"_Patrisk_2015"),paste0(domain_name,"_Nvul_2015"),paste0(domain_name,"_Pvul_2015"))
  
  
  return(temp_df)
}



AEDC_2015_health <- AEDC_stripping_function("health",2)
AEDC_2015_social <- AEDC_stripping_function("social",3)
AEDC_2015_emotional <- AEDC_stripping_function("emotional",4)
AEDC_2015_language <- AEDC_stripping_function("language",5)


#combining all of the data frames - for 2015 -  together 


AEDC_data_2015 <- cbind(AEDC_2015_health,AEDC_2015_social,AEDC_2015_emotional,AEDC_2015_language)
AEDC_data_2015 <- AEDC_data_2015[,-c(1,8,15,22,29)] #removing multiples of "SA3_NAME16"

# ----------------=--------------------------- #
# --- functionalised cleaning code - 2018  --- #
# -------------------------------------------- #

AEDC_stripping_function <- function(domain_name,sheet_index){
  
  
  temp_df <- cbind(
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "C8:C2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "O8:P2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "Y8:Z2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AI8:AJ2692", col_names = FALSE))
  
  
  colnames(temp_df) <- c("SA3_NAME16",paste0(domain_name,"_Nontrack_2018"),paste0(domain_name,"_Pontrack_2018"),paste0(domain_name,"_Natrisk_2018"),paste0(domain_name,"_Patrisk_2018"),paste0(domain_name,"_Nvul_2018"),paste0(domain_name,"_Pvul_2018"))
  
  
  return(temp_df)
}



AEDC_2018_health <- AEDC_stripping_function("health",2)
AEDC_2018_social <- AEDC_stripping_function("social",3)
AEDC_2018_emotional <- AEDC_stripping_function("emotional",4)
AEDC_2018_language <- AEDC_stripping_function("language",5)



#combining all of the data frames - for 2018 -  together 


AEDC_data_2018 <- cbind(AEDC_2018_health,AEDC_2018_social,AEDC_2018_emotional,AEDC_2018_language)
AEDC_data_2018 <- AEDC_data_2018[,-c(1,8,15,22,29)] #removing multiples of "SA3_NAME16"

# ----------------=--------------------------- #
# --- functionalised cleaning code - 2021  --- #
# -------------------------------------------- #

AEDC_stripping_function <- function(domain_name,sheet_index){
  
  
  temp_df <- cbind(
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "C8:C2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "Q8:R2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AA8:AB2692", col_names = FALSE),
    read_excel("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_RAW/AEDC_SA3/AEDC_SA3_2009-2021.xlsx",
               sheet = sheet_index,
               range = "AK8:AL2692", col_names = FALSE))
  
  
  colnames(temp_df) <- c("SA3_NAME16",paste0(domain_name,"_Nontrack_2021"),paste0(domain_name,"_Pontrack_2021"),paste0(domain_name,"_Natrisk_2021"),paste0(domain_name,"_Patrisk_2021"),paste0(domain_name,"_Nvul_2021"),paste0(domain_name,"_Pvul_2021"))
  
  
  return(temp_df)
}



AEDC_2021_health <- AEDC_stripping_function("health",2)
AEDC_2021_social <- AEDC_stripping_function("social",3)
AEDC_2021_emotional <- AEDC_stripping_function("emotional",4)
AEDC_2021_language <- AEDC_stripping_function("language",5)


#combining all of the data frames - for 2009 -  together 

AEDC_data_2021 <- cbind(AEDC_2021_health,AEDC_2021_social,AEDC_2021_emotional,AEDC_2021_language)
AEDC_data_2021 <- AEDC_data_2021[,-c(1,8,15,22,29)] #removing multiples of "SA3_NAME16"



# -------------------------------------- #
# --- combinging all years together  --- #
# -------------------------------------- #

AEDC_all_years <- cbind(AEDC_data_2009,AEDC_data_2012,AEDC_data_2015,AEDC_data_2018,AEDC_data_2021) #combinging all years together 


#reading in codes 


#SA3_codes <- read.csv("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_CLEAN/SA3_ASGS16_Codes_Names_only.csv")
#SA3_codes <- SA3_codes[,c(1,2)]

#view(SA3_codes)

#names(SA3_codes) <- c("SA3_CODE16","SA3_NAME16")


SA3_codes <- read.csv("/Users/harriettephillips/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/ANCHDA_QUT/Data_Collections_CLEAN/SA3_codes_names.csv")
SA3_codes <- SA3_codes[,c(2,3)]


AEDC_all_years <- inner_join(AEDC_all_years,SA3_codes,by="SA3_NAME16")


#pivot longer all years 

# owen example - preschool data 
# preschool_nopoly_longer <- Preschool_nopoly %>% pivot_longer(cols = -SA3_NAME16,
# names_to = c("age", "year") ,names_pattern = "([^\\_]*)\\_years_old_*(\\d{4})")

head(AEDC_all_years)
view(AEDC_all_years)

AEDC_all_years_long <- AEDC_all_years %>% pivot_longer(cols=c(-SA3_NAME16,-SA3_CODE16),
                                                       names_to = c("domain", "measure", "year"), names_pattern = "([^\\_]*)\\_([^\\_]*)\\_*(\\d{4})")

# pivot back wider for "measure" (POnTrack, NOnTrack etc...)
AEDC_all_years_long_fixed <- AEDC_all_years_long %>% pivot_wider(names_from = measure, values_from = value)

#HSLE removing year keeping all other variables (6)

#health, social, language, and emotional 

# ------------------------------------- #
# --- saving cleaned data as a csv  --- #
# ------------------------------------- #




write.csv(AEDC_all_years_long_fixed, file = "AEDC_SA3_2009-2021.csv",
          row.names = F) #cleaned data 

# write.csv(SA3_codes, file = "SA3_codes_names.csv") #codes and names only for ASGS 2016 SA3

# alternative 
#   save(var,name="xyz")
#   load("xyz")
                          

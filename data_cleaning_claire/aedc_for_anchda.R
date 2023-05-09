# Title: AEDC data preparation for ANCHDA

#Description: This code is written in R programming language and it is used to analyze data from the Australian Early Development Census (AEDC).
#The AEDC is a nationwide survey conducted every three years that assesses the developmental status of children in their first year of full-time school.
#The aim of this code is to calculate the prevalence of developmental vulnerabilities in different domains (physical health and wellbeing, social competence, emotional maturity,
#language and cognitive skills, and communication skills and general knowledge) across different Local Government Areas (LGAs) in Australia.
##The code first loads the necessary libraries including readxl, stringr, dplyr, purrr, and readr.
#Then, it sets the working directory to the folder where the input data is stored, and imports the data from a csv file using the read.csv function.
#The mutate function is then used to create new variables for each developmental domain, based on the values in the original variable for that domain.
#For example, a new variable DOT_PHW is created based on the PHYSCategory variable, and takes a value of 1 if the PHYSCategory is 3 or 4, and 0 otherwise.
##The code then calculates the prevalence of developmental vulnerabilities in each LGA for each developmental domain, based on the new variables created earlier.
#For example, for the physical health and wellbeing domain, the code uses the summarize function to calculate the number and proportion of children with
#a vulnerability in each LGA, as well as the number and proportion of children with a developmental delay or disorder (DAR),
#and the number and proportion of children with a developmental vulnerability only (DOT). This is repeated for each developmental domain.
##The results are stored in separate data frames LGA_df_PHW, LGA_df_SC, LGA_df_EM, LGA_df_LCS, and LGA_df_CSGK, which contain the prevalence estimates for
#each LGA, year, and gender. These data frames are saved in separate csv files in the output folder specified by path_out.

# Author: Claire Boulange
# Date: completed on 12/04/2023

# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

#set working directory and options
#set working directory and options
aedc_folder <- "Z:/CDA/Claire WD/aedc/inputs/"
path_out = "Z:/CDA/Claire WD/aedc/outputs/long/"

options(timeout = 600) 
setwd(aedc_folder)


#import unit record data from AEDC

df <- read.csv("220811B-Reeves (3).csv")
#create a copy for later mistakes
df_aedc <-df



# Create new variables for PHYSCategory
df <- df %>% 
  mutate(DOT_PHW = +(PHYSCategory %in% c(3, 4)),
         DV_PHW = +(PHYSCategory == 1),
         DAR_PHW = +(PHYSCategory == 2))

# Create new variables for SOCCategory
df <- df %>% 
  mutate(DOT_SC = +(SOCCategory %in% c(3, 4)),
         DV_SC = +(SOCCategory == 1),
         DAR_SC = +(SOCCategory == 2))

# Create new variables for EMOTCategory
df <- df %>% 
  mutate(DOT_EM = +(EMOTCategory %in% c(3, 4)),
         DV_EM = +(EMOTCategory == 1),
         DAR_EM = +(EMOTCategory == 2))

# Create new variables for LANGCOGCategory
df <- df %>% 
  mutate(DOT_LCS = +(LANGCOGCategory %in% c(3, 4)),
         DV_LCS = +(LANGCOGCategory == 1),
         DAR_LCS = +(LANGCOGCategory == 2))

# Create new variables for COMGENCategory
df <- df %>% 
  mutate(DOT_CSGK = +(COMGENCategory %in% c(3, 4)),
         DV_CSGK = +(COMGENCategory == 1),
         DAR_CSGK = +(COMGENCategory == 2))



#--------------------------------------------------------------------------------------------LGA----------------------------------------------
LGA_df_PHW <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")



LGA_df_SC <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")

LGA_df_EM <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")

LGA_df_LCS <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")

LGA_df_CSGK <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")


LGA_df_DV <- df %>%
  group_by(LGACode, Year, Gender) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")

#--------------------------------------------------------------------------------------------SA3----------------------------------------------
SA3_df_PHW <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")



SA3_df_SC <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")

SA3_df_EM <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")

SA3_df_LCS <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")

SA3_df_CSGK <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")


SA3_df_DV <- df %>%
  group_by(SA3Code, Year, Gender) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")
#--------------------------------------------------------------------------------------------SA2----------------------------------------------
SA2_df_PHW <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")



SA2_df_SC <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")

SA2_df_EM <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")

SA2_df_LCS <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")

SA2_df_CSGK <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")



SA2_df_DV <- df %>%
  group_by(SA2Code, Year, Gender) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")




#--------------------------------------------------------------------------------------------tables tidy up ----------------------------------------------
# Create a list of data frames
#df_list <- list(LGA_df, SA4_df, SA3_df, SA2_df)

df_list <- list(LGA_df_CSGK, LGA_df_EM, LGA_df_LCS, LGA_df_PHW, LGA_df_SC,
                SA2_df_CSGK, SA2_df_EM, SA2_df_LCS, SA2_df_PHW, SA2_df_SC,
                SA3_df_CSGK, SA3_df_EM, SA3_df_LCS, SA3_df_PHW, SA3_df_SC,
                LGA_df_DV, SA3_df_DV, SA2_df_DV)
# Rename the data frames
names(df_list) <- c("LGA_df_CSGK", "LGA_df_EM", "LGA_df_LCS", "LGA_df_PHW", "LGA_df_SC", 
                    "SA2_df_CSGK", "SA2_df_EM", "SA2_df_LCS", "SA2_df_PHW", "SA2_df_SC",
                    "SA3_df_CSGK", "SA3_df_EM", "SA3_df_LCS", "SA3_df_PHW", "SA3_df_SC",
                    "LGA_df_DV", "SA3_df_DV", "SA2_df_DV")

#df_list <- list(LGA_df_DV, SA3_df_DV, SA2_df_DV)

# Define a function to round numeric values and recode Gender column
round_and_recode <- function(df) {
  df %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    mutate(Gender = if_else(Gender == 1, "male", "female"))
}
# Apply the function to all data frames in the list
df_list <- map(df_list, round_and_recode)

# Define a function to rename the columns in a data frame
rename_cols <- function(df) {
  prefix <- str_replace(names(df)[1], "Code.*", "")
  names(df)[1:3] <- c(paste0(prefix, "_code16"), "calendar_year", "sex")
  names(df)[4:length(df)] <- paste0(names(df)[4:length(df)], "_aedc")
  names(df) <- tolower(names(df)) # convert column names to lowercase
  df
}

# Apply the function to each data frame in the list
df_list <- map(df_list, rename_cols)




# Define the function to replace invalid values
replace_invalid_vals <- function(df) {
  valid_cols <- names(df)[endsWith(names(df), "_valid_aedc")]
  for (col in valid_cols) {
    df[which(df[[col]] < 5), -(1:3)] <- 9999999
  }
  return(df)
}

# Apply the function to each tibble in df_list
df_list <- lapply(df_list, replace_invalid_vals)

#--------------------------------------------------------------------------------------------save all csv files ----------------------------------------------
for (df_name in names(df_list)) {
  # Extract the string before _df_ : this is [geography_code]
  geography_code <- str_extract(df_name, "^[^_]+(?=_df_)")
  # Extract the string after _df_ : this is [domain_code]
  domain_code <- str_extract(df_name, "(?<=_df_)[^_]+")
  
  # Construct the output file path and name based on domain code
  if (domain_code == "LCS") {
    domain_name <- "language_and_cognition"
    domain_code <- "421"
  } else if (domain_code == "SC") {
    domain_name <- "social_competence"
    domain_code <- "422"
  } else if (domain_code == "CSGK") {
    domain_name <- "communication_skills_and_general_knowledge"
    domain_code <- "423"
  } else if (domain_code == "EM") {
    domain_name <- "emotional_maturity"
    domain_code <- "424"
  } else if (domain_code == "PHW") {
    domain_name <- "physical_health_and_wellbeing"
    domain_code <- "425"
  } else if (domain_code == "DV") {
    domain_name <- "combined"
    domain_code <- "426"
  } else {
    stop(paste("Invalid domain code:", domain_code))
  }
  
  # Construct the output file path and name
  output_file <- paste0(path_out, "aedc_", domain_code,"_", domain_name,"_", geography_code, ".csv")
  
  # Save the dataframe as a CSV file
  write_csv(df_list[[df_name]], output_file)
  
  # Print confirmation message
  cat("Saved file:", output_file, "\n")
}

# Title: AEDC data preparation for ANCHDA
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
aedc_folder <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc/raw_data/"
path_out = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aedc/data_4_digits/"

#path_out <- "./output/data_4_digits/"
options(timeout = 600) 
setwd(aedc_folder)


#import unit record data from AEDC

df <- read.csv("./220811B-Reeves (3).csv")

##---------------------------------------------------------------------start cleaning here ------------------------------------------------------------------------
#remove rows with year = 2010
df <- df %>% filter(Year != 2010)

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


#--------------------------------------------------------------------------------------------LGA-------------------------------------------------------------------
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

LGA_df_PHW_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop") %>%
  mutate(Gender = "0")

LGA_df_PHW <- rbind(LGA_df_PHW, LGA_df_PHW_TOTAL )

###

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

LGA_df_SC_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop") %>%
  mutate(Gender = "0")

LGA_df_SC <- rbind(LGA_df_SC, LGA_df_SC_TOTAL)

###

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

LGA_df_EM_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

LGA_df_EM <- rbind(LGA_df_EM,LGA_df_EM_TOTAL )

#######

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

LGA_df_LCS_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop") %>%
  mutate(Gender = "0")

LGA_df_LCS <- rbind(LGA_df_LCS, LGA_df_LCS_TOTAL)

#####

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


LGA_df_CSGK_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop") %>%
  mutate(Gender = "0")

LGA_df_CSGK <- rbind(LGA_df_CSGK, LGA_df_CSGK_TOTAL)

#####

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


LGA_df_DV_TOTAL <- df %>%
  group_by(LGACode, Year) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")%>%
  mutate(Gender = "0")

LGA_df_DV <- rbind(LGA_df_DV, LGA_df_DV_TOTAL)
#--------------------------------------------------------------------------------------------SA3-------------------------------------------------------------------
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

SA3_df_PHW_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA3_df_PHW <- rbind(SA3_df_PHW, SA3_df_PHW_TOTAL )

####
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

SA3_df_SC_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop") %>%
  mutate(Gender = "0")

SA3_df_SC <- rbind(SA3_df_SC, SA3_df_SC_TOTAL)

######

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

SA3_df_EM_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA3_df_EM <- rbind(SA3_df_EM, SA3_df_EM_TOTAL)

#######

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

SA3_df_LCS_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA3_df_LCS <- rbind(SA3_df_LCS, SA3_df_LCS_TOTAL)



#####

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


SA3_df_CSGK_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA3_df_CSGK <- rbind(SA3_df_CSGK, SA3_df_CSGK_TOTAL)

######


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

SA3_df_DV_TOTAL <- df %>%
  group_by(SA3Code, Year) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop") %>%
  mutate(Gender = "0")

SA3_df_DV <- rbind(SA3_df_DV, SA3_df_DV_TOTAL)

#-------------------------------------------------------------SA2-------------------------------------------------------------------
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

SA2_df_PHW_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


SA2_df_PHW <- rbind(SA2_df_PHW, SA2_df_PHW_TOTAL )

######

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

SA2_df_SC_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA2_df_SC <- rbind(SA2_df_SC, SA2_df_SC_TOTAL )

######

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

SA2_df_EM_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA2_df_EM <- rbind(SA2_df_EM, SA2_df_EM_TOTAL)

######

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

SA2_df_LCS_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA2_df_LCS <- rbind(SA2_df_LCS, SA2_df_LCS_TOTAL)

####

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

SA2_df_CSGK_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


SA2_df_CSGK <- rbind(SA2_df_CSGK, SA2_df_CSGK_TOTAL)

####

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

SA2_df_DV_TOTAL <- df %>%
  group_by(SA2Code, Year) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA2_df_DV <- rbind(SA2_df_DV, SA2_df_DV_TOTAL)



#-------------------------------------------------------------SA4-------------------------------------------------------------------
SA4_df_PHW <- df %>%
  group_by(SA4Code, Year, Gender) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")

SA4_df_PHW_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


SA4_df_PHW <- rbind(SA4_df_PHW, SA4_df_PHW_TOTAL )

######

SA4_df_SC <- df %>%
  group_by(SA4Code, Year, Gender) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")

SA4_df_SC_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA4_df_SC <- rbind(SA4_df_SC, SA4_df_SC_TOTAL )

######

SA4_df_EM <- df %>%
  group_by(SA4Code, Year, Gender) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")

SA4_df_EM_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA4_df_EM <- rbind(SA4_df_EM, SA4_df_EM_TOTAL)

######

SA4_df_LCS <- df %>%
  group_by(SA4Code, Year, Gender) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")

SA4_df_LCS_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA4_df_LCS <- rbind(SA4_df_LCS, SA4_df_LCS_TOTAL)

####

SA4_df_CSGK <- df %>%
  group_by(SA4Code, Year, Gender) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")

SA4_df_CSGK_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


SA4_df_CSGK <- rbind(SA4_df_CSGK, SA4_df_CSGK_TOTAL)

####

SA4_df_DV <- df %>%
  group_by(SA4Code, Year, Gender) %>%
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

SA4_df_DV_TOTAL <- df %>%
  group_by(SA4Code, Year) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")%>%
  mutate(Gender = "0")

SA4_df_DV <- rbind(SA4_df_DV, SA4_df_DV_TOTAL)





#-------------------------------------------------------------STE-------------------------------------------------------------------
STE_df_PHW <- df %>%
  group_by(State, Year, Gender) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")

STE_df_PHW_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_DOT_PHW = sum(DOT_PHW[PHYSValid == 1]),
            P_DOT_PHW = sum(DOT_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DV_PHW = sum(DV_PHW[PHYSValid == 1]),
            P_DV_PHW = sum(DV_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            N_DAR_PHW = sum(DAR_PHW[PHYSValid == 1]),
            P_DAR_PHW = sum(DAR_PHW[PHYSValid == 1])/sum(PHYSValid == 1),
            PHW_Valid = sum(PHYSValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


STE_df_PHW <- rbind(STE_df_PHW, STE_df_PHW_TOTAL )

######

STE_df_SC <- df %>%
  group_by(State, Year, Gender) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")

STE_df_SC_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_DOT_SC = sum(DOT_SC[SOCValid == 1]),
            P_DOT_SC = sum(DOT_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DV_SC = sum(DV_SC[SOCValid == 1]),
            P_DV_SC = sum(DV_SC[SOCValid == 1])/sum(SOCValid == 1),
            N_DAR_SC = sum(DAR_SC[SOCValid == 1]),
            P_DAR_SC = sum(DAR_SC[SOCValid == 1])/sum(SOCValid == 1),
            SC_Valid = sum(SOCValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

STE_df_SC <- rbind(STE_df_SC, STE_df_SC_TOTAL )

######

STE_df_EM <- df %>%
  group_by(State, Year, Gender) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")

STE_df_EM_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_DOT_EM = sum(DOT_EM[EMOTValid == 1]),
            P_DOT_EM = sum(DOT_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DV_EM = sum(DV_EM[EMOTValid == 1]),
            P_DV_EM = sum(DV_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            N_DAR_EM = sum(DAR_EM[EMOTValid == 1]),
            P_DAR_EM = sum(DAR_EM[EMOTValid == 1])/sum(EMOTValid == 1),
            EM_Valid = sum(EMOTValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

STE_df_EM <- rbind(STE_df_EM, STE_df_EM_TOTAL)

######

STE_df_LCS <- df %>%
  group_by(State, Year, Gender) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")

STE_df_LCS_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1]),
            P_DOT_LCS = sum(DOT_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DV_LCS = sum(DV_LCS[LANGCOGValid == 1]),
            P_DV_LCS = sum(DV_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            N_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1]),
            P_DAR_LCS = sum(DAR_LCS[LANGCOGValid == 1])/sum(LANGCOGValid == 1),
            LCS_Valid = sum(LANGCOGValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")

STE_df_LCS <- rbind(STE_df_LCS, STE_df_LCS_TOTAL)

####

STE_df_CSGK <- df %>%
  group_by(State, Year, Gender) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")

STE_df_CSGK_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1]),
            P_DOT_CSGK = sum(DOT_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DV_CSGK = sum(DV_CSGK[COMGENValid == 1]),
            P_DV_CSGK = sum(DV_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            N_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1]),
            P_DAR_CSGK = sum(DAR_CSGK[COMGENValid == 1])/sum(COMGENValid == 1),
            CSGK_Valid = sum(COMGENValid == 1),
            .groups = "drop")%>%
  mutate(Gender = "0")


STE_df_CSGK <- rbind(STE_df_CSGK, STE_df_CSGK_TOTAL)

####

STE_df_DV <- df %>%
  group_by(State, Year, Gender) %>%
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

STE_df_DV_TOTAL <- df %>%
  group_by(State, Year) %>%
  summarize(N_V1 = sum(DV1, na.rm = TRUE),
            P_V1 = sum(DV1, na.rm = TRUE)/sum(!is.na(DV1)),
            V1_Valid = sum(!is.na(DV1)),
            N_V2 = sum(DV2 == 1, na.rm = TRUE),
            P_V2 = sum(DV2 == 1, na.rm = TRUE) / sum(!is.na(DV2)),
            V2_Valid = sum(!is.na(DV2)),
            N_OT5 = sum(OT5, na.rm = TRUE),
            P_OT5 = sum(OT5, na.rm = TRUE)/sum(!is.na(DV2)),
            OT5_Valid = sum(!is.na(OT5)),
            .groups = "drop")%>%
  mutate(Gender = "0")

STE_df_DV <- rbind(STE_df_DV, STE_df_DV_TOTAL)










#--------------------------------------------------------------------------------------------tables tidy up -------------------------------------------------------
# Create a list of data frames
df_list <- list(LGA_df_CSGK, LGA_df_EM, LGA_df_LCS, LGA_df_PHW, LGA_df_SC,
                SA2_df_CSGK, SA2_df_EM, SA2_df_LCS, SA2_df_PHW, SA2_df_SC,
                SA3_df_CSGK, SA3_df_EM, SA3_df_LCS, SA3_df_PHW, SA3_df_SC,
                SA4_df_CSGK, SA4_df_EM, SA4_df_LCS, SA4_df_PHW, SA4_df_SC,
                STE_df_CSGK, STE_df_EM, STE_df_LCS, STE_df_PHW, STE_df_SC,
                LGA_df_DV, SA3_df_DV, SA2_df_DV, SA4_df_DV, STE_df_DV)
# Rename the data frames
names(df_list) <- c("LGA_df_CSGK", "LGA_df_EM", "LGA_df_LCS", "LGA_df_PHW", "LGA_df_SC", 
                    "SA2_df_CSGK", "SA2_df_EM", "SA2_df_LCS", "SA2_df_PHW", "SA2_df_SC",
                    "SA3_df_CSGK", "SA3_df_EM", "SA3_df_LCS", "SA3_df_PHW", "SA3_df_SC",
                    "SA4_df_CSGK", "SA4_df_EM", "SA4_df_LCS", "SA4_df_PHW", "SA4_df_SC",
                    "STE_df_CSGK", "STE_df_EM", "STE_df_LCS", "STE_df_PHW", "STE_df_SC",
                    "LGA_df_DV", "SA3_df_DV", "SA2_df_DV", "SA4_df_DV", "STE_df_DV")


# Define function to filter out rows with 0 in LGA_Code, SA2_Code, SA3_Code, SA4_Code and State -------------------------------------------------------------------
filter_zeros <- function(df) {
  
  # Check for existence of each code column
  lga_col <- "LGACode" %in% names(df)
  sa2_col <- "SA2Code" %in% names(df)
  sa3_col <- "SA3Code" %in% names(df)
  sa4_col <- "SA4Code" %in% names(df)
  ste_col <- "State" %in% names(df)
  # Filter out rows with 0 in code columns
  if(lga_col) {
    df <- df %>% filter(LGACode != 0)
  }
  if(sa2_col) {
    df <- df %>% filter(SA2Code != 0)
  }
  if(sa3_col) {
    df <- df %>% filter(SA3Code != 0)
  }
  if(sa4_col) {
    df <- df %>% filter(SA4Code != 0)
  }
  if(ste_col) {
    df <- df %>% filter(State != "")
  }
  return(df)
}

# Apply the function to each data frame in the list
df_list <- map(df_list, filter_zeros)





# Define a function to round numeric values and re-code Gender column-----------------------------------------------------------------------------------------------
round_and_recode <- function(df) {
  df <- df %>%
    mutate(across(starts_with("P_"), ~ifelse(round(.x, 4) %% 1 == 0, sprintf("%.4f", .x), sprintf("%.4f", ceiling(.x * 10^4) / 10^4))))
  
  df$Gender[df$Gender == "0"] <- "all"
  df$Gender[df$Gender == "1"] <- "male"
  df$Gender[df$Gender == "2"] <- "female"
  
  if ("State" %in% colnames(df)) {
    df$State <- recode(df$State, "NSW" = 1, "VIC" = 2, "QLD" = 3, "SA" = 4, "WA" = 5, "TAS" = 6, "NT" = 7, "ACT" = 8)
  }
  
  df
}

# Apply the function to all data frames in the list-----------------------------------------------------------------------------------------------------------------
df_list <- map(df_list, round_and_recode)



# Define a function to rename the columns and add a new column in a data frame
rename_cols <- function(df) {
  if ("State" %in% colnames(df)) {
    names(df)[names(df) == "State"] <- "STE"  # Rename "State" column to "STE_CODE21"
  }
  prefix <- str_replace(names(df)[1], "Code.*", "")
  names(df)[1] <- paste0(prefix, "_CODE21")  # Rename first column using the prefix
  names(df)[-1] <- tolower(names(df)[-1])  # Convert column names to lowercase except for the first column
  names(df)[2:3] <- c("calendar_year", "sex")
  df <- cbind(df[,1:3], age_group = "5-5", df[,4:ncol(df)])  # Add a new column called age_group
  df
}


# Apply the function to each data frame in the list
df_list <- map(df_list, rename_cols)




# Create the output directory if it doesn't already exist --- save files pre-cell suppression
dir.create(path_out, showWarnings = FALSE)

# Loop through each CSV file and save them to the output directory
for (df_name in names(df_list)) {
  # Extract the string before _df_: this is [geography_code]
  geography_code <- str_extract(df_name, "^[^_]+(?=_df_)")
  # Extract the string after _df_: this is [domain_code]
  domain_code <- str_extract(df_name, "(?<=_df_)[^_]+")
  
  # Construct the output file path and name based on the domain code and geography_code
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
  
  # Create a subdirectory for each unique geography_code
  output_subdir <- file.path(path_out, geography_code)
  dir.create(output_subdir, showWarnings = FALSE, recursive = TRUE)
  
  # Construct the output file path and name including the geography_code subdirectory
  output_file <- file.path(output_subdir, paste0("aedc_", domain_code, "_", domain_name, "_", geography_code, ".csv"))
  
  # Save the dataframe as a CSV file using the updated file path
  write.csv(df_list[[df_name]], file = output_file, row.names = FALSE)
  
  # Print confirmation message
  cat("Saved file:", output_file, "\n")
}

# Define the function to replace invalid values with suppression --------------------------------------------------------------------------------------------------

replace_invalid_vals <- function(df, threshold = 15, suppression = "9999999") {
  valid_cols <- names(df)[endsWith(names(df), "_valid")]
  for (col in valid_cols) {
    invalid_cells <- which(df[[col]] < threshold)
    df[invalid_cells, -(1:4)] <- suppression
  }
  
  n_cols <- names(df)[startsWith(names(df), "n_")]
  for (col in n_cols) {
    invalid_cells <- which(df[[col]] %in% 1:4)
    df[invalid_cells, -(1:4)] <- suppression
  }
  
  return(df)
}

# Create a subfolder for the cell suppressed CSV files (save here the csv files post cell suppression)
subfolder <- "cell_suppressed"
output_subdir <- file.path(path_out, subfolder)
dir.create(output_subdir, showWarnings = FALSE)

# Loop through the files in the input directory
files <- list.files(path_out, pattern = "*.csv", full.names = TRUE)
for (file in files) {
  # Read the CSV file
  df <- read.csv(file)
  
  # Extract the geography_code from the file name
  filename <- basename(file)
  geography_code <- sub("^.+_(\\w+)\\.csv$", "\\1", filename)
  
  # Create a subdirectory for each unique geography_code
  output_geography_subdir <- file.path(output_subdir, geography_code)
  dir.create(output_geography_subdir, showWarnings = FALSE, recursive = TRUE)
  
  # Replace invalid values with suppression
  df_modified <- replace_invalid_vals(df)
  
  # Construct the output file path and name including the geography_code subdirectory
  out_file <- file.path(output_geography_subdir, basename(file))
  
  # Write the modified data frame to a new CSV file in the subdirectory based on geography_code
  write.csv(df_modified, out_file, row.names = FALSE)
  
  # Print the name and path of the file created
  cat(sprintf("File created: %s\n", out_file))
}

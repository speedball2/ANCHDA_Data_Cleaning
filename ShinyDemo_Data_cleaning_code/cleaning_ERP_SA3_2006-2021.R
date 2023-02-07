# ----------------------------------------- #
# --- CLeaning / Formatting ERP data SA3 2006-2021
# ----------------------------------------- #

setwd("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/Census_ERP/SA3")

# ----------------- #
# --- Libraries --- #
# ----------------- #

#library(sf)
#library(rmapshaper)
library(tidyverse)
library(readxl)
library(writexl)
#library(viridis)
#library(leaflet)
#library(htmltools)
library(stringr)



#---------------
# read in raw file
#--------------

ERP_SA3_raw <- read.csv("ABS_ERP_ASGS2016_SA3.csv")


ERP_SA3_cleaned <- ERP_SA3_raw[,c(3,4,6,8,9)]

names(ERP_SA3_cleaned) <- c("sex", "ageGroup", "SA3_code_name","year", "ERP")

ERP_SA3_cleaned$SA3_CODE16 <- str_sub(ERP_SA3_cleaned$SA3_code_name,1,5)

ERP_SA3_cleaned$SA3_NAME16 <- str_sub(ERP_SA3_cleaned$SA3_code_name,7,-1)

ERP_SA3_cleaned$sex <- str_sub(ERP_SA3_cleaned$sex,4,-1)

ERP_SA3_cleaned$ageGroup <- str_sub(ERP_SA3_cleaned$ageGroup,6,-1)

ERP_SA3_cleaned <- ERP_SA3_cleaned[,-3] #delete code-name column

view(ERP_SA3_cleaned)


write.csv(ERP_SA3_cleaned,file="ERP_SA3_age-sex_2006-2021.csv",row.names = FALSE)


SA3_codes_names <- ERP_SA3_cleaned[,c(5,6)]

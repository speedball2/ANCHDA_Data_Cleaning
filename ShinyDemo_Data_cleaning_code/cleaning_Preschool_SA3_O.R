

# ----------------------------------------- #
# --- Cleaning Preschool Data Harriette --- #
# ----------------------------------------- #

setwd("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_CLEAN/Data_cleaning_code")
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

# ------------------- #
# --- Shape files --- #
# ------------------- #

# SA2_rSHP <- ms_simplify(AUS_SA2_SHP, keep = 003) # Code for simplifying shapefiles - (you'll only ever need to do this once)
# #st_write(SA2_rSHP, "rSA2shp")
# 
# AUS_SA2_rSHP <- read_sf("SA2_SHP","SA2_2016_AUST_Simple") %>%
#   filter(!st_is_empty(.)) %>% filter(STE_CODE16 != "9") # SA2.
# 
# AUS_SA2_rSHP_noPoly <- st_drop_geometry(AUS_SA2_rSHP)
# 
# # --------------------------------------- #
# --- Reading in xlsx preschool data  --- #
# --------------------------------------- #

#preschool data from tablebuilder, SA2, age, child is in pre-school the year before full-time schooling 

#only read in SA2NAME from 1st sheet (?)



Preschool_nopoly <- cbind(read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2013.xlsx",
                                     sheet = 1,
                                     range = "B12:B363"),
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2013.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2013.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"), #5YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2014.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2014.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"), #5YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2015.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2015.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"), #5YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2016.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2016.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"), #5YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2017.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2017.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"),
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2018.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2018.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"),
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2019.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2019.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"),
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2020.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2020.xlsx",
                                     sheet = 1,
                                     range = "I12:J363"),
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2021.xlsx",
                                     sheet = 1,
                                     range = "F12:G363"), #4YO_MF
                          read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2021.xlsx",
                                     sheet = 1,
                                     range = "I12:J363")
                          ) #5YO_MF



#ASGS changes from 2018
# 
#                           read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2018.xlsx",
#                                      sheet = 1,
#                                      range = "C12:K364"),
#                           read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2019.xlsx",
#                                      sheet = 1,
#                                      range = "C12:K364"),
#                           read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2020.xlsx",
#                                      sheet = 1,
#                                      range = "C12:K364"),
#                           read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/preschool_SA3/age_sex_nototals/preschool_SA3_age_sex_2021.xlsx",
#                                      sheet = 1,
#                                      range = "C12:K364"))

ages <- c(4,5)
sexes <- c("Males","Females")
years <- seq(2013,2021)

names_df <- crossing(ages,sexes,years)

names_df$colnames <- paste(names_df$ages, names_df$sexes, names_df$years, sep="_")

names_df <- names_df %>% arrange(years,ages,desc(sexes)) #need to check male/female order with desc() for descending order
names_df$colnames

colnames(Preschool_nopoly) <- c("SA3_NAME16", names_df$colnames)

view(Preschool_nopoly)


# read in SA3 codes
SA3_codes <- read.csv("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_CLEAN/SA3_codes_names.csv")
SA3_codes <- SA3_codes[,c(2,3)]

# Add SA3 Codes column

Preschool_nopoly <- inner_join(Preschool_nopoly,SA3_codes,by="SA3_NAME16")



# Pivot LONGER --> long, tidy format. One column for age, one for year

preschool_nopoly_longer <- Preschool_nopoly %>% pivot_longer(cols=c(-SA3_NAME16,-SA3_CODE16),
                                                                    names_to = c("age", "sex", "year"), names_pattern = "(\\d{1})\\_([^\\_]*)\\_*(\\d{4})")


preschool_nopoly_longer <- preschool_nopoly_longer %>% rename(preschool_enrolled = value)



# Calculate total for sexes - sum M and F
preschool_wider_sexes <- pivot_wider(preschool_nopoly_longer, names_from = "sex",values_from="preschool_enrolled")


preschool_wider_sexes$Persons <- preschool_wider_sexes$Males + preschool_wider_sexes$Females


preschool_back_to_longer <- preschool_wider_sexes %>% pivot_longer(cols=c(Males,Females,Persons),
                                                              names_to = c("sex"), values_to = "preschool_enrolled")

# Save preschool SA3 csv

write.csv(preschool_back_to_longer,file="preschool_SA3_age_sex_2013-2021.csv")



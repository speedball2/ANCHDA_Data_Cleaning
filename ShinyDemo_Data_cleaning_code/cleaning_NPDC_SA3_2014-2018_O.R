# ----------------------------------------- #
# --- CLeaning / Formatting NPDC data 2014-2019
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



#---------------
# read in raw files
#--------------

NPDC_raw_2014 <- cbind(
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                                     sheet = 88,
                                     range = "C7:E333"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                             sheet = 88,
                             range = "G7:G333"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                             sheet = 89,
                             range = "D7:D333"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                             sheet = 89,
                             range = "G7:G333"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                             sheet = 90,
                             range = "D7:D333"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2014.xls",
                             sheet = 90,
                             range = "G7:G333"))

colnames(NPDC_raw_2014) <- c("SA3_NAME16", "N_antenatal_visit_14_weeks_2014", "N_totalbirths_2014","P_antenatal_visit_14_weeks_2014","N_smoked_20_weeks_2014","P_smoked_20_weeks_2014","N_low_birth_weight_2014","P_low_birth_weight_2014")
                        

NPDC_raw_2015 <- cbind(
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 88,
                             range = "C7:E334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 88,
                             range = "G7:G334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 89,
                             range = "D7:D334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 89,
                             range = "G7:G334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 90,
                             range = "D7:D334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2015.xls",
                             sheet = 90,
                             range = "G7:G334"))

colnames(NPDC_raw_2015) <- c("SA3_NAME16","N_antenatal_visit_14_weeks_2015", "N_totalbirths_2015","P_antenatal_visit_14_weeks_2015","N_smoked_20_weeks_2015","P_smoked_20_weeks_2015","N_low_birth_weight_2015","P_low_birth_weight_2015")

                        

NPDC_raw_2016 <- cbind(
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 93,
                             range = "C7:E334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 93,
                             range = "G7:G334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 94,
                             range = "D7:D334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 94,
                             range = "G7:G334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 96,
                             range = "D7:D334"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2016.xlsx",
                             sheet = 96,
                             range = "G7:G334"))

colnames(NPDC_raw_2016) <- c("SA3_NAME16","N_antenatal_visit_14_weeks_2016", "N_totalbirths_2016","P_antenatal_visit_14_weeks_2016","N_smoked_20_weeks_2016","P_smoked_20_weeks_2016","N_low_birth_weight_2016","P_low_birth_weight_2016")

NPDC_raw_2017 <- cbind(
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 93,
                             range = "C7:E341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 93,
                             range = "G7:G341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 94,
                             range = "D7:D341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 94,
                             range = "G7:G341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 96,
                             range = "D7:D341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2017.xlsx",
                             sheet = 96,
                             range = "G7:G341"))

colnames(NPDC_raw_2017) <- c("SA3_NAME16","N_antenatal_visit_14_weeks_2017", "N_totalbirths_2017","P_antenatal_visit_14_weeks_2017","N_smoked_20_weeks_2017","P_smoked_20_weeks_2017","N_low_birth_weight_2017","P_low_birth_weight_2017")

NPDC_raw_2018 <- cbind(
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 93,
                             range = "C7:E341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 93,
                             range = "G7:G341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 94,
                             range = "D7:D341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 94,
                             range = "G7:G341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 96,
                             range = "D7:D341"),
                  read_excel(col_names = F, path="/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/NPDC_SA3_2014-2018/AIHWMothers&Babies_2018.xlsx",
                             sheet = 96,
                             range = "G7:G341")
                  )

colnames(NPDC_raw_2018) <- c("SA3_NAME16","N_antenatal_visit_14_weeks_2018", "N_totalbirths_2018","P_antenatal_visit_14_weeks_2018","N_smoked_20_weeks_2018","P_smoked_20_weeks_2018","N_low_birth_weight_2018","P_low_birth_weight_2018")

NPDC_raw <- NPDC_raw_2014 %>% inner_join(NPDC_raw_2015, by="SA3_NAME16") %>% inner_join(NPDC_raw_2016, by="SA3_NAME16") %>% inner_join(NPDC_raw_2017, by="SA3_NAME16") %>% inner_join(NPDC_raw_2018, by="SA3_NAME16")



# colnames(NPDC_raw) <- c("SA3_NAME16", "N_antenatal_visit_14_weeks_2014", "N_totalbirths_2014","P_antenatal_visit_14_weeks_2014","N_smoked_20_weeks_2014","P_smoked_20_weeks_2014","N_low_birth_weight_2014","P_low_birth_weight_2014",
#                         "N_antenatal_visit_14_weeks_2015", "N_totalbirths_2015","P_antenatal_visit_14_weeks_2015","N_smoked_20_weeks_2015","P_smoked_20_weeks_2015","N_low_birth_weight_2015","P_low_birth_weight_2015",
#                         "N_antenatal_visit_14_weeks_2016", "N_totalbirths_2016","P_antenatal_visit_14_weeks_2016","N_smoked_20_weeks_2016","P_smoked_20_weeks_2016","N_low_birth_weight_2016","P_low_birth_weight_2016",
#                         "N_antenatal_visit_14_weeks_2017", "N_totalbirths_2017","P_antenatal_visit_14_weeks_2017","N_smoked_20_weeks_2017","P_smoked_20_weeks_2017","N_low_birth_weight_2017","P_low_birth_weight_2017",
#                         "N_antenatal_visit_14_weeks_2018", "N_totalbirths_2018","P_antenatal_visit_14_weeks_2018","N_smoked_20_weeks_2018","P_smoked_20_weeks_2018","N_low_birth_weight_2018","P_low_birth_weight_2018")
# 



  
  
# read in SA3 codes
SA3_codes <- read.csv("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_CLEAN/SA3_codes_names.csv")
SA3_codes <- SA3_codes[,c(2,3)]

# Add SA3 Codes column

NPDC_raw <- inner_join(NPDC_raw,SA3_codes,by="SA3_NAME16")



# Make all vars numeric (turn text to NA)
names(NPDC_raw)[2:36]
NPDC_raw[,2:36] <- sapply(NPDC_raw[,2:36],as.numeric)

# Pivot LONGER --> long, tidy format. One column for age, one for year

NPDC_longer <- NPDC_raw %>% pivot_longer(cols=c(-SA3_NAME16,-SA3_CODE16),
                                                             names_to = c("variable","year"), names_pattern = "(.*\\_)\\_*(\\d{4})")


# pivot back wider
NPDC_longer <- NPDC_longer %>% pivot_wider(names_from = variable, values_from = value)

view(NPDC_longer)



# Save csv

write.csv(NPDC_longer,file="NPDC_2014-2018.csv",row.names = F)



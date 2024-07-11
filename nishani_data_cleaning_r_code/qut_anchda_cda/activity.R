library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#------

source("./functions/read_files.R")
source("./functions/letters2numbers.R")
source("./functions/data_extraction.R")
#----------------------
#2016 geo data
geom_data <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

geom_data <- geom_data[, c("SA2_MAINCODE_2016", "SA2_NAME_2016")]



##### what ever the reason can't use RC in the re arrange definition, so have added blank column in the raw data file --- important --------################


#--------------------------------------------------------
#reading iadatasheet_mothers sheet ----------- COMPLETE | 
#--------------------------------------------------------
# mothers aged 15-19
#CDA_117_births_mothers_aged_15_19_SA2

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "C5:CK258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_births_mothers_aged_15_19_prior_moving_average_5year" , "p_births_mothers_aged_15_19_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mothers/CDA_1111_births_mothers_aged_15_19_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#-------------------------
# mothers aged 20-24
#CDA_117_births_mothers_aged_20_24_SA2

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "CL5:FT258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_births_mothers_aged_20_24_prior_moving_average_5year" , "p_births_births_mothers_aged_20_24_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mothers/CDA_1112_births_mothers_aged_20_24_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#-------------------------

# % Preterm births (< 37 weeks)
#CDA_117_pre_term_birth_less_37_weeks_SA2

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "FU5:JC258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_pre_term_birth_less_37_prior_moving_average_5year" , "p_pre_term_birth_less_37_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mothers/CDA_117_pre_term_birth_less_37_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#-------------------------
# % Preterm births (< 39 weeks)
#CDA_117_pre_term_birth_less_39_weeks_SA2

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "JD5:ML258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_pre_term_birth_less_39_prior_moving_average_5year" , "p_pre_term_birth_less_39_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA//SA2/mothers/CDA_117_pre_term_birth_less_39_prior_moving_average_5year_SA2.csv", row.names = FALSE)

#---------------------
# % Low birth weight (born alive < 2500g)
#CDA_111_lowbirthweight_under_2500g_SA2
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "MM5:PU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year" , "p_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA//SA2/mothers/CDA_111_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year_SA2.csv", row.names = FALSE)

#----------------------
# % Mothers who smoked tobacco at any time during pregnancy
#CDA_114_mother_smoked_tobacco_during_pregnancy_SA2
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "PV5:SF258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_mothers_smoked_tobacco_at_any_time_during_pregnancy" , "p_mothers_smoked_tobacco_at_any_time_during_pregnancy", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mothers/CDA_114_mothers_smoked_tobacco_at_any_time_during_pregnancy_SA2.csv", row.names = FALSE)


#----------------------------------------------------------
#reading iadatasheet_ED sheet ----------------- COMPLETE  |
#----------------------------------------------------------
# ED presentations for 0-4yo (per 1000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "C5:BA258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_ED_presentations" , "rate_ED_presentations", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_0_4_years_SA2.csv", row.names = FALSE)
#----------------------------------------------------
# ED presentations for 5-9yo (per 1000 persons)

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "BB5:CZ258" ) 
new_data <- data_extraction(data, SA_name , "all", "5-9", "n_ED_presentations" , "rate_ED_presentations", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_5_9_years_SA2.csv", row.names = FALSE)
#----------------------------------------------------
# ED presentations for 10-14yo (per 1000 persons)

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "DA5:EY258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-14", "n_ED_presentations" , "rate_ED_presentations", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_10_14_years_SA2.csv", row.names = FALSE)
#------------------------------------------------
# ED presentations for 15-19yo (per 1000 persons)

SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "EZ5:GX258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-19", "n_ED_presentations" , "rate_ED_presentations", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_15_19_years_SA2.csv", row.names = FALSE)

#------------------------------------------------
# ED presentations for 20-24yo (per 1000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "GY5:IW258" ) 
new_data <- data_extraction(data, SA_name , "all", "20-24", "n_ED_presentations" , "rate_ED_presentations", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_20_24_years_SA2.csv", row.names = FALSE)

#--------------------------------
#combining all ages for ED presentations

data_0_4 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_0_4_years_SA2.csv", header = TRUE, check.names = FALSE)
data_5_9 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_5_9_years_SA2.csv", header = TRUE, check.names = FALSE)
data_10_14 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_10_14_years_SA2.csv", header = TRUE, check.names = FALSE)
data_15_19 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_15_19_years_SA2.csv", header = TRUE, check.names = FALSE)
data_20_24 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_20_24_years_SA2.csv", header = TRUE, check.names = FALSE)


write.csv(rbind(rbind(rbind(rbind(data_0_4, data_5_9), data_10_14), data_15_19), data_20_24), "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/emergengy_dep/CDA_1302_ED_presentations_for_0_24_years_SA2.csv", row.names = FALSE)








#----------------------------------------------------------
#reading iadatasheet_physical sheet ----------- COMPLETE  |
#----------------------------------------------------------

# Injury hospitalisations for 0-24 year olds (per 10,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "C5:BA258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_injury_hospitalisations_for_0_4_year_olds" , "rate_injury_hospitalisations_for_0_4_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_174_injury_hospitalisations_for_0_4_year_olds_SA2.csv", row.names = FALSE)

#------------
# Injury hospitalisations for 0-24 year olds (per 10,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "BB5:CZ258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_injury_hospitalisations_for_0_24_year_olds" , "rate_injury_hospitalisations_for_0_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_174_injury_hospitalisations_for_0_24_year_olds_SA2.csv", row.names = FALSE)
#------------

# Intentional injury hospitalisations for 0-24 year olds (per 10,000 persons) -  Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "DA5:ES258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_intentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_intentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_174_intentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

# Unintentional injury hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ET5:GL258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_unintentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_unintentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_174_unintentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

# Fall related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "GM5:IE258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_fall_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year" , "rate_fall_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_fall_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Fall related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "IF5:JX258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_fall_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_fall_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_fall_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Transport related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "JY5:LQ258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_transport_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_transport_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_transport_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Assault related hospitalisations for 15-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "LR5:ND258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_assault_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year" , "rate_assault_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_assault_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

# Disability related hospitalisations for 0-4 year olds (per 10,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "NE5:PC258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_disability_related_hospitalisations_for_0_4_year_olds" , "rate_disability_related_related_for_0_4_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_disability_related_hospitalisations_for_0_4_year_olds_SA2.csv", row.names = FALSE)
#------------

# Disability related hospitalisations for 0-24 year olds
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "PD5:RB258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_disability_related_hospitalisations_for_0_24_year_olds" , "rate_disability_related_related_for_0_24_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_disability_related_hospitalisations_for_0_24_year_olds_SA2.csv", row.names = FALSE)
#------------

# Alcohol related hospitalisations for 15-24 year olds
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258")
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "RD5:SV258" )  
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_alcohol_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year" , "rate_alcohol_related_related_for_15_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_alcohol_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------


#Substance related hospitalisations for 15-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "SW5:UO258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_substance_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year" , "rate_substance_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_substance_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Chronic physical illness related hospitalisations for 0-4 year olds (per 10,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "UP5:WN258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds" , "rate_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds_SA2.csv", row.names = FALSE)
#------------

#Chronic physical illness related hospitalisations for 0-24 year olds (per 10,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "WO5:YM258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds" , "rate_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds_SA2.csv", row.names = FALSE)
#------------

#Cancer related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "YN5:AAF258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_cancer_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year" , "rate_cancer_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_cancer_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Cancer related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AAG5:ABY258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_cancer_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_cancer_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_cancer_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Cardiovascular related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ABZ5:ADL258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year" , "rate_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Cardiovascular related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ADM5:AEY258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_cardiovascular_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_cardiovascular_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_cardiovascular_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Diabetes related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AEZ5:AGL258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_diabetes_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_diabetes_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_diabetes_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Disorders of ear and hearing related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AGM5:AIE258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year" , "rate_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Disorders of ear and hearing related hospitalisations for 0-9 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AIF5:AJX258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-9", "n_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds_prior_moving_average_3year" , "rate_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

# Oral disease related hospitalisations for 0-4 year olds
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AJY5:ALW258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_oral_disease_related_hospitalisations_for_0_4_year_olds" , "rate_oral_disease_related_for_0_4_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_oral_disease_related_hospitalisations_for_0_4_year_olds_SA2.csv", row.names = FALSE)
#------------

# Oral disease related hospitalisations for 0-24 year olds
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ALX5:ANV258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_oral_disease_related_hospitalisations_for_0_24_year_olds" , "rate_oral_disease_related_for_0_24_year_olds", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_oral_disease_related_hospitalisations_for_0_24_year_olds_SA2.csv", row.names = FALSE)
#------------


#Obesity related hospitalisations for 10-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ANW5:API258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_obesity_related_hospitalisations_for_10_24_year_olds_prior_moving_average_5year" , "rate_obesity_related_hospitalisations_for_10_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_obesity_related_hospitalisations_for_10_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Kidney related hospitalisations for 15-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "APJ5:AQV258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_kidney_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year" , "rate_kidney_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_kidney_related_hospitalisations_for_15_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Musculoskeletal disease related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AQW5:ASI258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------

#Respiratory related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ASJ5:AUB258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_respiratory_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year" , "rate_respiratory_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_respiratory_related_hospitalisations_for_0_4_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Respiratory related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AUC5:AVU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_respiratory_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_respiratory_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_respiratory_related_hospitalisations_for_0_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#------------

#Vaccine preventable disease related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AVV5:AXH258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year" , "rate_vaccine_preventable_disease_related_for_0_4_year_oldsprior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------
#
# Vaccine preventable disease related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AXI5:AYU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_vaccine_preventable_disease_related_for_0_24_year_olds_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------
#
#Influenza related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AYV5:BAH258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_influenza_related_hospitalisations_0_24_years_prior_moving_average_5year" , "rate_influenza_related_hospitalisations_0_24_years_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_influenza_related_hospitalisations_0_24_years_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------
#
#Group A Streptococcus related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "BAI5:BBU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_5year" , "rate_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------
#
#Rheumatic heart disease related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "BBV5:BDH258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year" , "rate_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------
#
#Rotovirus related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "BDI5:BEU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year" , "rate_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/hospital/CDA_1204_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year_SA2.csv", row.names = FALSE)
#------------





#-------------------------------------------------------
#reading iadatasheet_mental sheet
#-------------------------------------------------------
#
# Births to mothers with a mental illness (%) - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "AV5:CT258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_births_to_mothers_with_a_mental_illness" , "p_births_to_mothers_with_a_mental_illness", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_158_births_to_mothers_with_a_mental_illness_SA2.csv", row.names = FALSE)
#----------------------
#
# Mental health and behavioural disorder hospitalisations for 10-24 year old
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "AV5:CT258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds_SA2.csv", row.names = FALSE)
#----------------------
# 
# Mental health and behavioural disorder hospitalisations for 15-24 year old
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "CU5:ES258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds_SA2.csv", row.names = FALSE)
#----------------------
#
# deliberate_self_harm_related hospitalisations for 10-24 year old  - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "ET5:GL258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_deliberate_self_harm_related_hospitalisations_for_10_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#----------------------
#
# Deliberate self-harm related hospitalisations for 15-24 year old  - Prior moving average (3 year)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "GM5:IE258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_deliberate_self_harm_related_hospitalisations_for_15_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year_SA2.csv", row.names = FALSE)
#----------------------
#
# Mental health related hospitalisations for 10-24 year old
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "IF5:KD258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_related_hospitalisations_for_10_24_year" , "rate_mental_health_related_hospitalisations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_mental_health_related_hospitalisations_for_10_24_year_olds_SA2.csv", row.names = FALSE)
#----------------------
#
# Mental health related hospitalisations for 15-24 year old (per 10,000 persons)
#
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "KE5:MC258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_related_hospitalisations_for_15_24_year" , "rate_mental_health_related_hospitalisations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_156_mental_health_related_hospitalisations_for_15_24_year_olds_SA2.csv", row.names = FALSE)
#----------------------
#
# Deliberate self-harm related ED presentations for 10-24 year old (per 10,000 persons) - Prior moving average (5 year)
#
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "MD5:NP258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_prior_moving_average_5_year" , "rate_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_per_10000_persons_prior_moving_average_5_year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_155_deliberate_self_harm_related_ED_presentations_for_10_24_year_prior_moving_average_5_year_SA2.csv", row.names = FALSE)
#----------------------
# Deliberate self-harm related ED presentations for 15-24 year old (per 10,000 persons) - Prior moving average (5 year)
#
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "NQ5:PC258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_prior_moving_average_5_year" , "rate_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_per_10000_persons_prior_moving_average_5_year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_155_deliberate_self_harm_related_ED_presentations_for_15_24_year_prior_moving_average_5_year_SA2.csv", row.names = FALSE)
#----------------------
#
# Mental health related ED presentations for 10-24 year old (per 10,000 persons)
#
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "PD5:RB258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_related_ED_presentations_for_10_24_year_olds" , "rate_mental_health_related_ED_presentations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_155_mental_health_related_ED_presentations_for_10_24_year_olds_SA2.csv", row.names = FALSE)

#------------------------------
# Mental health related ED presentations for 15-24 year old
#SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "RD5:TB258" )
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_related_ED_presentations_for_15_24_year_olds" , "rate_mental_health_related_ED_presentations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_155_mental_health_related_ED_presentations_for_15_24_year_olds_SA2.csv", row.names = FALSE)




# Community mental health service contacts for 0-24 year olds (per 1,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "TC5:VA258" )
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_community_mental_health_service_contacts_for_0_24_year" , "rate_community_mental_health_service_contacts_for_0_24_year", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_0_24_year_SA2.csv", row.names = FALSE)



#--------------------------------
# Community mental health service contacts for 5-9 year olds (per 1,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "VB5:WZ258" )
new_data <- data_extraction(data, SA_name , "all", "5-9", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_5_9_year_SA2.csv", row.names = FALSE)

#--------------------------------
# Community mental health service contacts for 10-14 year olds (per 1,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "XA5:YY258" )
new_data <- data_extraction(data, SA_name , "all", "10-14", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_10_14_year_SA2.csv", row.names = FALSE)
#--------------------------------
# Community mental health service contacts for 15-19 year olds (per 1,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "YZ5:AAX258" )
new_data <- data_extraction(data, SA_name , "all", "15-19", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_15_19_year_SA2.csv", row.names = FALSE)

#--------------------------------
# Community mental health service contacts for 20-24 year olds (per 1,000 persons)
SA_name <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "AAY5:ACW258" )
new_data <- data_extraction(data, SA_name , "all", "20-24", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts", geom_data)
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_20_24_year_SA2.csv", row.names = FALSE)



data_5_9 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_5_9_year_SA2.csv", header = TRUE, check.names = FALSE)
data_10_14 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_10_14_year_SA2.csv", header = TRUE, check.names = FALSE)
data_15_19 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_15_19_year_SA2.csv", header = TRUE, check.names = FALSE)
data_20_24 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_20_24_year_SA2.csv", header = TRUE, check.names = FALSE)



write.csv(rbind(rbind(rbind(data_5_9, data_10_14), data_15_19), data_20_24), "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA2/mental_health/CDA_157_community_mental_health_service_contacts_for_5_24_years_SA2.csv", row.names = FALSE)





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
geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

geom_data <- geom_data[, c("SA2_MAINCODE_2016", "SA2_NAME_2016")]



#-------------------------------------------------------
#reading iadatasheet_mothers sheet
#-------------------------------------------
# % Preterm births (< 37 weeks)
#CDA_117_pre_term_birth_less_37_weeks_SA2

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "FU5:JC258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_pre_term_birth_less_37_prior_moving_average_5year" , "p_pre_term_birth_less_37_prior_moving_average_5year", geom_data)
write.csv(new_data, "./output/CDA_117_pre_term_birth_less_37_prior_moving_average_5year.csv", row.names = FALSE)
#-------------------------
# % Preterm births (< 39 weeks)
#CDA_117_pre_term_birth_less_39_weeks_SA2

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "JD5:ML258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_pre_term_birth_less_39_prior_moving_average_5year" , "p_pre_term_birth_less_39_prior_moving_average_5year", geom_data)
write.csv(new_data, "./output/CDA_117_pre_term_birth_less_39_prior_moving_average_5year.csv", row.names = FALSE)

#---------------------
# % Low birth weight (born alive < 2500g)
#CDA_111_lowbirthweight_under_2500g_SA2

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "MM5:PU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year" , "p_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year", geom_data)
write.csv(new_data, "./output/CDA_111_low_birth_weight_born_alive_less_2500g_prior_movingaverage_5year.csv", row.names = FALSE)


#----------------------
# % Mothers who smoked tobacco at any time during pregnancy
#CDA_114_mother_smoked_tobacco_during_pregnancy_SA2
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mothers", "PV5:SF258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-0", "n_mothers_smoked_tobacco_at_any_time_during_pregnancy" , "p_mothers_smoked_tobacco_at_any_time_during_pregnancy", geom_data)
write.csv(new_data, "./output/CDA_114_mothers_smoked_tobacco_at_any_time_during_pregnancy.csv", row.names = FALSE)


#-------------------------------------------------------
#reading iadatasheet_ED sheet
#------------------------------------
# ED presentations for 0-4yo (per 1000 persons)

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "C5:BA258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_ED_presentations_for_0_4_years" , "rate_ED_presentations_for_0_4_years_per_1000_persons", geom_data)
write.csv(new_data, "./output/CDA_1302_ED_presentations_for_0_4_years.csv", row.names = FALSE)
#-------------------------
# ED presentations for 5-9yo (per 1000 persons)

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "BB5:CZ258" ) 
new_data <- data_extraction(data, SA_name , "all", "5-9", "n_ED_presentations_for_5_9_years" , "rate_ED_presentations_for_5_9_years_per_1000_persons", geom_data)
write.csv(new_data, "./output/CDA_1302_ED_presentations_for_5_9_years.csv", row.names = FALSE)
#----------------------------------------
# ED presentations for 10-14yo (per 1000 persons)

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "DA5:EY258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-14", "n_ED_presentations_for_10_14_years" , "rate_ED_presentations_for_10_14_years_per_1000_persons", geom_data)
write.csv(new_data, "./output/CDA_1302_ED_presentations_for_10_14_years.csv", row.names = FALSE)
#----------------------------------------------
# ED presentations for 15-19yo (per 1000 persons)

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "EZ5:GX258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-19", "n_ED_presentations_for_15_19_years" , "rate_ED_presentations_for_15_19_years_per_1000_persons", geom_data)
write.csv(new_data, "./output/CDA_1302_ED_presentations_for_15_19_years.csv", row.names = FALSE)

#-------------------------------------
# ED presentations for 20-24yo (per 1000 persons)

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_ED", "GY5:IW258" ) 
new_data <- data_extraction(data, SA_name , "all", "20-24", "n_ED_presentations_for_20_24_years" , "rate_ED_presentations_for_20_24_years_per_1000_persons", geom_data)
write.csv(new_data, "./output/CDA_1302_ED_presentations_for_20_24_years.csv", row.names = FALSE)
#


#-------------------------------------------------------
#reading iadatasheet_physical sheet
#-------------------------------------------

# Injury hospitalisations for 0-4 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "C5:BA258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_injury_hospitalisations_for_0_4_year_olds" , "rate_injury_hospitalisations_for_0_4_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_174_injury_hospitalisations_for_0_4_year_olds.csv", row.names = FALSE)
#--------------------------------------------
# Injury hospitalisations for 0-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "BB5:CZ258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_injury_hospitalisations_for_0_24_year_olds" , "rate_injury_hospitalisations_for_0_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_174_injury_hospitalisations_for_0_24_year_olds.csv", row.names = FALSE)
#--------------------------

# Intentional injury hospitalisations for 0-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "DA5:ES258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_intentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_intentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "./output/CDA_174_intentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year.csv", row.names = FALSE)
#-------
# Unintentional injury hospitalisations for 0-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ET5:GL258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_unintentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year" , "rate_unintentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "./output/CDA_174_unintentional_injury_hospitalisations_for_0_24_year_olds_prior_moving_average_3year.csv", row.names = FALSE)
#-------

# Disability related hospitalisations for 0-4 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "NE5:PC258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_disability_related_hospitalisations_for_0_4_year_olds" , "rate_disability_related_related_for_0_4_year_olds", geom_data)
write.csv(new_data, "./output/CDA_1204_disability_related_hospitalisations_for_0_4_year_olds.csv", row.names = FALSE)

#------

# Disability related hospitalisations for 0-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "PD5:RB258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_disability_related_hospitalisations_for_0_24_year_olds" , "rate_disability_related_related_for_0_24_year_olds", geom_data)
write.csv(new_data, "./output/CDA_1204_disability_related_hospitalisations_for_0_24_year_olds.csv", row.names = FALSE)

#------
# Alcohol related hospitalisations for 15-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "RC5:SU258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_alcohol_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year" , "rate_alcohol_related_related_for_15_24_year_olds_prior_moving_average_3year", geom_data)
write.csv(new_data, "./output/CDA_1204_alcohol_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year.csv", row.names = FALSE)

#------
# Oral disease related hospitalisations for 0-4 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AJX5:ALV258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_oral_disease_related_hospitalisations_for_0_4_year_olds" , "rate_oral_disease_related_for_0_4_year_olds", geom_data)
write.csv(new_data, "./output/CDA_1204_oral_disease_related_hospitalisations_for_0_4_year_olds.csv", row.names = FALSE)
#------------
# Oral disease related hospitalisations for 0-24 year olds


SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "ALW5:ANU258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_oral_disease_related_hospitalisations_for_0_24_year_olds" , "rate_oral_disease_related_for_0_24_year_olds", geom_data)
write.csv(new_data, "./output/CDA_1204_oral_disease_related_hospitalisations_for_0_24_year_olds.csv", row.names = FALSE)
#------------

# Vaccine preventable disease related hospitalisations for 0-4 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AVU5:AXG258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-4", "n_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year" , "rate_vaccine_preventable_disease_related_for_0_4_year_oldsprior_moving_average_5year", geom_data)
write.csv(new_data, "./output/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year.csv", row.names = FALSE)
#------------
# Vaccine preventable disease related hospitalisations for 0-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_physical", "AXH5:AYT258" ) 
new_data <- data_extraction(data, SA_name , "all", "0-24", "n_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_vaccine_preventable_disease_related_for_0_24_year_oldsprior_moving_average_5year", geom_data)
write.csv(new_data, "./output/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year.csv", row.names = FALSE)
#------------



#-------------------------------------------------------
#reading iadatasheet_mental sheet
#-------------------------------------------

# Mental health and behavioural disorder hospitalisations for 10-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "AV5:CT258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds.csv", row.names = FALSE)
#----------------------
# 
# Mental health and behavioural disorder hospitalisations for 15-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "CU5:ES258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds.csv", row.names = FALSE)
#----------------------
# deliberate_self_harm_related hospitalisations for 10-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "ET5:GL258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_deliberate_self_harm_related_hospitalisations_for_10_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "./output/CDA_156_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_prior_moving_average_3year.csv", row.names = FALSE)
#----
# Deliberate self-harm related hospitalisations for 15-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "GM5:IE258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_deliberate_self_harm_related_hospitalisations_for_15_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_per_10000_persons_prior_moving_average_3year", geom_data)
write.csv(new_data, "./output/CDA_156_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year.csv", row.names = FALSE)
#---

# Mental health related hospitalisations for 10-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "IF5:KD258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_related_hospitalisations_for_10_24_year" , "rate_mental_health_related_hospitalisations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_156_mental_health_related_hospitalisations_for_10_24_year_olds.csv", row.names = FALSE)

#----
# Mental health related hospitalisations for 15-24 year olds
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "KE5:MC258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_related_hospitalisations_for_15_24_year" , "rate_mental_health_related_hospitalisations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_156_mental_health_related_hospitalisations_for_15_24_year_olds.csv", row.names = FALSE)

#----
#Deliberate self-harm related ED presentations for 10-24 year olds

#deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_per_10000_persons_prior_moving_average_5_year

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "MD5:NP258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_prior_moving_average_5_year" , "rate_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_per_10000_persons_prior_moving_average_5_year", geom_data)
write.csv(new_data, "./output/CDA_155_deliberate_self_harm_related_ED_presentations_for_10_24_year_prior_moving_average_5_year.csv", row.names = FALSE)

# Deliberate self-harm related ED presentations for 15-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "NQ5:PC258" ) 
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_prior_moving_average_5_year" , "rate_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_per_10000_persons_prior_moving_average_5_year", geom_data)
write.csv(new_data, "./output/CDA_155_deliberate_self_harm_related_ED_presentations_for_15_24_year_prior_moving_average_5_year.csv", row.names = FALSE)


#-----------------------
# Mental health related ED presentations for 10-24 year olds

SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "PD5:RB258" ) 
new_data <- data_extraction(data, SA_name , "all", "10-24", "n_mental_health_related_ED_presentations_for_10_24_year_olds" , "rate_mental_health_related_ED_presentations_for_10_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_155_mental_health_related_ED_presentations_for_10_24_year_olds.csv", row.names = FALSE)

#------------------------------
# Mental health related ED presentations for 15-24 year olds


#what ever the reason can't use RC in the rrange definition, so have added balbk column in the raw data file
SA_name <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "B5:B258" )
data <- read_files("./data/workbook_CDA_SA2.xlsx", "iadatasheet_mental", "RD5:TB258" )
new_data <- data_extraction(data, SA_name , "all", "15-24", "n_mental_health_related_ED_presentations_for_15_24_year_olds" , "rate_mental_health_related_ED_presentations_for_15_24_year_olds_per_10000_persons", geom_data)
write.csv(new_data, "./output/CDA_155_mental_health_related_ED_presentations_for_15_24_year_olds.csv", row.names = FALSE)

#--------------------------------


#combining all ages for ED presentations

data_0_4 <- read.csv("./output/CDA_1302_ED_presentations_for_0_4_years.csv", header = TRUE, check.names = FALSE)

data_5_9 <- read.csv("./output/CDA_1302_ED_presentations_for_5_9_years.csv", header = TRUE, check.names = FALSE)

data_10_14 <- read.csv("./output/CDA_1302_ED_presentations_for_10_14_years.csv", header = TRUE, check.names = FALSE)

data_15_19 <- read.csv("./output/CDA_1302_ED_presentations_for_15_19_years.csv", header = TRUE, check.names = FALSE)

data_20_24 <- read.csv("./output/CDA_1302_ED_presentations_for_20_24_years.csv", header = TRUE, check.names = FALSE)


write.csv(rbind(rbind(rbind(rbind(data_0_4, data_5_9), data_10_14), data_15_19), data_20_24), "./output/CDA_1302_ED_presentations_for_0_24_years.csv", row.names = FALSE)
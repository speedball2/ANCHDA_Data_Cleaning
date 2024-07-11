#-----------------------------------------------------------------------------------------------------------------------------
#                                                        SA3 SA3 SA3 SA3 SA3  SA3 SA3 SA3 SA3  SA3 SA3 SA3 SA3  SA3 SA3 SA3  |
#-----------------------------------------------------------------------------------------------------------------------------
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
#geom_data <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/SA2_2016_AUST_no_geom_SA3.csv", header = TRUE, check.names = FALSE)
#geom_data <- geom_data[, c("SA3_CODE_2016", "SA3_NAME_2016")]



##### what ever the reason can't use RC in the re arrange definition, so have added blank column in the raw data file --- important --------################

#--------------------------------------------------------
#reading iadatasheet_gp sheet ----------- COMPLETE | 
#--------------------------------------------------------

#People aged 0-24 years who attended GP (%)
#GP attendances per 100 people aged 0-24 years
#Medicare benefits per 100 people aged 0-24 years ($)





#People aged 0-24 years who attended GP (%)
SA3_CODE16  <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "C5:Q40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_people_aged_0_24_years_who_attended_GP" , "p_people_aged_0_24_years_who_attended_GP")
new_data <- mutate(new_data, year_range = gsub("(\\d+)-(\\d+)", "\\1-20\\2", calendar_year))
new_data <- select(new_data, matches("SA3_CODE16|sex|age_group|year_range"), everything()) %>%
  select(-calendar_year)
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/gp/CDA_1401_people_aged_0_24_years_who_attended_GP_SA3.csv", row.names = FALSE)


#GP attendances per 100 people aged 0-24 years
SA3_CODE16  <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "R5:AF40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_gp_attendances_per_100_people_aged_0_24_years" , "rate_gp_attendances_per_100_people_aged_0_24_years")
new_data <- mutate(new_data, year_range = gsub("(\\d+)-(\\d+)", "\\1-20\\2", calendar_year))
new_data <- select(new_data, matches("SA3_CODE16|sex|age_group|year_range"), everything()) %>%
  select(-calendar_year)
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/gp/CDA_1401_gp_attendances_per_100_people_aged_0_24_years_SA3.csv", row.names = FALSE)

#Medicare benefits per 100 people aged 0-24 years ($)
SA3_CODE16  <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_gp", "AG5:AU40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_medicare_benefits_per_100_people_aged_0_24_years" , "rate_medicare_benefits_per_100_people_aged_0_24_years")
new_data <- mutate(new_data, year_range = gsub("(\\d+)-(\\d+)", "\\1-20\\2", calendar_year))
new_data <- select(new_data, matches("SA3_CODE16|sex|age_group|year_range"), everything()) %>%
  select(-calendar_year)
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/gp/CDA_1401_medicare_benefits_per_100_people_aged_0_24_years_SA3.csv", row.names = FALSE)





#--------------------------------------------------------
#reading iadatasheet_mothers sheet ----------- COMPLETE | 
#--------------------------------------------------------
# what is in the cda WB
# Births to mothers aged 15-19 (%) - Prior moving average (3 year)
# Births to mothers aged 20-24 (%) - Prior moving average (3 year)
# Preterm births < 37 weeks (%) - Prior moving average (3 year)
# Preterm births < 39 weeks (%) - Prior moving average (3 year)
# Low birth weight, born alive < 2500g (%) - Prior moving average (3 year)
# Mothers who smoked tobacco at any time during pregnancy (%)




# Births to mothers aged 15-19 (%) - Prior moving average (3 year)
#CDA_117_births_mothers_aged_15_19_SA2
SA3_CODE16  <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "C5:CK40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_births_mothers_aged_15_19_prior_moving_average_3year" , "p_births_mothers_aged_15_19_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_1111_births_mothers_aged_15_19_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#-------------------------
# Births to mothers aged 20-24 (%) - Prior moving average (3 year)
#CDA_117_births_mothers_aged_20_24_SA2
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "CL5:FT40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_births_mothers_aged_20_24_prior_moving_average_3year" , "p_births_births_mothers_aged_20_24_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_1112_births_mothers_aged_20_24_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#-------------------------

# Preterm births < 37 weeks (%) - Prior moving average (3 year)
#CDA_117_pre_term_birth_less_37_weeks_SA2
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "FU5:JC40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_pre_term_birth_less_37_prior_moving_average_3year" , "p_pre_term_birth_less_37_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_117_pre_term_birth_less_37_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#-------------------------
# Preterm births < 39 weeks (%) - Prior moving average (3 year)
#CDA_117_pre_term_birth_less_39_weeks_SA2
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "JD5:ML40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_pre_term_birth_less_39_prior_moving_average_3year" , "p_pre_term_birth_less_39_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_117_pre_term_birth_less_39_prior_moving_average_3year_SA3.csv", row.names = FALSE)

#---------------------
# Low birth weight, born alive < 2500g (%) - Prior moving average (3 year)
#CDA_111_lowbirthweight_under_2500g_SA2
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "MM5:PU40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_low_birth_weight_born_alive_less_2500g_prior_movingaverage_3year" , "p_low_birth_weight_born_alive_less_2500g_prior_movingaverage_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_111_low_birth_weight_born_alive_less_2500g_prior_movingaverage_3year_SA3.csv", row.names = FALSE)

#----------------------
# Mothers who smoked tobacco at any time during pregnancy (%)
#CDA_114_mother_smoked_tobacco_during_pregnancy_SA2
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mothers", "PV5:SF40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-0", "n_mothers_smoked_tobacco_at_any_time_during_pregnancy" , "p_mothers_smoked_tobacco_at_any_time_during_pregnancy")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mothers/CDA_114_mothers_smoked_tobacco_at_any_time_during_pregnancy_SA3.csv", row.names = FALSE)





#----------------------------------------------------------
#reading iadatasheet_ED sheet ----------------- COMPLETE  |
#----------------------------------------------------------
# what is available in wb is 

# ED presentations for 0-4 year olds (per 1,000 persons)
# ED presentations for 5-9 year olds (per 1,000 persons)
# ED presentations for 10-14 year olds (per 1,000 persons)
# ED presentations for 15-19 year olds (per 1,000 persons)
# ED presentations for 20-24 year olds (per 1,000 persons)



# ED presentations for 0-4yo (per 1000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "C5:BA40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_ED_presentations" , "rate_ED_presentations")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_0_4_years_SA3.csv", row.names = FALSE)
#----------------------------------------------------
# ED presentations for 5-9yo (per 1000 persons)

SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "BB5:CZ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "5-9", "n_ED_presentations" , "rate_ED_presentations")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_5_9_years_SA3.csv", row.names = FALSE)
#----------------------------------------------------
# ED presentations for 10-14yo (per 1000 persons)

SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "DA5:EY40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-14", "n_ED_presentations" , "rate_ED_presentations")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_10_14_years_SA3.csv", row.names = FALSE)
#------------------------------------------------
# ED presentations for 15-19yo (per 1000 persons)

SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "EZ5:GX40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-19", "n_ED_presentations" , "rate_ED_presentations")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_15_19_years_SA3.csv", row.names = FALSE)

#------------------------------------------------
# ED presentations for 20-24yo (per 1000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_ED", "GY5:IW40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "20-24", "n_ED_presentations" , "rate_ED_presentations")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_20_24_years_SA3.csv", row.names = FALSE)

#--------------------------------
#combining all ages for ED presentations

data_0_4 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_0_4_years_SA3.csv", header = TRUE, check.names = FALSE)
data_5_9 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_5_9_years_SA3.csv", header = TRUE, check.names = FALSE)
data_10_14 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_10_14_years_SA3.csv", header = TRUE, check.names = FALSE)
data_15_19 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_15_19_years_SA3.csv", header = TRUE, check.names = FALSE)
data_20_24 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_20_24_years_SA3.csv", header = TRUE, check.names = FALSE)


write.csv(rbind(rbind(rbind(rbind(data_0_4, data_5_9), data_10_14), data_15_19), data_20_24), "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/emergengy_dep/CDA_1302_ED_presentations_for_0_24_years_SA3.csv", row.names = FALSE)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------






#------------------------------------------------------------------------------------------------------------#
#reading iadatasheet_physical sheet ----------- COMPLETE  | sub folder ==== HOSPITAL
#------------------------------------------------------------------------------------------------------------#
#Injury hospitalisations for 0-4 year olds (per 10,000 persons)
#Injury hospitalisations for 0-24 year olds (per 10,000 persons)
#Intentional injury hospitalisations for 0-24 year olds (per 10,000 persons)
#Unintentional injury hospitalisations for 0-24 year olds (per 10,000 persons)
#Fall related hospitalisations for 0-4 year olds (per 10,000 persons)
#Fall related hospitalisations for 0-24 year olds (per 10,000 persons)
#Transport related hospitalisations for 0-24 year olds (per 10,000 persons)
#Assault related hospitalisations for 15-24 year olds (per 10,000 persons)
#Disability related hospitalisations for 0-4 year olds (per 10,000 persons)
#Disability related hospitalisations for 0-24 year olds (per 10,000 persons)
#Alcohol related hospitalisations for 15-24 year olds (per 10,000 persons)
#Substance related hospitalisations for 15-24 year olds (per 10,000 persons)
#Chronic physical illness related hospitalisations for 0-4 year olds (per 10,000 persons)
#Chronic physical illness related hospitalisations for 0-24 year olds (per 10,000 persons)
#Cancer related hospitalisations for 0-4 year olds (per 10,000 persons)
#Cancer related hospitalisations for 0-24 year olds (per 10,000 persons)
#Cardiovascular related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
#Cardiovascular related hospitalisations for 0-24 year olds (per 10,000 persons)
#Diabetes related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
#Diabetes related hospitalisations for 0-24 year olds (per 10,000 persons)
#Disorders of ear and hearing related hospitalisations for 0-4 year olds (per 10,000 persons)
#Disorders of ear and hearing related hospitalisations for 0-9 year olds (per 10,000 persons)
#Oral disease related hospitalisations for 0-4 year olds (per 10,000 persons)
#Oral disease related hospitalisations for 0-24 year olds (per 10,000 persons)
#Obesity related hospitalisations for 10-24 year olds (per 10,000 persons)
#Kidney related hospitalisations for 15-24 year olds (per 10,000 persons)
#Musculoskeletal disease related hospitalisations for 0-24 year olds (per 10,000 persons)
#Respiratory related hospitalisations for 0-4 year olds (per 10,000 persons)
#Respiratory related hospitalisations for 0-24 year olds (per 10,000 persons)
#Vaccine preventable disease related hospitalisations for 0-4 year olds (per 10,000 persons)
#Vaccine preventable disease related hospitalisations for 0-24 year olds (per 10,000 persons)
#Influenza related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
#Influenza related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Group A Streptococcus related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Rheumatic heart disease related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
#Rotovirus related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)




# Injury hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "C5:BA40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_injury_hospitalisations_for_0_4_year_olds" , "rate_injury_hospitalisations_for_0_4_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_174_injury_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)

#------------
# Injury hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BB5:CZ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_injury_hospitalisations_for_0_24_year_olds" , "rate_injury_hospitalisations_for_0_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_174_injury_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Intentional injury hospitalisations for 0-24 year olds (per 10,000 persons) 
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "DA5:EY40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_intentional_injury_hospitalisations_for_0_24_year_olds" , "rate_intentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_174_intentional_injury_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Unintentional injury hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "EZ5:GX40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_unintentional_injury_hospitalisations_for_0_24_year_olds" , "rate_unintentional_injury_hospitalisations_for_0_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_174_unintentional_injury_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Fall related hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "GY5:IW40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_fall_related_hospitalisations_for_0_4_year_olds" , "rate_fall_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_fall_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Fall related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "IX5:KV40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_fall_related_hospitalisations_for_0_24_year_olds" , "rate_fall_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_fall_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Transport related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "KW5:MU40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_transport_related_hospitalisations_for_0_24_year_olds" , "rate_transport_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_transport_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year_SA3.csv", row.names = FALSE)
#------------

#Assault related hospitalisations for 15-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "MV5:OT40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_assault_related_hospitalisations_for_15_24_year_olds" , "rate_assault_related_hospitalisations_for_15_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_assault_related_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Disability related hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "OU5:QS40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_disability_related_hospitalisations_for_0_4_year_olds" , "rate_disability_related_related_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_disability_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

# Disability related hospitalisations for 0-24 year olds
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "QT5:SR40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_disability_related_hospitalisations_for_0_24_year_olds" , "rate_disability_related_related_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_disability_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Alcohol related hospitalisations for 15-24 year olds
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40")
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "SS5:UQ40" )  
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_alcohol_related_hospitalisations_for_15_24_year_olds" , "rate_alcohol_related_related_for_15_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_alcohol_related_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#------------


#Substance related hospitalisations for 15-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "UR5:WP40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_substance_related_hospitalisations_for_15_24_year_olds" , "rate_substance_related_hospitalisations_for_15_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_substance_related_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Chronic physical illness related hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "WQ5:YO40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds" , "rate_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_chronic_physical_illness_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Chronic physical illness related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "YP5:AAN40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds" , "rate_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_chronic_physical_illness_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Cancer related hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AAO5:ACM40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_cancer_related_hospitalisations_for_0_4_year_olds" , "rate_cancer_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_cancer_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Cancer related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "ACN5:AEL40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_cancer_related_hospitalisations_for_0_24_year_olds" , "rate_cancer_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_cancer_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Cardiovascular related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AEM5:AFY40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year" , "rate_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_cardiovascular_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year_SA3.csv", row.names = FALSE)
#------------

#Cardiovascular related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AFZ5:AHX40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_cardiovascular_related_hospitalisations_for_0_24_year_olds" , "rate_cardiovascular_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_cardiovascular_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Diabetes related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AHY5:AJK40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_diabetes_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year" , "rate_diabetes_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_diabetes_related_hospitalisations_for_0_4_year_olds_prior_moving_average_5year_SA3.csv", row.names = FALSE)
#------------
#
#Diabetes related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AJL5:ALJ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_diabetes_related_hospitalisations_for_0_24_year_olds" , "rate_diabetes_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_diabetes_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Disorders of ear and hearing related hospitalisations for 0-4 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "ALK5:ANI40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds" , "rate_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_disorders_of_ear_and_hearing_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Disorders of ear and hearing related hospitalisations for 0-9 year olds (per 10,000 persons) 
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "ANJ5:APH40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-9", "n_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds" , "rate_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_disorders_of_ear_and_hearing_related_hospitalisations_for_0_9_year_olds_SA3.csv", row.names = FALSE)
#------------

# Oral disease related hospitalisations for 0-4 year olds
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "API5:ARG40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_oral_disease_related_hospitalisations_for_0_4_year_olds" , "rate_oral_disease_related_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_oral_disease_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

# Oral disease related hospitalisations for 0-24 year olds
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "ARH5:ATF40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_oral_disease_related_hospitalisations_for_0_24_year_olds" , "rate_oral_disease_related_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_oral_disease_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------


# Obesity related hospitalisations for 10-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "ATG5:AVE40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_obesity_related_hospitalisations_for_10_24_year_olds" , "rate_obesity_related_hospitalisations_for_10_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_obesity_related_hospitalisations_for_10_24_year_olds_SA3.csv", row.names = FALSE)
#------------

# Kidney related hospitalisations for 15-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AVF5:AXD40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_kidney_related_hospitalisations_for_15_24_year_olds" , "rate_kidney_related_hospitalisations_for_15_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_kidney_related_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Musculoskeletal disease related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AXE5:AZC40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds" , "rate_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_musculoskeletal_disease_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Respiratory related hospitalisations for 0-4 year olds (per 10,000 persons) 
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "AZD5:BBB40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_respiratory_related_hospitalisations_for_0_4_year_olds" , "rate_respiratory_related_hospitalisations_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_respiratory_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------

#Respiratory related hospitalisations for 0-24 year olds (per 10,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BBC5:BDA40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_respiratory_related_hospitalisations_for_0_24_year_olds" , "rate_respiratory_related_hospitalisations_for_0_24_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_respiratory_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------

#Vaccine preventable disease related hospitalisations for 0-4 year olds (per 10,000 persons) 
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BDB5:BEZ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds" , "rate_vaccine_preventable_disease_related_for_0_4_year_olds")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_4_year_olds_SA3.csv", row.names = FALSE)
#------------
#
# Vaccine preventable disease related hospitalisations for 0-24 year olds (per 10,000 persons) 
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BFA5:BGY40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_prior_moving_average_5year" , "rate_vaccine_preventable_disease_related_for_0_24_year_olds_prior_moving_average_5year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_vaccine_preventable_disease_related_hospitalisations_for_0_24_year_olds_SA3.csv", row.names = FALSE)
#------------
#
#Influenza related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (3 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BGZ5:BIR40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_influenza_related_hospitalisations_0_4_years_prior_moving_average_3year" , "rate_influenza_related_hospitalisations_0_4_years_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_influenza_related_hospitalisations_0_4_years_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#------------
#
#Influenza related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BIS5:BKK40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_influenza_related_hospitalisations_0_24_years_prior_moving_average_3year" , "rate_influenza_related_hospitalisations_0_24_years_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_influenza_related_hospitalisations_0_24_years_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#------------
#
#Group A Streptococcus related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (3 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BKL5:BMD40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_3year" , "rate_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_group_a_streptococcus_related_hospitalisations_0_24_years_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#------------
#
#Rheumatic heart disease related hospitalisations for 0-24 year olds (per 10,000 persons) - Prior moving average (5 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BME5:BNQ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year" , "rate_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_rheumatic_heart_disease_related_hospitalisations_0_24_years_prior_moving_average_5year_SA3.csv", row.names = FALSE)
#------------
#
#Rotovirus related hospitalisations for 0-4 year olds (per 10,000 persons) - Prior moving average (5 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_physical", "BNR5:BPD40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-4", "n_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year" , "rate_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/hospital/CDA_1204_rotavirus_related_hospitalisations_0_4_years_prior_moving_average_5year_SA3.csv", row.names = FALSE)
#------------





#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#reading iadatasheet_mental sheet
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#What is available in workbook ===============================================================================================================================================================================
#Births to mothers with a mental illness (%)
#Mental health and behavioural disorder hospitalisations for 10-24 year olds (per 10,000 persons)
#Mental health and behavioural disorder hospitalisations for 15-24 year olds (per 10,000 persons)
#Deliberate self-harm related hospitalisations for 10-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Deliberate self-harm related hospitalisations for 15-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Mental health related hospitalisations for 10-24 year olds (per 10,000 persons)
#Mental health related hospitalisations for 15-24 year olds (per 10,000 persons)
#Deliberate self-harm related ED presentations for 10-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Deliberate self-harm related ED presentations for 15-24 year olds (per 10,000 persons) - Prior moving average (3 year)
#Mental health related ED presentations for 10-24 year olds (per 10,000 persons)
#Mental health related ED presentations for 15-24 year olds (per 10,000 persons)
#Community mental health service contacts for 0-24 year olds (per 1,000 persons)
#Community mental health service contacts for 5-9 year olds (per 1,000 persons)
#Community mental health service contacts for 10-14 year olds (per 1,000 persons)
#Community mental health service contacts for 15-19 year olds (per 1,000 persons)
#Community mental health service contacts for 20-24 year olds (per 1,000 persons)





# Births to mothers with a mental illness (%)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "C5:BA40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_births_to_mothers_with_a_mental_illness" , "p_births_to_mothers_with_a_mental_illness")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_158_births_to_mothers_with_a_mental_illness_SA3.csv", row.names = FALSE)
#----------------------
#
# Mental health and behavioural disorder hospitalisations for 10-24 year old
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "BB5:CZ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_10_24_year_olds_SA3.csv", row.names = FALSE)
#----------------------
# 
# Mental health and behavioural disorder hospitalisations for 15-24 year old
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "DA5:EY40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year" , "rate_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_mental_health_and_behavioural_disorder_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#----------------------
#
# deliberate_self_harm_related hospitalisations for 10-24 year old  - Prior moving average (3 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "EZ5:GR40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_deliberate_self_harm_related_hospitalisations_for_10_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_per_10000_persons_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_deliberate_self_harm_related_hospitalisations_for_10_24_year_olds_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#----------------------
#
# Deliberate self-harm related hospitalisations for 15-24 year old  - Prior moving average (3 year)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "GS5:IK40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_deliberate_self_harm_related_hospitalisations_for_15_24_year_prior_moving_average_3year" , "rate_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_per_10000_persons_prior_moving_average_3year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_deliberate_self_harm_related_hospitalisations_for_15_24_year_olds_prior_moving_average_3year_SA3.csv", row.names = FALSE)
#----------------------
#
# Mental health related hospitalisations for 10-24 year old
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "IL5:KJ40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_mental_health_related_hospitalisations_for_10_24_year" , "rate_mental_health_related_hospitalisations_for_10_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_mental_health_related_hospitalisations_for_10_24_year_olds_SA3.csv", row.names = FALSE)
#----------------------
#
# Mental health related hospitalisations for 15-24 year old (per 10,000 persons)
#
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "KK5:MI40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_mental_health_related_hospitalisations_for_15_24_year" , "rate_mental_health_related_hospitalisations_for_15_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_156_mental_health_related_hospitalisations_for_15_24_year_olds_SA3.csv", row.names = FALSE)
#----------------------
#
# Deliberate self-harm related ED presentations for 10-24 year old (per 10,000 persons) - Prior moving average (3 year)
#
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "MJ5:OB40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_prior_moving_average_3_year" , "rate_deliberate_self_harm_related_ED_presentations_for_10_24_year_olds_per_10000_persons_prior_moving_average_3_year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_155_deliberate_self_harm_related_ED_presentations_for_10_24_year_prior_moving_average_3_year_SA3.csv", row.names = FALSE)
#----------------------
# Deliberate self-harm related ED presentations for 15-24 year old (per 10,000 persons) - Prior moving average (3 year)
#
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "OC5:PU40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_prior_moving_average_3_year" , "rate_deliberate_self_harm_related_ED_presentations_for_15_24_year_olds_per_10000_persons_prior_moving_average_3_year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_155_deliberate_self_harm_related_ED_presentations_for_15_24_year_prior_moving_average_3_year_SA3.csv", row.names = FALSE)
#----------------------
#
# Mental health related ED presentations for 10-24 year old (per 10,000 persons)
#
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "PV5:RT40" ) 
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-24", "n_mental_health_related_ED_presentations_for_10_24_year_olds" , "rate_mental_health_related_ED_presentations_for_10_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_155_mental_health_related_ED_presentations_for_10_24_year_olds_SA3.csv", row.names = FALSE)

#------------------------------
# Mental health related ED presentations for 15-24 year old
#SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "RU5:TS40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-24", "n_mental_health_related_ED_presentations_for_15_24_year_olds" , "rate_mental_health_related_ED_presentations_for_15_24_year_olds_per_10000_persons")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_155_mental_health_related_ED_presentations_for_15_24_year_olds_SA3.csv", row.names = FALSE)




# Community mental health service contacts for 0-24 year olds (per 1,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "TT5:VR40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "0-24", "n_community_mental_health_service_contacts_for_0_24_year" , "rate_community_mental_health_service_contacts_for_0_24_year")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_157_community_mental_health_service_contacts_for_0_24_year_SA3.csv", row.names = FALSE)

#--------------------------------
# Community mental health service contacts for 5-9 year olds (per 1,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "VS5:XQ40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "5-9", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_157_community_mental_health_service_contacts_for_5_9_year_SA3.csv", row.names = FALSE)

#--------------------------------
# Community mental health service contacts for 10-14 year olds (per 1,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "XR5:ZP40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "10-14", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_157_community_mental_health_service_contacts_for_10_14_year_SA3.csv", row.names = FALSE)
#--------------------------------
# Community mental health service contacts for 15-19 year olds (per 1,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "ZQ5:ABO40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "15-19", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_157_community_mental_health_service_contacts_for_15_19_year_SA3.csv", row.names = FALSE)

#--------------------------------
# Community mental health service contacts for 20-24 year olds (per 1,000 persons)
SA3_CODE16 <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "A5:A40" )
data <- read_files("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/cda_data/workbook_CDA_SA3.xlsx", "iadatasheet_mental", "ABP5:ADN40" )
new_data <- data_extraction(data, SA3_CODE16 , "all", "20-24", "n_community_mental_health_service_contacts" , "rate_community_mental_health_service_contacts")
# Remove rows with SA3_CODE16 equal to "#WA"
new_data <- new_data[new_data$SA3_CODE16 != "#WA", ]
write.csv(new_data, "C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_READY_FOR_QA/CDA/SA3/mental_health/CDA_157_community_mental_health_service_contacts_for_20_24_year_SA3.csv", row.names = FALSE)


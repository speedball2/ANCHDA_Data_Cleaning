
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)

source("./functions/sexual_health_dat_extraction.R")

#--------------------


site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8
#-----------

sex_health_data <- read.csv("./data/ATLAS project data.csv", header = FALSE, check.names = FALSE)


#HIV KNOWLADGE
sexual_health_dat_extraction(sex_health_data, "average_score_HIV_knowledge_range_0_6", "p_HIV_knowledge", 18,21 ,1, site, sie_code, "./output/NSSHS_1151_HIV_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")
  
#STI KNOWLADGE
sexual_health_dat_extraction(sex_health_data, "average_score_STIs_knowledge_range_0_4", "p_STIs_knowledge", 13, 16 ,1, site, sie_code, "./output/NSSHS_1151_STI_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#EVER HAD SEX
sexual_health_dat_extraction(sex_health_data, "n_ever_had_sex", "p_ever_had_sex", 46, 47 ,1, site, sie_code, "./output/NSSHS_1151_EVER_HAD_SEX_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#SEXSTING
#"n_engaged_in_sexting"
sexual_health_dat_extraction(sex_health_data, "n_engaged_in_sexting", "p_engaged_in_sexting", 79, 80 ,1, site, sie_code, "./output/NSSHS_1151_SEXTING_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#RSE AT SCHOOL
sexual_health_dat_extraction(sex_health_data, "n_received_RSE_school", "p_received_RSE_school", 220, 221 ,1, site, sie_code, "./output/NSSHS_1151_RSE_AT_SCHOOL_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")


#could get a STI (AA, AB)
sexual_health_dat_extraction(sex_health_data, "n_believe_get_STI", "p_believe_get_STI", 27, 28 ,1, site, sie_code, "./output/NSSHS_1151_GET_STI_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#‘students reporting they think most people their age use condoms (AD, AE)

sexual_health_dat_extraction(sex_health_data, "n_think_most_people_use_condoms", "p_think_most_people_use_condoms", 30, 31 ,1, site, sie_code, "./output/NSSHS_1151_CONDOM_USE_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")


#sexually active students reporting they always use a condom (AW + AX)

sexual_health_dat_extraction(sex_health_data, "n_sex_act_always_condom", "p_sex_act_always_condom", 49, 50 ,1, site, sie_code, "./output/NSSHS_1151_ALWAYS_USE_CONDOM_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")


#----------------------

#students who had discussions related to protecting sexual health prior to having sex: using a condom

sexual_health_dat_extraction(sex_health_data, "n_had_discussions_prior_use_condom", "p_had_discussions_prior_use_condom", 56, 57 ,1, site, sie_code, "./output/NSSHS_1151_DISCUSSION_USE_CONDOM_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")


#students who had discussions related to protecting sexual health prior to having sex: avoiding pregnancy (BJ, BK)

sexual_health_dat_extraction(sex_health_data, "n_had_discussions_prior_avoid_pregnancy", "p_had_discussions_prior_avoid_pregnancy", 62, 63 ,1, site, sie_code, "./output/NSSHS_1151_DISCUSSION_AVOIDING_PREGNANCY_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#students who had discussions related to protecting sexual health prior to having sex:  avoiding STIs and HIV (BG, BH)

sexual_health_dat_extraction(sex_health_data, "n_had_discussions_prior_avoid_STIs_HIV", "p_had_discussions_prior_avoid_STIs_HIV", 59, 60 ,1, site, sie_code, "./output/NSSHS_1151_DISCUSSION_AVOIDING_STI_HIV_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#---------------------------

#students reporting feeling pressure from a partner (BO + BQ, BP + BR)

sexual_health_dat_extraction(sex_health_data, "n_pressure_partner", "p_pressure_partner", c(67,69), c(68,70) ,2, site, sie_code, "./output/NSSHS_1151_FEELING_PRESSURE_PARTNER_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")


#students reporting feeling pressure from peers (BV + BX, BW + BY)

sexual_health_dat_extraction(sex_health_data, "n_pressure_peers", "p_pressure_peers", c(74,76), c(75,77) ,2, site, sie_code, "./output/NSSHS_1151_FEELING_PRESSURE_PEERS_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#students reporting their most common sources of sexual health information is parents (HF, HG)

#sexual_health_dat_extraction(sex_health_data, "n_info_source_parents", "p_info_source_parents", 214, 215 ,1, site, sie_code, "./output/NSSHS_1151_COMMON_SOURCE_PARENTS_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#students reporting their most common sources of sexual health information is father (HC, HD)

sexual_health_dat_extraction(sex_health_data, "n_info_source_father", "p_info_source_father", 211, 212 ,1, site, sie_code, "./output/NSSHS_1151_COMMON_SOURCE_FATHER_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#students reporting their most common sources of sexual health information is mother (HC, HD)

sexual_health_dat_extraction(sex_health_data, "n_info_source_mother", "p_info_source_mother", 208, 209 ,1, site, sie_code, "./output/NSSHS_1151_COMMON_SOURCE_MOTHER _knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")



#students reporting their most common sources of sexual health information is doctor/gp (HI, HJ)

sexual_health_dat_extraction(sex_health_data, "n_info_source_doctor_gp", "p_info_source_doctor_gp", 217, 218 ,1, site, sie_code, "./output/NSSHS_1151_COMMON_SOURCE_DOCTOR_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

#Students reporting they think sexuality/relationship classes are relevant (HQ + HS, HR + HT)

sexual_health_dat_extraction(sex_health_data, "n_rel_classes_relevant", "p_rel_classes_relevant", c(225,227), c(226,228) ,2, site, sie_code, "./output/NSSHS_1151_CLASS_RELAVENCE_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv")

test <- read.csv("./output/NSSHS_1151_SEXTING_knowledge_and_skills_on_safe_sexual_and_reproductive_health_behaviours_STE.csv", header = TRUE)
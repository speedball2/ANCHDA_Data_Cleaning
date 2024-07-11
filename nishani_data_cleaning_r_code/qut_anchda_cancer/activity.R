
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)

#-----------
source("./functions/calculating_total_erp_SA.R")
source("./functions/read_files.R")
source("./functions/extracting_cancer_type.R")
source("./functions/rounding_function.R")
source("./functions/cell_suppression.R")

#-----------------------------

geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

#-----------------------
#VIC data
table2_vic <- read_files("./data/Results for client - VIC.xlsx", "Table 2" , "A1:E5146")  


table2_vic$diagnosis_age <- as.numeric(table2_vic$diagnosis_age)

table2_vic$diagnosis_age_group <- "0-24"

# table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 0 & table2_vic$diagnosis_age <= 19)] <- "0-19"
# 
# table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 20 & table2_vic$diagnosis_age <= 24)] <- "20-24"

# table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 10 & table2_vic$diagnosis_age <= 14)] <- "10-14"
# 
# table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 15 & table2_vic$diagnosis_age <= 19)] <- "15-19"

#table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 20 & table2_vic$diagnosis_age <= 24)] <- "20-24"

table2_vic <- table2_vic[, c("sa3_2016", "diagnosis_year", "diagnosis_age_group", "sex", "cancer")]

table2_vic$sex <- "all"

#---------------------------------------

#QLD data
table2_qld <- read_files("./data/Results for client - QLD.xlsx", "Table 2" , "A1:E4865")  

table2_qld <- table2_qld[, c("sa3_2016", "diagnosis_year", "diagnosis_age_group", "sex", "cancer")]

table2_qld$diagnosis_age_group  <- "0-24"

table2_qld$sex <- "all"

#table2_qld$diagnosis_age_group <- ifelse(table2_qld$diagnosis_age_group == "19-24", "20-24", "0-19")
#table2_vic$diagnosis_age_group[which(table2_vic$diagnosis_age >= 0 & table2_vic$diagnosis_age <= 19)] <- "0-19"



#--------------------------
#

geom_data <- geom_data[which(geom_data$STATE_CODE_2016 == 3 | geom_data$STATE_CODE_2016 == 2 ),]

geom_data <- geom_data[, c("SA3_CODE_2016","SA4_CODE_2016", "STATE_CODE_2016")]

geom_data <- geom_data[!duplicated(geom_data$SA3_CODE_2016),]

names(geom_data) <- c("SA3_CODE16", "SA4_CODE16", "STE_CODE16")

#-------------------

new_data <- rbind(table2_vic, table2_qld)

names(new_data)[1] <- "SA3_CODE16"
new_data$SA3_CODE16 <- as.numeric(new_data$SA3_CODE16 )

#-----------------

new_data <- left_join(new_data, geom_data, by = "SA3_CODE16" )

names(new_data) <-  c("SA3_CODE16", "calendar_year", "age_group" ,"sex", "cancer", "SA4_CODE16", "STE_CODE16" )    

new_data$calendar_year <- as.numeric(new_data$calendar_year)
#--------------

SA3_total <- calculating_total_erp_SA("./data/ABS_ERP_181_ERP_SA3.csv", 0:24, 9999999, "SA3_CODE16")
SA4_total <- calculating_total_erp_SA("./data/ABS_ERP_181_ERP_SA4.csv", 0:24, 9999999, "SA4_CODE16")
STE_total <- calculating_total_erp_SA("./data/ABS_ERP_181_ERP_STE.csv", 0:24, 9999999, "STE_CODE16")


#---------------------

extracting_cancer_type(new_data, "all",SA3_total, SA4_total,STE_total,  "n_all_cancer_types", "rate_all_cancer_types", "./output/AIHW_cancer_1221_n_all_cancer_types", "./output/AIHW_cancer_1221_rate_all_cancer_types")
extracting_cancer_type(new_data, 1, SA3_total, SA4_total,STE_total, "n_leukaemia", "rate_leukaemia", "./output/AIHW_cancer_1221_n_leukaemia", "./output/AIHW_cancer_1221_rate_leukaemia")
extracting_cancer_type(new_data, 2, SA3_total, SA4_total,STE_total, "n_brain_cancer_and_other_CNS_tumours", "rate_brain_cancer_and_other_CNS_tumours", "./output/AIHW_cancer_1221_n_brain_cancer_and_other_CNS_tumours", "./output/AIHW_cancer_1221_rate_brain_cancer_and_other_CNS_tumours")
extracting_cancer_type(new_data, 3, SA3_total, SA4_total,STE_total, "n_lymphoma", "rate_lymphoma", "./output/AIHW_cancer_1221_n_lymphoma", "./output/AIHW_cancer_1221_rate_lymphoma")
extracting_cancer_type(new_data, 99, SA3_total, SA4_total,STE_total, "n_other_types", "rate_other_types", "./output/AIHW_cancer_1221_n_other_types", "./output/AIHW_cancer_1221_rate_other_types")




#----------------------------


#cell supression

cancer_type <- c("all_cancer_types", "brain_cancer_and_other_CNS_tumours", "leukaemia", "lymphoma", "other_types")
geo_type <- c("SA3", "SA4", "STE")
base_file_name <- "./output/AIHW_cancer_1221_"

for( i in 1:length(cancer_type)){
  
  for(j in 1:length(geo_type)){
    
      n_file <- paste0(base_file_name, "n_",cancer_type[i],"_", geo_type[j] , ".csv" )
      rate_file <- paste0(base_file_name, "rate_",cancer_type[i], "_", geo_type[j], ".csv" )
      n_col <-paste0("n_",cancer_type[i])
      rate_col <-paste0("rate_",cancer_type[i])
      combined <- paste0("AIHW_cancer_1221_", cancer_type[i], "_combined_")
      
  
      cell_suppression(n_file, rate_file, n_col, rate_col, NULL, 3,combined)
      
  }
}








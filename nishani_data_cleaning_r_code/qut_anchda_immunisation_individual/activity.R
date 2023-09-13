library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-----------

source("./functions/read_files.R")
#----------


SA3_2011_SA3_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F376", col_names = TRUE)

SA3_2011_SA3_2016 <-  SA3_2011_SA3_2016[-1,] #remove empty first row


SA3_2011_SA3_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                        sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SA3_2011_SA3_2016_quality <-  SA3_2011_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA3_2011_SA3_2016 <-  dplyr::left_join(SA3_2011_SA3_2016,SA3_2011_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()


#------------------

site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8


code_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE )

table_1 <- read_files("./data/NEW_AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.xlsx", "ALL 1yo" , "A2:N3045")

table_2 <- read_files("./data/NEW_AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.xlsx", "ALL 2yo" , "A2:N3041")

table_5 <- read_files("./data/NEW_AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.xlsx", "ALL 5yo" , "A2:N3042")

table_full <- rbind(rbind(table_1, table_2), table_5)


#------------------

#read corrected  duplicated rows

duplicated_data <- read.csv("./data/dupliated_data.csv", header = TRUE, check.names = FALSE)


d_code <- unique(duplicated_data$SA3_Code)

table_full <- table_full[-which(table_full$SA3_Code %in% d_code & table_full$Year == 2017),]

table_full <- rbind(table_full, duplicated_data)



#new_table <- table_full[duplicated(table_full[, c("SA3_Code", "Year", "Age Group")]),]


#-----------------------

var_name <- c("% Varicella", "% MenC", "% MMR","% Hep B", "% HIB", "% Polio", "% DTP", "% Fully")
var_name_p <- c( "p_immunised_chickenpox", "p_immunised_meningococcal_disease", "p_immunised_Measles", "p_immunised_hepatitis_b", "p_immunised_haemophilus_influenzae_type_b",
                 "p_immunised_polio", "p_immunised_diphteria", "p_immunised_fully")

inicator_code <- c(128,127,126,125,124,123,122,121)

#table_full <- as.data.frame(table_full)

for(i in 1:length(var_name)){
  
  
  table_new <- table_full[,c("Year", "SA3_Code", "SA3_Name", "State", "Age Group", var_name[i])]
  
  table_new$State <- unlist(lapply(table_new$State, function (x){
    
    site_cat[which(site == x)]
  }))
  
  table_new[1:nrow(table_new),var_name[i]] <- lapply(table_new[1:nrow(table_new),var_name[i]] , function(x)gsub("NP", NA, x) ) 
  
  table_new[1:nrow(table_new),var_name[i]] <- lapply(table_new[1:nrow(table_new),var_name[i]] , function(x)gsub("≥", "", x) ) 
  
  table_new[1:nrow(table_new),var_name[i]] <- lapply(table_new[1:nrow(table_new),var_name[i]] , function(x)gsub("?", "", x, fixed = TRUE) ) 
  
  table_new$sex <- "all"
  table_new$age_group <- "0-5"
  names(table_new)[which(names(table_new)== "Age Group")] <- "age_immunised_AIR"
  names(table_new)[which(names(table_new)== var_name[i])] <- var_name_p[i]
  names(table_new)[which(names(table_new)== "Year")] <- "calendar_year"
  
  table_new$age_immunised_AIR[which(table_new$age_immunised_AIR == "1 Year olds")] <- "1 year"
  
  table_new$age_immunised_AIR[which(table_new$age_immunised_AIR == "2 Year olds")] <- "2 years"
  
  table_new$age_immunised_AIR[which(table_new$age_immunised_AIR == "5 Year olds")] <- "5 years"
  
  #-----------------
  
  #applying temporal correspondance
  
  names(table_new)[which(names(table_new) == "SA3_Code")] <- "SA3_CODE_2011"
  
  table_new$SA3_CODE_2011 <- as.numeric(table_new$SA3_CODE_2011)
  
  joint_table <- left_join(table_new, SA3_2011_SA3_2016 , by = "SA3_CODE_2011")
  
  joint_table <- as.data.frame(joint_table)
  
  joint_table$new_vals_raw <- as.numeric(joint_table[, var_name_p[i]]) * joint_table$RATIO
  
  new_table <- joint_table[,c("SA3_CODE_2016","sex" , "age_group", "calendar_year", "age_immunised_AIR", "new_vals_raw", "QI_INDICATOR")]
  
  new_table <- new_table %>% group_by(SA3_CODE_2016, sex, age_group, calendar_year, age_immunised_AIR) %>% mutate(new_vals = round(sum(new_vals_raw)))
  
  new_table <- new_table %>% drop_na(SA3_CODE_2016)
  
  original_new_table <- new_table[!duplicated(new_table[, c("SA3_CODE_2016","sex" , "age_group", "calendar_year", "age_immunised_AIR")]),]
  
  #-----------------
  
  names(original_new_table)[which(names(original_new_table)== "SA3_CODE_2016")] <- "SA3_CODE16"
  
  names(original_new_table)[which(names(original_new_table) == "new_vals")] <- var_name_p[i]
  
  final_table <- original_new_table[,c("SA3_CODE16", "sex", "age_group", "calendar_year", "age_immunised_AIR", var_name_p[i], "QI_INDICATOR")]
  
  names(final_table)[ncol(final_table)] <- paste0(var_name_p[i], "_uncertainty_correspondence")
  
  final_table[,var_name_p[i]] <- as.numeric(unlist(final_table[,var_name_p[i]]))/100
  
  final_table[,var_name_p[i]] <- round(final_table[,var_name_p[i]],2)
  #final_table <- final_table %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))
  na_index <- which(is.na(final_table$SA3_CODE16) == TRUE)
  
  if(length(na_index) > 0){
    
    
    final_table <- final_table[-na_index, ]
  }
  write.csv(final_table, paste0("./output/AIR_", inicator_code[i], "_", var_name_p[i], "_SA3.csv"), row.names = FALSE)
}
#Extract the relavant columns






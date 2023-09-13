
library(dplyr)

#-----------------

#This function allwos us to claculate the total erp for given age group
calculating_total_erp_SA <- function(erp_data, required_age_groups){
  

  if(length(which(names(erp_data) == "")) > 0){
    erp_data <- erp_data[, -which(names(erp_data) == "")]
  }
  
  
  SA_CODE <-names(erp_data)[1]
  
  names(erp_data)[1] <- "SA_code"
  
  #required_age_group <- paste0(required_age_group, "-", required_age_group)
  
  
  if(SA_CODE == "LGA_CODE16"){
    
    erp_data_new <- NULL
    for(k in 1:(length(required_age_groups))){
      
      spilt_char <- strsplit(required_age_groups[k],split='-', fixed=TRUE)
      
      req_age <- paste0(as.numeric(spilt_char[[1]][1]) : as.numeric(spilt_char[[1]][2]), "-", as.numeric(spilt_char[[1]][1]) : as.numeric(spilt_char[[1]][2]))
      
      sub_data <- erp_data[which(erp_data$age_group %in% req_age),]
      sub_data$age_group  <- required_age_groups[k]
      erp_data_new <- rbind(erp_data_new, sub_data)
      
     
      
    }
    
  }else{
    
    erp_data_new <- erp_data[which(erp_data$age_group %in% required_age_groups),]
  }
  
  erp_data_new <- as.data.frame(erp_data_new)
 
  
  #erp_data_18_24[which(erp_data_18_24$estimated_regional_population == sup_val), "estimated_regional_population"]  <- NA
  
  erp_data_new_summary <- erp_data_new %>% group_by(SA_code, calendar_year, age_group) %>% summarise(total = sum(estimated_regional_population, na.rm  = TRUE))
  
  names(erp_data_new_summary)[1] <- SA_CODE
  
  return(erp_data_new_summary)
  
} 

geom_data <- read.csv("./data/census/ERP/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)


geom_data <-  geom_data[, c("SA2_MAINCODE_2016", "SA2_5DIGITCODE_2016")]

names(geom_data)[1] <- "SA2_CODE16"
#----------------------
# 
# base_file_name <- c("ABS_census_671_religious_affiliation_of_15_24_year_olds", "ABS_census_462_highest_year_of_school_completed", "ABS_census_633_children_and_young_people_who_speak_english_not_well_or_not_at_all", "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home",
#                     "ABS_census_551_volunteering","ABS_census_251_unemployed_and_seeking_full_time_work", "ABS_census_1121_children_with_core_activity_need_for_assistance")

#base_file_name <- "ABS_census_671_religious_affiliation_of_15_24_year_olds"
  

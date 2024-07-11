
library(dplyr)

#-------------



base_file_name <- "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home"

#geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

geo_name <- "SA2"
for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n", "p", "language_spoken_at_home_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year", "language_group_spoken_at_home"), base_file_name, geo_name[i])
  
  
}


cell_suppression <- function(data,  count_indicator_name, rate_indicator_name, name_uncertiniy_col, filetrs, base_file_name, geo){
  
  
  
  #get the name of SA code column
  SA_CODE <- names(data)[1]
  
  fitr_combined <- NULL
  
  for(k in 1:nrow(data) ){
    
    print(paste0(k, "/", nrow(data)))
    fitr_combined <- c(fitr_combined,paste0(data[k, filetrs],collapse ="-"))
    
  }
    
  
  data$filter_combined <- fitr_combined 
  
  ###################
  
  unique_code <- unique(data$filter_combined)
  
  full_data <- NULL
  
  for(i in 1:length(unique_code)){
     
      sub_data <- data[which(data[,"filter_combined"]== unique_code[i]),]
      
      if(nrow(sub_data) > 0){
        male_count <- sub_data[which(sub_data$sex == "male"),count_indicator_name] 
        female_count <- sub_data[which(sub_data$sex == "female"),count_indicator_name] 
        all_count <- sub_data[which(sub_data$sex == "all"),count_indicator_name] 
        
        if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
          
          sub_data[which(sub_data$sex == "male"),c(count_indicator_name, rate_indicator_name)] <- 9999999
          sub_data[which(sub_data$sex == "female"),c(count_indicator_name, rate_indicator_name)]  <- 9999999
          
          if(all_count > 0 & all_count < 5){
            
            sub_data[which(sub_data$sex == "all"),c(count_indicator_name, rate_indicator_name)]  <- 9999999
          }
          
        }
        
        full_data <- rbind(full_data, sub_data)
        
      }

  }
  
  
  if(any(names(full_data) %in% name_uncertiniy_col)){
    
    sup_index2 <- which(full_data[,name_uncertiniy_col] == "Poor")
    
    if(length(sup_index2) > 0 ){
      
      full_data[sup_index2, c(count_indicator_name, rate_indicator_name)] <- 9999999
      
    }
    
    full_data <- full_data[, -which(names(full_data) == name_uncertiniy_col )]
  }

  full_data <- full_data[, -which(names(full_data) == "filter_combined" )]
  
  write.csv(full_data, paste0("./data/census/cell_suppressed/" ,base_file_name, "_", geo, ".csv" ) , row.names = FALSE)
  
 
}





#------------

base_file_name <- "ABS_census_671_religious_affiliation_of_15_24_year_olds"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n", "p", "religion_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year", "religious_affiliation"), base_file_name, geo_name[i])
    
    
}

#-------------------------

base_file_name <- "ABS_census_661_country_of_birth"

geo_name <- c("Australia", "STE", "SA4", "SA3", "LGA")
for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n", "p", "country_of_birth_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year", "region_children_and_young_people_born"), base_file_name, geo_name[i])
  
  
}

#------------------



#-------------------------------
base_file_name <- "ABS_census_462_highest_year_of_school_completed"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")
for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n_young_people_with_year_12_or_equivalent_highest_year_of_school_completed", "p_young_people_with_year_12_or_equivalent_highest_year_of_school_completed", "completed_year12_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year"), base_file_name, geo_name[i])
  
  
}


#-------------------------


base_file_name <- "ABS_census_633_children_and_young_people_who_speak_english_not_well_or_not_at_all"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n_children_and_young_people_who_speak_english_not_well_or_not_at_all", "p_children_and_young_people_who_speak_english_not_well_or_not_at_all", "n_children_and_young_people_who_speak_english_not_well_or_not_at_all_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year"), base_file_name, geo_name[i])
  
  
}
#-------------------------


base_file_name <- "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home"

#geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

geo_name <- "SA2"
for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n", "p", "language_spoken_at_home_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year", "language_group_spoken_at_home"), base_file_name, geo_name[i])
  
  
}
#-------------------------


base_file_name <- "ABS_census_551_volunteering"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n_young_people_doing_unpaid_voluntary_work", "p_young_people_doing_unpaid_voluntary_work", "volunteering_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year"), base_file_name, geo_name[i])
  
  
}
          

#------------------------------
base_file_name <- "ABS_census_251_unemployed_and_seeking_full_time_work"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n_young_people_unemployed_seeking_full_time_work", "p_young_people_unemployed_seeking_full_time_work", "labour_force_status_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year"), base_file_name, geo_name[i])
  
  
}
#--------------------

base_file_name <- "ABS_census_1121_children_with_core_activity_need_for_assistance"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")


for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/proportion_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n_children_and_young_people_with_core_activity_need_for_assistance", "p_children_and_young_people_with_core_activity_need_for_assistance", "core_activity_need_for_assistance_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year"), base_file_name, geo_name[i])
  
  
}

#------------------------
base_file_name <- "ABS_census_321_one_parent_families_with_children_under_15_years_old"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  un_col <- "one_parent_children_uncertainty_correspondence"
  var_col <- "n_of_one_parent_families_with_children_under_15_years_old"
  

  if(length(which(names(data) == un_col)) == 0 ){
    
    sup_index <- which(data[, var_col] > 0 & data[, var_col] < 5 )
    
  }else{
    
    sup_index1 <- which(data[, var_col] > 0 & data[, var_col] < 5)
    sup_index2 <- which(data[, un_col]  == "Poor")
    sup_index <- unique(c(sup_index1,sup_index2))
    
    data <- data[, -which(names(data) == as.character(un_col))]
    
  }
  
  if(length(sup_index) > 0){
    
    data[sup_index, var_col] <- 9999999
  }
  
 

  write.csv(new_data, paste0("./data/census/cell_suppressed/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}
#-------------------

base_file_name <- "ABS_census_323_same_sex_parents_with_dependent_children"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  un_col <- "n_same_sex_couples_with_one_or_more_dependent_children"
  var_col <- "same_sex_couple_children_uncertainty_correspondence"
  
  
  if(length(which(names(data) == un_col)) == 0 ){
    
    sup_index <- which(data[, var_col] > 0 & data[, var_col] < 5 )
    
  }else{
    
    sup_index1 <- which(data[, var_col] > 0 & data[, var_col] < 5)
    sup_index2 <- which(data[, un_col]  == "Poor")
    sup_index <- unique(c(sup_index1,sup_index2))
    
    data <- data[, -which(names(data) == as.character(un_col))]
    
  }
  
  if(length(sup_index) > 0){
    
    data[sup_index, var_col] <- 9999999
  }
  
  
  
  write.csv(new_data, paste0("./data/census/cell_suppressed/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}
#-----------

#-------------------




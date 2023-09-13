
library(dplyr)


base_file_name <- "ABS_census_671_religious_affiliation_of_15_24_year_olds"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , religious_affiliation, calendar_year) %>% mutate(n = sum(n)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}

#-------------------------


base_file_name <- "ABS_census_462_highest_year_of_school_completed"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
  
  names(data)[which(names(data) == var_col)] <- "old_val"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , calendar_year) %>% mutate(old_val = sum(old_val)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  names(new_data)[which(names(new_data) == "old_val")] <-  var_col
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}


#-------------------------


base_file_name <- "ABS_census_633_children_and_young_people_who_speak_english_not_well_or_not_at_all"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
  
  if(length(var_col) > 1){
    
    var_col <- var_col[-which(grepl("uncertainty", var_col))]
  }
  
  names(data)[which(names(data) == var_col)] <- "old_val"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , calendar_year) %>% mutate(old_val = sum(old_val)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  names(new_data)[which(names(new_data) == "old_val")] <-  var_col
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}

#-------------------------


base_file_name <- "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , language_group_spoken_at_home, calendar_year) %>% mutate(n = sum(n)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}

#-------------------------


base_file_name <- "ABS_census_551_volunteering"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
  
  if(length(var_col) > 1){
    
    var_col <- var_col[-which(grepl("uncertainty", var_col))]
  }
  
  names(data)[which(names(data) == var_col)] <- "old_val"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , calendar_year) %>% mutate(old_val = sum(old_val)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  names(new_data)[which(names(new_data) == "old_val")] <-  var_col
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}


#------------------------------
base_file_name <- "ABS_census_251_unemployed_and_seeking_full_time_work"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
  
  if(length(var_col) > 1){
    
    var_col <- var_col[-which(grepl("uncertainty", var_col))]
  }
  
  names(data)[which(names(data) == var_col)] <- "old_val"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , calendar_year) %>% mutate(old_val = sum(old_val)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  names(new_data)[which(names(new_data) == "old_val")] <-  var_col
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
}

#--------------------
base_file_name <- "ABS_census_321_one_parent_families_with_children_under_15_years_old"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  new_data_1 <- data[, 1:2]
  new_data_2 <- data[, 3:ncol(data)]
  new_data_1$age_group <- "0-14"
  
  new_data <- cbind(new_data_1, new_data_2)
  
  names(new_data)[4] <- "n_of_one_parent_families_with_children_under_15_years_old"
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}
#-------------------

base_file_name <- "ABS_census_323_same_sex_parents_with_dependent_children"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  new_data_1 <- data[, 1:2]
  new_data_2 <- data[, 3:ncol(data)]
  new_data_1$age_group <- "0-14"
  
  new_data <- cbind(new_data_1, new_data_2)
  
  names(new_data)[4] <- "n_same_sex_couples_with_one_or_more_dependent_children"
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}
#-----------

#-------------------

base_file_name <- "ABS_census_1121_children_with_core_activity_need_for_assistance"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
  
  if(length(var_col) > 1){
    
    var_col <- var_col[-which(grepl("uncertainty", var_col))]
  }
  
  names(data)[which(names(data) == var_col)] <- "old_val"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , calendar_year) %>% mutate(old_val = sum(old_val)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  names(new_data)[which(names(new_data) == "old_val")] <-  var_col
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
}


#-----------------------


base_file_name <- "ABS_census_661_country_of_birth"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data_total <- data %>% group_by(SA_CODE,  age_group , region_children_and_young_people_born, calendar_year) %>% mutate(n = sum(n)) %>% mutate(sex = "all")
  
  data_total <- data_total[!duplicated(data_total),]
  
  new_data <- rbind(data, data_total)
  
  new_data <- new_data[!duplicated(new_data),]
  
  names(new_data)[1] <- sa_code
  
  write.csv(new_data, paste0("./data/census/files_with_all_calculation/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}

#-------------------------


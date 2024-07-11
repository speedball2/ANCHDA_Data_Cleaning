



base_file_name <- "ABS_census_671_religious_affiliation_of_15_24_year_olds"


geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/cell_suppressed/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  with_r_data <- data[which(data$religious_affiliation == "Other"),]
  without_r_data <- data[which(data$religious_affiliation == "Secular Beliefs and Other Spiritual Beliefs and No Religious Affiliation"),]
  
 
  with_r_data <- with_r_data %>% select(-religious_affiliation)
  without_r_data <- without_r_data %>% select(-religious_affiliation)
  
  write.csv(with_r_data, paste0("./data/census/cell_suppressed/relegious_split/" ,base_file_name, "_with_religious_affiliation_", geo_name[i], ".csv" ) , row.names = FALSE)
  
  write.csv(without_r_data, paste0("./data/census/cell_suppressed/relegious_split/" ,base_file_name, "_without_religious_affiliation_", geo_name[i], ".csv" ) , row.names = FALSE)
  
}



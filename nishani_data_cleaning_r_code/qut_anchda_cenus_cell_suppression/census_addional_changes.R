
library(dplyr)


#we only want counts for age group 20-24 – to be recalculated and other age groups removed from count
base_file_name <- "ABS_census_462_highest_year_of_school_completed"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/changes_required/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  unique(data$age_group)
  
  data <- data[which(data$age_group == "20-24"),]
  
  write.csv(data, paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}


#-----------------------
# 
# we only want counts for age groups 15-19 and 20-24. we also only want counts for category
# Secular Beliefs and Other Spiritual Beliefs and No Religious Affiliation and create second (new) category which is the sum of all other categories (Buddhism, Christianity, …, not stated)
# 

base_file_name <- "ABS_census_671_religious_affiliation_of_15_24_year_olds"

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("./data/census/changes_required/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  data <- data[which(data$age_group == "20-24" | data$age_group == "15-19"),]
  
  reuired_index <- which(data$religious_affiliation == "Secular Beliefs and Other Spiritual Beliefs and No Religious Affiliation" | data$religious_affiliation == "No Religion" )
  
  other_index <- setdiff(1:nrow(data), reuired_index )
  
  data$religious_affiliation[other_index] <- "Other"
  
  data$religious_affiliation[reuired_index] <- "Secular Beliefs and Other Spiritual Beliefs and No Religious Affiliation"
  
  sa_code <- names(data)[1]
  
  names(data)[1] <- "SA_CODE"
  
  data <- data %>% group_by(SA_CODE , age_group , sex, religious_affiliation, calendar_year) %>% mutate(n = sum(n))
  
  data <- data[!duplicated(data),]
  
  names(data)[1] <- sa_code
  
  write.csv(data, paste0("./data/census/" ,base_file_name, "_", geo_name[i], ".csv" ), row.names = FALSE)
  
}




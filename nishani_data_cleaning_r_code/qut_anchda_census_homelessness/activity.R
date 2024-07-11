

#this allows user to get the output files for count - bmi indicators 
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)

#---------------------------------
source("./functions/read_files.R")

#-----------------------

filling_data <- function(data, col){
  
  
  non_na_index <- which(is.na(data[, col])== FALSE)
  
  non_na_index <- sort(non_na_index)
  
  for(i in 1:length(non_na_index)){
    
    
    if(i == length(non_na_index)){
      
      data[(non_na_index[i] + 1): nrow(data), col] <- data[non_na_index[i], col]
      
    }else{
      
      data[(non_na_index[i] + 1): (non_na_index[i + 1] - 1), col] <- data[non_na_index[i], col]
      
    }
    
  }
  
  return(data)
}

##############


#This function allwos us to claculate the total erp for given age group
calculating_total_erp_SA <- function(erp_data, required_age_groups){
  
  
  if(length(which(names(erp_data) == "")) > 0){
    erp_data <- erp_data[, -which(names(erp_data) == "")]
  }
  
  
  SA_CODE <-names(erp_data)[1]
  
  names(erp_data)[1] <- "SA_code"
  
  #required_age_group <- paste0(required_age_group, "-", required_age_group)
  
  erp_data_new <- erp_data[which(erp_data$age_group %in% required_age_groups),]
  
  #erp_data_18_24[which(erp_data_18_24$estimated_regional_population == sup_val), "estimated_regional_population"]  <- NA
  
  erp_data_new_summary <- erp_data_new %>% group_by(SA_code, calendar_year, age_group) %>% summarise(total = sum(estimated_regional_population, na.rm  = TRUE))
  
  names(erp_data_new_summary)[1] <- SA_CODE
  
  return(erp_data_new_summary)
  
} 
#----------------------------
data <- read_files("./data/census_estimating_homelessness_SA3_2016.xlsx", "census_estimating_homelessness_", "A11:E38401")

names(data) <- c("SA3_CODE16","age_group", "sex", "opgp_homelessness_operational_groups", "n_of_homeless_children_and_young_people")

data <- filling_data(data, "SA3_CODE16")

data <- filling_data(data, "age_group")

data <- filling_data(data, "sex")

data$sex <- tolower(data$sex)

data$opgp_homelessness_operational_groups <- tolower(data$opgp_homelessness_operational_groups)

data$age_group <- ifelse(data$age_group == "0-4 years", "0-4", data$age_group)
data$age_group <- ifelse(data$age_group == "5-9 years", "5-9", data$age_group)
data$age_group <- ifelse(data$age_group == "10-14 years", "10-14", data$age_group)
data$age_group <- ifelse(data$age_group == "15-19 years", "15-19", data$age_group)
data$age_group <- ifelse(data$age_group == "20-24 years", "20-24", data$age_group)

data$calendar_year <- "2016"

data <- data[, c("SA3_CODE16","age_group", "sex", "opgp_homelessness_operational_groups", "calendar_year", "n_of_homeless_children_and_young_people")]

data <- data[-which(data$opgp_homelessness_operational_groups == "total"),]

#-------------------------

sa_code <- names(data)[1]

names(data)[1] <- "SA_CODE"

data$n_of_homeless_children_and_young_people <- as.numeric(data$n_of_homeless_children_and_young_people)

data_total <- data %>% group_by(SA_CODE,  age_group ,opgp_homelessness_operational_groups, calendar_year) %>% mutate(n_of_homeless_children_and_young_people = sum(n_of_homeless_children_and_young_people)) %>% mutate(sex = "all")

data_total <- data_total[!duplicated(data_total),]

new_data <- rbind(data, data_total)

new_data <- new_data[!duplicated(new_data),]

names(new_data)[1] <- sa_code

new_data$SA3_CODE16 <- as.integer(new_data$SA3_CODE16)
new_data$calendar_year <- as.integer(new_data$calendar_year)

new_data <- new_data[-which(new_data$opgp_homelessness_operational_groups == "not applicable"),]

write.csv(new_data, "./output/ABS_census_291_homeless_children_and_young_people_SA3.csv", row.names = FALSE)

################################################
data <- read_files("./data/census_estimating_homelessness_SA4_2016.xlsx", "census_estimating_homelessness_", "A11:E10791")

names(data) <- c("SA4_CODE16","age_group", "sex", "opgp_homelessness_operational_groups", "n_of_homeless_children_and_young_people")

data <- filling_data(data, "SA4_CODE16")

data <- filling_data(data, "age_group")

data <- filling_data(data, "sex")

data$sex <- tolower(data$sex)

data$opgp_homelessness_operational_groups <- tolower(data$opgp_homelessness_operational_groups)

data$age_group <- ifelse(data$age_group == "0-4 years", "0-4", data$age_group)
data$age_group <- ifelse(data$age_group == "5-9 years", "5-9", data$age_group)
data$age_group <- ifelse(data$age_group == "10-14 years", "10-14", data$age_group)
data$age_group <- ifelse(data$age_group == "15-19 years", "15-19", data$age_group)
data$age_group <- ifelse(data$age_group == "20-24 years", "20-24", data$age_group)

data$calendar_year <- "2016"

data <- data[, c("SA4_CODE16","age_group", "sex", "opgp_homelessness_operational_groups", "calendar_year", "n_of_homeless_children_and_young_people")]

data <- data[-which(data$opgp_homelessness_operational_groups == "total"),]

#-------------------------

sa_code <- names(data)[1]

names(data)[1] <- "SA_CODE"

data$n_of_homeless_children_and_young_people <- as.numeric(data$n_of_homeless_children_and_young_people)

data_total <- data %>% group_by(SA_CODE,  age_group ,opgp_homelessness_operational_groups, calendar_year) %>% mutate(n_of_homeless_children_and_young_people = sum(n_of_homeless_children_and_young_people)) %>% mutate(sex = "all")

data_total <- data_total[!duplicated(data_total),]

new_data <- rbind(data, data_total)

new_data <- new_data[!duplicated(new_data),]

names(new_data)[1] <- sa_code

new_data$SA4_CODE16 <- as.integer(new_data$SA4_CODE16)
new_data$calendar_year <- as.integer(new_data$calendar_year)
new_data <- new_data[-which(new_data$opgp_homelessness_operational_groups == "not applicable"),]

write.csv(new_data, "./output/ABS_census_291_homeless_children_and_young_people_SA4.csv", row.names = FALSE)

#------------------------------------------

#state level calculation

new_data$SA4_CODE16 <- substr(new_data$SA4_CODE16,1,1)
names(new_data)[1] <- "STE_CODE16"

ste_summary_data <- new_data %>% group_by(STE_CODE16, sex, age_group, calendar_year, opgp_homelessness_operational_groups) %>% summarise(n_of_homeless_children_and_young_people = sum(n_of_homeless_children_and_young_people, na.rm = TRUE))


write.csv(ste_summary_data, "./output/ABS_census_291_homeless_children_and_young_people_STE.csv", row.names = FALSE)

#----------------------

#Australia


names(ste_summary_data)[1] <- "Australia"
ste_summary_data$Australia <- 0
Aus_summary_data <- ste_summary_data %>% group_by(Australia, sex, age_group, calendar_year, opgp_homelessness_operational_groups) %>% summarise(n_of_homeless_children_and_young_people = sum(n_of_homeless_children_and_young_people, na.rm = TRUE))


write.csv(Aus_summary_data, "./output/ABS_census_291_homeless_children_and_young_people_Australia.csv", row.names = FALSE)



#########################################################3333

#calculating proprotion
base_file_name <- "ABS_census_291_homeless_children_and_young_people"

geo_name <- c("Australia", "STE", "SA4", "SA3")

for(i in 1:length(base_file_name)){
  
  for(j in 1:length(geo_name)){
    
    
    data <- read.csv(paste0("./output/" ,base_file_name[i], "_", geo_name[j], ".csv" ), header = TRUE, check.names = FALSE)
    
    
    erp_data <- read.csv(paste0("./data/ERP/ABS_ERP_181_ERP_",  geo_name[j], ".csv"), header = TRUE, check.names = FALSE)
    
    erp_data_total <- calculating_total_erp_SA (erp_data, unique(data$age_group))

    combined_data <- left_join(data,erp_data_total, by = c(names(data)[1],"age_group", "calendar_year"))
    
    var_col <- names(combined_data)[which(startsWith( names(combined_data), "n") == TRUE)]
    
    if(length(var_col) > 1){
      
      var_col <- var_col[-which(grepl("uncertainty", var_col))]
    }
    
    combined_data$p <- ifelse(combined_data[,var_col] == 0, 0, round(as.numeric(combined_data[,var_col]) /as.numeric(combined_data$total), 4))
    
    
    names(combined_data)[which(names(combined_data) == "p")] <- paste0("p", substr(var_col,2, nchar(var_col)))
    
    combined_data <- combined_data %>% select(-total)
    
    un_col <- combined_data[which(grepl("uncertainty", names(combined_data)))]
    
    if(length(un_col) > 0){
      
      combined_data <- combined_data[, -which(grepl("uncertainty", names(combined_data)))]
      
      combined_data <- cbind(combined_data, un_col)
    }
    
    
    
    write.csv(combined_data, paste0("./output/propotion/" ,base_file_name[i], "_", geo_name[j], ".csv" ), row.names = FALSE)
    
  }
  
}
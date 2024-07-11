library(dplyr)
library(tidyr)

cell_suppression <- function(data,filetrs, count_indicator_name){
  
  fitr_combined <- NULL
  
  for(k in 1:nrow(data) ){
    
    fitr_combined <- c(fitr_combined,paste0(data[k, filetrs],collapse ="-"))
    
  }
  
  data$filter_combined <- fitr_combined 
  
  unique_code <- unique(data$filter_combined)
  
  full_data <- NULL
  
  for(i in 1:length(unique_code)){ 
    
    sub_data <- data[which(data[,"filter_combined"]== unique_code[i]),]
    
    num_row_male <- length(which(sub_data$sex == "male"))
    num_row_female <- length(which(sub_data$sex == "female"))
    
    if(nrow(sub_data) > 0){
      
      for(k in 1:length(count_indicator_name)){
        
        
        if(num_row_male == 1 & num_row_female == 1){
          
          
          male_count <- sub_data[which(sub_data$sex == "male"),count_indicator_name[k]] 
          female_count <- sub_data[which(sub_data$sex == "female"),count_indicator_name[k]] 
          all_count <- sub_data[which(sub_data$sex == "all"),count_indicator_name[k]] 
          
          if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
            
            sub_data[which(sub_data$sex == "male"),c(count_indicator_name[k])] <- 9999999
            sub_data[which(sub_data$sex == "female"),c(count_indicator_name[k])]  <- 9999999
            
            if(all_count > 0 & all_count < 5){
              
              sub_data[which(sub_data$sex == "all"),c(count_indicator_name[k])]  <- 9999999
            }
            
          }
        }else{
          
          if(num_row_male == 0 & num_row_female == 1){
            
            female_count <- sub_data[which(sub_data$sex == "female"),count_indicator_name[k]] 
            #all_count <- sub_data[which(sub_data$sex == "all"),count_vars[k]] 
            
            if(female_count > 0 & female_count < 5){
              
              sub_data[which(sub_data$sex == "female"),c(count_indicator_name[k])]  <- 9999999
              sub_data[which(sub_data$sex == "all"),c(count_indicator_name[k])]  <- 9999999
            }
            
          }else{
            
            
            if(num_row_male == 1 & num_row_female == 0){
              
              male_count <- sub_data[which(sub_data$sex == "male"),count_indicator_name[k]] 
              #all_count <- sub_data[which(sub_data$sex == "all"),count_vars[k]] 
              
              if(male_count > 0 & male_count < 5){
                
                sub_data[which(sub_data$sex == "male"),c(count_indicator_name[k])] <- 9999999
                sub_data[which(sub_data$sex == "all"),c(count_indicator_name[k])]  <- 9999999
              }
              
            }
          }
          
        }
      }
      
      
    }
    
  full_data <- rbind(full_data, sub_data)
  }
  
  full_data <- full_data[, -which(names(full_data) == "filter_combined" )]
  
  return(full_data)
}

#----------------------------------------

file_name <- "ABS_schools_411_attendance_at_primary_school_year_5_STE.csv"

data <- read.csv(paste0("./data/SCHOOLS/", file_name), header = TRUE, check.names = FALSE)

data1 <- data[, 1:7]

data2 <- data[, c(1:6, 8)]

filetrs <- c(names(data)[1], "calendar_year", "age_group", "affiliation_abs_schools", "school_grade")

new_data1 <- cell_suppression(data1, filetrs, "n_full_time_student")


new_data2 <- cell_suppression(data2, filetrs, "n_part_time_student")

new_data <-left_join(new_data1, new_data2, by = c(names(data)[1], "calendar_year", "age_group","sex","affiliation_abs_schools", "school_grade"))

write.csv(new_data, paste0("./data/SCHOOLS/cell_suppressed/", file_name), row.names = FALSE)

#----------------------------------------

file_name <- "ABS_schools_462_school_completion_year_12_STE.csv"

data <- read.csv(paste0("./data/SCHOOLS/", file_name), header = TRUE, check.names = FALSE)

data1 <- data[, 1:7]

data2 <- data[, c(1:6, 8)]

filetrs <- c(names(data)[1], "calendar_year", "age_group", "affiliation_abs_schools", "school_grade")

new_data1 <- cell_suppression(data1, filetrs, "n_full_time_student")


new_data2 <- cell_suppression(data2, filetrs, "n_part_time_student")

new_data <-left_join(new_data1, new_data2, by = c(names(data)[1], "calendar_year", "age_group","sex","affiliation_abs_schools", "school_grade"))

write.csv(new_data, paste0("./data/SCHOOLS/cell_suppressed/", file_name), row.names = FALSE)

#----------------------------------------

file_name <- "ABS_schools_473_full_time_and_part_time_students_STE.csv"

data <- read.csv(paste0("./data/SCHOOLS/", file_name), header = TRUE, check.names = FALSE)

data1 <- data[, 1:7]

data2 <- data[, c(1:6, 8)]

filetrs <- c(names(data)[1], "calendar_year", "age_group", "affiliation_abs_schools", "school_grade")

new_data1 <- cell_suppression(data1, filetrs, "n_full_time_student")


new_data2 <- cell_suppression(data2, filetrs, "n_part_time_student")

new_data <-left_join(new_data1, new_data2, by = c(names(data)[1], "calendar_year", "age_group","sex","affiliation_abs_schools", "school_grade"))

write.csv(new_data, paste0("./data/SCHOOLS/cell_suppressed/", file_name), row.names = FALSE)

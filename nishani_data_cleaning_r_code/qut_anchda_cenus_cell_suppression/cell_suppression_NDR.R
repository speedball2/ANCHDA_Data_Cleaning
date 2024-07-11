
file_base_names <- c('NDR_1122_chronic_conditions_diabetes_incidence_SA4', "NDR_1122_chronic_conditions_diabetes_prevalence_SA4")

geo_name <- c("SA4")

var_col <- c("n_new_cases_ndr", "n_cases_ndr")


for(k in 1:length(file_base_names)){
  
  combined_data <- read.csv(paste0("./data/NDR/",file_base_names[k], ".csv" ), header = TRUE, check.names = FALSE)
  
  SA_CODE <- names(combined_data)[1]
  
  unique_code <- unique(combined_data[, SA_CODE])
  
  u_year <- unique(combined_data$calendar_year)
  
  u_age_group <- unique(combined_data$age_group)
  
  full_data <- NULL
  
  for(i in 1:length(unique_code)){
    
    for(j in 1:length(u_year)){
      
      for(l in 1:length(u_age_group)){
        
        sub_data <- combined_data[which(combined_data[,SA_CODE]== unique_code[i] & combined_data$calendar_year == u_year[j] & combined_data$age_group == u_age_group[l]),]
        if(nrow(sub_data) > 0){
          
          male_count <- sub_data[which(sub_data$sex == "male"),var_col[k]] 
          female_count <- sub_data[which(sub_data$sex == "female"),var_col[k]] 
          all_count <- sub_data[which(sub_data$sex == "all"),var_col[k]] 
          
          if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
            
            sub_data[which(sub_data$sex == "male"),var_col[k]] <- 9999999
            sub_data[which(sub_data$sex == "female"),var_col[k]]  <- 9999999
            
            if(all_count > 0 & all_count < 5){
              
              sub_data[which(sub_data$sex == "all"),var_col[k]]  <- 9999999
            }
            
          }
          
          full_data <- rbind(full_data, sub_data)
          
        }
        
      }
      
      
     
      
    }
    
  }
  
  write.csv(full_data, paste0("./output/NDR/",file_base_names[k], ".csv" ), row.names = FALSE)
  
}




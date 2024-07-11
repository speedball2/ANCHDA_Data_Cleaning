file_name <- "ABS_census_291_homeless_children_and_young_people"

geo_names <- c('Australia', 'STE', "SA3", "SA4")

for(i in 1:length(file_name)){
  
  for(j in 1:length(geo_names)){
    
    
    data <- read.csv(paste0("./output/propotion/",file_name[i],"_", geo_names[j], ".csv" ), header = TRUE, check.names = FALSE)
    count_indicator_name <- "n_of_homeless_children_and_young_people"
    rate_indicator_name <- "p_of_homeless_children_and_young_people"
    u_code <- unique(data[,1])
    u_age <- unique(data$age_group)
    u_op_group <- unique(data$opgp_homelessness_operational_groups)
    
    full_data <- NULL
    
    for(k in 1:length(u_code)){
      
      for(l in 1:length(u_age)){
        
        for(m in 1:length(u_op_group)){
          
          sub_data <- data[which(data[,1]== u_code[k] & data$age_group == u_age[l] & data$opgp_homelessness_operational_groups == u_op_group[m]),]
          
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
      }
      
    }
    write.csv(full_data, paste0("./output/cell_suppressed/",file_name[i],"_", geo_names[j], ".csv" ), row.names = FALSE)
    
  }
  

}

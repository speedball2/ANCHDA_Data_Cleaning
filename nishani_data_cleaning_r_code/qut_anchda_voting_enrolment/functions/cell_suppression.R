

cell_suppression <- function(count_file, rate_file, count_indicator_name, rate_indicator_name, name_uncertiniy_col, num_filetrs, b_name){
  
  
  #reading files
  count_data <- read.csv(count_file, header = TRUE, check.names = FALSE)
  
  rate_data <- read.csv(rate_file, header = TRUE, check.names = FALSE)
  
  rate_data <- rate_data[, -which(names(rate_data) == name_uncertiniy_col)]
  
  #get the name of SA code column
  SA_CODE <- names(count_data)[1]
  
  #filter names
  filter_names <- names(count_data)[2:(2 + num_filetrs -1)]
  
  #combined count and rate data
  combined_data <- left_join(count_data, rate_data, by = c(SA_CODE, filter_names ))
  
  
  
  ###################
  
  unique_code <- unique(combined_data[, SA_CODE])
  u_year <- unique(combined_data$calendar_year)
  
  full_data <- NULL
  
  for(i in 1:length(unique_code)){
    
    for(j in 1:length(u_year)){
      
      sub_data <- combined_data[which(combined_data[,SA_CODE]== unique_code[i] & combined_data$calendar_year == u_year[j]),]
      
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
  
  #cel value with por for uncertainty correspondence
  sup_index2 <- which(full_data[,name_uncertiniy_col] == "Poor")
  
  if(length(sup_index2) > 0 ){

    full_data[sup_index2, c(count_indicator_name, rate_indicator_name)] <- 9999999
    
  }
  
  # NULL_index <- which(combined_data[, rate_indicator_name]> 1 & combined_data[, rate_indicator_name] != 9999999)
  # 
  # if(length(NULL_index) > 0){
  #   
  #   combined_data[NULL_index, c(count_indicator_name, rate_indicator_name)] <- "NA"
  #   
  # }
  
  write.csv(full_data[, c(SA_CODE,filter_names, count_indicator_name, rate_indicator_name)], paste0("./output/cell_suppresed/",b_name,substr(SA_CODE, 1,3) , ".csv") , row.names = FALSE)
  
  write.csv(full_data[, c(SA_CODE,filter_names, count_indicator_name)], paste0("./output/cell_suppresed/", basename(count_file)) , row.names = FALSE)
  
  write.csv(full_data[, c(SA_CODE,filter_names, rate_indicator_name)], paste0("./output/cell_suppresed/", basename(rate_file)) , row.names = FALSE)
  
}


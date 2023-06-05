
cell_suppression <- function(file_name, num_filetrs){
  
  
  #reading files
  combined_data <- read.csv(file_name, header = TRUE, check.names = FALSE)
  
  
  #get the name of SA code column
  SA_CODE <- names(combined_data)[1]
  
  #filter names
  filter_names <- names(combined_data)[2:(2 + num_filetrs -1)]
  
  count_indicator_name <-  names(combined_data)[ncol(combined_data)- 1]
  
  rate_indicator_name <- names(combined_data)[ncol(combined_data)]
  
  ###################
  
  unique_code <- unique(combined_data[, SA_CODE])
  #u_year <- unique(combined_data$calendar_year)
  
  full_data <- NULL
  
  for(i in 1:length(unique_code)){
    
  
      
      sub_data <- combined_data[which(combined_data[,SA_CODE]== unique_code[i] ),]
      
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
  
  

  write.csv(full_data[, c(SA_CODE,filter_names, count_indicator_name,rate_indicator_name)], paste0("./output/CELL_SUPPRESSED/", basename(file_name)) , row.names = FALSE)
  
}


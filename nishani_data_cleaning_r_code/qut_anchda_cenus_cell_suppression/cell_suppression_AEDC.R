

cell_suppression_aedc <- function(count_file, num_filetrs){
  
 
  
  #reading files
  count_data <- read.csv(count_file, header = TRUE, check.names = FALSE)

  
  #get the name of SA code column
  SA_CODE <- names(count_data)[1]
  
  #filter names
  filter_names <- names(count_data)[2:(2 + num_filetrs -1)]
  
  combined_data <- count_data
  ###################
  
  unique_code <- unique(combined_data[, SA_CODE])
  u_year <- unique(combined_data$calendar_year)
  
  full_data <- NULL
  
  start_col <- num_filetrs + 2
  
  for(i in 1:length(unique_code)){
    
    for(j in 1:length(u_year)){
      
      sub_data <- combined_data[which(combined_data[,SA_CODE]== unique_code[i] & combined_data$calendar_year == u_year[j]),]
      
      num_row_male <- length(which(sub_data$sex == "male"))
      num_row_female <- length(which(sub_data$sex == "female"))
      count_vars <- names(sub_data)[which(grepl("n_",names(sub_data)[start_col:ncol(sub_data)])) + start_col - 1]
      
      if(nrow(sub_data) > 0){
        
        for(k in 1:length(count_vars)){
          
          rate_indicator_name <- paste0("p", substr(count_vars[k],2, nchar(count_vars[k])))
          
         if(num_row_male == 1 & num_row_female == 1){
           
          
           male_count <- sub_data[which(sub_data$sex == "male"),count_vars[k]] 
           female_count <- sub_data[which(sub_data$sex == "female"),count_vars[k]] 
           all_count <- sub_data[which(sub_data$sex == "all"),count_vars[k]] 
           
           if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
             
             sub_data[which(sub_data$sex == "male"),c(count_vars[k], rate_indicator_name)] <- 9999999
             sub_data[which(sub_data$sex == "female"),c(count_vars[k], rate_indicator_name)]  <- 9999999
             
             if(all_count > 0 & all_count < 5){
               
               sub_data[which(sub_data$sex == "all"),c(count_vars[k], rate_indicator_name)]  <- 9999999
             }
             
           }
         }else{
           
           if(num_row_male == 0 & num_row_female == 1){
             
             female_count <- sub_data[which(sub_data$sex == "female"),count_vars[k]] 
             #all_count <- sub_data[which(sub_data$sex == "all"),count_vars[k]] 
             
             if(female_count > 0 & female_count < 5){
               
               sub_data[which(sub_data$sex == "female"),c(count_vars[k], rate_indicator_name)]  <- 9999999
               sub_data[which(sub_data$sex == "all"),c(count_vars[k], rate_indicator_name)]  <- 9999999
             }
             
           }else{
             
             
             if(num_row_male == 1 & num_row_female == 0){
               
               male_count <- sub_data[which(sub_data$sex == "male"),count_vars[k]] 
               #all_count <- sub_data[which(sub_data$sex == "all"),count_vars[k]] 
               
               if(male_count > 0 & male_count < 5){
                 
                 sub_data[which(sub_data$sex == "male"),c(count_vars[k], rate_indicator_name)] <- 9999999
                 sub_data[which(sub_data$sex == "all"),c(count_vars[k], rate_indicator_name)]  <- 9999999
               }
               
             }
           }
           
         }
        }
        
        valid_sup_index <- which(sub_data[,which(grepl("valid_aedc", names(sub_data)) == TRUE)] > 0 & sub_data[,which(grepl("valid_aedc", names(sub_data)) == TRUE)] < 5)
   

        if(length(valid_sup_index) > 0){
          
          sub_data[valid_sup_index ,which(grepl("valid_aedc", names(sub_data)) == TRUE)] <- 9999999
        }
        
      }
    
        
      full_data <- rbind(full_data, sub_data)

    }
    
  }

  
  write.csv(full_data, paste0("./output/data_4_digits/cell_suppressed/",basename(count_file)) , row.names = FALSE)
  
 
}


#########################################
fn_names <- list.files(path = "./output/data_4_digits/", pattern = "*.csv", full.names = TRUE)


for(t in 1:length(fn_names)){
  
  
  cell_suppression_aedc(fn_names[t], 3)
}




file_names <- list.files(path = "./output/PHIDU/", pattern = "*.csv" ,full.names = TRUE)


un_col <- "quality_indicator"

for(i in 1:length(file_names)){
  
  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  var_col <- names(data)[which(startsWith( names(data), "n_") == TRUE)]
  
  p_col <- names(data)[which(startsWith( names(data), "rate_") == TRUE)]
  
  num_cat_sex <- length(unique(data$sex))
  
  if(num_cat_sex == 1){
    
    sup_index <- which(data[, var_col] > 0 & data[, var_col] < 5 )
    sup_index1 <- which(data[, var_col] > 0 & data[, var_col] < 5)
    sup_index2 <- which(data[, un_col]  == "Poor")
    
    sup_index <- unique(c(sup_index1,sup_index2))
    
    
    if(length(sup_index) > 0){
      
      data[sup_index, c(var_col, p_col)] <- 9999999
    }
    
    full_data <- data
    
  }else{
    
    full_data <- NULL
    u_code <- unique(data[,1])
    u_age_group <- unique(data$age_group)
    
    for(j in 1:length(u_code)){
      
      for(k in 1:length(u_age_group)){
        
        sub_data <- data[which(data[,1] == u_code[j] & data$age_group == u_age_group[k]),]
        
        if(nrow(sub_data) > 0){
          
          male_count <- sub_data[which(sub_data$sex == "male"),var_col] 
          female_count <- sub_data[which(sub_data$sex == "female"),var_col] 
          all_count <- sub_data[which(sub_data$sex == "all"),var_col] 
          
          if(!is.na(male_count) & !is.na(female_count) & !is.na(all_count)){
            
            if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
              
              sub_data[which(sub_data$sex == "male"),c(var_col, p_col)] <- 9999999
              sub_data[which(sub_data$sex == "female"),c(var_col, p_col)]  <- 9999999
              
              if(all_count > 0 & all_count < 5){
                
                sub_data[which(sub_data$sex == "all"),c(var_col, p_col)]  <- 9999999
              }
              
            }
          }else{
            
            length_na <- length(which(is.na(sub_data[, var_col]) == TRUE))
            
            if(length_na == 3){
              
              sub_data <- sub_data
            }else{
              
              if(length_na == 2){ #only one sex category with value
                
                if(sub_data[which(is.na(sub_data[, var_col]) == FALSE), var_col] > 0 & sub_data[which(is.na(sub_data[, var_col]) == FALSE), var_col] <5){
                  
                  sub_data[which(is.na(sub_data[, var_col]) == FALSE), c(var_col, p_col)]  <- 9999999
                }
                
                
              }else{
                
                if(is.na(male_count)){
                  
                  if(female_count > 0 & female_count < 5){
                    
                    sub_data[which(sub_data$sex == "female"),c(var_col, p_col)]  <- 9999999
                  }
                  if(all_count > 0 & all_count < 5){
                    
                    sub_data[which(sub_data$sex == "all"),c(var_col, p_col)]  <- 9999999
                  }
                  
                }
                
                if(is.na(female_count)){
                  
                  if(male_count > 0 & male_count < 5){
                    
                    sub_data[which(sub_data$sex == "male"),c(var_col, p_col)]  <- 9999999
                  }
                  if(all_count > 0 & all_count < 5){
                    
                    sub_data[which(sub_data$sex == "all"),c(var_col, p_col)]  <- 9999999
                  }
                  
                }
                
                if(is.na(all_count)){
                  
                  if(male_count > 0 & male_count < 5){
                    
                    sub_data[which(sub_data$sex == "male"),c(var_col, p_col)]  <- 9999999
                  }
                  if(female_count > 0 & female_count < 5){
                    
                    sub_data[which(sub_data$sex == "female"),c(var_col, p_col)]  <- 9999999
                  }
                  
                }
              }
              
            }
          }
          full_data <- rbind(full_data, sub_data)
        }
        
        
        
        #cel value with por for uncertainty correspondence
        
      }
      
    }
    
  }
  
  sup_index2 <- which(full_data[,un_col] == "Poor")
  
  if(length(sup_index2) > 0 ){
    
    full_data[sup_index2, c(var_col, p_col)] <- 9999999
    
  }
  
  
  
  
  
  full_data <- full_data[, -which(names(full_data) == as.character(un_col))]
  
  
  write.csv(full_data, paste0("./output/PHIDU_SUPPRESSED/",basename(file_names[i])), row.names = FALSE)
  
  
}

#------------------------------

file_names <- list.files(path = "./output/ED_SUPPRESSED/", pattern = "*.csv", full.names = TRUE)

for(i in 1:length(file_names)){
  
  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  index <- which(data$LGA_CODE16 == 19999)
  
  if(length(index) > 0){
    
    print(basename(file_names[i]))
    data <- data[-index,]
    
  }
  
  write.csv(data, paste0("./output/ED_SUPPRESSED/LGA_REMOVED/", basename(file_names[i])), row.names = FALSE)
  
  
}

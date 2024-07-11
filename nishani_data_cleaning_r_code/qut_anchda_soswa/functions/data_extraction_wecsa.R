
source("./functions/letters2numbers.R")

data_extraction_wecsa <- function(data, essentail_col_number, indicator_letters, indicator_name, age_group, file_name){
  
  
  indicator_col_numbers <- letters2numbers(indicator_letters) - 1
  
  data[,indicator_col_numbers ] <- (lapply( data[,indicator_col_numbers ], function(x) as.numeric(x)))
  
  if(length(indicator_col_numbers) == 1){
    
    indicat_col <- data[,indicator_col_numbers] 
    
  }else{
    
    
    indicat_col <- rowSums(data[,indicator_col_numbers ] )
    
  }
  
  new_data <- cbind(data[,essentail_col_number ], indicat_col)
  
  
  
  names(new_data) <- c("STE_CODE16", "calendar_year", "age_group", "sex" , indicator_name)
  
  new_data <- new_data[which(new_data$age_group == age_group ),]
  
  index_na  <- which(is.na(new_data[, indicator_name])== TRUE)
  
  if(length(index_na) > 0){
    
    new_data <- new_data[-index_na, ]
  }
  
  if(nrow(new_data) > 1){
    
    if(age_group == "12-17" | age_group == "15-17" ){
      new_data <- new_data[-which(new_data$sex =="all" ),]
    }
    
  }
 
  new_data <- new_data[, c("STE_CODE16", "sex","age_group" ,"calendar_year", indicator_name)]
  
  new_data[, indicator_name] <- unlist(round(new_data[, indicator_name]/100, 2))
  
  write.csv(new_data,file_name, row.names = FALSE)
  
}


data_extraction  <- function(data, essential_col, indicator_col_letter, sex, age_group, calendar_year, count_name, rate_name){
  
  #covert column letters to column numbers
  indicator_col <- letters2numbers(indicator_col_letter)
  
  #covert to numeric - indicator columns
  data[,indicator_col] <- lapply( data[,indicator_col], function(x) as.numeric(x))
  
  #combined key columns and indicator columns
  new_data <- cbind(data[, essential_col], data[,indicator_col])
  
  new_data <- as.data.frame(new_data)
  
  #convert LGA code to numeric in order to identify character values such as "NEW SOUTH WALES" and remove those values
  new_data[,1] <- as.numeric(new_data[,1])
  
  na_index <- which(is.na(new_data[,1]) == TRUE)
  
  if(length(na_index) > 0){
    
    new_data <- new_data[-na_index, ]
  }
  
  #rounding to second decimal points
  new_data[, ncol(new_data)- 1] <- round( new_data[, ncol(new_data) - 1], 2)
  new_data[, ncol(new_data)] <- round( new_data[, ncol(new_data)], 2)
  
  #rename te data frame
  names(new_data) <- c("LGA_CODE16", "quality_indicator", count_name, rate_name)
  
  new_data$sex <- sex
  new_data$age_group <- age_group
  new_data$year_range <- calendar_year
  
  #reodering the columns
  new_data <- new_data[, c("LGA_CODE16", "sex", "age_group", "year_range", count_name, rate_name, "quality_indicator")]
  
  return(new_data)
  
}
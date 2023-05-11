
sexual_health_dat_extraction <- function(sex_health_data, n_name, p_name, n_cols,p_cols ,number_of_col, site, sie_code, file_name){
  
  sex_health_data <- sex_health_data[-c(1:5),]
  
  #extract column for correct responses to HIV transmission questions
  
  sex_health_data <- sex_health_data[,c(1:5,n_cols,p_cols)]
  
  sex_health_data[, 6:ncol(sex_health_data)] <- lapply(  sex_health_data[, 6:ncol(sex_health_data)], function(x) as.numeric(x))
  
  if(number_of_col> 1){
     
    sex_health_data[, 6] <- ifelse(rowSums(is.na(sex_health_data[, 6:(6 + number_of_col - 1)])) == number_of_col, NA,  rowSums(sex_health_data[, 6:(6 + number_of_col - 1)], na.rm = TRUE) )
      
    sex_health_data[, 6 + number_of_col]  <- ifelse(rowSums(is.na(sex_health_data[, (6 + number_of_col):((6 + number_of_col) + number_of_col - 1)])) == number_of_col, NA,  rowSums(sex_health_data[, (6 + number_of_col):((6 + number_of_col) + number_of_col - 1)], na.rm = TRUE) )
    
    sex_health_data <- sex_health_data[, c(1:5, 6,6 + number_of_col)]
  
  }
  
  sex_health_data <- sex_health_data[, -1]
  
  
  names(sex_health_data) <- c("STE_NAME" ,"calendar_year", "grade", "sex", n_name, p_name)

  sex_health_data <- sex_health_data[which(sex_health_data$calendar_year %in% c("2008", "2013", "2018", "2021")),]
  
  sex_health_data$sex <- tolower(sex_health_data$sex)
  
  sex_health_data$sex[which(sex_health_data$sex == "non-binary")] <- "tgd"
  
  sex_health_data[, n_name] <- as.numeric(sex_health_data[, n_name])
  
  sex_health_data[, p_name] <- as.numeric(sex_health_data[, p_name])
  
  #convert precentage in to proportion
  sex_health_data[, p_name] <- sex_health_data[, p_name]/100
  
  #defining the age group
  sex_health_data$age_group <- "15-18"
  #rounding for two decimal points
  sex_health_data <- sex_health_data %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))
  
  #state code
  
  sex_health_data$STE_CODE16 <- NA
  
  sex_health_data$STE_CODE16 <- unlist(lapply(1:length(sex_health_data$STE_NAME), function(x){
    
    site_cat[which(site == sex_health_data$STE_NAME[x])]
  }))
  
  sex_health_data <- as.data.frame(sex_health_data)
  
  sex_health_data <- sex_health_data[c("STE_CODE16", "calendar_year", "sex", "age_group", n_name, p_name)]
  
  write.csv(sex_health_data, file_name , row.names = FALSE)
  
  
}
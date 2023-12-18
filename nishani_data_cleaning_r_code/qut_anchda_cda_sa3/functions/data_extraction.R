data_extraction <- function(data, SA_CODE16, sex, age_group, count_name, indicator_name) {
  
  numeric_col <- as.data.frame(data)
  indicator_data <- NULL
  count_data <- NULL
  year <- NULL
  
  for (i in 1:(ncol(numeric_col) / 3)) {
    
    year <- c(year, rep(names(numeric_col)[3 * (i - 1) + 1], nrow(numeric_col)))
    indicator_data <- c(indicator_data, numeric_col[, 3 * (i - 1) + 1])
    count_data <- c(count_data, numeric_col[, 3 * (i - 1) + 2])
    
  }
  
  SA_CODE16 <- unlist(rep(as.vector(SA_CODE16), times = ncol(numeric_col) / 3))
  
  new_data <- data.frame(SA_CODE16, sex, age_group, year, count_data, indicator_data)
  
  names(new_data) <- c("SA3_CODE16", "sex", "age_group", "calendar_year", count_name, indicator_name)
  
  new_data[, 5] <- as.numeric(new_data[, 5])
  new_data[, 6] <- as.numeric(new_data[, 6])
  
  if (startsWith(indicator_name, "p_") == TRUE) {
    new_data[, indicator_name] <- new_data[, indicator_name] / 100
  }
  
  new_data[, indicator_name] <- round(new_data[, indicator_name], 2)
  
  if (length(which(is.na(new_data[, 5]) == TRUE)) > 0) {
    new_data[which(is.na(new_data[, 5]) == TRUE), 5] <- 9999999
  }
  
  if (length(which(is.na(new_data[, 6]) == TRUE)) > 0) {
    new_data[which(is.na(new_data[, 6]) == TRUE), 6] <- 9999999
  }
  
  return(new_data)
}

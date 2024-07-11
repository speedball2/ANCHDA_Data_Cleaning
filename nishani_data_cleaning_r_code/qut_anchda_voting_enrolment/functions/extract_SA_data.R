
extract_SA_data <- function(new_data, code_name,new_code_name , uncertainty_colname, file_name1, file_name2){
  
 
  new_data_SA2 <- new_data[, c("calendar_year", 'female', "male", "all", code_name, uncertainty_colname)]
  
  
  new_data_SA2 <- melt(new_data_SA2, id.vars = c("calendar_year", code_name, uncertainty_colname),  value.name = "number_of_enrolment")
  
  new_data_SA2$number_of_enrolment <- as.numeric(new_data_SA2$number_of_enrolment)
  
  names(new_data_SA2)[which(names(new_data_SA2) == "variable")] <- "sex"
  
  summary_data_SA2 <- new_data_SA2 %>% group_by(eval(parse(text = code_name)), sex,  calendar_year) %>%   mutate(number_of_enrolment = sum(number_of_enrolment))
  
 # summary_data_SA2$number_of_enrolment <-   unlist(rounding_fun(summary_data_SA2$number_of_enrolment))
    
  #mutate(across(where(is.numeric), function(x) unlist(rounding_fun(x)))) #rounding function
  
  #names(summary_data_SA2) <- c(new_code_name, "sex", "calendar_year", "number_of_enrolment")
  
  names(summary_data_SA2)[which(names(summary_data_SA2) == code_name)] <- new_code_name
  
  summary_data_SA2$age_group <- "18-24"
  
  summary_data_SA2_number <- summary_data_SA2[,c(new_code_name, "sex", "age_group", "calendar_year", "number_of_enrolment", uncertainty_colname)]

  summary_data_SA2_number <- summary_data_SA2_number[!duplicated(summary_data_SA2_number[, c(new_code_name, "sex", "age_group", "calendar_year", "number_of_enrolment")]),]
  
  summary_data_SA2_number <- summary_data_SA2_number %>%  drop_na(.data[[new_code_name]])%>% drop_na(calendar_year)
  
  #new_summary_data_SA2_number <- summary_data_SA2_number[which(summary_data_SA2_number$sex != "total"),]
 
  
  summary_data_SA2_number <- arrange(summary_data_SA2_number, new_code_name, calendar_year )
  write.csv(summary_data_SA2_number, file_name1, row.names = FALSE)
  
  return(summary_data_SA2_number)
  
  #-----------------------------------

  dcast_data <- pivot_wider(summary_data_SA2_number, names_from = sex, values_from = number_of_enrolment)
  
  dcast_data$female  <- ifelse(dcast_data$female == 0, 0, dcast_data$female / dcast_data$all)
  
  dcast_data$male  <- ifelse(dcast_data$male == 0, 0, dcast_data$male / dcast_data$all)
  
  dcast_data$all  <- ifelse(dcast_data$all == 0, 0, dcast_data$all / dcast_data$all)
  
  #dcast_data <- dcast_data[ , -which(names(dcast_data) == "total")]
  
  long_dcast_data <- melt(dcast_data, id.vars = c("calendar_year", new_code_name, "age_group",uncertainty_colname ),  value.name = "rate_of_enrolment")
  
  names(long_dcast_data)[which(names(long_dcast_data) == "variable")] <- "sex"
  
  long_dcast_data <- long_dcast_data[,c(new_code_name, "sex", "age_group", "calendar_year", "rate_of_enrolment", uncertainty_colname)]
  
  long_dcast_data$rate_of_enrolment <- as.numeric(long_dcast_data$rate_of_enrolment)
  
  #long_dcast_data <- long_dcast_data %>%  mutate(across(where(is.numeric), function(x) unlist(rounding_fun(x)))) #rounding function
  long_dcast_data$rate_of_enrolment <-   unlist(round(long_dcast_data$rate_of_enrolment, 2))
  
  
  long_dcast_data <- long_dcast_data %>% drop_na(.data[[new_code_name]])%>% drop_na(calendar_year)
  reshape(summary_data_SA2, direction = "wide", idvar = c(new_code_name, "age_group", "calendar_year"), timevar = "number_of_enrolment", v.names = "sex")
  
  write.csv(long_dcast_data, file_name2, row.names = FALSE)
}

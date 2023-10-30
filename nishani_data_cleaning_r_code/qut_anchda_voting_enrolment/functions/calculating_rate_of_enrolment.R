calculating_rate_of_enrolment <- function(number_data, total_data, join_vars, required_vars, file_name2, join_file_name){
  
  joint_data <- left_join(number_data, total_data, by = join_vars)
  
  joint_data$rate_of_enrolment <- ifelse(joint_data$number_of_enrolment == 0, 0, joint_data$number_of_enrolment/joint_data$total)
  joint_data$rate_of_enrolment <-   unlist(round(joint_data$rate_of_enrolment,2))
  
  write.csv(joint_data, join_file_name, row.names = FALSE)
  
  joint_data <- joint_data[, required_vars]
  
  names(joint_data)[ncol(joint_data)] <- "rate_of_enrolment_uncertainty_correspondence"
  
  joint_data <- arrange(joint_data,  eval(parse( text = join_vars[1])), eval(parse( text = join_vars[2])))
  
  write.csv(joint_data, file_name2, row.names = FALSE)
  
}
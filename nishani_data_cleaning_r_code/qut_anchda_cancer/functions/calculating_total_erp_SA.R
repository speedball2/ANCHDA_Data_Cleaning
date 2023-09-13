#This function allwos us to claculate the total erp for given age group
calculating_total_erp_SA <- function(erp_file, required_age_group, sup_val, key_col){
  
  erp_data <- read.csv(erp_file, header = TRUE, check.names = FALSE)

  if(length(which(names(erp_data) == "")) > 0){
    erp_data <- erp_data[, -which(names(erp_data) == "")]
  }
  required_age_group <- paste0(required_age_group, "-", required_age_group)
  
  erp_data_18_24 <- erp_data[which(erp_data$age_group %in% required_age_group),]
  
  #erp_data_18_24[which(erp_data_18_24$estimated_regional_population == sup_val), "estimated_regional_population"]  <- NA
  
  summary_erp_data_18_24 <- erp_data_18_24 %>% group_by(eval(parse(text = key_col)), calendar_year) %>% summarise(total = sum(estimated_regional_population, na.rm  = TRUE))
  
  names(summary_erp_data_18_24)[1] <- key_col
  return(summary_erp_data_18_24)
  
} 

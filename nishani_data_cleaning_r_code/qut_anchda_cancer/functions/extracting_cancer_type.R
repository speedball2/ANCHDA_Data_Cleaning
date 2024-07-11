extracting_cancer_type <- function(data, cancer_type, SA3_total, SA4_total,STE_total, var1, var2, file_name1, file_name2){
  
  if(cancer_type == "all"){
    
    summary_data_SA3 <- data %>% group_by(SA3_CODE16, calendar_year,age_group, sex) %>% summarise(n_count = n())
    
    summary_data_SA4 <- data %>% group_by(SA4_CODE16, calendar_year,age_group, sex) %>% summarise(n_count = n())
    
    summary_data_STE <- data %>% group_by(STE_CODE16, calendar_year,age_group, sex) %>% summarise(n_count = n())
    
  }else{
    
    # summary_data_total_SA3 <- data %>% group_by(SA3_CODE16, diagnosis_year,diagnosis_age_group, sex) %>% summarise(n_count_total = n())
    # 
    # summary_data_total_SA4 <- data %>% group_by(SA4_CODE16, diagnosis_year,diagnosis_age_group, sex) %>% summarise(n_count_total = n())
    # 
    # summary_data_total_STE <- data %>% group_by(STE_CODE16, diagnosis_year,diagnosis_age_group, sex) %>% summarise(n_count_total = n())
    
    summary_data_SA3 <- data[which(data$cancer == cancer_type), ] %>% group_by(SA3_CODE16, calendar_year, age_group, sex) %>% summarise(n_count = n())
    
    summary_data_SA4 <- data[which(data$cancer == cancer_type), ] %>% group_by(SA4_CODE16, calendar_year,age_group, sex) %>% summarise(n_count = n())
    
    summary_data_STE <- data[which(data$cancer == cancer_type), ] %>% group_by(STE_CODE16, calendar_year,age_group, sex) %>% summarise(n_count = n())
    
    
  }
  
  summary_data_SA3 <- left_join(summary_data_SA3, SA3_total , by = c("SA3_CODE16", "calendar_year"))
  
  summary_data_SA4 <- left_join(summary_data_SA4, SA4_total , by = c("SA4_CODE16", "calendar_year"))
  
  summary_data_STE <- left_join(summary_data_STE, STE_total , by = c("STE_CODE16", "calendar_year"))
  
  
  summary_data_SA3$p <- ifelse(summary_data_SA3$n_count == 0, 0, summary_data_SA3$n_count/ summary_data_SA3$total )
  summary_data_SA4$p <- ifelse(summary_data_SA4$n_count == 0, 0, summary_data_SA4$n_count/ summary_data_SA4$total )
  summary_data_STE$p <- ifelse(summary_data_STE$n_count == 0, 0, summary_data_STE$n_count/ summary_data_STE$total )
  
  
  summary_data_SA3$p <- summary_data_SA3$p * 100000
  summary_data_SA4$p <- summary_data_SA4$p * 100000
  summary_data_STE$p <- summary_data_STE$p * 100000
  
  summary_data_SA3 <- summary_data_SA3 %>% select(-total)
  summary_data_SA4 <- summary_data_SA4 %>% select(-total)
  summary_data_STE <- summary_data_STE %>% select(-total)
  
  summary_data_SA3$p <- unlist(rounding_fun(summary_data_SA3$p))
  summary_data_SA4$p <- unlist(rounding_fun(summary_data_SA4$p))
  summary_data_STE$p <- unlist(rounding_fun(summary_data_STE$p))
  
  
  names(summary_data_SA3)[which(names(summary_data_SA3) == "n_count")] <- var1
  names(summary_data_SA4)[which(names(summary_data_SA4) == "n_count")] <- var1
  names(summary_data_STE)[which(names(summary_data_STE) == "n_count")] <- var1
  
 
  names(summary_data_SA3)[which(names(summary_data_SA3) == "p")] <- var2
  names(summary_data_SA4)[which(names(summary_data_SA4) == "p")] <- var2
  names(summary_data_STE)[which(names(summary_data_STE) == "p")] <- var2
    
  write.csv(summary_data_SA3[, c("SA3_CODE16", "calendar_year","age_group", "sex", var1)], paste0(file_name1,"_SA3.csv"), row.names = FALSE)
  write.csv(summary_data_SA3[, c("SA3_CODE16", "calendar_year","age_group", "sex", var2)],paste0(file_name2,"_SA3.csv"), row.names = FALSE)
    
  write.csv(summary_data_SA4[, c("SA4_CODE16", "calendar_year","age_group", "sex", var1)], paste0(file_name1,"_SA4.csv"), row.names = FALSE)
  write.csv(summary_data_SA4[, c("SA4_CODE16", "calendar_year","age_group", "sex", var2)],paste0(file_name2,"_SA4.csv"), row.names = FALSE)
    
  write.csv(summary_data_STE[, c("STE_CODE16", "calendar_year","age_group", "sex", var1)], paste0(file_name1,"_STE.csv"), row.names = FALSE)
  write.csv(summary_data_STE[, c("STE_CODE16", "calendar_year","age_group", "sex", var2)],paste0(file_name2,"_STE.csv"), row.names = FALSE)

  
  
}
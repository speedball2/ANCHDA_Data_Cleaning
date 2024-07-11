data_extraction_for_indicator <- function(guiq_data, col_num_indicator, new_name_indicator, intrested_cat, qld_geom_data, out_fname){
  
  data_152 <- cbind( guiq_data[, c(1:2,4)], guiq_data[, col_num_indicator])
  
  names(data_152)[4] <- "old_val"
  
  if(length(intrested_cat) == 1){
    
    data_152[, 4] <- ifelse(data_152[, 4] == intrested_cat, 1, 0)
  }else{
    
    data_152[, 4] <- ifelse(data_152[, 4] == intrested_cat[1] | data_152[, 4] == intrested_cat[2], 1, 0)
  }
  
  
  STE_data <-  data_152 
  names(STE_data)[which(names(STE_data) == "SA4")] <- "STE_CODE16"
  

  data_152 <- data_152 %>% group_by(SA4, sex, age_group) %>%   mutate(new_val = sum(old_val))
  
  #QLD data
  STE_data$STE_CODE16 <- 3
  STE_data <- STE_data %>% group_by(STE_CODE16, sex, age_group) %>%   mutate(new_val = sum(old_val))
  STE_data <- STE_data %>% group_by(STE_CODE16, age_group) %>%   mutate(total = n())
  STE_data <- STE_data[, c("STE_CODE16", "sex", "age_group","new_val", "total" )]
  STE_data <- STE_data %>% distinct(.keep_all = TRUE)
  #------------------------
  

  data_152 <- data_152 %>% group_by(SA4, age_group) %>%   mutate(total = n())
  data_152 <- data_152[, c("SA4", "sex", "age_group","new_val", "total" )]
  data_152 <- data_152 %>% distinct(.keep_all = TRUE)
  
  #creating new all row
  u_code <- unique(data_152$SA4)
  new_data_152 <- NULL
  
  for(i in 1:length(u_code)){
    
    sub_data <- data_152[which(data_152$SA4 == u_code[i]),]
    new_row <- sub_data[3,]
    new_row$sex <- "all"
    new_row$new_val <- sum(sub_data$new_val, na.rm = TRUE)
    sub_data <- rbind(sub_data, new_row)
    new_data_152 <- rbind(new_data_152,sub_data)
    
  }
  
  new_data_152$new_val_p <- ifelse(new_data_152$new_val == 0, 0, new_data_152$new_val / new_data_152$total)
  new_data_152$calendar_year <- 2020
  new_data_152 <- new_data_152[, c("SA4", "sex", "age_group","calendar_year","new_val", "new_val_p" )]
  new_data_152 <- new_data_152 %>% distinct(.keep_all = TRUE) %>%  mutate(across(where(is.numeric), function(x) round(x, 2))) #rounding function
  
  
  names(new_data_152)[1] <- "SA4_NAME_2016"
  names(new_data_152)[which(names(new_data_152) == "new_val")] <- paste0("n", substr(new_name_indicator, 2, nchar(new_name_indicator)))
  
  names(new_data_152)[which(names(new_data_152) == "new_val_p")] <- new_name_indicator
  
  joint_data <- left_join(new_data_152, qld_geom_data , by = "SA4_NAME_2016")
  
  joint_data <- joint_data[, c("SA4_CODE_2016", "sex", "age_group","calendar_year", paste0("n", substr(new_name_indicator, 2, nchar(new_name_indicator))),new_name_indicator ) ]
  
  names(joint_data)[1] <- "SA4_CODE16"
  
  joint_data <- joint_data[order(joint_data$SA4_CODE16),]
  
  joint_data <- joint_data %>% drop_na(SA4_CODE16)
  
  joint_data <- joint_data[-which(joint_data$sex == "other"),]
  
  #--------------------------------
  
  ste_new_row <- STE_data[3,]
  ste_new_row$sex <- "all"
  ste_new_row$new_val <- sum(STE_data$new_val, na.rm = TRUE)
  STE_data <- rbind(STE_data,ste_new_row )
  STE_data$new_val_p <- ifelse(STE_data$new_val == 0, 0, STE_data$new_val / STE_data$total)
  STE_data$calendar_year <- 2020
  STE_data <- STE_data[, c("STE_CODE16", "sex", "age_group","calendar_year","new_val", "new_val_p" )]
  
  #------------------------
  STE_data <- STE_data %>% distinct(.keep_all = TRUE) %>%  mutate(across(where(is.numeric), function(x)  round(x, 2))) #rounding function
  
  STE_data <- STE_data[-which(STE_data$sex == "other"),]
  #----------------------------
  
  names(STE_data)[which(names(STE_data) == "new_val")] <- paste0("n", substr(new_name_indicator, 2, nchar(new_name_indicator)))
  
  names(STE_data)[which(names(STE_data) == "new_val_p")] <- new_name_indicator
  
  write.csv(joint_data, paste0(out_fname, "_SA4.csv"), row.names = FALSE)
  
  write.csv(STE_data, paste0(out_fname, "_STE.csv"), row.names = FALSE)
}

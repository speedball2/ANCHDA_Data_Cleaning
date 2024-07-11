
extract_SA_data_general <- function(new_data, filter_vars, var_name , code_name, new_code_name , uncertainty_colname, valu_type, file_name1 ){
  
  
  data <- new_data[, c(c(filter_vars, var_name), code_name, uncertainty_colname, "AREA_ALBERS_SQKM")]
  
  data <- as.data.frame(data)
  
  data[, var_name] <- as.numeric( data[, var_name])
  
  # filters combo
  if(length(filter_vars) == 1){ 
    data$filters_combo <- data[,filter_vars[1]]
  } else if(length(filter_vars) == 2){
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],sep="_")
  } else if(length(filter_vars) == 3){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],df_2021[,data[3]],sep="_")
  }else if(length(filter_vars) == 4){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],data[,filter_vars[3]],data[,filter_vars[4]],sep="_")
  } else if(length(filter_vars) == 5){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],data[,filter_vars[3]],data[,filter_vars[4]], data[,filter_vars[5]],sep="_")
  } else {print("check number of filter variables in filter_vars argument")}
  
  data$old_val <- data[, var_name] 
  
  data <- data %>% group_by(filters_combo,eval(parse(text = code_name))) %>%   mutate(TOTAL_AREA = sum(AREA_ALBERS_SQKM))
  
  data$ratio <-  data$AREA_ALBERS_SQKM / data$TOTAL_AREA
    
    
  if(valu_type == "p"){
    
        data$old_val  <-  data$old_val  * data$ratio 
  }
        
  summary_data_SA2 <- data %>% group_by(filters_combo,eval(parse(text = code_name))) %>%   mutate(new_vals = sum(old_val))
  
  
  
  summary_data_SA2$new_vals <-   unlist(rounding_fun(summary_data_SA2$new_vals))
  
  names(summary_data_SA2)[which(names(summary_data_SA2) == code_name)] <- new_code_name
  

  summary_data_SA2_number <- summary_data_SA2[,c(new_code_name, filter_vars, "new_vals", uncertainty_colname)]
  
  names(summary_data_SA2_number)[which(names(summary_data_SA2_number) == "new_vals" )] <- var_name
  
  summary_data_SA2_number <- summary_data_SA2_number[!duplicated(summary_data_SA2_number[, c(new_code_name, filter_vars, var_name)]),]
  
  summary_data_SA2_number <- summary_data_SA2_number %>%  drop_na(.data[[new_code_name]])%>% drop_na(calendar_year)
  
  write.csv(summary_data_SA2_number, file_name1, row.names = FALSE)
   
 
}

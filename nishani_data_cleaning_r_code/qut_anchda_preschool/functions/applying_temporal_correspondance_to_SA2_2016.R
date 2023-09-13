
applying_temporal_correspondance <- function(data,key_data_col_name, tc_data, key_tc_data_col_name, var_names, filter_vars, tc_type, valu_type, geo_to_name, uncertainty_colname){
  
  names(data)[which(names(data) == key_data_col_name )] <- key_tc_data_col_name

  # filters combo
  if(length(filter_vars) == 1){
    data$filters_combo <- data[,filter_vars[1]]
  } else if(length(filter_vars) == 2){
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],sep="_")
  } else if(length(filter_vars) == 3){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]], data[,filter_vars[3]],sep="_")
  }else if(length(filter_vars) == 4){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],data[,filter_vars[3]],data[,filter_vars[4]],sep="_")
  } else if(length(filter_vars) == 5){ # If there is an additional filter column in the data, use the following line instead
    data$filters_combo <- paste(data[,filter_vars[1]],data[,filter_vars[2]],data[,filter_vars[3]],data[,filter_vars[4]], data[,filter_vars[5]],sep="_")
  } else {print("check number of filter variables in filter_vars argument")}
  
  
  # Outer join
  merge_data <- merge(data, tc_data, group_by = key_tc_data_col_name ,all = T )
  
  if(tc_type == "backward"){
    merge_data <- merge_data[-which(merge_data$BMOS_NULL_FLAG != 0),]
  }
 
  
  if(length(var_names) == 1){
    merge_data[,var_names] <- as.numeric( merge_data[,var_names])
  }else{
    
    merge_data[,var_names] <- lapply(merge_data[,var_names], function(x)as.numeric(x))
  }
  
  merge_data$new_vals_raw <- merge_data[,var_names]

  # if(tc_type == "backward" ){
  #   
  #   merge_data[,var_names] <- merge_data[,var_names] * as.numeric(merge_data$new_ratio)
  # }
  
  if(tc_type == "foward" | (tc_type == "backward" & valu_type == "p")){
    
    merge_data$new_vals_raw <- merge_data[,var_names] * as.numeric(merge_data$RATIO)
  }
  
  merge_data <- merge_data %>% group_by(filters_combo, eval(parse(text = geo_to_name)) ) %>% mutate(new_vals = (sum(new_vals_raw)))
  
  # rename uncertainty indicator
  merge_data[[uncertainty_colname]] <- merge_data$QI_INDICATOR
  
  merge_data <- merge_data %>% ungroup()
  
  out_data <- merge_data[, c(geo_to_name, filter_vars, "new_vals", uncertainty_colname)]
  
  #names(out_data)[1] <- geo_to_name
  
  names(out_data)[which(names(out_data) == "new_vals" )] <- var_names
  
  out_data <- out_data[!duplicated(out_data[,c(geo_to_name, filter_vars, var_names)]),]
  
  out_data <-  out_data %>% drop_na(.data[[geo_to_name]])
  
  return(out_data)
  
  
}
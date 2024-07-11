join_to_SA1_2016 <- function(data, key_data_col_name , tc_data, key_tc_data_col_name, var_names, geo_to_name, uncertainty_colname){
  
  names(data)[which(names(data) == key_data_col_name )] <- key_tc_data_col_name
  
  merge_data <- merge(data, tc_data, group_by = key_tc_data_col_name ,all = T )
  
  merge_data[,var_names] <- lapply(merge_data[,var_names], function(x)as.numeric(x))
  
  merge_data[,var_names] <- merge_data[,var_names] * as.numeric(merge_data$RATIO)
  
  # group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
  merge_data <- merge_data %>% 
    group_by(calendar_year, eval(parse(text = geo_to_name))) %>% mutate(male = round(sum(male)))  %>% mutate(female = round(sum(female)))  %>% mutate(all = round(sum(all)))
  
  
  # rename uncertainty indicator
  merge_data[[uncertainty_colname]] <- merge_data$QI_INDICATOR
  
  # ungroup
  merge_data <- merge_data %>% ungroup()
  
  out_data <- merge_data[, c(geo_to_name, "SA1_7DIGITCODE_2016", "calendar_year", var_names, uncertainty_colname)]
  
  out_data <- distinct(out_data, .keep_all = T) %>% drop_na(.data[[geo_to_name]])
  
  return(out_data)
}

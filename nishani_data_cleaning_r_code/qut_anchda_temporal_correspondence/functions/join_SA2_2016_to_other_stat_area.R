
join_SA2_2016_to_other_stat_area<- function(data, SA2_2016_AUST, by_var, required_var_names){
  
  joint_data <- left_join(data,SA2_2016_AUST, by = by_var)
  
  joint_data <- joint_data[,c(required_var_names, by_var , "SA3_CODE_2016", "SA4_CODE_2016", "STATE_CODE_2016", "AREA_ALBERS_SQKM")]
  
  return(joint_data)
  
}
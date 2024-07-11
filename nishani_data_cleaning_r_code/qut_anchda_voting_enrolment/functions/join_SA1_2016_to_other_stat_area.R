
join_SA1_2016_to_other_stat_area<- function(data, SA1_2016_AUST, by_var, uncertainty_colname){
  
  joint_data <- left_join(data,SA1_2016_AUST, by = by_var)
  
  joint_data <- joint_data[,c("calendar_year", 'female', "male","all" ,uncertainty_colname,"SA2_MAINCODE_2016","SA2_5DIGITCODE_2016" ,"SA3_CODE_2016", "SA4_CODE_2016", "STATE_CODE_2016")]
  
  return(joint_data)
}
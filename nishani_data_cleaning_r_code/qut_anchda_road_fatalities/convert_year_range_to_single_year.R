
fn <- c("./output/cell_suppression/BITRE_172_children_0_16_rolling_sum_over_10_years_motor_vehicle_accidents_SA4.csv", "./output/cell_suppression/BITRE_172_children_17_25_rolling_sum_over_10_years_motor_vehicle_accidents_SA4.csv")



for(i in 1:length(fn)){
  
  
  old <- read.csv(fn[i], header = TRUE, check.names = FALSE)
  
  old$year_range <- substr(old$year_range, 6, nchar(old$year_range))
  
  names(old)[which(names(old) == "year_range")] <- "calendar_year"
  
  write.csv(old, paste0("./output/cell_suppression/single_year/", basename(fn[i])), row.names = FALSE)
}


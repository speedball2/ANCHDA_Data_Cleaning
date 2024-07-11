base_file_names_count <- c("NPDC_111_n_lowbirthweight_under_2500g", "NPDC_114_n_smoking_during_pregnancy", "NPDC_115_n_1st_antenatal_visit_14_to_19_weeks", "NPDC_115_n_1st_antenatal_visit_less_14_weeks", "NPDC_115_n_1st_antenatal_visit_over_20_weeks",
                           "NPDC_117_n_pre_term_birth_less_37_weeks", "NPDC_1112_n_births_mothersage_20_24_years", "NPDC_1111_n_births_mothersage_15_19_years")
base_file_names_p <- c("NPDC_111_p_lowbirthweight_under_2500g", "NPDC_114_p_smoking_during_pregnancy", "NPDC_115_p_1st_antenatal_visit_14_to_19_weeks", "NPDC_115_p_1st_antenatal_visit_less_14_weeks", "NPDC_115_p_1st_antenatal_visit_over_20_weeks",
                           "NPDC_117_p_pre_term_birth_less_37_weeks", "NPDC_1112_p_births_mothersage_20_24_years", "NPDC_1111_p_births_mothersage_15_19_years")

geo_names <- c("SA3", "SA4", "STE")

k<- 0
for(i in 1:length(base_file_names_count)){
  
  for(j in 1:length(geo_names)){
    
    
    count_data <- read.csv(paste0("./output/",base_file_names_count[i], "_", geo_names[j], ".csv" ), header = TRUE, check.names = FALSE)
    p_data <- read.csv(paste0("./output/",base_file_names_p[i], "_", geo_names[j], ".csv" ), header = TRUE, check.names = FALSE)
    
    if(ncol(count_data) != 5){
      
      print(paste0(base_file_names_count[i]))
    }
    
    SA_CODE <- names(count_data)[1]
    # names(count_data)[1] <- "SA_code"
    # names(p_data)[1] <- "SA_code"
    # 
    combined_data <- left_join(count_data, p_data, by = c(SA_CODE,"year_range", "age_group", "sex"))
    
    
    sup_index <- which(combined_data[,5] > 0 & combined_data[,5] < 5)
    
    if(length(sup_index) > 0){
       k <- k + 1
      combined_data[sup_index, c(5,6)] <- 9999999
    }
    
    write.csv(combined_data[, c(1:5)], paste0("./output/cell_suppressed/",base_file_names_count[i], "_", geo_names[j], ".csv" ), row.names = FALSE)
    
    write.csv(combined_data[, c(1:4,6)], paste0("./output/cell_suppressed/",base_file_names_p[i], "_", geo_names[j], ".csv" ),row.names = FALSE)
    
  }
  
}
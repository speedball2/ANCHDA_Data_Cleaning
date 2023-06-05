file_base_names <-c("ABS_census_1121_children_with_core_activity_need_for_assistance",
                     "ABS_census_671_religious_affiliation_of_15_24_year_olds",
                     "ABS_census_661_country_of_birth",
                     "ABS_census_633_children_and_young_people_who_speak_english_not_well_or_not_at_all",
                     "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home",
                     "ABS_census_551_volunteering",
                     "ABS_census_462_highest_year_of_school_completed",
                     "ABS_census_323_same_sex_parents_with_dependent_children",
                     "ABS_census_321_one_parent_families_with_children_under_15_years_old",
                     "ABS_census_283_household_tenure",
                     "ABS_census_282_internet_connection_at_home",
                     "ABS_census_251_unemployed_and_seeking_full_time_work")

geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")


k <- 0

for(i in 1:length(file_base_names)){
  
  for(j in 1:length(geo_name)){

    data <- read.csv(paste0("./data/",file_base_names[i], "_", geo_name[j], ".csv" ), header = TRUE, check.names = FALSE)
  
    code <- names(data)[1]
    var_col <- names(data)[which(startsWith( names(data), "n") == TRUE)]
     #obtaining the name of the uncertainty, if there is uncertainty colum will return the name of that column else return NULL
    un_col <- lapply(1:length(names(data)), function(x){
      
      if(any(unlist(strsplit(names(data)[x], split="_")) == "uncertainty") == TRUE){
        
        return(names(data)[x])
        
      }
      
    })
    
    un_col <- do.call("rbind", un_col)
    
    if(is.null(un_col) == TRUE){
      
      sup_index <- which(data[, var_col] > 0 & data[, var_col] < 5 )
      
    }else{
      
      sup_index1 <- which(data[, var_col] > 0 & data[, var_col] < 5)
      sup_index2 <- which(data[, un_col]  == "Poor")
      sup_index <- unique(c(sup_index1,sup_index2))
      
      data <- data[, -which(names(data) == as.character(un_col))]
    
    }
    
    if(length(sup_index) > 0){
      
      data[sup_index, var_col] <- 9999999
    }
   
    if(length(which(names(data) == "age_group"))== 0){
      
      data$age_group <- "0-24"
    }
    if(length(which(names(data) == "sex"))== 0){
      
      data$sex <- "all"
    }
    
    data <- data[,c(code, "age_group", "sex", "calendar_year",var_col)]
  
    write.csv(data, paste0("./output/",file_base_names[i], "_", geo_name[j], ".csv" ), row.names = FALSE)
  }
  

}

base_file_name <- "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home"
geo_name <- "SA2"
data <- read.csv(paste0("C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/proportion_calculation/" ,base_file_name, "_", geo_name, ".csv" ), header = TRUE, check.names = FALSE)
u_year <- unique(data$calendar_year)

for(m in 1:length(u_year)){
  
  sub_data <- data[which(data$calendar_year == u_year[m]),]
  
  write.csv(sub_data,paste0("C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/proportion_calculation/632/", base_file_name, "_", u_year[m], "_", geo_name, ".csv"), row.names = FALSE)
  
}

#---------------------------------------

base_file_name <- "ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home_2006"

#geo_name <- c("Australia", "STE", "SA4", "SA3", "SA2", "LGA")

geo_name <- "SA2"

for(i in 1:length(geo_name)){
  
  
  data <- read.csv(paste0("C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/proportion_calculation/632/" ,base_file_name, "_", geo_name[i], ".csv" ), header = TRUE, check.names = FALSE)
  
  
  cell_suppression(data,  "n", "p", "language_spoken_at_home_uncertainty_correspondence", c(names(data)[1], "age_group", "calendar_year", "language_group_spoken_at_home"), base_file_name, geo_name[i])
  
  
}
#-----------------------------------------------

cell_suppression <- function(data,  count_indicator_name, rate_indicator_name, name_uncertiniy_col, filetrs, base_file_name, geo){
  
  
  
  #get the name of SA code column
  SA_CODE <- names(data)[1]
  

  u_code <- unique(data[, SA_CODE])
  u_age <- unique(data[, "age_group"])
  u_fil <- unique(data[, "language_group_spoken_at_home"])
  
  pp <- 0
  
  full_data <- NULL
  
  for(i in 1:length(u_code)){
    
    for(j in 1:length(u_age)){
      
      for(k in 1:length(u_fil)){
        
        sub_data <- data[which(data[,SA_CODE]== u_code[i] & data[,"age_group"]== u_age[j] & data[, "language_group_spoken_at_home"] == u_fil[k]),]
        pp <- pp + 1
        print(paste0(pp, "/",length(u_code)* length(u_age) *  length(u_fil) ))
        if(nrow(sub_data) > 0){
          male_count <- sub_data[which(sub_data$sex == "male"),count_indicator_name] 
          female_count <- sub_data[which(sub_data$sex == "female"),count_indicator_name] 
          all_count <- sub_data[which(sub_data$sex == "all"),count_indicator_name] 
          
          if((male_count > 0 & male_count < 5) | (female_count > 0 & female_count < 5) ){
            
            sub_data[which(sub_data$sex == "male"),c(count_indicator_name, rate_indicator_name)] <- 9999999
            sub_data[which(sub_data$sex == "female"),c(count_indicator_name, rate_indicator_name)]  <- 9999999
            
            if(all_count > 0 & all_count < 5){
              
              sub_data[which(sub_data$sex == "all"),c(count_indicator_name, rate_indicator_name)]  <- 9999999
            }
            
          }
          
          full_data <- rbind(full_data, sub_data)
          
        }
        
      }
    }
    
  }
  
  
  if(any(names(full_data) %in% name_uncertiniy_col)){
    
    sup_index2 <- which(full_data[,name_uncertiniy_col] == "Poor")
    
    if(length(sup_index2) > 0 ){
      
      full_data[sup_index2, c(count_indicator_name, rate_indicator_name)] <- 9999999
      
    }
    
    full_data <- full_data[, -which(names(full_data) == name_uncertiniy_col )]
  }
  
 # full_data <- full_data[, -which(names(full_data) == "filter_combined" )]
  
  write.csv(full_data, paste0("C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/cell_suppressed/632/" ,base_file_name, "_", geo, ".csv" ) , row.names = FALSE)
  
  
}



#---------------------------------

files_632 <- list.files(path = "C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/cell_suppressed/632/", full.names = TRUE)


new_data <- NULL

for(m in 1:length(files_632)){
  
  data <- read.csv(files_632[m], header = TRUE, check.names = FALSE)
  new_data <-rbind(new_data, data)
  
  
}

write.csv(new_data, "C:/Users/Nishani/Documents/QUT_ANCHDA/qut_anchda_cenus_cell_suppression/data/census/cell_suppressed/632/ABS_census_632_children_and_young_people_who_speak_a_language_other_than_english_at_home_SA2.csv", row.names = FALSE )

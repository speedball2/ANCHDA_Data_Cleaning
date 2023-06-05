
file_base_names <- c('NMD_112_infant_mortality', "NMD_1110_births", "NMD_1211_mortality", "NMD_1212_young_people_mortality")

geo_name <- c("SA3", "SA2")

var_col <- c("total_deaths_nmd", "total_births_nmd", "total_deaths_nmd", "total_deaths_nmd")

rate_col <- c("crude_rate_per_1000_nmd", "crude_rate_per_1000_nmd", "crude_rate_per_100,000_nmd", "crude_rate_per_100,000_nmd")

for(i in 1:length(file_base_names)){
  
  for(j in 1:length(geo_name)){
    
    data <- read.csv(paste0("./data/NMD/",file_base_names[i], "_", geo_name[j], ".csv" ), header = TRUE, check.names = FALSE)
    
    data$year_range <- unlist(lapply(data$year_range, function(x) gsub("–", "-" , x)))
    
    data$age_group <- unlist(lapply(data$age_group, function(x) gsub("–", "-" , x)))
    
    sup_index1 <- which(data[, var_col[i]] > 0 & data[, var_col[i]] < 5)
    
    if(length(sup_index1) > 0){
      
      data[sup_index1, var_col[i]] <- 9999999
      data[sup_index1, rate_col[i]] <- 9999999
    }
    
    na_index <-which(is.na(data[,1]) == TRUE)
    
    if(length(na_index) > 0){
      
      data <- data[-na_index,]
    }
    
    data$year_range <- substr(data$year_range,6, nchar(data$year_range))
    
    
    if(geo_name[j] == "SA2"){
      
      if(var_col[i] == "total_deaths_nmd"){
        
        new_var <- "mortality_10_year_rolling_sum"
      }else{
        
        new_var <- "births_10_year_rolling_sum"
      }
      
    }else{
      
      if(var_col[i] == "total_deaths_nmd"){
        
        new_var <- "mortality_4_year_rolling_sum"
      }else{
        
        new_var <- "births_4_year_rolling_sum"
      }
    }
    
    names(data) [which(names(data) == var_col[i])] <- new_var
    names(data) [which(names(data) == "year_range")] <- "calendar_year"
    write.csv(data, paste0("./output/NMD/",file_base_names[i], "_", geo_name[j], ".csv" ), row.names = FALSE)
    
  }
}
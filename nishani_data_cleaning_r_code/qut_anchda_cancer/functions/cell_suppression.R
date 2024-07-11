

cell_suppression <- function(count_file, rate_file, count_indicator_name, rate_indicator_name, name_uncertiniy_col, num_filetrs, b_name){
  
  
  #reading files
  count_data <- read.csv(count_file, header = TRUE, check.names = FALSE)
  
  rate_data <- read.csv(rate_file, header = TRUE, check.names = FALSE)
  
  #get the name of SA code column
  SA_CODE <- names(count_data)[1]
  
  #filter names
  filter_names <- names(count_data)[2:(2 + num_filetrs -1)]
  
  #combined count and rate data
  combined_data <- left_join(count_data, rate_data, by = c(SA_CODE, filter_names ))
  
  #cell value > 0 & <5
  sup_index1 <- which(combined_data[,count_indicator_name] > 0 & combined_data[,count_indicator_name] < 5)
  
  #cel value with por for uncertainty correspondence
  #sup_index2 <- which(combined_data[,name_uncertiniy_col] == "Poor")
  
  if(length(sup_index1) > 0){
    
    #sup_index <- unique(c(sup_index1, sup_index2))
    
    combined_data[sup_index1, c(count_indicator_name, rate_indicator_name)] <- 9999999
    
  }
  
  # NULL_index <- which(combined_data[, rate_indicator_name]> 1 & combined_data[, rate_indicator_name] != 9999999)
  # 
  # if(length(NULL_index) > 0){
  #   
  #   combined_data[NULL_index, c(count_indicator_name, rate_indicator_name)] <- "NA"
  #   
  # }
  
  write.csv(combined_data[, c(SA_CODE,filter_names, count_indicator_name, rate_indicator_name)], paste0("./output/cell_suppresed/",b_name,substr(SA_CODE, 1,3) , ".csv") , row.names = FALSE)
  
  write.csv(combined_data[, c(SA_CODE,filter_names, count_indicator_name)], paste0("./output/cell_suppresed/", basename(count_file)) , row.names = FALSE)
  
  write.csv(combined_data[, c(SA_CODE,filter_names, rate_indicator_name)], paste0("./output/cell_suppresed/", basename(rate_file)) , row.names = FALSE)
  
}



file_names <- list.files(path = "./output/", pattern = "*.csv", full.names = TRUE)


for(i in 1:length(file_names)){

  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  n_names <- names(data)[which(startsWith( names(data), "n_" ) == TRUE)]
  p_names <- names(data)[which(startsWith( names(data), "p_"  ) == TRUE)]
  
  
  if(length(n_names) > 0){
    
    
    sup_index1 <- which(data[, n_names] > 0 & data[, n_names] < 5)
    
    if(length(sup_index1) > 0){
      
      data[sup_index1, n_names] <- 9999999
      
      data[sup_index1, p_names] <- 9999999
      
      
    }
    
  }
  
  
  
  
  write.csv(data, paste0("./output/CELL_SUPPRESSED/",basename(file_names[i])), row.names = FALSE)
  
  
}
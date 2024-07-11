
file_names <- list.files(path = "./data/ASSAD/", full.names = TRUE)

for(i in 1:length(file_names)){
  
  
    
    data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
    
    n_names <- names(data)[which(grepl("n_",  names(data)) == TRUE)]
    
    for(j in 1:length(n_names)){
      
      sup_index1 <- which(data[, n_names[j]] > 0 & data[, n_names[j]] < 5)
      
      if(length(sup_index1) > 0){
        
        data[sup_index1, n_names[j]] <- 9999999
        
        if(names(data) %in% paste0("p", substr(n_names[j], 2, nchar(n_names[j])))){
          
          data[sup_index1, paste0("p", substr(n_names[j], 2, nchar(n_names[j])))] <- 9999999
          
        }else{
          
          print("no rate name")
          
        }

      }
      
    }
    
    
    write.csv(data, paste0("./output/ASSAD/",basename(file_names[i])), row.names = FALSE)
  
  
}
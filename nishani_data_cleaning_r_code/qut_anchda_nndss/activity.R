
file_names <- list.files(path = "./data/", pattern = ".csv", full.names = TRUE)

for(i in 1:length(file_names)){
  
  
  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  if(length(which(data$sex == "NULL")) > 0){
    
    data <- data[-which(data$sex == "NULL"),]
  }
 
  
  write.csv(data, paste0("./data/remove_null/",basename(file_names[i]) ), row.names = FALSE)
}

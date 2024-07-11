

file_names <- list.files(path = "./data/", pattern = "*.csv", full.names = TRUE)

for(i in 1:length(file_names)){
  
  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  new_data <- data[-which(data$age_group == 9999999),]
  
  data$age_group[which(data$age_group == 9999999)] <- unique(new_data$age_group)
  
  names(data)[1] <- paste0(substr(names(data)[1], 1,nchar(names(data)[1]) - 2),"21")

  print(paste0(file_names[i],"-" ,length(unique(data$age_group))))
  
 write.csv(data, paste0("./output/", basename(file_names[i])), row.names = FALSE)
}
file_names <- list.files(path = "./data/acara_attendance_level_473_TC", pattern = "*.csv", full.names = TRUE)

var_name <- "attendance_level"

un_col <- "attendance_level_uncertainty_correspondence"

for(i in 1:length(file_names)){
  
  data <- read.csv(file_names[i], header = TRUE, check.names = FALSE)
  
  sup_index <- which(data[,un_col] == "Poor")
  
  if(length(sup_index) > 0){
    
     data[sup_index, var_name] <- 9999999
  }
  
  data <- data[, -which(names(data) == un_col)]
  
  write.csv(data, paste0("./data/acara_attendance_level_473_TC/cell_suppressed/", basename(file_names[i])), row.names = FALSE)
  
}
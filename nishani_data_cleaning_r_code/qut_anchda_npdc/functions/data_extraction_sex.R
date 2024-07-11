

data_extraction_sex <- function(data, colnumbers, n_name, p_name, file_name_base){
  
  #subset data for required columns
  sub_data <- data[, colnumbers]
  
  #rename col names
  names(sub_data)<- c("SA3_CODE16", "year_range", "sex", "n", "p")
  
  #values contains in sex col to  lowercase
  sub_data$sex <- tolower(sub_data$sex)
  
  #extract data for male and female
  new_data <- sub_data[which(sub_data$sex == "male" |sub_data$sex == "female"), ]
  
  #make sure we use the right hypne
  new_data$year_range[1:nrow(new_data)] <- unlist(lapply(new_data$year_range[1:nrow(new_data)], function(x)gsub("–", "-", x)))
  
  #n.p value conver in to NULL
  new_data[,4:5]<- lapply( new_data[,4:5], function(x)gsub("n.p.", "NA", x))
  
  new_data <- as.data.frame(new_data)
  
  new_data$n <- as.numeric(new_data$n)
  
  new_data$p<- as.numeric(new_data$p)
  
  #percentage values convert in to proportion
  #new_data$p <- unlist(rounding_fun(new_data$p/100))
  new_data$p <- round((new_data$p/100), 4)
  
  
  #if number is 0, proportion is also 0
  new_data$p <- ifelse( new_data$n == 0, 0,  new_data$p)
  
  #creating age group
  new_data$age_group <- "0-0"
  
  new_data$SA4_CODE16 <- substr(new_data$SA3_CODE16, 1, 3)
  new_data$STE_CODE16 <- substr(new_data$SA3_CODE16, 1, 1)
  
  #data for number - SA3
  SA3_new_data_number <- new_data[,  c("SA3_CODE16", "year_range","age_group" ,"sex", "n")]
  names(SA3_new_data_number)[5]<- n_name
  
  #data for proportion - SA3
  SA3_new_data_p <- new_data[,  c("SA3_CODE16", "year_range","age_group" ,"sex", "p")]
  names(SA3_new_data_p)[5]<- p_name
  
  #data for number - SA4
  SA4_new_data_number <- new_data[,  c("SA4_CODE16", "year_range","age_group" ,"sex", "n")]
  SA4_new_data_number <- SA4_new_data_number %>% group_by(SA4_CODE16, year_range, age_group, sex) %>% summarise(n = sum(n, na.rm = TRUE))
  names(SA4_new_data_number)[5]<- n_name
  
  #data for proportion - SA4
  SA4_new_data_p <- new_data[,  c("SA4_CODE16", "year_range","age_group" ,"sex", "p")]
  SA4_new_data_p <- SA4_new_data_p %>% group_by(SA4_CODE16, year_range, age_group, sex) %>% summarise(p = mean(p, na.rm = TRUE))
  SA4_new_data_p$p <- round((SA4_new_data_p$p), 4)
  names(SA4_new_data_p)[5]<- p_name
  
  #data for number - STE
  STE_new_data_number <- new_data[,  c("STE_CODE16", "year_range","age_group" ,"sex", "n")]
  STE_new_data_number <- STE_new_data_number %>% group_by(STE_CODE16, year_range, age_group, sex) %>% summarise(n = sum(n, na.rm = TRUE))
  names(STE_new_data_number)[5]<- n_name
  
  #data for proportion - STE
  STE_new_data_p <- new_data[,  c("STE_CODE16", "year_range","age_group" ,"sex", "p")]
  STE_new_data_p <- STE_new_data_p %>% group_by(STE_CODE16, year_range, age_group, sex) %>% summarise(p = mean(p, na.rm = TRUE))
  STE_new_data_p$p <- round((STE_new_data_p$p),4)
  names(STE_new_data_p)[5]<- p_name
  
  write.csv(SA3_new_data_number, paste0("./output/", file_name_base, n_name,  "_SA3.csv"), row.names = FALSE)
  write.csv(SA3_new_data_p, paste0("./output/", file_name_base, p_name,  "_SA3.csv"), row.names = FALSE)
  
  write.csv(SA4_new_data_number, paste0("./output/", file_name_base, n_name,  "_SA4.csv"), row.names = FALSE)
  write.csv(SA4_new_data_p, paste0("./output/", file_name_base, p_name,  "_SA4.csv"), row.names = FALSE)
  
  write.csv(STE_new_data_number, paste0("./output/", file_name_base, n_name,  "_STE.csv"), row.names = FALSE)
  write.csv(STE_new_data_p, paste0("./output/", file_name_base, p_name,  "_STE.csv"), row.names = FALSE)
  
}
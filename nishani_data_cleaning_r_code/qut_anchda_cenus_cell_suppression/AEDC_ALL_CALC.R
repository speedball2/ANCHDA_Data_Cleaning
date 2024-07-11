#creating all for gender

path_in <- "./output/data_4_digits/"
path_out <- "./output/data_4_digits/all/"

fn <- list.files(path_in, pattern = ".csv", full.names = TRUE)

for(i in 1:length(fn)){
  
  
  data <- read.csv(fn[i], header = TRUE, check.names = FALSE)


  data$filter <- paste0(data[,1],"_", data$calendar_year,"_", data$age_group, "_")
  # all_data <- data %>% group_by(code, calendar_year, age_group)  %>%  summarise( n_dot_lcs = sum(n_dot_lcs, na.rm = TRUE), p_dot_lcs =  sum(p_dot_lcs, na.rm = TRUE), n_dv_lcs= sum(n_dv_lcs, na.rm = TRUE), p_dv_lcs = sum(p_dv_lcs, na.rm = TRUE), n_dar_lcs = sum(n_dar_lcs, na.rm = TRUE), p_dar_lcs = sum(p_dar_lcs, na.rm = TRUE), lcs_valid = sum(lcs_valid, na.rm = TRUE)) %>% mutate( sex = "all")
  # 
  # all_data$p_dot_lcs <- round(all$n_dot_lcs / all_data$lcs_valid,4)
  # all_data$p_dar_lcs <- all_data$p_dar_lcs / all_data$lcs_valid
  # all_data$p_dv_lcs <- all_data$n_dv_lcs / all_data$lcs_valid
  # 
  # names(data)[1] <- scode
  # names(all_data)[which(names(all_data) == "code")] <- scode
  # 
  # all_data <- all_data[, names(data)]
  
  #write.csv(rbind(data, all_data), paste0(path_out, basename(fn[i]), ".csv"), row.names = FALSE)
  
  
}
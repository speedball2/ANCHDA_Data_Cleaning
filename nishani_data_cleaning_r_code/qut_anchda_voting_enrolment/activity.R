library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")

geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

data_enrolment <- read_files("./data/ANCHDA Enrolment Request_v2.xlsx", "18-24 yo Enrolments 2008-2022", "A3:CL55432" )  

colnames(data_enrolment) <- rep(c("calendar_year", "SA1_CODE", "female", "male", "total", "free"), 15)

data_full <- NULL

#covert data frame in to correct format
for(i in 1:15){
  
print(i)
sub_data <-   data_enrolment[, ((6 * (i-1)) + 1) : (6 * i)]
sub_data <- sub_data[!rowSums(is.na(sub_data[ , 1:6])) == 6, ]

data_full <- rbind(data_full, sub_data)
  
}

#remove the empty column
data_full <- data_full[ , -6]

###################################################
#covert 7 digit SA1 code 2 SA2_CODE2016, SA3_CODE2016, SA4_CODE2016, STE_CODE2016
####################################################
#check any SA1 code more than 7 didgit

data_full[which(nchar(data_full$SA1_CODE) != 7),]

#there are two observation with SA1 code 0. Removing those two observations as SA1 code are invalid

if(length(which(nchar(data_full$SA1_CODE) != 7)) > 0){
  
  data_full <- data_full[-which(nchar(data_full$SA1_CODE) != 7),]
}

data_full$SA2_5DIGITCODE_2016 <- substr(data_full$SA1_CODE, 1,5)

geom_data$SA2_5DIGITCODE_2016 <- as.character(geom_data$SA2_5DIGITCODE_2016)

joint_data <- left_join(data_full, geom_data, by = "SA2_5DIGITCODE_2016")



write.csv(joint_data, "full_joint_data.csv", row.names = FALSE)


write.csv(joint_data[which(is.na(joint_data$SA2_MAINCODE_2016) == TRUE),], "no_maching_code_joint_data.csv", row.names = FALSE)

#-------------------------------------

new_data <- joint_data[-which(is.na(joint_data$SA2_MAINCODE_2016) == TRUE),]

new_data <- new_data[, c("calendar_year", 'female', "male","total" ,"SA2_MAINCODE_2016", "SA3_CODE_2016", "SA4_CODE_2016", "STATE_CODE_2016")]


----------------------------------------
  
extract_SA_data <- function(new_data, code_name,new_code_name , file_name1, file_name2){
  
  new_data_SA2 <- new_data[, c("calendar_year", 'female', "male", "total", code_name)]
  
  new_data_SA2 <- melt(new_data_SA2, id.vars = c("calendar_year", code_name),  value.name = "number_of_enrolment")
  
  new_data_SA2$number_of_enrolment <- as.numeric(new_data_SA2$number_of_enrolment)
  
  summary_data_SA2 <- new_data_SA2 %>% group_by(eval(parse(text = code_name)), variable,  calendar_year) %>%   summarise(number_of_enrolment = sum(number_of_enrolment)) %>% 
    mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function
  
  names(summary_data_SA2) <- c(new_code_name, "sex", "calendar_year", "number_of_enrolment")
  
  summary_data_SA2$age_group <- "18-24"
  
  
  summary_data_SA2_number <- summary_data_SA2[which(summary_data_SA2$sex != "total"),c(new_code_name, "sex", "age_group", "calendar_year", "number_of_enrolment")]
  
  write.csv(summary_data_SA2_number, file_name1, row.names = FALSE)
  
  #-----------------------------------
  
  dcast_data <- pivot_wider(summary_data_SA2, names_from = sex, values_from = number_of_enrolment)
  
  dcast_data$female  <- ifelse(dcast_data$female == 0, 0, dcast_data$female / dcast_data$total)
  
  dcast_data$male  <- ifelse(dcast_data$male == 0, 0, dcast_data$male / dcast_data$total)
  
  
  #dcast_data$male <-   dcast_data$male / dcast_data$total
  
  dcast_data <- dcast_data[ , -which(names(dcast_data) == "total")]
  
  long_dcast_data <- melt(dcast_data, id.vars = c("calendar_year", new_code_name, "age_group"),  value.name = "rate_of_enrolment")
  
  names(long_dcast_data)[which(names(long_dcast_data) == "variable")] <- "sex" 
  
  long_dcast_data <- long_dcast_data[,c(new_code_name, "sex", "age_group", "calendar_year", "rate_of_enrolment")]
  
  long_dcast_data$rate_of_enrolment <- as.numeric(long_dcast_data$rate_of_enrolment)
  
  long_dcast_data <- long_dcast_data %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function
  
  #reshape(summary_data_SA2, direction = "wide", idvar = c(new_code_name, "age_group", "calendar_year"), timevar = "number_of_enrolment", v.names = "sex")

  write.csv(long_dcast_data, file_name2, row.names = FALSE)
}


#------------------

extract_SA_data(new_data, "SA2_MAINCODE_2016", "SA2_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA2.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA2.csv")
extract_SA_data(new_data, "SA3_CODE_2016", "SA3_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA3.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA3.csv")
extract_SA_data(new_data, "SA4_CODE_2016", "SA4_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA4.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA4.csv")
extract_SA_data(new_data, "STATE_CODE_2016", "STE_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_STE.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_STE.csv")
#------------------------


data1 <- read.csv("./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_STE.csv",  header = TRUE, check.names = FALSE)

data2 <- read.csv("./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_STE.csv", header = TRUE, check.names = FALSE)


data <- left_join(data1, data2, by = c("STE_CODE16","sex" , "age_group" , "calendar_year"  ))

write.csv(data, "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_rate_of_enrolled_to_vote_STE.csv", row.names = FALSE)
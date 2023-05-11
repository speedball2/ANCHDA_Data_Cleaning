
#---------------------------
               
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------
               
source("./functions/read_files.R")
 

table <- c("Table 3.1", "Table 3.2", "Table 3.3", "Table 3.4", "Table 3.5", "Table 3.6", "Table 3.7")
data_range <- c("L8:M150", "P8:Q150", "T8:U150", "X8:Y150","AB8:AC150","AF8:AG150","AJ8:AK150","AN8:AO150", "AR8:AS150")
year <- 2013:2021

data_3_1_full <- NULL

for(j in 1:length(table)){
  for(i in 1:length(data_range)){
    
    LGA_CODE21 <- read_files("./data/births_LGA_2011_2021.xlsx", table[j] , "A8:A150")    
    names(LGA_CODE21) <- "LGA_CODE21"
    data_3_1  <- read_files("./data/births_LGA_2011_2021.xlsx", table[j] , data_range[i])
    names(data_3_1) <- c("n_births","fertility_rate")
    data_3_1$calendar_year <- year[i]
    data_3_1$sex <- "all"
    data_3_1$age_group <- "0-0"
    data_3_1 <- cbind(LGA_CODE21,data_3_1)
    
    if(length(which(nchar(data_3_1$LGA_CODE21) < 5)) > 0){
      
      data_3_1 <- data_3_1[-which(nchar(data_3_1$LGA_CODE21) < 5),]
    }
    
    if(length(which(is.na(data_3_1$n_births) == TRUE & is.na(data_3_1$fertility_rate) == TRUE)) > 0){
      
      
      data_3_1 <- data_3_1[-which(is.na(data_3_1$n_births) == TRUE & is.na(data_3_1$fertility_rate) == TRUE  ),]
    }
    data_3_1_full  <- rbind(data_3_1_full,data_3_1)
  }
  
}

#------------------------------

data_3_1_full_birth <- data_3_1_full[, c("LGA_CODE21","calendar_year", "sex", "age_group", "n_births")]

if(length(which(data_3_1_full_birth$n_births == "np"))> 0){
  
  data_3_1_full_birth$n_births[which(data_3_1_full_birth$n_births == "np")] <- NA
}
data_3_1_full_birth$n_births <- as.numeric(data_3_1_full_birth$n_births)

data_3_1_full_birth <- data_3_1_full_birth %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))


#------------------
data_3_1_full_fertility <- data_3_1_full[, c("LGA_CODE21","calendar_year", "sex", "age_group", "fertility_rate")]

if(length(which(data_3_1_full_fertility$fertility_rate == "np"))> 0){
  
  data_3_1_full_fertility$fertility_rate[which(data_3_1_full_fertility$fertility_rate == "np")] <- NA
}

data_3_1_full_fertility$fertility_rate <- as.numeric(data_3_1_full_fertility$fertility_rate)

data_3_1_full_fertility <- data_3_1_full_fertility %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))


write.csv(data_3_1_full_birth, "./output/ABS_Births_1110_n_birth_LGA.csv", row.names = FALSE)

write.csv(data_3_1_full_fertility, "./output/ABS_Births_1114_fertility_rate_LGA.csv", row.names = FALSE)

#--------------------------

combined_data <- left_join(data_3_1_full_birth,data_3_1_full_fertility, by = c("LGA_CODE21", "calendar_year", "sex","age_group")) 
write.csv(combined_data, "./output/ABS_Births_1110_1114_n_birth_fertility_rate_LGA.csv", row.names = FALSE)


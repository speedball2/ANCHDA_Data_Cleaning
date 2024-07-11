
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")

#n birth

table <- c("Table 1.1")
data_range <- c("AB8:AB130", "AN8:AN130", "AZ8:AZ130", "BL8:BL130","BX8:BX130","CJ8:CJ130","CV8:CV130","DH8:DH130", "DT8:DT130")
year <- 2013:2021

data_3_1_full <- NULL

for(j in 1:length(table)){
  for(i in 1:length(data_range)){
    
    SA4_CODE21 <- read_files("./data/births_SA4_2011-2021.xlsx", table[j] , "A8:A130")    
    names(SA4_CODE21) <- "SA4_CODE21"
    data_3_1  <- read_files("./data/births_SA4_2011-2021.xlsx", table[j] , data_range[i])
    names(data_3_1) <- "n_births"
    data_3_1$calendar_year <- year[i]
    data_3_1$sex <- "all"
    data_3_1$age_group <- "0-0"
    data_3_1 <- cbind(SA4_CODE21,data_3_1)
    
    if(length(which(nchar(data_3_1$SA4_CODE21) != 3)) > 0){
      
      data_3_1 <- data_3_1[-which(nchar(data_3_1$SA4_CODE21) != 3),]
    }
    
    if(length(which(is.na(data_3_1$n_births) == TRUE )) > 0){
      
      data_3_1 <- data_3_1[-which(is.na(data_3_1$n_births) == TRUE),]
    }
    
    data_3_1_full  <- rbind(data_3_1_full,data_3_1)
  }
  
}


data_3_1_full_birth <- data_3_1_full[, c("SA4_CODE21","calendar_year", "sex", "age_group", "n_births")]
if(length(which(data_3_1_full_birth$n_births == "np"))> 0){
  
  data_3_1_full_birth$n_births[which(data_3_1_full_birth$n_births == "np")] <- NA
}
data_3_1_full_birth$n_births <- as.numeric(data_3_1_full_birth$n_births)

data_3_1_full_birth <- data_3_1_full_birth %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))


write.csv(data_3_1_full_birth, "./output/ABS_Births_1110_n_birth_SA4.csv", row.names = FALSE)

#----------------------------

#n birth

table <- c("Table 1.1")
data_range <- c("AJ8:AJ130", "AV8:AV130", "BH8:BH130", "BT8:BT130","CF8:CF130","CR8:CR130","DD8:DD130","DP8:DP130", "EB8:EB130")
year <- 2013:2021

data_3_1_full <- NULL

for(j in 1:length(table)){
  for(i in 1:length(data_range)){
    
    SA4_CODE21 <- read_files("./data/births_SA4_2011-2021.xlsx", table[j] , "A8:A130")    
    names(SA4_CODE21) <- "SA4_CODE21"
    data_3_1  <- read_files("./data/births_SA4_2011-2021.xlsx", table[j] , data_range[i])
    names(data_3_1) <- "fertility_rate"
    data_3_1$calendar_year <- year[i]
    data_3_1$sex <- "all"
    data_3_1$age_group <- "0-0"
    data_3_1 <- cbind(SA4_CODE21,data_3_1)
      
    if(length(which(nchar(data_3_1$SA4_CODE21) != 3)) > 0){
      
      data_3_1 <- data_3_1[-which(nchar(data_3_1$SA4_CODE21) != 3),]
    }
    
    if(length(which(is.na(data_3_1$fertility_rate) == TRUE )) > 0){
      
      data_3_1 <- data_3_1[-which(is.na(data_3_1$fertility_rate) == TRUE),]
    }
    
    data_3_1_full  <- rbind(data_3_1_full,data_3_1)
  }
  
}

data_3_1_full_fertility <- data_3_1_full[, c("SA4_CODE21","calendar_year", "sex", "age_group", "fertility_rate")]

if(length(which(data_3_1_full_fertility$fertility_rate == "np"))> 0){
  
  data_3_1_full_fertility$fertility_rate[which(data_3_1_full_fertility$fertility_rate == "np")] <- NA
}

data_3_1_full_fertility$fertility_rate <- as.numeric(data_3_1_full_fertility$fertility_rate)

data_3_1_full_fertility <- data_3_1_full_fertility %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))


write.csv(data_3_1_full_fertility, "./output/ABS_Births_1114_fertility_rate_SA4.csv", row.names = FALSE)

#-------

combined_data <- left_join(data_3_1_full_birth,data_3_1_full_fertility, by = c("SA4_CODE21", "calendar_year", "sex","age_group")) 

write.csv(combined_data, "./output/ABS_Births_1110_1114_n_birth_fertility_rate_SA4.csv", row.names = FALSE)
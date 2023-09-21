library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")
#----------

site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8

setwd("C:\\Users\\Mudki\\OneDrive - Queensland University of Technology\\ANCHDA_QUT_new\\General\\Data_Collections_RAW\\from_custodians\\AIR_SA3\\")


code_data <- read.csv("../../public_data/ASGS2016_SA2_SA3_SA4_code_name_matching_ref_csv/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE )

table_1 <- read_excel("AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.XLSX", "ALL 1yo" , "A2:N3045")

table_2 <- read_excel("AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.xlsx", "ALL 2yo" , "A2:N3041")

table_5 <- read_excel("AIR SDQU - ANCHDA - RMS2671 Data Request Childhood Immunisation.xlsx", "ALL 5yo" , "A2:N3042")

table_new <- rbind(rbind(table_1, table_2), table_5)


table_new <- table_new[,c("Year", "SA3_Code", "SA3_Name", "State", "Age Group","% Fully")]

table_new$State <- unlist(lapply(table_new$State, function (x){
  
  site_cat[which(site == x)]
}))

table_new <- as.data.frame(table_new)

table_new$`% Fully` <- gsub("NP", NA, table_new$`% Fully`)
table_new$`% Fully` <- gsub("≥95.00", 95.00, table_new$`% Fully`)
table_new$`% Fully` <- gsub("≥99.00", 99.00, table_new$`% Fully`)

table_new$sex <- "all"
table_new$age_group <- "0-5"
names(table_new)[which(names(table_new)== "Age Group")] <- "age_fully_immunised_AIR"
names(table_new)[which(names(table_new)== "% Fully")] <- "p"
names(table_new)[which(names(table_new)== "Year")] <- "calendar_year"

table_new$age_fully_immunised_AIR[which(table_new$age_fully_immunised_AIR == "1 Year olds")] <- "1 year"

table_new$age_fully_immunised_AIR[which(table_new$age_fully_immunised_AIR == "2 Year olds")] <- "2 years"

table_new$age_fully_immunised_AIR[which(table_new$age_fully_immunised_AIR == "5 Year olds")] <- "5 years"



#filling missing SA3_code

missing_index <- which(table_new$SA3_Code == "-")

table_new$SA3_Code_data <- NA
table_new$SA3_Code_file <- NA

for(i in missing_index){
  
  sa3_name <- table_new$SA3_Name[i]
  sa3_stae <- table_new$State[i]
  u_val_data <- unique(table_new$SA3_Code[which(table_new$SA3_Name == sa3_name & table_new$State == sa3_stae)])
  u_val_data <- u_val_data[-which(u_val_data == "-")]
  
  
  u_val_code <- unique(code_data$SA3_CODE_2016[which(code_data$SA3_NAME_2016 == sa3_name & code_data$STATE_CODE_2016 == sa3_stae)])
  
  if(length(u_val_data) > 0){
   
    table_new$SA3_Code_data[i] <- paste(u_val_data, collapse = " & ")
  } 
  

  if(length(u_val_data) > 0){
    
    table_new$SA3_Code_file[i] <- paste(u_val_code, collapse = " & ")
  }
  
  
  print(paste(unique(table_new$State[which(table_new$SA3_Name == sa3_name)]), collapse = " & "))
  
}

names(table_new)[which(names(table_new)== "SA3_Code")] <- "SA3_CODE16"

table_new <- table_new[-which(table_new$SA3_CODE16 == "-"),]

final_table <- table_new[,c("SA3_CODE16", "sex", "age_group", "calendar_year", "age_fully_immunised_AIR", "p")]

final_table$p <- as.numeric(final_table$p)/100

final_table <- final_table %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))

write.csv(final_table, "./output/AIR_121_fully_immunised_single_year_SA3.csv", row.names = FALSE)




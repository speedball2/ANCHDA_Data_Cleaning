library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")
source("./functions/output_table_creation_household_income.R")


#------------

tables <- c("Table 1", "Table 2","Table 3","Table 4")
calender_year <- c("2021", "2016","2011","2006")
last_cell <- c("2471", "2309", "2303", "2301")
main_output_table <- NULL

for(i in 1:length(tables)){
  
  #reading tables for 2021
  table_side_code <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("A8:A", last_cell[i], sep = ""))
  table_input_n <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("B8:F", last_cell[i], sep = ""))
  table_input_total <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("T8:X", last_cell[i], sep = ""))
  
  #extracting the value for n which are less than $1000 income
  table_output_n <- output_table_creation_household_income(cbind(table_side_code,table_input_n), "SA2_CODE16" , "n_live_in_household_income_less_1000", "all", calender_year[i])
  
  #extracting total
  table_output_total <- output_table_creation_household_income(cbind(table_side_code,table_input_total), "SA2_CODE16" , "total", "all", calender_year[i])
  
  #joint the total and n
  joint_table <- left_join(table_output_n, table_output_total, by = c("SA2_CODE16", "sex", "age_group","calendar_year")) 
  
  #calculation of p
  joint_table$p_live_in_household_income_less_1000  <-
    ifelse(joint_table$n_live_in_household_income_less_1000 == 0 & joint_table$total == 0, 0, joint_table$n_live_in_household_income_less_1000/ joint_table$total)
  
  joint_table$p_live_in_household_income_less_1000  <-
    ifelse(joint_table$n_live_in_household_income_less_1000 > joint_table$total, NA, joint_table$p_live_in_household_income_less_1000)
  
  #remove the total column & rounding to two decimal places
  joint_table <- joint_table %>% select(-contains("total")) %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) 
  
  main_output_table <- rbind(main_output_table,joint_table)
  
  
}

write.csv(main_output_table, "./output/ABS_Labour_Force_Survey_281_households_earning_less_than_1000_SA2.csv", row.names = FALSE)



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
  table_input_less_1000 <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("B8:F", last_cell[i], sep = ""))
  table_input_more_1000 <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("H8:L", last_cell[i], sep = ""))
  table_input_not_stated <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("N8:R", last_cell[i], sep = ""))
  table_input_total <- read_files("./data/Census_Population_by_Household_Income.xlsx", tables[i] , paste("T8:X", last_cell[i], sep = ""))
  
  #This function creates the required output table for household income
  table_output_n_less_1000 <- output_table_creation_household_income(cbind(table_side_code,table_input_less_1000), "SA2_CODE16" , "n_live_in_household_income_less_1000", "all", calender_year[i])
  
  table_output_n_more_1000 <- output_table_creation_household_income(cbind(table_side_code,table_input_more_1000), "SA2_CODE16" , "n_live_in_household_income_more_1000", "all", calender_year[i])
  
  table_output_n_not_stated <- output_table_creation_household_income(cbind(table_side_code,table_input_not_stated), "SA2_CODE16" , "n_live_in_household_income_not_stated", "all", calender_year[i])
  
  table_output_total <- output_table_creation_household_income(cbind(table_side_code,table_input_total), "SA2_CODE16" , "total", "all", calender_year[i])
  
  joint_table1 <- left_join(table_output_n_less_1000, table_output_n_more_1000, by = c("SA2_CODE16", "sex", "age_group","calendar_year")) 
  
  joint_table2 <- left_join(joint_table1, table_output_n_not_stated, by = c("SA2_CODE16", "sex", "age_group","calendar_year")) 
  
  joint_table3 <- left_join(joint_table2, table_output_total, by = c("SA2_CODE16", "sex", "age_group","calendar_year")) 
  
  joint_table3$total_nishani  <- joint_table3$n_live_in_household_income_less_1000 + joint_table3$n_live_in_household_income_more_1000 + joint_table3$n_live_in_household_income_not_stated
    
  #joint_table <- joint_table %>% select(-contains("total")) %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) 
  
  main_output_table <- rbind(main_output_table,joint_table3)
  
  
}

write.csv(main_output_table, "./output/test_total.csv", row.names = FALSE)

eq <- length(which(main_output_table$total == main_output_table$total_nishani ))

less <- length(which(main_output_table$total < main_output_table$total_nishani ))

greater <- length(which(main_output_table$total > main_output_table$total_nishani ))

df <- as.data.frame(cbind(c("total == total_nishani" , "total < total_nishani", "total > total_nishani"), c(eq, less, greater)))

names(df) <- c("cat", "value")

pie(as.numeric(df$value), label = df$cat)

length(which(main_output_table$total < main_output_table$n_live_in_household_income_less_1000))

main_output_table[which(main_output_table$total < main_output_table$n_live_in_household_income_less_1000),]


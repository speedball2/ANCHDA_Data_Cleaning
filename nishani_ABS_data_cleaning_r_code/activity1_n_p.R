

#this allows user to get the output files for count - bmi indicators 
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-------------------------------
source("./functions/adding_stars_to_count.R")
source("./functions/finding_cell_with_one_and_two_stars.R")
source("./functions/read_files.R")
source("./functions/output_table_creation.R")
source("./functions/main_function.R")
#-----------------------------
sex <- c("males", "females")
site_code <- c("do020", "do021", "do022", "do023", "do024", "do025", "do026", "do027")
site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8
#-----------------------------

#getting name of the xls files in the data folder
filenames <- list.files("./data", pattern="*.xlsx", full.names=TRUE)
#-----------------------------------
#defining the bmi_category for child
bmi_categories_child <- c("underweight","normal", "overweight", "obese")
#defining the bmi_category for young
bmi_categories_young <- c("underweight (less than 18.50)","normal (18.50–24.99)", "overweight (25.00–29.99)", "total obese (30.00 or more)")
#writing 16.1 output tables
table_16_1 <- main_function(bmi_categories_child, "bmi_categories", filenames, "Table 16.1", "A7:F37", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "NO", sex)

write.csv(as.data.frame(table_16_1), "./output/ABS_NHS_131_n_children_0_to_17_by_bmi_STE.csv", row.names = FALSE )

#writing 16.3 output tables
table_16_3 <- main_function(bmi_categories_child, "bmi_categories",filenames, "Table 16.3", "A7:F37", 7, 1, "\"#\"#,##0.0" ,  NULL, 1, "p", "NO", sex)
write.csv(as.data.frame(table_16_3), "./output/ABS_NHS_131_p_children_0_to_17_by_bmi_STE.csv", row.names = FALSE )

#writing 8.1 output table
table_8_1 <- main_function(bmi_categories_young, "bmi_categories",filenames, "Table 8.1", "A7:B55", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", sex)
write.csv(as.data.frame(table_8_1),"./output/ABS_NHS_131_n_young_people_18_to_24_by_bmi_STE.csv", row.names = FALSE )


#writing 8.3 output tables
table_8_3 <- main_function(bmi_categories_young,"bmi_categories", filenames, "Table 8.3", "A7:B55", 7, 1, "\"#\"#,##0.0" ,  NULL, 1, "p", "YES", sex)
write.csv(as.data.frame(table_8_3),"./output/ABS_NHS_131_p_young_people_18_to_24_by_bmi_STE.csv", row.names = FALSE )



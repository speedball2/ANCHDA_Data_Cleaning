

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
sex <- c("persons", "males", "females")
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
table_16_1$n <- as.numeric(table_16_1$n)
#table_16_1 <- table_16_1 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))



#writing 16.3 output tables
table_16_3 <- main_function(bmi_categories_child, "bmi_categories",filenames, "Table 16.3", "A7:F37", 7, 1, "\"#\"#,##0.0" ,  NULL, 1, "p", "NO", sex)
#table_16_3$p <- as.numeric(table_16_3$p)/100
#table_16_3 <- table_16_3 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))
#write.csv(as.data.frame(table_16_3), "./output/ABS_NHS_131_p_children_0_to_17_by_bmi_STE.csv", row.names = FALSE )







#writing 8.1 output table
table_8_1 <- main_function(bmi_categories_young, "bmi_categories",filenames, "Table 8.1", "A7:B55", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", sex)
table_8_1$n <- as.numeric(table_8_1$n)
#table_8_1 <- table_8_1 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))

#write.csv(as.data.frame(table_8_1),"./output/ABS_NHS_131_n_young_people_18_to_24_by_bmi_STE.csv", row.names = FALSE )


#writing 8.3 output tables
table_8_3 <- main_function(bmi_categories_young,"bmi_categories", filenames, "Table 8.3", "A7:B55", 7, 1, "\"#\"#,##0.0" ,  NULL, 1, "p", "YES", sex)
table_8_3$p <- as.numeric(table_8_3$p)
#table_8_3 <- table_8_3 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))

#write.csv(as.data.frame(table_8_3),"./output/ABS_NHS_131_p_young_people_18_to_24_by_bmi_STE.csv", row.names = FALSE )

# Assuming both dataframes have the same number of rows
if (nrow(table_16_1) == nrow(table_16_3)) {
  # Add column 'p' from dataframe 'table_16_3' to 'table_16_1'
  table_16 <- cbind(table_16_1, p = table_16_3$p)
  
  # Drop the 'uncertainty' column
  table_16 <- table_16[, !(colnames(table_16) %in% "uncertainty")]
} else {
  print("Dataframes have different numbers of rows, cannot merge.")
}

# Assuming both dataframes have the same number of rows
if (nrow(table_8_1) == nrow(table_8_3)) {
  # Add column 'p' from dataframe 'table_16_3' to 'table_16_1'
  table_8 <- cbind(table_8_1, p = table_8_3$p)
  
  # Drop the 'uncertainty' column
  table_8 <- table_8[, !(colnames(table_8) %in% "uncertainty")]
} else {
  print("Dataframes have different numbers of rows, cannot merge.")
}

# Rename 'person' to 'all' in the 'sex' column of table_16
table_16$sex <- ifelse(table_16$sex == "person", "all", table_16$sex)

# Rename 'person' to 'all' in the 'sex' column of table_8
table_8$sex <- ifelse(table_8$sex == "person", "all", table_8$sex)


# Replace n=0 with NULL in table_16
table_16$n <- ifelse(table_16$n == 0, "NULL", table_16$n)

# Replace p=0 with NULL in table_16
table_16$p <- ifelse(table_16$p == 0, "NULL", table_16$p)

# Replace n=0 with NULL in table_8
table_8$n <- ifelse(table_8$n == 0, "NULL", table_8$n)

# Replace p=0 with NULL in table_8
table_8$p <- ifelse(table_8$p == 0, "NULL", table_8$p)



write.csv(as.data.frame(table_8), "./output/ABS_NHS_131_young_people_18_to_24_by_bmi_STE.csv", row.names = FALSE )
write.csv(as.data.frame(table_16), "./output/ABS_NHS_131_children_0_to_17_by_bmi_STE.csv", row.names = FALSE )

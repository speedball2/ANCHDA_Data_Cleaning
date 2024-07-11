

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
#############################################
#nutrition 

nutrition_categories_children <- c("met recommendation(c)", "did not meet recommendation")
indicator_name = "daily_consumption_fruit"
table_13_1 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F31", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F98", 7, 86, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F165", 7, 153, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
# Replace p=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_133_nutrition_n_children_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#--------------------------------

indicator_name = "daily_consumption_vegetables"
table_13_1 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F36", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F103", 7, 91, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F170", 7, 158, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
# Replace p=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_133_nutrition_n_children_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#---------------------

nutrition_categories_children <- c("did not consume", "1 metric cup or less","more than 1 to less than 2 cups", "2 metric cups or more")

indicator_name = "usual_consumption_selected_sugar_sweetened_diet_drinks"
table_13_1 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F55", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F115", 7, 101, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 17.1", "A7:F182", 7, 168, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")


table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
# Replace p=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_133_nutrition_n_children_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

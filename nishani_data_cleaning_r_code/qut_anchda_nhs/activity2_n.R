

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
##################################################################################
#physical activity

physical_categories_young <- c("met guidelines", "did not meet guidelines")

###################################
indicator_name <- "2014_pa_guidelines_any"
table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------------------------------

indicator_name <- "2014_pa_guidelines_exercise_only"
table_13_1 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.1", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)


################################################


indicator_name <-"days_pa_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more")


table_13_1 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.1", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#-------------------------------

indicator_name <-"days_exercise_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more")


table_13_1 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.1", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------


indicator_name <- "min_PA_last_week"
physical_categories_young <- c("0 minutes", "between 1 and 149 minutes","between 150 and 300 minutes", "between 150 and 300 minutes", "more than 300 minutes", "total 150 minutes or more")


table_13_1 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.1", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------------------------


indicator_name <- "min_exercise_last_week"
physical_categories_young <- c("0 minutes", "between 1 and 149 minutes","between 150 and 300 minutes", "between 150 and 300 minutes", "more than 300 minutes", "total 150 minutes or more")


table_13_1 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.1", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.5", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, tolower(indicator_name), filenames, "Table 13.9", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_1, table_13_5, table_13_9)
table_13_1_5_9 <- subset(table_13_1_5_9, select = -uncertainty)
table_13_1_5_9$sex <- ifelse(table_13_1_5_9$sex == "person", "all", table_13_1_5_9$sex)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]
# Replace n=0 with NULL 
table_13_1_5_9$n  <- ifelse(table_13_1_5_9$n == 0, "NULL", table_13_1_5_9$n)
write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#-------------------------------------

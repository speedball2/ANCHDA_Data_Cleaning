

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
##################################################################################
#physical activity

physical_categories_young <- c("met guidelines", "did not meet guidelines","total(c)")

###################################
indicator_name <- "2014_PA_guidelines_any"
#table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------------------------------

indicator_name <- "2014_PA_guidelines_exercise_only"
#table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C18", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind( table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)


################################################

#	

indicator_name <- "min_PA_last_week"
physical_categories_young <- c("0 minutes", "between 1 and 149 minutes","between 150 and 300 minutes", "between 150 and 300 minutes", "more than 300 minutes", "total 150 minutes or more", "total(e)")


#table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C26", 7, 12, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------------------------


indicator_name <- "min_exercise_last_week"
physical_categories_young <- c("0 minutes", "between 1 and 149 minutes","between 150 and 300 minutes", "between 150 and 300 minutes", "more than 300 minutes", "total 150 minutes or more", "total(e)")


#table_13_1 <- main_function(physical_categories_young, "min_exercise_last_week", filenames, "Table 13.1", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C34", 7, 20, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind( table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#-------------------------------------

#	Indicator name = PA_at_work

physical_categories_young <- c("mostly sitting", "mostly standing","mostly walking", "mostly heavy labour or physically demanding work", "total persons in the workplace in the last week(e)")


table_13_1 <- main_function(physical_categories_young, "PA_at_work", filenames, "Table 13.1", "A7:C41", 7, 28, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, "PA_at_work", filenames, "Table 13.5", "A7:C41", 7, 28, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, "PA_at_work", filenames, "Table 13.9", "A7:C41", 7, 28, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(rbind(table_13_1, table_13_5), table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, "./output/ABS_NHS_n_young_people_18_to_24_PA_level_PA_at_work_STE.csv", row.names = FALSE)

#------------------------


indicator_name <-"days_PA_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


#table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C49", 7, 35, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind( table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#-------------------------------

indicator_name <-"days_exercise_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


#table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C57", 7, 43, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind( table_13_5, table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_132_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------



indicator_name <-"days_PA_min_30_min_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C65", 7, 51, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C65", 7, 51, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C65", 7, 51, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(rbind(table_13_1, table_13_5), table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#--------------------------------



indicator_name <-"days_exercise_min_30_min_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C73", 7, 59, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C73", 7, 59, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C73", 7, 59, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(rbind(table_13_1, table_13_5), table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#---------------------------------


indicator_name <-"n_days_PA_min_60_min_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C81", 7, 67, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C81", 7, 67, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C81", 7, 67, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(rbind(table_13_1, table_13_5), table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#------------------------


indicator_name <-"n_days_exercise_min_60_min_last_week"

physical_categories_young <- c("none", "1–4","5–6", "7", "5 or more", "total(e)")


table_13_1 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.1", "A7:C89", 7, 75, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.5", "A7:C89", 7, 75, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")
table_13_9 <- main_function(physical_categories_young, indicator_name, filenames, "Table 13.9", "A7:C89", 7, 75, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

#write.csv

table_13_1_5_9 <- rbind(rbind(table_13_1, table_13_5), table_13_9)
table_13_1_5_9 <- table_13_1_5_9[table_13_1_5_9$age_group == "18-24",]

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_n_young_people_18_to_24_PA_level_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

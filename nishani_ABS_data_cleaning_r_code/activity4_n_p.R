
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
#############################################
#psychological  

psychological_categories_young_people <- c("low distress level", "moderate distress level","high/very high distress level", "total(b)")
indicator_name = "psychological_distress"
#table_13_1 <- main_function(nutrition_categories_children, indicator_name, filenames, "Table 13.1", "A7:C13", 7, 1, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "persons")
table_13_5 <- main_function(psychological_categories_young_people, indicator_name, filenames, "Table 7.1", "A7:B19", 7, 7, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "males")

table_13_9 <- main_function(psychological_categories_young_people, indicator_name, filenames, "Table 7.1", "A7:B25", 7, 13, "\"*\"#,##0.0",  "\"**\"#,##0.0", 1000, "n", "YES", "females")

table_13_1_5_9 <- rbind(table_13_5, table_13_9)

names(table_13_1_5_9)[5] <- "psychological_distress_level"

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_151_n_young_people_18_to_24_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)

#-------------------
#proportion

table_13_5 <- main_function(psychological_categories_young_people, indicator_name, filenames, "Table 7.3", "A7:B19", 7, 7, "\"#\"#,##0.0",  NULL, 1, "p", "YES", "males")

table_13_9 <- main_function(psychological_categories_young_people, indicator_name, filenames, "Table 7.3", "A7:B25", 7, 13, "\"#\"#,##0.0",  NULL, 1, "p", "YES", "females")


table_13_1_5_9 <- rbind(table_13_5, table_13_9)

names(table_13_1_5_9)[5] <- "psychological_distress_level"

write.csv(table_13_1_5_9, paste("./output/ABS_NHS_151_p_young_people_18_to_24_", indicator_name, "_STE.csv", sep = ""), row.names = FALSE)



library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-----------
source("./functions/read_files.R")
source("./functions/rounding_function.R")
source("./functions/data_extraction_sex.R")

#--------------------------------

table_sex <- read_files("./data/NPDC_ANCHDA.xlsx", "Sex" , "A23:Z5399")

#low birth
data_extraction_sex(table_sex, c(1,3,4,5,6), "n_lowbirthweight_under_2500g", "p_lowbirthweight_under_2500g","NPDC_111_")

#premature
data_extraction_sex(table_sex, c(1,3,4,7,8), "n_pre_term_birth_less_37_weeks", "p_pre_term_birth_less_37_weeks","NPDC_117_")

#smoking during pregnancy
data_extraction_sex(table_sex, c(1,3,4,10,11), "n_smoking_during_pregnancy", "p_smoking_during_pregnancy","NPDC_114_")

#1st_antenatal_visit
data_extraction_sex(table_sex, c(1,3,4,19,20), "n_1st_antenatal_visit_less_14_weeks", "p_1st_antenatal_visit_less_14_weeks","NPDC_115_")

data_extraction_sex(table_sex, c(1,3,4,21,22), "n_1st_antenatal_visit_14_to_19_weeks", "p_1st_antenatal_visit_14_to_19_weeks","NPDC_115_")

data_extraction_sex(table_sex, c(1,3,4,23,24), "n_1st_antenatal_visit_over_20_weeks", "p_1st_antenatal_visit_over_20_weeks","NPDC_115_")


###############################################################

#20–24 - 1112, Under 20 (15-19) - 1111
group_select <- "Under 20"
re_code_group <- "15-19"
file_group <- "15_19"
base_file_name <- paste0("./output/NPDC_1111_")
#-----------------
table_ma_age <- read_files("./data/NPDC_ANCHDA.xlsx", "Maternal age" , "A23:P10775")

table_ma_age <- table_ma_age[ c(1,3,4,ncol(table_ma_age))]

names(table_ma_age) <- c("SA3_CODE16","year_range", "age_group","n_births_mothersage")

#----------




  
table_under19 <- table_ma_age[which(table_ma_age$age_group == group_select | table_ma_age$age_group == "Total"),]

wider_table_under19 <- table_under19 %>% pivot_wider(names_from = age_group, values_from = n_births_mothersage)

names(wider_table_under19) <- c("SA3_CODE16","year_range", "n_births_mothersage", "total")

wider_table_under19$n_births_mothersage <- gsub("n.p.", "NA", wider_table_under19$n_births_mothersage )
wider_table_under19$n_births_mothersage <- as.numeric(wider_table_under19$n_births_mothersage)

wider_table_under19$total <- gsub("n.p.", "NA", wider_table_under19$total )
wider_table_under19$total <- as.numeric(wider_table_under19$total)

wider_table_under19$SA4_CODE16 <- substr(wider_table_under19$SA3_CODE16,1,3 )
wider_table_under19$STE_CODE16 <- substr(wider_table_under19$SA3_CODE16,1,1 )

wider_table_under19$age_group <- re_code_group

wider_table_under19$sex <- "female"

wider_table_under19$year_range[1:nrow(wider_table_under19)] <- unlist(lapply(wider_table_under19$year_range[1:nrow(wider_table_under19)], function(x)gsub("–", "-", x)))


#creating table for SA3

SA3_table <- wider_table_under19[,   c("SA3_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage", "total")]

SA3_table$p_births_mothersage <- ifelse(SA3_table$n_births_mothersage == 0, 0, SA3_table$n_births_mothersage/SA3_table$total)

SA3_table$p_births_mothersage <-  round((SA3_table$p_births_mothersage), 4)

SA3_table <- SA3_table[,  c("SA3_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage","p_births_mothersage" )]


#creating SA4 table

SA4_table <- wider_table_under19[,   c("SA4_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage", "total")]

SA4_table <- SA4_table %>% group_by(SA4_CODE16,year_range, age_group, sex) %>% summarise(n_births_mothersage = sum(n_births_mothersage, na.rm = TRUE), total = sum(total, na.rm = TRUE))


SA4_table$p_births_mothersage <- ifelse(SA4_table$n_births_mothersage == 0, 0, SA4_table$n_births_mothersage/SA4_table$total)

SA4_table$p_births_mothersage <-  round((SA4_table$p_births_mothersage), 4)
  

SA4_table <- SA4_table[,  c("SA4_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage","p_births_mothersage" )]


#creating STE table

STE_table <- wider_table_under19[,   c("STE_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage", "total")]

STE_table <- STE_table %>% group_by(STE_CODE16,year_range, age_group, sex) %>% summarise(n_births_mothersage = sum(n_births_mothersage, na.rm = TRUE), total = sum(total, na.rm = TRUE))


STE_table$p_births_mothersage <- ifelse(STE_table$n_births_mothersage == 0, 0, STE_table$n_births_mothersage/STE_table$total)

STE_table$p_births_mothersage <-  round((STE_table$p_births_mothersage),4)


STE_table <- STE_table[,  c("STE_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage","p_births_mothersage" )]


#--------------------

write.csv(SA3_table[, c("SA3_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage")],paste0(base_file_name, "n_births_mothersage_", file_group, "_years_SA3.csv"), row.names = FALSE)

write.csv(SA3_table[, c("SA3_CODE16", "year_range","age_group" ,"sex", "p_births_mothersage")],paste0(base_file_name, "p_births_mothersage_", file_group, "_years_SA3.csv"), row.names = FALSE)


write.csv(SA4_table[, c("SA4_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage")],paste0(base_file_name, "n_births_mothersage_", file_group, "_years_SA4.csv"), row.names = FALSE)

write.csv(SA4_table[, c("SA4_CODE16", "year_range","age_group" ,"sex", "p_births_mothersage")],paste0(base_file_name, "p_births_mothersage_", file_group, "_years_SA4.csv"), row.names = FALSE)


write.csv(STE_table[, c("STE_CODE16", "year_range","age_group" ,"sex", "n_births_mothersage")],paste0(base_file_name, "n_births_mothersage_", file_group, "_years_STE.csv"), row.names = FALSE)

write.csv(STE_table[, c("STE_CODE16", "year_range","age_group" ,"sex", "p_births_mothersage")],paste0(base_file_name, "p_births_mothersage_", file_group, "_years_STE.csv"), row.names = FALSE)


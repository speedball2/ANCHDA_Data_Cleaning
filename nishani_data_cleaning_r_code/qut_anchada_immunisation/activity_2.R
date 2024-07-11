
#---------------------
                
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyverse)
#-----------
                
source("./functions/read_files.R")
#----------
                
site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8
                
#-----------------------

#Australia
table_1 <- read_files("./data/aihw-mhc-hpf-16-immunisation-datasheet-report-hc42.xlsx", "TAB 1" , "A16:F34")

names(table_1)[which(names(table_1)== "Age group")] <- "age_fully_immunised_AIR"
names(table_1)[which(names(table_1)== "Number fully immunised")] <- "n"
names(table_1)[which(names(table_1)== "Percent fully immunised (%)")] <- "p"
names(table_1)[which(names(table_1)== "Reporting Year")] <- "year_range"
table_1$sex <- "all"
table_1$age_group <- "0-5"
table_1$Australia <- 0

table_1$n <- as.numeric(table_1$n)
table_1$p <- as.numeric(table_1$p)/100
table_1 <- table_1 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))



table_1$year_range[1:length(table_1$year_range)] <- unlist(lapply(table_1$year_range[1:length(table_1$year_range)], function(x){
  
   val <- strsplit(x,split = "–")

   return(paste(val[[1]][1],paste("20",val[[1]][2], sep = ""), sep = "-"))
  
}))

test <- str_split("2011-12", "-")

table_1 <- table_1[, c("Australia", "sex", "age_group", "year_range", "age_fully_immunised_AIR", "n", "p")]
write.csv(table_1, "./output/AIR_121_fully_immunised_Australia.csv", row.names = FALSE)

#--------------------------------

#SA3

table_1 <- read_files("./data/aihw-mhc-hpf-16-immunisation-datasheet-report-hc42.xlsx", "TAB 3" , "A16:J1018")

names(table_1)[which(names(table_1)== "SA3 code")] <- "SA3_CODE16"
names(table_1)[which(names(table_1)== "Age group")] <- "age_fully_immunised_AIR"
names(table_1)[which(names(table_1)== "Number fully immunised")] <- "n"
names(table_1)[which(names(table_1)== "Percent fully immunised (%)")] <- "p"
names(table_1)[which(names(table_1)== "Reporting Year")] <- "year_range"
#names(table_1)[which(names(table_1)== "Interpret with caution (#)")] <- "uncertainty"
table_1$sex <- "all"
table_1$age_group <- "0-5"

table_1$uncertainty <- 0
table_1$uncertainty <- ifelse(is.na(table_1$`Interpret with caution (#)`) == TRUE,0,2)

table_1$n <- gsub("NP", NA, table_1$n)
table_1$p <- gsub("NP", NA, table_1$p)

unique(table_1$p[which(is.na(table_1$n) == TRUE)])

unique(table_1$n[which(is.na(table_1$p) == TRUE)])

table_1$p[which(is.na(table_1$n) == TRUE)] <- NA

table_1$n <- as.numeric(table_1$n)
table_1$p <- as.numeric(table_1$p)/100
table_1 <- table_1 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))

table_1$year_range[1:length(table_1$year_range)] <- unlist(lapply(table_1$year_range[1:length(table_1$year_range)], function(x){
  
  val <- strsplit(x,split = "-")
  
  return(paste(val[[1]][1],paste("20",val[[1]][2], sep = ""), sep = "-"))
  
}))

table_1 <- table_1[, c("SA3_CODE16", "sex", "age_group", "year_range", "age_fully_immunised_AIR", "n", "p", "uncertainty")]
write.csv(table_1, "./output/AIR_121_fully_immunised_SA3.csv", row.names = FALSE)

#------------------

#SA4


table_1 <- read_files("./data/aihw-mhc-hpf-16-immunisation-datasheet-report-hc42.xlsx", "TAB 6" , "A17:J281")

names(table_1)[which(names(table_1)== "SA4 code")] <- "SA4_CODE16"
names(table_1)[which(names(table_1)== "Age group")] <- "age_fully_immunised_AIR"
names(table_1)[which(names(table_1)== "Number fully immunised")] <- "n"
names(table_1)[which(names(table_1)== "Percent fully immunised (%)")] <- "p"
names(table_1)[which(names(table_1)== "Reporting Year")] <- "year_range"
#names(table_1)[which(names(table_1)== "Interpret with caution (#)")] <- "uncertainty"
table_1$sex <- "all"
table_1$age_group <- "0-5"

table_1$uncertainty <- 0
table_1$uncertainty <- ifelse(is.na(table_1$`Interpret with caution (#)`) == TRUE,0,2)

table_1$n <- gsub("NP", NA, table_1$n)
table_1$p <- gsub("NP", NA, table_1$p)

unique(table_1$p[which(is.na(table_1$n) == TRUE)])

unique(table_1$`Number of registered children`[which(is.na(table_1$n) == TRUE)])

unique(table_1$n[which(is.na(table_1$p) == TRUE)])

table_1$p[which(is.na(table_1$n) == TRUE)] <- NA

table_1$n <- as.numeric(table_1$n)
table_1$p <- as.numeric(table_1$p)/100
table_1 <- table_1 %>% mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2))))

table_1$year_range[1:length(table_1$year_range)] <- unlist(lapply(table_1$year_range[1:length(table_1$year_range)], function(x){
  
  val <- strsplit(x,split = "-")
  
  return(paste(val[[1]][1],paste("20",val[[1]][2], sep = ""), sep = "-"))
  
}))

table_1 <- table_1[, c("SA4_CODE16", "sex", "age_group", "year_range", "age_fully_immunised_AIR", "n", "p", "uncertainty")]

write.csv(table_1, "./output/AIR_121_fully_immunised_SA4.csv", row.names = FALSE)

unique(table_1$SA4_CODE16)
unique(table_1$sex)
unique(table_1$age_group)
unique(table_1$year_range)
unique(table_1$age_fully_immunised_AIR)
unique(table_1$uncertainty)
unique(table_1$n)
unique(table_1$p)

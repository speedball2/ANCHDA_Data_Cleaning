
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-----------

source("./functions/read_files.R")

#########################################
# 2006 (CCD) --> 2016 (SA1)
#############################################
table3a <- read_files("./data/CG_CD_2006_SA1_2016.xlsx", "Table 3a" , "A6:F60006") 

table3b <- read_files("./data/CG_CD_2006_SA1_2016.xlsx", "Table 3b" , "A6:F32344") 

table4 <- read_files("./data/CG_CD_2006_SA1_2016.xlsx", "Table 4" , "A6:F202") 

Q_INDICATOR <- read_files("./data/CG_CD_2006_SA1_2016.xlsx", "Table 2" , "A6:C57100") 

#remove empty 1st row
table3a <- table3a[-1,]
table3b <- table3b[-1,]
table4 <- table4[-1,]
Q_INDICATOR <- Q_INDICATOR[-1,]

table_new <- rbind(rbind(table3a, table3a),table4)

#checking firs two column contain different values or not
which(table_new$CD_CODE_2006...1 != table_new$CD_CODE_2006...2)


#table_new <- table_new[,-1]

names(table_new)[1] <- "CD_CODE_2006"
names(table_new)[2] <- "CD_CODE_2006_DUP"


#blank fill with NA

which(is.na(table_new$SA1_MAINCODE_2016) == TRUE)

View(table_new[which(table_new$CD_CODE_2006 == 1030601),])

full_table <- merge(table_new, Q_INDICATOR, by = "SA1_MAINCODE_2016", all = TRUE )

full_table <- full_table[, c(1:6,8)]

names(full_table)[4]<- "SA1_7DIGITCODE_2016"

full_table <- full_table %>% distinct() %>% drop_na(CD_CODE_2006)

full_table <- full_table[, c("CD_CODE_2006","CD_CODE_2006_DUP", "SA1_MAINCODE_2016", "SA1_7DIGITCODE_2016", "RATIO", "PERCENTAGE", "QI_INDICATOR" )]



write.csv(full_table, "./output/CG_CD_2006_SA1_2016.csv", row.names = FALSE)


#-----------------

#########################################
# 2011 (SA1) --> 2016 (SA1)
#############################################

table3 <- read_files("./data/CG_SA1_2011_SA1_2016.xls", "Table 3" , "A6:F58876") 

table4 <- read_files("./data/CG_SA1_2011_SA1_2016.xls", "Table 4" , "A6:F16") 

Q_INDICATOR <- read_files("./data/CG_SA1_2011_SA1_2016.xls", "Table 2" , "A6:C57489") 


#remove empty 1st row
table3 <- table3[-1,]
table4 <- table4[-1,]
Q_INDICATOR <- Q_INDICATOR[-1,]

table_new <- rbind(table3, table4)

full_table <- merge(table_new, Q_INDICATOR, by = "SA1_MAINCODE_2016", all = TRUE )

full_table <- full_table[, c(1:6,8)]

names(full_table)[4]<- "SA1_7DIGITCODE_2016"

full_table <- full_table %>% distinct() %>% drop_na(SA1_MAINCODE_2011)

full_table <- full_table[, c("SA1_MAINCODE_2011", "SA1_7DIGITCODE_2011","SA1_MAINCODE_2016", "SA1_7DIGITCODE_2016", "RATIO", "PERCENTAGE", "QI_INDICATOR" )]



write.csv(full_table, "./output/CG_SA1_2011_SA1_2016.csv", row.names = FALSE)

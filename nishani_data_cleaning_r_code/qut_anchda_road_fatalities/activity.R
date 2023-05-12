library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")
#------------------------

table_1 <- read_files("./data/ardd_fatalities.xlsx", "in" , "A1:U54642")
geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

unique(table_1$`Age Group`)

#extracting 0-16 age group

table_1 <- table_1[which(table_1$`Age Group` == "0_to_16"),]

#convert state to upper

table_1$State <- toupper(table_1$State)

table_1$Year <- as.numeric(table_1$Year)

table_1 <- table_1[which(table_1$Year >= 2008 & table_1$Year <= 2022),]

names(table_1)[which(names(table_1) == "SA4 Name 2016")] <- "SA4_NAME_2016"

table_1 <- table_1[, c("Crash ID","Year","State", "Gender", "Age Group", "SA4_NAME_2016")]

#587 observations removed due to NA for SA4 name (Total:1003)
table_1 <- table_1[-which(table_1$ SA4_NAME_2016 == -9),]

#---------------------
#testing s4 name and s4 code


s4_name <- unique(table_1$SA4_NAME_2016)
s4_code <- matrix(NA, nrow = length(s4_name), ncol = 1)

for(i in 1:length(s4_name)){
  
  if(length(unique(geom_data$SA4_CODE_2016[which(geom_data$SA4_NAME_2016 == s4_name[i])]))== 1){
    
    s4_code[i,1] <- unique(geom_data$SA4_CODE_2016[which(geom_data$SA4_NAME_2016 == s4_name[i])])
    
  }else{
    
    
    print(paste(s4_name[i], "have multiple codes ", unique(geom_data$SA4_CODE_2016[which(geom_data$SA4_NAME_2016 == s4_name[i])]) ))
  }
  
}

#-------------------------

#left joinf of the SA4 code
SA4_geom_data <- geom_data[, c("SA4_NAME_2016", "SA4_CODE_2016")]

new_data <- left_join(table_1,SA4_geom_data, by = "SA4_NAME_2016" )

#remove any duplicated by crash ID
new_data <- new_data[!duplicated(new_data), ]

#-------------
#some observation has been duplicated, that's why table_1 has 426 observatio and new_data has 406 observation
table_1$`Crash ID`[which(duplicated(table_1$`Crash ID`) == TRUE)]

#-------------------

new_data <- new_data[, -1]


names(new_data) <- c("calendar_year", "State", "sex", "age_group", "SA4_NAME_2016", "SA4_CODE16")

new_data <- new_data[, c("SA4_CODE16", "sex", "age_group", "calendar_year")]

new_data$sex[which(new_data$sex == "-9")] <- NA

new_data$sex <- "all"

#get the count
summary_data <- new_data %>% group_by(SA4_CODE16, sex, age_group, calendar_year) %>%   summarise(n_fatally_injured_in_road_accident = n()) %>% 
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

summary_data <- summary_data[, c("SA4_CODE16", "sex", "age_group", "calendar_year", "n_fatally_injured_in_road_accident")]

summary_data$sex <- tolower(summary_data$sex )
summary_data$age_group <- "0-16" 
write.csv(summary_data, "./output/BITRE_172_children_0_16_motor_vehicle_accidents_SA4.csv", row.names = FALSE)

#---------------------------


year <- 2008:2013
ucode <- unique(summary_data$SA4_CODE16)

rollin_avg_data <- matrix(NA,length(ucode)* length(year), 5 )

year_add <- 9
for(i in 1:length(ucode)){
  
  for(j in 1:length(year)){
    
    #print(((i -1) * length(year)) + j)
    rollin_avg_data[((i -1) * length(year)) + j, 1] <- ucode[i]
    rollin_avg_data[((i -1) * length(year)) + j, 2] <- "all"
    rollin_avg_data[((i -1) * length(year)) + j, 3] <- "0-16"
    rollin_avg_data[((i -1) * length(year)) + j, 4] <- paste(year[j], "-", year[j] + year_add, sep = "")
    #rollin_avg_data[((i -1) * length(year)) + j, 5]<- sum(summary_data$n_fatally_injured_in_road_accident[which(summary_data$SA4_CODE16 == ucode[i] & summary_data$calendar_year <= (year[j] + 4) & summary_data$calendar_year >= year[j])])/5
    
    rollin_avg_data[((i -1) * length(year)) + j, 5]<- sum(summary_data$n_fatally_injured_in_road_accident[which(summary_data$SA4_CODE16 == ucode[i] & summary_data$calendar_year <= (year[j] + year_add) & summary_data$calendar_year >= year[j])])
    
    
  }
}

rollin_avg_data <- as.data.frame(rollin_avg_data)
names(rollin_avg_data) <-  c("SA4_CODE16", "sex", "age_group", "year_range", "rolling_average_fatally_injured_in_road_accident")
write.csv(rollin_avg_data, "./output/BITRE_172_children_0_16_rolling_sum_over_10_years_motor_vehicle_accidents_SA4.csv", row.names = FALSE)

#--------------------------------------------

###############################
#STATE
###########################
site <-c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
site_cat <- 1:8

site_code_data <- as.data.frame(cbind(site, site_cat))

names(site_code_data) <- c("STE_CODE16_NAME", "STE_CODE16")

table_1 <- read_files("./data/ardd_fatalities.xlsx", "in" , "A1:U54642")
#geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

unique(table_1$`Age Group`)

#extracting 0-16 age group

table_1 <- table_1[which(table_1$`Age Group` == "0_to_16"),]

#convert state to upper

table_1$State <- toupper(table_1$State)

table_1$Year <- as.numeric(table_1$Year)

table_1 <- table_1[which(table_1$Year >= 2008 & table_1$Year <= 2022),]


table_1 <- table_1[, c("Crash ID","Year","State", "Gender", "Age Group")]

names(table_1) <- c("Crash ID","calendar_year", "STE_CODE16_NAME", "sex", "age_group")

new_data <- left_join(table_1,site_code_data, by = "STE_CODE16_NAME" )

#remove any duplicated crash ids
new_data <- new_data[!duplicated(new_data), ]

new_data <- new_data[, -1]


new_data$sex[which(new_data$sex == "-9")] <- NA


#get the count
summary_data <- new_data %>% group_by(STE_CODE16, sex, age_group, calendar_year) %>%   summarise(n_fatally_injured_in_road_accident = n()) %>% 
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

summary_data <- summary_data[, c("STE_CODE16", "sex", "age_group", "calendar_year", "n_fatally_injured_in_road_accident")]


summary_data$sex <- tolower(summary_data$sex )
summary_data$age_group <- "0-16" 

write.csv(summary_data, "./output/BITRE_172_children_0_16_motor_vehicle_accidents_STE.csv", row.names = FALSE)
#---------------------------


#calculating rolling average


year <- 2008:2018
ucode <- unique(summary_data$STE_CODE16)

rollin_avg_data <- matrix(NA,length(ucode)* length(year), 5 )

for(i in 1:length(ucode)){
  
  for(j in 1:length(year)){
   
    #print(((i -1) * length(year)) + j)
    rollin_avg_data[((i -1) * length(year)) + j, 1] <- ucode[i]
    rollin_avg_data[((i -1) * length(year)) + j, 2] <- "all"
    rollin_avg_data[((i -1) * length(year)) + j, 3] <- "0-16"
    rollin_avg_data[((i -1) * length(year)) + j, 4] <- paste(year[j], "-", year[j] + 4, sep = "")
    rollin_avg_data[((i -1) * length(year)) + j, 5]<- sum(summary_data$n_fatally_injured_in_road_accident[which(summary_data$STE_CODE16 == ucode[i] & summary_data$calendar_year <= (year[j] + 4) & summary_data$calendar_year >= year[j])])/5


  }
}


rollin_avg_data <- as.data.frame(rollin_avg_data)
names(rollin_avg_data) <-  c("STE_CODE16", "sex", "age_group", "year_range", "rolling_average_fatally_injured_in_road_accident")
write.csv(rollin_avg_data, "./output/BITRE_172_children_0_16_rolling_average_motor_vehicle_accidents_STE.csv", row.names = FALSE)

#-------------------------------------




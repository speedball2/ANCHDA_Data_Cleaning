#---------------------------

library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")


#------------------

filenames <- list.files(path = "./data/", "*.csv", full.names=TRUE)

index <- c(13403 - 11, 13403 - 11, 13877 - 11, 13877 - 11,13877 - 11,20810 -11 )

table_name_pre <- "preschool_attendance_sa2_"

not_all_na <- function(x) any(!is.na(x))

full_data <- NULL

for( i in 1:length(filenames)){
  
 
 #extracting year from file name
 year <- substr(filenames[i], nchar(filenames[i]) - 7 , nchar(filenames[i]) - 4)
 
 
 #reading csv file
 SA2_data <- read.csv(filenames[i], skip = 11, header = FALSE, check.names = FALSE)  
 
 #remove columns with all na
 SA2_data <- SA2_data %>% select(where(not_all_na))
 
 #extracted required rows
 SA2_data <- SA2_data[1:index[i],]
 
 #new column names
 colnames(SA2_data) <-  c("SA2_CODE16", "sex", "attended", "n_children_attending_preschool")
 
 #select the required "Attended in reference week
 SA2_data <- SA2_data[which(SA2_data$attended == "Attended in reference week" ),]
 
 #remove the unwanted column
 SA2_data <- SA2_data[ , -which(names(SA2_data) == "attended")]

 SA2_data$age_group <- "3-6"
 SA2_data$calendar_year <- year
 SA2_data$sex <- tolower(SA2_data$sex)
 
 print(unique(paste(year, "- ", SA2_data$sex)))
 SA2_data$sex <- ifelse(SA2_data$sex == "male" | SA2_data$sex == "female", SA2_data$sex , NA)
 
 #filling the SA2 code
 for(j in 1:nrow(SA2_data)){
   
   if(j %% 3 == 1 ){
     
     SA2_data$SA2_CODE16[j + 1] <- SA2_data$SA2_CODE16[j]
     
     SA2_data$SA2_CODE16[j + 2] <- SA2_data$SA2_CODE16[j]
     
   }
 }
  
 SA2_data <- SA2_data[, c("SA2_CODE16", "age_group", "sex", "calendar_year", "n_children_attending_preschool" )]
 
 full_data <-  rbind(full_data, SA2_data)
 
}

write.csv(full_data, "./output/abs_preschools_411_preschool_attendance_sa2.csv", row.names = FALSE)

#----------------------------------------------------

#geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

#SA3

#SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","SA3_CODE_2016")]

#names(SA3_geom_data) <- c("SA2_CODE16", "SA3_CODE16")

#SA3_geom_data$SA2_CODE16 <- as.character(SA3_geom_data$SA2_CODE16)

#new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )

#new_data <- left_join(full_data, SA3_geom_data, by = "SA2_CODE16" )

#write.csv((new_data[which(is.na(new_data$SA3_CODE16) == TRUE),]), "no_match_SA2_code.csv", row.names = FALSE)

new_data <- full_data

new_data$SA3_CODE16 <- substr(new_data$SA2_CODE16,1,5)

new_data <- new_data %>% group_by(SA3_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("SA3_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]

write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_sa3.csv", row.names = FALSE)

#-----------------------

#SA4


# geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)
# 
# 
# SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","SA4_CODE_2016")]
# 
# names(SA3_geom_data) <- c("SA2_CODE16", "SA4_CODE16")
# 
# SA3_geom_data$SA2_CODE16 <- as.character(SA3_geom_data$SA2_CODE16)
# 
# #new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )
#new_data <- left_join(full_data, SA3_geom_data, by = "SA2_CODE16" )

new_data <- full_data

new_data$SA4_CODE16 <- substr(new_data$SA2_CODE16,1,3)

new_data <- new_data %>% group_by(SA4_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("SA4_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]
unique(new_data$SA4_CODE16)
write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_sa4.csv", row.names = FALSE)
#-----------------

#STATE


# geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)
# 
# 
# SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","STATE_CODE_2016")]
# 
# names(SA3_geom_data) <- c("SA2_CODE16", "STE_CODE16")
# 
# new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )

new_data <- full_data

new_data$STE_CODE16 <- substr(new_data$SA2_CODE16,1,1)

new_data <- new_data %>% group_by(STE_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("STE_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]

unique(new_data$STE_CODE16)
write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_ste.csv", row.names = FALSE)


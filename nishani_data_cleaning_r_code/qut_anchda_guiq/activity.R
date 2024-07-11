
library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-----------

source("./functions/read_files.R")
source("./functions/data_extraction_for_indicator.R")

#--------------------

guiq_data <- read_files("./data/GUIQ 2020 data for ANCHDA.xlsx", "Sheet1" , "A1:V5925") 


#-----------------------------------

#creating sex column (male, sex, other)

unique(guiq_data$`What gender do you identify as?`)

names(guiq_data)[which(names(guiq_data) == "What gender do you identify as?")] <- "sex"

guiq_data$sex <- tolower(guiq_data$sex)

guiq_data$sex <- ifelse(guiq_data$sex == "female" | guiq_data$sex == "male", guiq_data$sex, "other" )

#guiq_data <- guiq_data[which(guiq_data$sex == "female" | guiq_data$sex == "male"),]

#---------------------------

#age_group collumn

names(guiq_data)[1] <- "age_group"
guiq_data$age_group <- "13-18"



#---------------

geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

qld_geom_data <- geom_data[geom_data$STATE_CODE_2016 == 3,]

qld_geom_data <- qld_geom_data[ , c("SA4_NAME_2016", "SA4_CODE_2016")]

qld_geom_data <- qld_geom_data %>% distinct(.keep_all = TRUE)

#mental health

#1.5.2 Mental health conditions

#% Young people reporting having a mental or emotional health condition_SA4_male, female, all (all incl gender diverse) (age group 13-18)
data_extraction_for_indicator(guiq_data, 3, "p_reporting_mental_or_emotional", "Yes", qld_geom_data,  "./output/GuiQ_152_mental_or_emotional_health_condition")


#1.5.3 Experiences of stress

#% Young people reporting they feel stressed about their school or uni results _SA4_male, female, all (all incl gender diverse) (age group 13-18)
names(guiq_data)[21]
unique(guiq_data[21])

data_extraction_for_indicator(guiq_data, 21, "p_stressed_school_or_uniresults", c("Strongly agree","Agree"), qld_geom_data,  "./output/GuiQ_153_stressed_school_or_uniresults")


#% Young people reporting they feel stressed about things in their personal life _SA4_male, female, all (all incl gender diverse) (age group 13-18)

names(guiq_data)[22]
unique(guiq_data[22])

data_extraction_for_indicator(guiq_data, 22, "p_stressed_personal_life", c("Strongly agree","Agree"), qld_geom_data,  "./output/GuiQ_153_stressed_personal_life")

#--------------

# Learning
# 
# 4.9 School satisfaction
# 
# 4.9.2 Sense of belonging at school (new indicator)
# 
# % Young people reporting feeling sense of belonging at school_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 5

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_feeling_sense_belonging_at_school", "Yes", qld_geom_data,  "./output/GuiQ_492_feeling_sense_belonging_at_school")

#------------------------------------

# Valued, loved and safe
# 
# 3.4 Support networks (new indicator domain)
# 
# 3.4.1 Turning to others for support (new indicator)
# 
# % Young people seeking support from family_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 6

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_family", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_family")


# 
# % Young people seeking support from friends_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 7

names(guiq_data)[col_num]
unique(guiq_data[col_num])
data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_friends", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_friends")

# 
# % Young people seeking support from support services_ SA4_male, female, all (all incl gender diverse) (age group 13-18)
col_num <- 10

names(guiq_data)[col_num]
unique(guiq_data[col_num])
data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_support_services", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_support_services")

# 
# % Young people seeking support from teacher/s_ SA4_male, female, all (all incl gender diverse) (age group 13-18)
col_num <- 8

names(guiq_data)[col_num]
unique(guiq_data[col_num])
data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_teachers", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_teachers")

# 
# % Young people seeking support from no one_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 9

names(guiq_data)[col_num]
unique(guiq_data[col_num])
data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_no_one", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_no_one")

# 
# % Young people seeking support online _ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 12

names(guiq_data)[col_num]
unique(guiq_data[col_num])
data_extraction_for_indicator(guiq_data, col_num, "p_seeking_support_from_online", "Yes", qld_geom_data,  "./output/GuiQ_341_seeking_support_from_online")


#3.13 Safe environments

#3.13.1 Safe school environments

#3.13.2 Safe community environments

#% Young people reporting they feel safe in their community _ SA4_male, female, all (all incl gender diverse) (age group 13-18) [combine agree and strongly agree]

col_num <- 19

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_feel_safe_in_their_community", c("Agree", "Strongly agree"), qld_geom_data,  "./output/GuiQ_3132_feel_safe_in_their_community")


#% Young people reporting they experienced physical bullying in the past 12 months _ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 20

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_experienced_physical_bullying", c("Often", "Always"), qld_geom_data,  "./output/GuiQ_3135_experienced_physical_bullying")


#Participating

# 5.6 Connection to Community (new indicator domain)
# 
# % Young people reporting feeling sense of belonging in their community [all students answering agree or strongly agree in column P]_ SA4_male, female, all (all incl gender diverse) (age group 13-18)
#

col_num <- 16

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_feeling_sense_of_belonging_in_community", c("Strongly agree", "Agree"), qld_geom_data,  "./output/GuiQ_560_feeling_sense_of_belonging_in_community")


# % Young people reporting they have a say in their community [all students answering agree or strongly agree in column Q]_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 17

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_have_a_say_in_their_community", c("Strongly agree", "Agree"), qld_geom_data,  "./output/GuiQ_561_have_a_say_in_their_community")

# % Young people reporting their community has fun places for them to spend time with friends [all students answering agree or strongly agree in column Q]_ SA4_male, female, all (all incl gender diverse) (age group 13-18)

col_num <- 18

names(guiq_data)[col_num]
unique(guiq_data[col_num])

data_extraction_for_indicator(guiq_data, col_num, "p_reporting_community_has_fun_places", c("Strongly agree", "Agree"), qld_geom_data,  "./output/GuiQ_562_reporting_community_has_fun_places")



######################################



#Cell suppression


source("./functions/cell_suppression.R")


file_names <- list.files(path = "./output/", pattern = "*.csv", full.names = TRUE)

for(k in 1:length(file_names)){
  
  cell_suppression(file_names[k], 3)
    
  
}

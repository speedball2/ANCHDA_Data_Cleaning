library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")

source("./functions/data_extraction_wecsa.R")

source("./functions/letters2numbers.R")

source("./functions/rounding_function.R")

#-------------------

#--------------------

data <- read_files("./data/08.02.2023_WEC_SA_tableshells_MR.xlsx", "Wellbeing engagement 2022" , "B6:AJ13")    

data$CODE <- 4

data$GENDER <- ifelse(data$GENDER == "Overall", "all", data$GENDER)
data$GENDER <- ifelse(data$GENDER == "M", "male", data$GENDER)
data$GENDER <- ifelse(data$GENDER == "F", "female", data$GENDER)


data$AGE <-  ifelse(data$AGE == "12 to 17", "12-17", data$AGE)
data$AGE <-  ifelse(data$AGE == "15 to 17", "15-17", data$AGE)
data$AGE <-  ifelse(data$AGE == "10 to 17", "10-17", data$AGE)
essentail_col_number <- c(1,3,4, 5)

# Healthy:
#   
#   1.11.1 Social and emotional wellbeing (new )
# % Young people reporting their life satisfaction is high_STE_all (age group 10 -17) 

data_extraction_wecsa(data, essentail_col_number, "g", "p_reporting_life_satisfaction_high", "10-17", "./output/WECSA/WECSA_1141_p_reporting_life_satisfaction_high_age_group_10_17_STE.csv")

#% Young people reporting their life satisfaction is medium_STE_all (age group 10-17)

data_extraction_wecsa(data, essentail_col_number, "h", "p_reporting_life_satisfaction_medium", "10-17", "./output/WECSA/WECSA_1141_p_reporting_life_satisfaction_medium_age_group_10_17_STE.csv")


#% Young people reporting their life satisfaction is low_STE_all (age group 10-17)

data_extraction_wecsa(data, essentail_col_number, "i", "p_reporting_life_satisfaction_low", "10-17", "./output/WECSA/WECSA_1141_p_reporting_life_satisfaction_low_age_group_10_17_STE.csv")

#% Young people reporting their life satisfaction is low_STE_male, female (age group 15-17)
data_extraction_wecsa(data, essentail_col_number, "i", "p_reporting_life_satisfaction_low", "15-17", "./output/WECSA/WECSA_1141_p_reporting_life_satisfaction_low_age_group_15_17_STE.csv")


# % Young people experiencing low wellbeing due to sadness_STE_all (age group 10-17)
data_extraction_wecsa(data, essentail_col_number, "o", "p_experiencing_low_wellbeing_due_to_sadness", "10-17", "./output/WECSA/WECSA_1144_p_experiencing_low_wellbeing_due_to_sadness_age_group_10_17_STE.csv")

 
# % Young people experiencing low wellbeing due to sadness_STE_male, female (age group 15-17)
data_extraction_wecsa(data, essentail_col_number, "o", "p_experiencing_low_wellbeing_due_to_sadness", "15-17", "./output/WECSA/WECSA_1144_p_experiencing_low_wellbeing_due_to_sadness_age_group_15_17_STE.csv")



# % Young people experiencing low wellbeing due to distress_STE_all (age group 15-17)
data_extraction_wecsa(data, essentail_col_number, "r", "p_experiencing_low_wellbeing_due_to_distress", "15-17", "./output/WECSA/WECSA_1145_p_experiencing_low_wellbeing_due_to_distress_age_group_15_17_STE.csv")


# % Young people experiencing low wellbeing due to feelings about their body_STE_all (age group 15-17)

data_extraction_wecsa(data, essentail_col_number, "x", "p_experiencing_low_wellbeing_due_to_feelings_about_body", "15-17", "./output/WECSA/WECSA_1146_p_experiencing_low_wellbeing_due_to_feelings_about_body_age_group_15_17_STE.csv")
# 
# Valued, loved and safe
# 
# 3.1 Positive peer relationships
# 
# 3.1.2 Friends
# 
# % Young people reporting high wellbeing in regard to friendships_STE_all (age group 10-17) [High wellbeing]


data_extraction_wecsa(data, essentail_col_number, "ah", "p_reporting_high_wellbeing_in_regard_to_friendships", "10-17", "./output/WECSA/WECSA_312_p_reporting_high_wellbeing_in_regard_to_friendships_age_group_10_17_STE.csv")
# 


# Learning
# 
# 4.9 School satisfaction
# 
# 4.9.2 Sense of belonging at school
# 
# % Young people reporting feeling high sense of belonging at school_STE_all (age group 10-17) [High wellbeing]


data_extraction_wecsa(data, essentail_col_number, "ae", "p_reporting_feeling_high_sense_of_belonging_at_school", "10-17", "./output/WECSA/WECSA_492_p_reporting_feeling_high_sense_of_belonging_at_school_age_group_10_17_STE.csv")


# % Young people reporting feeling low sense of belonging at school_STE_all (age group 10-17) [Low wellbeing]


data_extraction_wecsa(data, essentail_col_number, "ag", "p_reporting_feeling_low_sense_of_belonging_at_school", "10-17", "./output/WECSA/WECSA_492_p_reporting_feeling_low_sense_of_belonging_at_school_age_group_10_17_STE.csv")


# 4.8 Teacher support
# 
# 4.8.1 Teacher who cares
# 
# % Young people reporting high emotional engagement with teachers_STE_all (age group 10-17) [High wellbeing]

data_extraction_wecsa(data, essentail_col_number, "ab", "p_reporting_high_emotional_engagement_with_teachers", "10-17", "./output/WECSA/WECSA_481_p_reporting_high_emotional_engagement_with_teachers_age_group_10_17_STE.csv")

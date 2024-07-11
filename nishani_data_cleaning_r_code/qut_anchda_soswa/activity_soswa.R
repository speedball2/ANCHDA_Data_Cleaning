library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
#-----------

source("./functions/read_files.R")

source("./functions/data_extraction_soswa.R")

source("./functions/letters2numbers.R")


source("./functions/rounding_function.R")

#-------------------

#--------------------

data <- read_files("./data/ANCHDA data request - SOS_tableshells - 22 May 2023.xlsx", "Speaking Out Survey 2021" , "B6:BG10")    

data$GENDER <- ifelse(data$GENDER == "Overall", "all", data$GENDER)
data$GENDER <- ifelse(data$GENDER == "M", "male", data$GENDER)
data$GENDER <- ifelse(data$GENDER == "F", "female", data$GENDER)

data$AGE <-  ifelse(data$AGE == "12 to 17", "12-17", "14-17")

essentail_col_number <- c(1,3,4,5)

# 1.5.3 Experiences of stress
# % Young people reporting they feel stressed about school or study problems_STE_male, female (age group 14-17)
# 
data_extraction_soswa(data, essentail_col_number, "s", "p_reporting_feel_stressed_school_or_study", "14-17", "./output/SOSWA_153_p_reporting_feel_stressed_school_or_study_STE.csv")

#% Young people reporting they feel stressed about mental health_STE_male, female (age group 14-17)


data_extraction_soswa(data, essentail_col_number, "t", "p_reporting_feel_stressed_mental_health", "14-17", "./output/SOSWA_153_p_reporting_feel_stressed_mental_health_STE.csv")

#% Young people reporting they feel stressed about body image_STE_male, female (age group 14-17)


data_extraction_soswa(data, essentail_col_number, "u", "p_reporting_feel_stressed_body_image", "14-17", "./output/SOSWA_153_p_reporting_feel_stressed_body_image_STE.csv")

#% Young people reporting they feel stressed about problems with friends_STE_male, female (age group 14-17)
data_extraction_soswa(data, essentail_col_number, "v", "p_reporting_feel_stressed_problems_with_friends", "14-17", "./output/SOSWA_153_p_reporting_feel_stressed_problems_with_friends_STE.csv")

#% Young people reporting they feel stressed about family conflict_STE_male, female (age group 14-17)
data_extraction_soswa(data, essentail_col_number, "w", "p_reporting_feel_stressed_family_conflict", "14-17", "./output/SOSWA_153_p_reporting_feel_stressed_family_conflict_STE.csv")

#---------------------

#1.14.1 Social and emotional wellbeing (new )
# % Young people reporting their life satisfaction is high_STE_male, female (age group 12 -17)

data_extraction_soswa(data, essentail_col_number, "i", "p_reporting_life_satisfaction_high", "12-17", "./output/SOSWA_1141_p_reporting_life_satisfaction_high_STE.csv")

# % Young people reporting their life satisfaction is Medium_STE_male, female (age group 12 -17)

data_extraction_soswa(data, essentail_col_number, "h", "p_reporting_life_satisfaction_medium", "12-17", "./output/SOSWA_1141_p_reporting_life_satisfaction_medium_STE.csv")

# % Young people reporting their life satisfaction is low_STE_male, female (age group 12 -17)
data_extraction_soswa(data, essentail_col_number, "g", "p_reporting_life_satisfaction_low", "12-17", "./output/SOSWA_1141_p_reporting_life_satisfaction_low_STE.csv")

#-----------------
# % Young people reporting they can deal with things in their life_agree_STE_male, female (age group 12 -17) -1442

data_extraction_soswa(data, essentail_col_number, "k", "p_reporting_deal_with_things_life_agree", "12-17", "./output/SOSWA_1142_p_reporting_deal_with_things_life_agree_STE.csv")

# % Young people reporting they can deal with things in their life_disagree_STE_male, female (age group 12 -17)

data_extraction_soswa(data, essentail_col_number, "m", "p_reporting_deal_with_things_life_disagree", "12-17", "./output/SOSWA_1142_p_reporting_deal_with_things_life_disagree_STE.csv")

# % Young people reporting they are happy with themselves_agree_STE_male, female (age group 12 -17)

data_extraction_soswa(data, essentail_col_number, "n", "p_reporting_happy_with_themselves_agree", "12-17", "./output/SOSWA_1143_p_reporting_happy_with_themselves_agree_STE.csv")

# % Young people reporting they are happy with themselves_disagree_STE_male, female (age group 12 -17)
data_extraction_soswa(data, essentail_col_number, "o", "p_reporting_happy_with_themselves_disagree", "12-17","./output/SOSWA_1143_p_reporting_happy_with_themselves_disagree_STE.csv")

# % Young people reporting they feel sad or depressed_2_weeks or more_Yes_STE_male, female (age group 14 -17)

data_extraction_soswa(data, essentail_col_number, "p", "p_reporting_feel_sad_or_depressed_2weeks_more_yes", "14-17", "./output/SOSWA_1144_p_reporting_feel_sad_or_depressed_2weeks_more_yes_STE.csv")


# % Young people reporting they feel sad or depressed_2_weeks or more_No_STE_male, female (age group 14-17)
data_extraction_soswa(data, essentail_col_number, "q", "p_reporting_feel_sad_or_depressed_2weeks_more_no", "14-17", "./output/SOSWA_1144_p_reporting_feel_sad_or_depressed_2weeks_more_no_STE.csv")

#------------

#Learning
# 4.9 School satisfaction
# 4.9.1 Liking school
#% Young people reporting they like school_STE_male, female (age group 12-17) [Combine a lot and a little]


data_extraction_soswa(data, essentail_col_number, c("ay", "az"), "p_young_people_reporting_they_like_school", "12-17", "./output/SOSWA_491_p_young_people_reporting_they_like_school_STE.csv")


#% Young people reporting they do not like school_STE_male, female (age group 12-17)

data_extraction_soswa(data, essentail_col_number, "bb", "p_reporting_they_do_not_like_school", "12-17", "./output/SOSWA_491_p_reporting_they_do_not_like_school_STE.csv")

# 4.9.2 Sense of belonging at school
# % Young people reporting feeling sense of belonging at school_STE_male, female (age group 12-17) [Agree]
# p_feeling_sense_belonging_at_school



data_extraction_soswa(data, essentail_col_number, "bc", "p_feeling_sense_belonging_at_school", "12-17", "./output/SOSWA_492_p_feeling_sense_belonging_at_school_STE.csv")

#--------------------

# Valued, loved and safe
# 3.4.1 Turning to others for support
# % Young people reporting they have an adult they can turn to for support_STE_male, female (age group 12-17)

data_extraction_soswa(data, essentail_col_number, "ao", "p_reporting_have_an_adult_for_support", "12-17", "./output/SOSWA_341_p_reporting_have_an_adult_for_support_STE.csv")

#3.13 Safe environments
#3.13.2 Safe community environments
#% Young people reporting they feel safe in their community_ STE_male, female (age group 12-17) [combine all the time and most of the time]

data_extraction_soswa(data, essentail_col_number, c("ak", "al"), "p_reporting_feel_safe_in_their_community", "12-17", "./output/SOSWA_3132_p_reporting_feel_safe_in_their_community_STE.csv")



#3.13.3 Safe in the home

#% Young people reporting they worry that someone in their home will be fighting_ STE_male, female (age group 12-17) [combine somewhat and a lot]
data_extraction_soswa(data, essentail_col_number, c("ai", "aj"), "p_reporting_worry_that_someone_in_home_will_be_fighting", "12-17", "./output/SOSWA_3133_p_reporting_worry_that_someone_in_home_will_be_fighting_STE.csv")


#3.14 Experiences of violence
#% Young people reporting they have been physically harmed by someone on purpose_STE_male, female (age group 14-17)
data_extraction_soswa(data, essentail_col_number, "as", "p_reporting_have_been_physically_harmed_by_someone_on_purpose", "14-17", "./output/SOSWA_3135_p_reporting_ have_been_physically_harmed_by_someone_on_purpose_STE.csv")


#-------------
# 
# Participating
# 5.6 Connection to Community
#% Young people reporting feeling sense of belonging in their community_ STE_male, female (age group 12-17)



data_extraction_soswa(data, essentail_col_number, "aa", "p_feeling_sense_of_belonging_in_community", "12-17", "./output/SOSWA_560_p_feeling_sense_of_belonging_in_community_STE.csv")


#% Young people reporting their community has fun places for them to spend time with friends_STE_ male, female (age group 12-17)




data_extraction_soswa(data, essentail_col_number, "ad", "p_reporting_community_has_fun_places", "12-17", "./output/SOSWA_562_p_reporting_community_has_fun_places_STE.csv")





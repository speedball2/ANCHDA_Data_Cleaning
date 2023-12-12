library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-------------------------------

source("./functions/read_files.R")
source("./functions/join_SA1_2016_to_other_stat_area.R")
source("./functions/join_to_SA1_2016.R")
source("./functions/extract_SA_data.R")
source("./functions/rounding_function.R")
source("./functions/calculating_total_erp_SA.R")
source("./functions/calculating_rate_of_enrolment.R")


#----------------------------------

#2016 geo data
geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

#AEC data
data_enrolment <- read_files("./data/ANCHDA Enrolment Request_v2.xlsx", "18-24 yo Enrolments 2008-2022", "A3:CL55432" )  

#rename the column names
colnames(data_enrolment) <- rep(c("calendar_year", "SA1_CODE", "female", "male", "all", "free"), 15)

data_full <- NULL


#covert data frame in to correct format 
for(i in 1:15){
  
print(i)
sub_data <-   data_enrolment[, ((6 * (i-1)) + 1) : (6 * i)]
sub_data <- sub_data[!rowSums(is.na(sub_data[ , 1:6])) == 6, ]

data_full <- rbind(data_full, sub_data)
  
}

#remove the empty column
data_full <- data_full[ , -6]

#there are two observation with SA1 code 0. Removing those two observations as SA1 code are invalid

if(length(which(nchar(data_full$SA1_CODE) != 7)) > 0){
  
  data_full <- data_full[-which(nchar(data_full$SA1_CODE) != 7),]
}

########################
 
#apply temporal correspondence

#2008 - 2014 ---> 2006 (ASGC 2006)
#2015 - 2018 ---> 2011 (ASGS 2011)
#2019 - 2022 ---> 2016 (ASGS 2016)

##################################################

#read temporal correspondence 2006 correspondence (CCD) -> 2016 (ASGS 2016) SA1
CCD_2006_SA1_2016 <- read.csv("./data/CG_CD_2006_SA1_2016.csv", header = TRUE, check.names = FALSE)
#read temporal correspondence 2011 correspondence (SA1) -> 2016 (ASGS 2016) SA1
SA1_2011_SA1_2016 <- read.csv("./data/CG_SA1_2011_SA1_2016.csv", header = TRUE, check.names = FALSE)
#Spatial correlation of SA1 to , SA2,SA3,SA4
SA1_2016_AUST <- read.csv("./data/SA1_2016_AUST.csv", header = TRUE, check.names = FALSE)

#-------------------
data_full$calendar_year <- as.numeric(data_full$calendar_year)

#subset data 
data_2008_2014 <- data_full[which(data_full$calendar_year <= 2014 & data_full$calendar_year >= 2008),]

data_2015_2018 <- data_full[which(data_full$calendar_year <= 2018 & data_full$calendar_year >= 2015),]

data_2019_2022 <- data_full[which(data_full$calendar_year <= 2022 & data_full$calendar_year >= 2019),]

#rename data column
names(data_2019_2022)[2]<- "SA1_7DIGITCODE_2016"

data_2019_2022$number_of_enrolment_uncertainty_correspondence <- "Good"

#----------------------------

#convert 2006 ASGC to 2016 ASGS
out_2008_2014 <- join_to_SA1_2016 (data_2008_2014,"SA1_CODE" , CCD_2006_SA1_2016, "CD_CODE_2006", c("female", "male", "all"), "SA1_MAINCODE_2016", "number_of_enrolment_uncertainty_correspondence")

#convert 2011 ASGS to 2016 ASGS
out_2015_2018 <- join_to_SA1_2016 (data_2015_2018,"SA1_CODE" , SA1_2011_SA1_2016, "SA1_7DIGITCODE_2011", c("female", "male", "all"), "SA1_MAINCODE_2016", "number_of_enrolment_uncertainty_correspondence")


#-----------------------------
#spatial aggregation of SA1 2016 to other SAs
final_2008_2014 <- join_SA1_2016_to_other_stat_area(out_2008_2014, SA1_2016_AUST, "SA1_MAINCODE_2016", "number_of_enrolment_uncertainty_correspondence")
final_2015_2018 <- join_SA1_2016_to_other_stat_area(out_2015_2018, SA1_2016_AUST, "SA1_MAINCODE_2016", "number_of_enrolment_uncertainty_correspondence")
SA1_2016_AUST$SA1_7DIGITCODE_2016 <- as.character(SA1_2016_AUST$SA1_7DIGITCODE_2016)
final_2019_2022 <- join_SA1_2016_to_other_stat_area(data_2019_2022, SA1_2016_AUST, "SA1_7DIGITCODE_2016", "number_of_enrolment_uncertainty_correspondence")

#-----------------------------------

#new_data <- rbind(rbind(final_2008_2014, final_2015_2018),final_2019_2022)

# just using the data from 2019 to 2022
new_data <- final_2019_2022
#extract the data for relevant statistical areas
SA2_out_data <- extract_SA_data(new_data, "SA2_MAINCODE_2016", "SA2_CODE16", "number_of_enrolment_uncertainty_correspondence","./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA2.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA2.csv")

SA3_out_data <- extract_SA_data(new_data, "SA3_CODE_2016", "SA3_CODE16",  "number_of_enrolment_uncertainty_correspondence", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA3.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA3.csv")

SA4_out_data <-extract_SA_data(new_data, "SA4_CODE_2016", "SA4_CODE16", "number_of_enrolment_uncertainty_correspondence",  "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA4.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA4.csv")

STE_out_data <- extract_SA_data(new_data, "STATE_CODE_2016", "STE_CODE16", "number_of_enrolment_uncertainty_correspondence",  "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_STE.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_STE.csv")
#------------------------


#USE ERP WHEN CALCUATING RATE OF ENOLMENT
SA2_total <- calculating_total_erp_SA("./data/ABS_Estimated_resident_population_ASGS2016/ABS_ERP_181_ERP_SA2.csv", 18:24, 9999999, "SA2_CODE16")

# new_SA1_2016_AUST <- SA1_2016_AUST
# names(new_SA1_2016_AUST)[which(names(new_SA1_2016_AUST) == "SA2_5DIGITCODE_2016")] <- "SA2_CODE16"
# new_SA2_total <- left_join(SA2_total, new_SA1_2016_AUST, by = "SA2_CODE16")
# new_SA2_total <- new_SA2_total[, c("SA2_MAINCODE_2016","calendar_year", "total")]
# new_SA2_total <- new_SA2_total[!duplicated(new_SA2_total),]
# names(new_SA2_total)[1] <- "SA2_CODE16"

SA2_total <- calculating_total_erp_SA("./data/ABS_Estimated_resident_population_ASGS2016/ABS_ERP_181_ERP_SA2.csv", 18:24, 9999999, "SA2_CODE16")
SA3_total <- calculating_total_erp_SA("./data/ABS_Estimated_resident_population_ASGS2016/ABS_ERP_181_ERP_SA3.csv", 18:24, 9999999, "SA3_CODE16")
SA4_total <- calculating_total_erp_SA("./data/ABS_Estimated_resident_population_ASGS2016/ABS_ERP_181_ERP_SA4.csv", 18:24, 9999999, "SA4_CODE16")
STE_total <- calculating_total_erp_SA("./data/ABS_Estimated_resident_population_ASGS2016/ABS_ERP_181_ERP_STE.csv", 18:24, 9999999, "STE_CODE16")

calculating_rate_of_enrolment(SA2_out_data, new_SA2_total, c("SA2_CODE16", "calendar_year"), c("SA2_CODE16","sex", "age_group", "calendar_year", "rate_of_enrolment", "number_of_enrolment_uncertainty_correspondence"), "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA2.csv", "./output/AEC_521_young_people_18_24_combined_SA2.csv")

calculating_rate_of_enrolment(SA3_out_data, SA3_total, c("SA3_CODE16", "calendar_year"), c("SA3_CODE16","sex", "age_group", "calendar_year", "rate_of_enrolment", "number_of_enrolment_uncertainty_correspondence"), "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA3.csv", "./output/AEC_521_young_people_18_24_combined_SA3.csv")

calculating_rate_of_enrolment(SA4_out_data, SA4_total, c("SA4_CODE16", "calendar_year"),  c("SA4_CODE16","sex", "age_group", "calendar_year", "rate_of_enrolment", "number_of_enrolment_uncertainty_correspondence"), "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA4.csv", "./output/AEC_521_young_people_18_24_combined_SA4.csv")

calculating_rate_of_enrolment(STE_out_data, STE_total, c("STE_CODE16", "calendar_year"),  c("STE_CODE16","sex", "age_group", "calendar_year", "rate_of_enrolment", "number_of_enrolment_uncertainty_correspondence"), "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_STE.csv", "./output/AEC_521_young_people_18_24_combined_STE.csv")


#------------------------


required_age_group
sup_val
#joint the estimates population column




View(new_data[which(new_data$SA2_5DIGITCODE_2016 == 11007 & new_data$calendar_year == 2008),"total"])

#extract the data for relavant statistical areas
# extract_SA_data(new_data, "SA2_MAINCODE_2016", "SA2_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA2.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA2.csv", "number_of_enrolment_uncertainty_correspondence")
# extract_SA_data(new_data, "SA3_CODE_2016", "SA3_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA3.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA3.csv", "number_of_enrolment_uncertainty_correspondence")
# extract_SA_data(new_data, "SA4_CODE_2016", "SA4_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA4.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA4.csv", "number_of_enrolment_uncertainty_correspondence")
# extract_SA_data(new_data, "STATE_CODE_2016", "STE_CODE16", "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_STE.csv", "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_STE.csv", "number_of_enrolment_uncertainty_correspondence")
# #------------------------

#-----------------------------------

#########################################
data <- data_2008_2014
key_data_col_name <- "SA1_CODE"
tc_data <- CCD_2006_SA1_2016
key_tc_data_col_name <- "CD_CODE_2006"

var_names <- c("female", "male", "total")
geo_to_name <- "SA1_MAINCODE_2016"
uncertainty_colname <- "number_of_enrolment_uncertainty_correspondence"
############################################


----------------------------------------


#------------------



data1 <- read.csv("./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA2.csv",  header = TRUE, check.names = FALSE)

data2 <- read.csv("./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA2.csv", header = TRUE, check.names = FALSE)


data <- left_join(data1, data2, by = c("SA2_CODE16","sex" , "age_group" , "calendar_year"  ))

write.csv(data, "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_rate_of_enrolled_to_vote_SA2.csv", row.names = FALSE)

#--------------
source("./functions/cell_suppression.R")

count_file <- "./output/AEC_521_young_people_18_24_number_of_enrolled_to_vote_SA4.csv"
rate_file <- "./output/AEC_521_young_people_18_24_rate_of_enrolled_to_vote_SA4.csv"


cell_suppression(count_file,rate_file , "number_of_enrolment", "rate_of_enrolment", "number_of_enrolment_uncertainty_correspondence", 3, "AEC_521_young_people_18_24_vote_combined_")



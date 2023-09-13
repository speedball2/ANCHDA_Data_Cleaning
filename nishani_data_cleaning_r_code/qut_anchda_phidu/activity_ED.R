library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-------------------------------

source("./functions/read_files.R")
source("./functions/letters2numbers.R")
source("./functions/data_extraction.R")
#----------------------

#reading data

#ED presentations.

#-------------------------------------------------------
#155 emergency_department_presentations_for_mental_and_behavioural_disorders_public_hospitals

data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "ED_mental_disorders_age_sex", "A5:AF572" )  

file_name <- "./output/ED/PHIDU_155_emergency_department_presentations_for_mental_and_behavioural_disorders_public_hospitals_LGA.csv"


male_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "male", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

male_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "male", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


female_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "female", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

female_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "female", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(male_data_0_14, male_data_15_24, female_data_0_14, female_data_15_24, all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------------------------

#173 emergency_department_presentations_for_injury_poisoning_other_external_causes
data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "ED_injury_age_sex", "A5:AF572" )  

file_name <- "./output/ED/PHIDU_173_emergency_department_presentations_for_injury_poisoning_other_external_causes_LGA.csv"


male_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "male", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

male_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "male", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


female_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "female", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

female_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "female", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(male_data_0_14, male_data_15_24, female_data_0_14, female_data_15_24, all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )


#---------------

#1301 Emergency_department_presentations_public_hospitals

data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "ED_total_age_sex", "A5:AF572" )  

file_name <- "./output/ED/PHIDU_1301_emergency_department_presentations_public_hospitals_LGA.csv"


male_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "male", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

male_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "male", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


female_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "female", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

female_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "female", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")


all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(male_data_0_14, male_data_15_24, female_data_0_14, female_data_15_24, all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#############################################################

#1302 Emergency_department_presentations_by_triage_category_public_hospitals


data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "ED_total_triage_category", "A5:BJ572" )  

file_name <- "./output/ED/PHIDU_1302_emergency_department_resuscitation_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "all", "0-14", "2018-2019", "n_ed_resuscitation_presentations", "rate_ed_resuscitation_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "all", "15-24", "2018-2019", "n_ed_resuscitation_presentations", "rate_ed_resuscitation_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )


#----------------------------------------

file_name <- "./output/ED/PHIDU_1302_emergency_department_emergency_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "all", "0-14", "2018-2019", "n_ed_emergency_presentations", "rate_ed_emergency_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "all", "15-24", "2018-2019", "n_ed_emergency_presentations", "rate_ed_emergency_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------------------------------

file_name <- "./output/ED/PHIDU_1302_emergency_department_urgent_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_ed_urgent_presentations", "rate_ed_urgent_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_ed_urgent_presentations", "rate_ed_urgent_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------

file_name <- "./output/ED/PHIDU_1302_emergency_department_semi_urgent_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("ah", "ai"), "all", "0-14", "2018-2019", "n_ed_semi_urgent_presentations", "rate_ed_semi_urgent_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("am", "an"), "all", "15-24", "2018-2019", "n_ed_semi_urgent_presentations", "rate_ed_semi_urgent_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#------------


file_name <- "./output/ED/PHIDU_1302_emergency_department_non_urgent_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("ar", "as"), "all", "0-14", "2018-2019", "n_ed_non_urgent_presentations", "rate_ed_non_urgent_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("aw", "ax"), "all", "15-24", "2018-2019", "n_ed_non_urgent_presentations", "rate_ed_non_urgent_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#-----------------------------------
file_name <- "./output/ED/PHIDU_1302_emergency_department_total_presentations_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("bb", "bc"), "all", "0-14", "2018-2019", "n_ed_total_presentations", "rate_ed_total_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("bg", "bh"), "all", "15-24", "2018-2019", "n_ed_total_presentations", "rate_ed_total_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#######################

#1303 Emergency_department_presentations_by_principal_diagnosis_public hospitals

data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "ED_total", "A5:CN572" )  

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_certain_infectious_and_parasitic_diseases_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_mental_and_behavioural_disorders_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_diseases_of_the_circulatory_system_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )
#------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_diseases_of_the_respiratory_system_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("ah", "ai"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("am", "an"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )
#------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_diseases_of_the_digestive_system_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("ar", "as"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("aw", "ax"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )
#------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_diseases_of_the_musculoskeletal_system_and_connective_tissue_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("bb", "bc"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("bg", "bh"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#-----------------------------------
file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_diseases_of_the_genitourinary_system_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("bl", "bm"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("bq", "br"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_injury_poisoning_and_certain_other_consequences_of_external_causes_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("bv", "bw"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ca", "cb"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------------

file_name <- "./output/ED/PHIDU_1303_emergency_department_presentations_for_factors_influencing_health_status_and_contact_with_health_services_LGA.csv"



all_data_0_14 <- data_extraction(data, c(1,3), c("cf", "cg"), "all", "0-14", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ck", "cl"), "all", "15-24", "2018-2019", "n_emergency_department_presentations", "rate_emergency_department_presentations_per_100000")



full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )


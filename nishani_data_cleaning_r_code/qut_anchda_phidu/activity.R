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

#Hospital admissions

#-------------------------------------------------------
#1.20.1 Total_admissions_public hospitals

data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "Hosp_type_sex", "A5:AF572" )  

file_name <- "./output/PHIDU_1201_total_admissions_public_hospitals_LGA.csv"
  
  
male_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "male", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

male_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "male", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


female_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "female", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

female_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "female", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")



full_data <- do.call("rbind",list(male_data_0_14, male_data_15_24, female_data_0_14, female_data_15_24, all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )


#####################################################

#1.20.2 Admissions by principal diagnosis_public hospitals


data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "Admiss_principal_diag_persons", "A5:EV572" )  


file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_infectious_and_parasitic_diseases_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#-------------------------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_all_cancers_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#---------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_endocrine_nutritional_and_metabolic_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#------------------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_mental_health_related_conditions_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("ah", "ai"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("am", "an"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#----------------


file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_nervous_system_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("ar", "as"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("aw", "ax"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#-------------


file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_ear_and_mastoid_process_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("bb", "bc"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("bg", "bh"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_circulatory_system_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("bl", "bm"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("bq", "br"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_respiratory_system_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("bv", "bw"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ca", "cb"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )


#------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_asthma_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("cf", "cg"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ck", "cl"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#------------------

file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_digestive_system_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("cp", "cq"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("cu", "cv"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#-----------------------
file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_skin_and_subcutaneous_tissue_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("cz", "da"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("de", "df"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#-----------------------
file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_musculoskeletal_system_and_connective_tissue_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("dj", "dk"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("do", "dp"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )
#--------------------------
file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_genitourinary_system_diseases_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("dt", "du"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("dy", "dz"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )
#--------------------------
file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_congenital_malformations_deformations_and_chromosomal_abnormalities_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("ed", "ee"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("el", "ej"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )

#--------------------------
file_name <- "./output/PHIDU_1202_public_hosipital_admissions_for_injury_poisoning_and_other_external_causes_LGA.csv"


all_data_0_14 <- data_extraction(data, c(1,3), c("en", "eo"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("es", "et"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")


full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))


write.csv(full_data, file_name , row.names = FALSE )


###########################################

#Injury

#1.7.1 Hospital admissions due to injury or poisoning_public hospitals


data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "Admiss_principal_ext_persons", "A5:AP572" )  


file_name <- "./output/PHIDU_171_public_hosipital_admissions_for_transport_crash_injury_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")

full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#----------------

file_name <- "./output/PHIDU_171_public_hosipital_admissions_for_falls_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("n", "o"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("s", "t"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")

full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#-----------------------------

file_name <- "./output/PHIDU_171_public_hosipital_admissions_for_injury_due_to_exposure_to_inanimate_mechanical_forces_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("x", "y"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("ac", "ad"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")

full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )

#-----------------------------

file_name <- "./output/PHIDU_171_public_hosipital_admissions_for_all_diagnosis_of_injury_or_poisoning_by_external_cause_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("ah", "ai"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("am", "an"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")

full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )


###########################################



#1.20.3 Admissions by procedure_public hospitals


data <- read_files("./data/PHIDU_child_and_youth_data_LGA_AUST.xlsx", "Admissions_procedures", "A5:Q572" )  


file_name <- "./output/PHIDU_1203_public_hosipital_admissions_for_a_tonsillectomy_LGA.csv"

all_data_0_14 <- data_extraction(data, c(1,3), c("d", "e"), "all", "0-14", "2018-2019", "n_admissions", "rate_admissions_per_100000")

all_data_15_24 <- data_extraction(data, c(1,3), c("i", "j"), "all", "15-24", "2018-2019", "n_admissions", "rate_admissions_per_100000")

full_data <- do.call("rbind",list(all_data_0_14, all_data_15_24))

write.csv(full_data, file_name , row.names = FALSE )     

#--------------


file_name <- "./output/PHIDU_1203_public_hosipital_admissions_for_a_myringotomy_LGA.csv"

all_data_0_9 <- data_extraction(data, c(1,3), c("n", "o"), "all", "0-9", "2018-2019", "n_admissions", "rate_admissions_per_100000")

write.csv(all_data_0_9, file_name , row.names = FALSE )                   

                                    
                                    
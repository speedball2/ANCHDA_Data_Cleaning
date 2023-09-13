library(tidyverse)

region_eg <- "West Arnhem" # SA3 REGION!
year_eg <- 2010
data_set <- "acara_451_naplan_results_reading_year_5" #EVERYTHING BUT REGION IDENDIFIER AND .CSV IN FILE NAME


asgs <- read.csv("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/ASGS2016_SA2_SA3_SA4_code_name_matching_ref_csv/SA2_2016_AUST_no_geom.csv")
SA3_eg <- asgs$SA3_CODE_2016[grep(region_eg, asgs$SA3_NAME_2016)][1]
SA2_eg <- asgs$SA2_MAINCODE_2016[grep(SA3_eg, asgs$SA3_CODE_2016)]

dfSA2 <- read.csv(paste0("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_CLEANED_QA_DONE/ACARA/acara_naplan_results/sa2/", data_set, "_sa2.csv"))
dfSA2 <- filter(dfSA2, calendar_year == year_eg)
dfSA2 <- filter(dfSA2, SA2_CODE16 %in% SA2_eg)
dfSA3 <- read.csv(paste0("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_CLEANED_QA_DONE/ACARA/acara_naplan_results/sa3/", data_set, "_sa3.csv"))
dfSA3 <- filter(dfSA3, calendar_year == year_eg)
dfSA3 <- filter(dfSA3, SA3_CODE16 == SA3_eg)

dfSA3$naplan_score_acara
dfSA2$naplan_score_acara

mean(dfSA3$naplan_score_acara)
mean(dfSA2$naplan_score_acara)

# Outstanding: Is the difference at the LGA level? This is harder to compare.
# WOULD NEED TO PLOT OR CREATE STATE AVERAGE COMPARISIONS 





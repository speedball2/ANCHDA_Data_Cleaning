# R script: nnds_data_cleaning
# Author: claire boulange
# Date: 2023 04 28
# Description: This script reads the raw data from NNDS, and puts it in the correct format for VISER 

# Libraries required
# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
#set working directory and options
setwd("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NNDSS/")
path_out = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/NNDSS/"
options(timeout = 600) 


file_to_clean <- "537_2022_MARCH2023_NoACT_NoSA_NoTAS.XLSX"


tablesa2_sa3 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/gis/SA2_2016_AUST.csv")
tablesa2_sa3 <- tablesa2_sa3 %>%
  mutate(SA2_MAINCODE_2016 = as.character(SA2_MAINCODE_2016)) %>%
  mutate(SA3_CODE_2016 = as.character(SA3_CODE_2016))


#read the area_code correspondence table from VISER
area_code_filtered <- read.csv("C:/Users/00095998/Area_Ref.csv") %>% 
  filter(zone %in% c("SA2", "SA3"), year == 2016) %>% 
  select(code, name)


#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
NNDSS <- read_excel(file_to_clean) %>%
  select(YEAR, SA2, DISEASE, AGE, SEX, COUNT, `SEIFA_SCORE_ON_SA2*`) %>%
  #mutate(irsd_quintile = ntile(`SEIFA_SCORE_ON_SA2*`, 10)) %>%
  inner_join(area_code_filtered, by = c("SA2" = "name")) %>%    #need to load area_code_filtered before
  select(code, AGE, SEX, YEAR, DISEASE, COUNT) %>%
  rename(SA2_CODE16 = code,
         age_group = AGE,
         sex = SEX,
         calendar_year = YEAR,
         disease = DISEASE,
         count = COUNT) %>%
  mutate(sex = case_when(sex == "FEMALE" ~ "female",
                         sex == "MALE" ~ "male",
                         TRUE ~ sex)) %>%
  mutate(sex = if_else(sex %in% c("NULL", "NOT STATED", "UNDETERMINED"), "NULL", sex)) %>%
  mutate(SA2_CODE16 = as.character(SA2_CODE16))



joined_data <- inner_join(NNDSS, tablesa2_sa3, by = c("SA2_CODE16" = "SA2_MAINCODE_2016"))


# add SA3_CODE_2016 column
joined_data <- mutate(joined_data, SA3_CODE16 = SA3_CODE_2016)



# summarise by all columns except disease
summarised_data <- joined_data %>%
  group_by(SA3_CODE16, age_group, sex, calendar_year, disease) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# create a new row for all STIs
all_sti_data <- summarised_data %>%
  group_by(SA3_CODE16, age_group, sex, calendar_year) %>%
  summarise(disease = "all_sti", count = sum(count)) %>%
  ungroup()

# bind the all_sti_data to the original data
summarised_data <- bind_rows(summarised_data, all_sti_data)

# arrange the data by SA3 code, age group, sex, calendar year, and disease
final_data <- summarised_data %>%
  arrange(SA3_CODE16, age_group, sex, calendar_year, disease)


# Split NNDSS dataframe into multiple dataframes based on the unique values in disease column
df_list <- split(final_data, final_data$disease)


#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
suffix_list <- lapply(df_list, function(x) tolower(unique(x$disease)))

df_list <- lapply(df_list, function(x) {
  suffix <- tolower(unique(x$disease))
  colnames(x)[6] <- paste0("count_cases_", suffix)
  x %>% select(-disease)
})

lapply(names(df_list), function(suffix) {
  file_name <- paste0("NNDSS_1152_prevalence_STI_young_people_15_24_", tolower(suffix), "_SA3.csv")
  file_path <- paste0(path_out, file_name)
  write.csv(df_list[[suffix]], file_path, row.names = FALSE)
})

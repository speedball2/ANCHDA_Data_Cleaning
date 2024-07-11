
# ----------------- #
# --- libraries --- #
# ----------------- #


library(readxl)
library(dplyr)
library(tidyr)

# READING IN DATA --------------------------------------------------------------

cleaning <- function(sht, ran){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path = "ANCHDA data request_16 Mar 2023.xlsx",
                                sht,
                                ran,
                                col_names = T))
  
  
  # REMOVE COLUMNS FOR 95% CI, PAST YEAR ...
  
  df = df[,!grepl("*yea", names(df))]
  df = df[,!grepl("*95%", names(df))]
  df = df[,!grepl("*friend", names(df))]
  df = df[,!grepl("*someone", names(df))]
  
  
  #RENAMING TOTAL COLUMNS FROM BASE n TO n
  df <- df %>% 
    rename_with(~ gsub("Base_N", "N", .x, fixed = TRUE))
  
  #REMOVING SPACES IN COLUMN NAMES AND REPLACING WITH _
  df <- df %>% 
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE))
  
  df <- df %>% 
    rename_with(~ gsub("__", "_", .x, fixed = TRUE))
  
  #fixing spelling error in raw data 
  df <- df %>% 
    rename_with(~ gsub("cigerette", "cigarette", .x, fixed = TRUE))
  
  #CHANGING TOTAL TO BE CONCISTENT WITH DATA DICTIONARY 
  df["Age_and_Sex"][df["Age_and_Sex"] == "Overall Total (M/F)"] <- "total"
  df["Age_and_Sex"][df["Age_and_Sex"] == "12M to 17M"] <- "12-17M"
  df["Age_and_Sex"][df["Age_and_Sex"] == "12F to 17F"] <- "12-17F"
  
  
  # RENAMING CODE COLUMNS
  colnames(df)[colnames(df) == "National_Code"] ="Australia"
  colnames(df)[colnames(df) == "STATE_CODE_2016"] ="STE_CODE16"
  
  
  
  #ADDING YEAR COLUMN
  df$calendar_year <- 2017
  
  #REMOVING NAMES COL
  df <- df[,-2]
  
  #CHANGING COL NAMES TO BE SAME AS DATA DICT
  df <- df %>% 
    rename_with(~ gsub("%_", "p_", .x, fixed = TRUE))
  
  df <- df %>% 
    rename_with(~ gsub("N_", "n_", .x, fixed = TRUE))
  
  return(df)
  
}

# STE SPECIFIC FUNCTIONS -------------------------------------------------------

rem <- function(df) {
  
  df <- df %>%
    mutate(age_group = gsub("^(\\d{2}-\\d{2}|\\d{2}-\\d|\\d{1,2}-\\d{2}).*$", "\\1", Age_and_Sex),
           sex = ifelse(Age_and_Sex == "total", NA, ifelse(grepl("[Ff]", Age_and_Sex), "female", "male"))) %>%
    select(age_group, sex, everything())
  
  # Remove rows where STE_CODE16 is NA or age_group is "total"
  df <- df[!is.na(df$STE_CODE16) & df$age_group != "total", ]
  
  # Remove Age_and_Sex column
  df <- df[, -which(names(df) == "Age_and_Sex")]
  
  # Reorder the columns
  df <- df[, c("STE_CODE16", "sex", "age_group", "calendar_year", setdiff(names(df), c("STE_CODE16", "sex", "age_group", "calendar_year")))]
  
  return(df)
}


#NSW 

df1 <- cleaning(4, "B3:CM13")
df1 <- rem(df1)


#VIC

df2 <- cleaning(8, "B3:CM13")
df2 <- rem(df2)

#QLD

df3 <- cleaning(5, "B3:CM13")
df3 <- rem(df3)

#SA

df4 <- cleaning(6, "B3:CM13")
df4 <- rem(df4)

#WA

df5 <- cleaning(9, "B3:CM13")
df5 <- rem(df5)

#TAS

df6 <- cleaning(7, "B3:CM13")
df6 <- rem(df6)

#ACT

df8 <- cleaning(3, "B3:CM13")
df8 <- rem(df8)

df8["STE_CODE16"][is.na(df8["STE_CODE16"])] <- 8


# NATIONAL SPECIFIC ------------------------------------------------------------

#NATIONAL ----------------------------------------------------------------------
rem2 <- function(df) {
  df <- df %>%
    mutate(
      Australia = ifelse(is.na(Age_and_Sex), Australia, 0),
      age_group = gsub("^(\\d+).*", "\\1", Age_and_Sex),
      age_group = ifelse(grepl("^\\d+$", age_group), paste(age_group, age_group, sep = "-"), age_group),
      sex = ifelse(Age_and_Sex == "total", NA, ifelse(grepl("[Ff]", Age_and_Sex), "female", "male"))
    ) %>%
    select(age_group, sex, everything())
  
  # Remove rows where STE_CODE16 is NA or age_group is "total"
  df <- df[!is.na(df$Australia) & df$age_group != "total", ]
  
  # Remove Age_and_Sex column
  df <- df[, -which(names(df) == "Age_and_Sex")]
  
  # Reorder the columns
  df <- df[, c("Australia", "sex", "age_group", "calendar_year", setdiff(names(df), c("Australia", "sex", "age_group", "calendar_year")))]
  
  return(df)
}

df0 <- cleaning( 2, "B3:CM26")
df0 <- rem2(df0)

df0 <- df0 %>%
  mutate(across(.cols = age_group, ~ ifelse(grepl("^\\d+-\\d+$", .), ., paste(., ., sep = "-"))))
#fix this col name
colnames(df0)[colnames(df0) == "n_accessed_last_alcoholic_drink_from_bought_themselves_"] ="n_accessed_last_alcoholic_drink_from_bought_themselves"

# COMBINE STE TOGETHER ---------------------------------------------------------

STE <- rbind(df1, df2, df3, df4, df5, df6, df8)
#fix this col name
colnames(STE)[colnames(STE) == "n_accessed_last_alcoholic_drink_from_bought_themselves_"] ="n_accessed_last_alcoholic_drink_from_bought_themselves"


# Select columns with specified patterns
alcoholic_columns <- grep("alcoholic|drinkers", colnames(STE), ignore.case = TRUE, value = TRUE)
smoking_columns <- grep("smokers", colnames(STE), ignore.case = TRUE, value = TRUE)
drugs_columns <- grep("cannabis|dexamphetamine|methamphetamine", colnames(STE), ignore.case = TRUE, value = TRUE) #
cigarette_columns <- grep("cigarette", colnames(STE), ignore.case = TRUE, value = TRUE)


####----------------------------------STATE_WIDE---------------------------------------------------------
df_alcohol_ste <- STE[, c("STE_CODE16", "calendar_year", "sex", "age_group", alcoholic_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_smoking_ste <- STE[, c("STE_CODE16", "calendar_year", "sex", "age_group", smoking_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_drugs_ste <- STE[, c("STE_CODE16", "calendar_year", "sex", "age_group", drugs_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_cigarette_ste <- STE[, c("STE_CODE16", "calendar_year", "sex", "age_group", cigarette_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))

####----------------------------------AUSTRALIA_WIDE-----------------------------------------------------
df_alcohol_aus <- df0[, c("Australia", "calendar_year", "sex", "age_group", alcoholic_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_smoking_aus <- df0[, c("Australia", "calendar_year", "sex", "age_group", smoking_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_drugs_aus <- df0[, c("Australia", "calendar_year", "sex", "age_group", drugs_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))
df_cigarette_aus <- df0[, c("Australia", "calendar_year", "sex", "age_group", cigarette_columns)] %>%
  rename_with(~paste0(., "_ASSAD"), -c(1:4))

# WRITE CSVS -------------------------------------------------------------------

#NATIONAL SINGLE AGE + AGE RANGE COMBINED 
write.csv(df_smoking_aus, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad/ASSAD_191_smoking_National.csv", row.names = F)
write.csv(df_alcohol_aus, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad/ASSAD_192_alcohol_National.csv", row.names = F)
write.csv(df_drugs_aus, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad/ASSAD_193_drugs_National.csv", row.names = F)
write.csv(df_cigarette_aus, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad/ASSAD_194_e_cigarettes_National.csv", row.names = F)


#STATE
write.csv(df_smoking_ste, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad//ASSAD_191_smoking_STE.csv", row.names = F)
write.csv(df_alcohol_ste, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad//ASSAD_192_alcohol_STE.csv", row.names = F)
write.csv(df_drugs_ste, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad//ASSAD_193_drugs_STE.csv", row.names = F)
write.csv(df_cigarette_ste, "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/assad//ASSAD_194_e_cigarettes_STE.csv", row.names = F)


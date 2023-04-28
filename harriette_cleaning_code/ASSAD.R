setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/ASSAD_STE_national")


# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)
library(dplyr)
library(tidyr)

cleaning <- function(sht, ran){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path = "ANCHDA data request_16 Mar 2023.xlsx",
                                sht,
                                ran,
                                col_names = T))
  
  
  # REMOVE COLUMNS FOR 95% CI, PAST YEAR ...
  
  df = df[,!grepl("*year", names(df))]
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
  df["Age_and_Sex"][df["Age_and_Sex"] == "12M to 17M"] <- "male_total"
  df["Age_and_Sex"][df["Age_and_Sex"] == "12F to 17F"] <- "female_total"
  
  
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

#NATIONAL ----------------------------------------------------------------------

df0 <- cleaning( 2, "B3:CM26")

# NATIONAL SPECIFIC CODE 

#REMOVING EMPTY ROWS
df0 <- df0[-c(8,16,19,22),]

#CHANGING NAS TO 0 (NATIONAL CODE)
df0["Australia"][is.na(df0["Australia"])] <- 0

#SEPERATING AGE AND SEX INTO DIFFERENT COLUMNS

df0 <- df0 %>% 
  separate(`Age_and_Sex`, into = c("age_group", "sex"), 
           sep = "(?<=[0-9])(?=\\s?[A-Z])", remove = FALSE) %>% 
  mutate(sex = trimws(sex))

#throws an error in rows 7,14, 19 but that's ok because these are changed anyway, as below

#CHANGING NA VALUES
df0[7,4] = "male_total"
df0[14,4] = "female_total"
df0[19,4] = "total"

#REMOVING COMBINED COLUMN
df0 <- df0[,-2]

# REMOVE TOTAL COLS

df0 <- df0[!grepl("_total", df0$sex),]

#CHANGING COL NAMES FOR DATA DIC STANDARD

df0 <- df0 %>% 
  rename_with(~ gsub("%_", "p_", .x, fixed = TRUE))

df0 <- df0 %>% 
  rename_with(~ gsub("N_", "n_", .x, fixed = TRUE))


# STE SPECIFIC FUNCTIONS -------------------------------------------------------

rem <- function(df){
  
  df <- df[-c(3,6,9),]
  
  df <- df %>% 
    separate(`Age_and_Sex`, into = c("age_group", "sex"), 
             sep = "(?<=[0-9])(?=\\s?[A-Z])", remove = FALSE) %>% 
    mutate(sex = trimws(sex))
  
  #throws an error in rows 7,14, 19 but that's ok because these are changed anyway, as below
  
  #CHANGING NA VALUES
  df[1,4] = "male_total"
  df[2,4] = "female_total"
  df[7,4] = "total"
  
  #REMOVING COMBINED COLUMN
  df <- df[,-2]
  
  #REMOVING TOTAL COL
  df <- df[!grepl("_total", df$sex),]
  
  return(df)
  
}
#STE ---------------------------------------------------------------------------

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


# RE ORDERING COLUMNS (ALL) ----------------------------------------------------


corder1 <- c("Australia","calendar_year", "sex", "age_group",
             "n_accessed_last_alcoholic_drink_from_parents", "p_accessed_last_alcoholic_drink_from_parents", 
             "n_accessed_last_alcoholic_drink_from_sibling", "p_accessed_last_alcoholic_drink_from_sibling",
             "n_accessed_last_alcoholic_drink_from_took_from_home", "p_accessed_last_alcoholic_drink_from_took_from_home",    
             "n_accessed_last_alcoholic_drink_from_bought_themselves_", "p_accessed_last_alcoholic_drink_from_bought_themselves", 
             "n_ever_drinkers","p_ever_drinkers",
             "n_past_month_drinkers", "p_past_month_drinkers",    
             "n_past_week_drinkers", "p_past_week_drinkers",
             "n_ever_smokers", "p_ever_smokers",
             "n_past_month_smokers", "p_past_month_smokers",
             "n_past_week_smokers", "p_past_week_smokers",
             "n_ever_e-cigarette_users", "p_ever_e-cigarette_users",
             "n_past_month_e-cigarette_users", "p_past_month_e-cigarette_users",
             "n_ever_cannabis_users", "p_ever_cannabis_users",
             "n_past_month_cannabis_users", "p_past_month_cannabis_users",  
             "n_past_week_cannabis_users", "p_past_week_cannabis_users" ,   
             "n_ever_dexamphetamine_users", "p_ever_dexamphetamine_users",   
             "n_past_month_dexamphetamine_users", "p_past_month_dexamphetamine_users",
             "n_past_week_dexamphetamine_users", "p_past_week_dexamphetamine_users",   
             "n_ever_methamphetamine_users", "p_ever_methamphetamine_users", 
             "n_past_month_methamphetamine_users", "p_past_month_methamphetamine_users",  
             "n_past_week_methamphetamine_users", "p_past_week_methamphetamine_users")

corder2 <- c("STE_CODE16","calendar_year", "sex", "age_group",
             "n_accessed_last_alcoholic_drink_from_parents", "p_accessed_last_alcoholic_drink_from_parents", 
             "n_accessed_last_alcoholic_drink_from_sibling", "p_accessed_last_alcoholic_drink_from_sibling",
             "n_accessed_last_alcoholic_drink_from_took_from_home", "p_accessed_last_alcoholic_drink_from_took_from_home",    
             "n_accessed_last_alcoholic_drink_from_bought_themselves_", "p_accessed_last_alcoholic_drink_from_bought_themselves", 
             "n_ever_drinkers","p_ever_drinkers",
             "n_past_month_drinkers", "p_past_month_drinkers",    
             "n_past_week_drinkers", "p_past_week_drinkers",
             "n_ever_smokers", "p_ever_smokers",
             "n_past_month_smokers", "p_past_month_smokers",
             "n_past_week_smokers", "p_past_week_smokers",
             "n_ever_e-cigarette_users", "p_ever_e-cigarette_users",
             "n_past_month_e-cigarette_users", "p_past_month_e-cigarette_users",
             "n_ever_cannabis_users", "p_ever_cannabis_users",
             "n_past_month_cannabis_users", "p_past_month_cannabis_users",  
             "n_past_week_cannabis_users", "p_past_week_cannabis_users" ,   
             "n_ever_dexamphetamine_users", "p_ever_dexamphetamine_users",   
             "n_past_month_dexamphetamine_users", "p_past_month_dexamphetamine_users",
             "n_past_week_dexamphetamine_users", "p_past_week_dexamphetamine_users",   
             "n_ever_methamphetamine_users", "p_ever_methamphetamine_users", 
             "n_past_month_methamphetamine_users", "p_past_month_methamphetamine_users",  
             "n_past_week_methamphetamine_users", "p_past_week_methamphetamine_users")


reorder <- function(df, corder){
  
  #CHANGE COLUMN ORDER - FILTERS AT FRONT, DRINKS W/ DRINKING DATA
  df <- df[,corder]
  
  # ADD SUFFIX "ASSAD" TO INDICATORS THAT ARE THE SAME AS NDSHS 
  colnames(df)[c(13:46)] <- paste(colnames(df)[c(13:46)], 'ASSAD', sep = '_')
  
  #CHANGE M/F TO FOLLOW DATA DICTIONARY STANDARDS: MALE FEMALE
  df["sex"][df["sex"] == "M"] <- "male"
  df["sex"][df["sex"] == "F"] <- "female"
  
  colnames(df)[colnames(df) == "n_accessed_last_alcoholic_drink_from_bought_themselves_"] ="n_accessed_last_alcoholic_drink_from_bought_themselves"
  

  return(df)
}

df0 <- reorder(df0, corder = corder1)
df1 <- reorder(df1, corder = corder2)
df2 <- reorder(df2, corder = corder2)
df3 <- reorder(df3, corder = corder2)
df4 <- reorder(df4, corder = corder2)
df5 <- reorder(df5, corder = corder2)
df6 <- reorder(df6, corder = corder2)
df8 <- reorder(df8, corder = corder2)

# COMBINE STE TOGETHER ---------------------------------------------------------

STE <- rbind(df1, df2, df3, df4, df5, df6, df8)


# SORTING/ SEPERATING BY INDICATOR ---------------------------------------------


n <- c("Australia","calendar_year", "sex", "age_group")
s <- c("STE_CODE16","calendar_year", "sex", "age_group")


#1.9.1 SMOKING
smo <- c("n_ever_smokers_ASSAD", "p_ever_smokers_ASSAD",
         "n_past_month_smokers_ASSAD", "p_past_month_smokers_ASSAD",
         "n_past_week_smokers_ASSAD", "p_past_week_smokers_ASSAD")

#1.9.2 ALCOHOL
alc <-  c("n_accessed_last_alcoholic_drink_from_parents", "p_accessed_last_alcoholic_drink_from_parents", 
       "n_accessed_last_alcoholic_drink_from_sibling", "p_accessed_last_alcoholic_drink_from_sibling",
       "n_accessed_last_alcoholic_drink_from_took_from_home", "p_accessed_last_alcoholic_drink_from_took_from_home",    
       "n_accessed_last_alcoholic_drink_from_bought_themselves", "p_accessed_last_alcoholic_drink_from_bought_themselves", 
       "n_ever_drinkers_ASSAD","p_ever_drinkers_ASSAD",
       "n_past_month_drinkers_ASSAD", "p_past_month_drinkers_ASSAD",    
       "n_past_week_drinkers_ASSAD", "p_past_week_drinkers_ASSAD")

#1.9.3 DRUGS
dru <- c("n_ever_cannabis_users_ASSAD", "p_ever_cannabis_users_ASSAD",
         "n_past_month_cannabis_users_ASSAD", "p_past_month_cannabis_users_ASSAD",  
         "n_past_week_cannabis_users_ASSAD", "p_past_week_cannabis_users_ASSAD",   
         "n_ever_dexamphetamine_users_ASSAD", "p_ever_dexamphetamine_users_ASSAD",   
         "n_past_month_dexamphetamine_users_ASSAD", "p_past_month_dexamphetamine_users_ASSAD",
         "n_past_week_dexamphetamine_users_ASSAD", "p_past_week_dexamphetamine_users_ASSAD",   
         "n_ever_methamphetamine_users_ASSAD", "p_ever_methamphetamine_users_ASSAD", 
         "n_past_month_methamphetamine_users_ASSAD", "p_past_month_methamphetamine_users_ASSAD",  
         "n_past_week_methamphetamine_users_ASSAD", "p_past_week_methamphetamine_users_ASSAD")

#1.9.4 E-CIGS

cig <- c("n_ever_e-cigarette_users_ASSAD", "p_ever_e-cigarette_users_ASSAD",
         "n_past_month_e-cigarette_users_ASSAD", "p_past_month_e-cigarette_users_ASSAD")


# SEPERATING BY INDICATOR ------------------------------------------------------

#NATIONAL 

#1.9.1 SMOKING

nsmo <- df0[,c(n,smo)]

#1.9.2 ALCOHOL

nalc <- df0[,c(n, alc)]

#1.9.3 DRUGS

ndru <- df0[,c(n, dru)]

#1.9.4 E-CIGS

ncig <- df0[,c(n, cig)]


#STE 

ssmo <- STE[,c(s,smo)]

#1.9.2 ALCOHOL

salc <- STE[,c(s, alc)]

#1.9.3 DRUGS

sdru <- STE[,c(s, dru)]

#1.9.4 E-CIGS

scig <- STE[,c(s, cig)]

# WRITE CSVS -------------------------------------------------------------------

#NATIONAL
write.csv(nsmo, "../../../Data_Collections_INTERIM/ASSAD_191_smoking_National.csv", row.names = F)
write.csv(nalc, "../../../Data_Collections_INTERIM/ASSAD_192_alcohol_National.csv", row.names = F)
write.csv(ndru, "../../../Data_Collections_INTERIM/ASSAD_193_drugs_National.csv", row.names = F)
write.csv(ncig, "../../../Data_Collections_INTERIM/ASSAD_194_e_cigarettes_National.csv", row.names = F)

#STATE
write.csv(ssmo, "../../../Data_Collections_INTERIM/ASSAD_191_smoking_STE.csv", row.names = F)
write.csv(salc, "../../../Data_Collections_INTERIM/ASSAD_192_alcohol_STE.csv", row.names = F)
write.csv(sdru, "../../../Data_Collections_INTERIM/ASSAD_193_drugs_STE.csv", row.names = F)
write.csv(scig, "../../../Data_Collections_INTERIM/ASSAD_194_e_cigarettes_STE.csv", row.names = F)




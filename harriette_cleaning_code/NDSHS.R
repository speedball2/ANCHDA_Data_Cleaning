#Harriette's WD
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NDSHS_STE_National")

# LIBRARIES --------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)

# CLEANING CODE ----------------------------------------------------------------
# COLUMNS NAMES  ---------------------------------------------------------------


coln1 <- c("STE_CODE16", "calendar_year", 
           "n_current_smoker", "p_current_smoker",
           "n_ex_smoker", "p_ex_smoker",
           "n_never_smoked", "p_never_smoked",
           "n_current_vaper", "p_current_vaper",
           "n_ex_vaper", "p_ex_vaper",
           "n_never_vaped", "p_never_vaped", 
           "n_current_drinker", "p_current_drinker", 
           "n_ex_drinker", "p_ex_drinker", "n_never_drinker", "p_never_drinker",
           "n_ever_used_illicit_drugs_yes", "p_ever_used_illicit_drugs_yes", 
           "n_ever_used_illicit_drugs_no", "p_ever_used_illicit_drugs_no",
           "n_ever_used_pharmaceuticals_for_non_medical_purposes_yes",
           "p_ever_used_pharmaceuticals_for_non_medical_purposes_yes",
           "n_ever_used_pharmaceuticals_for_non_medical_purposes_no", 
           "p_ever_used_pharmaceuticals_for_non_medical_purposes_no", 
           "n_recently_used_illicit_drugs_yes",
           "p_recently_used_illicit_drugs_yes", 
           "n_recently_used_illicit_drugs_no", 
           "p_recently_used_illicit_drugs_no", 
           "n_recently_used_cannabis_yes", 
           "p_recently_used_cannabis_yes", 
           "n_recently_used_cannabis_no", 
           "p_recently_used_cannabis_no")


coln2 <- c("STE_CODE16", "calendar_year", 
           "n_age_of_initiation_of_smoking", "n_age_of_initiation_of_drinking",
           "p_type_of_alcohol_usually_consumed_bottled_wine", 
           "p_type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", 
           "p_type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", 
           "p_type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
           "p_type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", 
           "p_type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", 
           "p_type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
           "p_type_of_alcohol_usually_consumed_cider", 
           "p_type_of_alcohol_usually_consumed_other", 
           "n_age_of_initiation_of_illicit_drug_use_lifetime", 
           "n_age_of_initiation_of_illicit_drug_use_recent",
           "p_cannabis_use_frequency_every_day", 
           "p_cannabis_use_frequency_once_a_week_or_more", 
           "p_cannabis_use_frequency_about_once_a_month", 
           "p_cannabis_use_frequency_every_few_months", 
           "p_cannabis_use_frequency_once_or_twice_a_year")


# READ IN EXCEL FILES ----------------------------------------------------------

cleaning <- function(sheet, range, coln){
  # sheet: Excel Sheet.
  # range: Spreadsheet range.
  
  
  # READ SHEET:
  df <- as.data.frame(read_xlsx("NDSHS_Final data tables.xlsx", sheet, range, F))
  
  
  # FILLING FIRST COLUMN:
  df <- df %>% fill(names(df)[1], .direction = "down")
  
  
  # NEW COLUMN NAMES:
  names(df) <- coln
  
  
  # RECODING STATE NAMES TO CODES:
  if(names(df)[1] == "STE_CODE16"){
    df$STE_CODE16 <- recode(df$STE_CODE16,
                            "NSW" = 1,
                            "VIC" = 2,
                            "Vic" = 2,
                            "QLD" = 3,
                            'Qld' = 3,
                            "SA" = 4,
                            "WA" = 5,
                            "TAS" = 6,
                            "Tas" = 6,
                            "NT(l)" = 7,
                            "NT(h)" = 7,
                            "ACT" = 8, 
                            "National" = 0) 
    
  }
  
  
  
  # REMOVE AGE FROM COLUMS IN DATA 
  
  if ("age_group" %in% colnames(df)) {
    
    df["age_group"][df["age_group"] == "Aged 14-17"] <- "14-17"
    df["age_group"][df["age_group"] == "Aged 18-24"] <- "18-24"
  }
  
  
  # REMOVE NAs & NPs:
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  
  
  # OUTPUT:
  return(df)
}

# READING IN EXCEL FILES -------------------------------------------------------

# STATE ------------------------------------------------------------------------
df1 <- cleaning(2, "A6:AJ50", coln1) #AOD status by state

#CHANGE DATA BASED ON STATE CODE ORDER (CURRENTLY INCORRECT ORDER)

df1 <- with(df1, df1[order(match(STE_CODE16, c(1,2,3,4,5,6,7,8,0))),])

#REMOVE NATIONAL DATA FROM STATE DATA

df1 <- df1[df1$STE_CODE16 != 0, ]


df2 <- cleaning(3, "A6:T50", coln2) #AOD Qs by State 

#CHANGE DATA BASED ON STATE CODE ORDER (CURRENTLY INCORRECT ORDER)

df2 <- with(df2, df2[order(match(STE_CODE16, c(1,2,3,4,5,6,7,8,0))),])


#REMOVE NATIONAL DATA FROM STATE DATA

df2 <- df2[df2$STE_CODE16 != 0, ]

# RESHAPING DATA ---------------------------------------------------------------


reshape_data <- function(df){
  
  # ROUNDING:
  for(i in seq(3,ncol(df),2)){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
  
  
  
  # ADD AND CALCULATE UNCERTAINTY
  for(i in ncol(df):3){
    # Insert column using cursed new notation: https://stackoverflow.com/questions/60311773/mutate-with-paste0
    df <- add_column(df, !!paste0(names(df)[i],"_uncertainty") := NA, .after = i)
    df[which(substr(df[, i],1,1) == "*"),i+1] <- "1"
    df[which(substr(df[, i],1,2) == "**"),i+1] <- "2"
    df[which(substr(df[, i],1,1) == "`"),i+1] <- "1"
    df[which(substr(df[, i],1,2) == "``"),i+1] <- "2"
    df[,i] <- str_replace_all(df[,i],"[*]","")
    df[,i] <- str_replace_all(df[,i],"[`]","")
  }
  
  
  #GETTING PERCENTAGE
  df <- df %>%
    mutate_at(vars(starts_with("p_") & !ends_with("uncertainty")), as.numeric)
  
  df <- df %>% 
    mutate(across(starts_with("p_") & !ends_with("uncertainty"), ~ round(./100, 2)))
  
 
  return(df)
}


df1 <- reshape_data(df1)
df2 <- reshape_data(df2)

# REMOVING, COMBINING COLUMNS --------------------------------------------------

# 
# Table AOD status by state
# 
# N and p_14-24_current smoker_state 
# 
# N and p_14-24_never smoked_state
# 
# N and p_14-24_current vaper_state
# 
# N and p_14-24_never vaped_state
# 
# N and p_14-24_current drinker_state
# 
# N and p_14-24_never drinker_state
# 
# N and p_14-24_ever used illicit drug_state s
# 
# N and p_14-24_never used illicit drugs_state
# 
# N and p_14-24_recently used cannabis_state
# 
# N and p_14-24_recently used illicit drugs_state
# 

# REMOVING UNWANTED COLUMNS, TO MATCH ABOVE
df1_newcols <- df1[, !grepl("_ex_|pharmaceuticals_for_non_medical_purposes|_no", colnames(df1))]


# 
# Table AOD Qs by state
# 
# N (Mean)_14-24_age of initiation of drinking_state
# 
# N (Mean)_14-24_Age of initiation of illicit drug use - recent
# 
# P_14-24_Type of alcohol usually consumed_regular strength beer_state
# 
# P_14-24_Type of alcohol usually consumed_bottled spirits and liquers_state
# 
# P_14-24_Type of alcohol usually consumed_bottled wine_state
# 
# P_14-24_Type of alcohol usually consumed_pre-mixed spirits_state (combine columns ‘I’ and ‘K’)
# 
# P_14-24_ Cannabis use frequency(g) - Once a week or more
# 
# P_14-24_ Cannabis use frequency(g) - About once a month
# 
# P_14-24_ Cannabis use frequency(g) – Every few months or less (combine columns ‘S’ and ‘T’)


# REMOVING UNWANTED COLUMNS, TO MATCH ABOVE
df2_newcols <- df2[, !grepl("col1|col2|col3", colnames(df2))]




coln1 <- c("STE_CODE16", "calendar_year", 
           "n_current_smoker", "p_current_smoker",
           "n_ex_smoker", "p_ex_smoker",
           "n_never_smoked", "p_never_smoked",
           "n_current_vaper", "p_current_vaper",
           "n_ex_vaper", "p_ex_vaper",
           "n_never_vaped", "p_never_vaped", 
           "n_current_drinker", "p_current_drinker", 
           "n_ex_drinker", "p_ex_drinker", "n_never_drinker", "p_never_drinker",
           "n_ever_used_illicit_drugs_yes", "p_ever_used_illicit_drugs_yes", 
           "n_ever_used_illicit_drugs_no", "p_ever_used_illicit_drugs_no",
           "n_ever_used_pharmaceuticals_for_non_medical_purposes_yes",
           "p_ever_used_pharmaceuticals_for_non_medical_purposes_yes",
           "n_ever_used_pharmaceuticals_for_non_medical_purposes_no", 
           "p_ever_used_pharmaceuticals_for_non_medical_purposes_no", 
           "n_recently_used_illicit_drugs_yes",
           "p_recently_used_illicit_drugs_yes", 
           "n_recently_used_illicit_drugs_no", 
           "p_recently_used_illicit_drugs_no", 
           "n_recently_used_cannabis_yes", 
           "p_recently_used_cannabis_yes", 
           "n_recently_used_cannabis_no", 
           "p_recently_used_cannabis_no")


coln2 <- c("STE_CODE16", "calendar_year", 
           "n_age_of_initiation_of_smoking", "n_age_of_initiation_of_drinking",
           "p_type_of_alcohol_usually_consumed_bottled_wine", 
           "p_type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", 
           "p_type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", 
           "p_type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
           "p_type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", 
           "p_type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", 
           "p_type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
           "p_type_of_alcohol_usually_consumed_cider", 
           "p_type_of_alcohol_usually_consumed_other", 
           "n_age_of_initiation_of_illicit_drug_use_lifetime", 
           "n_age_of_initiation_of_illicit_drug_use_recent",
           "p_cannabis_use_frequency_every_day", 
           "p_cannabis_use_frequency_once_a_week_or_more", 
           "p_cannabis_use_frequency_about_once_a_month", 
           "p_cannabis_use_frequency_every_few_months", 
           "p_cannabis_use_frequency_once_or_twice_a_year")









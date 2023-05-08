
#Harriette's WD
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NDSHS_STE_National")

#national in state col - remove?


# DF NAMES ---------------------------------------------------------------------

#df1 = AOD status by state, Sheet 2
#df2 = AOD Qs by state, Sheet 3
#df3 = AOD Status - disaggs (national, age), Sheet 4
#df4 = AOD Status - disaggs (national, sex), Sheet 4
#df5 = AOD Status - disaggs (national, IRSD), Sheet 4
#df6 = AOD Status - disaggs (national, age), Sheet 5
#df7 = AOD Status - disaggs (national, sex), Sheet 5
#df8 = AOD Status - disaggs (national, IRSD), Sheet 5

#IRSD NOT INCLUDED IN PROTOTYPE, DATA READ IN BUT NOT DEALT W/ FURTHER

# LIBRARIES --------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)

# CLEANING CODE ----------------------------------------------------------------
# COLUMNS NAMES  ---------------------------------------------------------------


coln1 <- c("STE_CODE16", "calendar_year", 
           "n_current_smoker", "p_current_smoker", "n_ex_smoker", "p_ex_smoker", "n_never_smoked", "p_never_smoked",
           "n_current_vaper", "p_current_vaper", "n_ex_vaper", "p_ex_vaper", "n_never_vaped", "p_never_vaped", 
           "n_current_drinker", "p_current_drinker", "n_ex_drinker", "p_ex_drinker", "n_never_drinker", "p_never_drinker",
           "n_ever_used_illicit_drugs_yes", "p_ever_used_illicit_drugs_yes", "n_ever_used_illicit_drugs_no", "p_ever_used_illicit_drugs_no",
           "n_ever_used_pharmaceuticals_for_non_medical_purposes_yes", "p_ever_used_pharmaceuticals_for_non_medical_purposes_yes", "n_ever_used_pharmaceuticals_for_non_medical_purposes_no", "p_ever_used_pharmaceuticals_for_non_medical_purposes_no", 
           "n_recently_used_illicit_drugs_yes", "p_recently_used_illicit_drugs_yes", "n_recently_used_illicit_drugs_no", "p_recently_used_illicit_drugs_no", 
           "n_recently_used_cannabis_yes", "p_recently_used_cannabis_yes", "n_recently_used_cannabis_no", "p_recently_used_cannabis_no")

coln1.1 <- c("age_group",coln1[2:length(coln1)])
coln1.2 <- c("sex",coln1[2:length(coln1)])
#coln1.3 <- c("irsd_quintile",coln1[2:length(coln1)])


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


coln2.1 <- c("age_group",coln2[2:length(coln2)])
coln2.2 <- c("sex",coln2[2:length(coln2)])
#coln2.3 <- c("irsd_quintile",coln2[2:length(coln2)])


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

#NATIONAL ----------------------------------------------------------------------

df3 <- cleaning(4, "A7:AJ16", coln1.1) #AOD Status - disaggs (national, age)

df3$Australia <- 0

df4 <- cleaning(4, "A44:AJ53", coln1.2) #AOD Status - disaggs (national, sex) 

df4$Australia <- 0

#df5 <- cleaning(4, "A62:AJ86", coln1.3) #AOD Status - disaggs (national, IRSD) 
df6 <- cleaning(5, "A7:T16", coln2.1) #AOD Qs - disaggs (national, age)

df6$Australia <- 0

df7 <- cleaning(5, "A39:T48", coln2.2) #AOD Qs - disaggs (national, sex)

df7$Australia <- 0
#df8 <- cleaning(5, "A57:T81", coln2.3) #AOD Qs - disaggs (national, IRSD)


# RESHAPING DATA ---------------------------------------------------------------

reshape_data <- function(df){
  
  
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
  
  # ROUNDING:
  for(i in seq(3,ncol(df),2)){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
  
  
 #GETTING PERCENTAGE
 df <- df %>% mutate(across(starts_with("p_")& !ends_with("uncertainty"), ~ round(./100, 2)))
  
  return(df)
}


df1 <- reshape_data(df1)
df2 <- reshape_data(df2)
df3 <- reshape_data(df3)
df4 <- reshape_data(df4)
df6 <- reshape_data(df6)
df7 <- reshape_data(df7)


# SPLITTING BY INDICATOR -------------------------------------------------------

smoking1 <- c("n_current_smoker", "n_current_smoker_uncertainty",
              "p_current_smoker", "p_current_smoker_uncertainty",
              "n_ex_smoker", "n_ex_smoker_uncertainty",
              "p_ex_smoker", "p_ex_smoker_uncertainty",                                             
              "n_never_smoked", "n_never_smoked_uncertainty",                                          
              "p_never_smoked", "p_never_smoked_uncertainty",                                          
              "n_current_vaper", "n_current_vaper_uncertainty",                                         
              "p_current_vaper", "p_current_vaper_uncertainty",                                         
              "n_ex_vaper", "n_ex_vaper_uncertainty",                                              
              "p_ex_vaper", "p_ex_vaper_uncertainty",                                              
              "n_never_vaped", "n_never_vaped_uncertainty",                                          
              "p_never_vaped", "p_never_vaped_uncertainty")

smoking2 <- c("n_age_of_initiation_of_smoking", "n_age_of_initiation_of_smoking_uncertainty")


drinking1 <- c("n_current_drinker","n_current_drinker_uncertainty",
               "p_current_drinker", "p_current_drinker_uncertainty",
               "n_ex_drinker", "n_ex_drinker_uncertainty",
               "p_ex_drinker", "p_ex_drinker_uncertainty",
               "n_never_drinker", "n_never_drinker_uncertainty",
               "p_never_drinker", "p_never_drinker_uncertainty")

drinking2 <- c("n_age_of_initiation_of_drinking", "n_age_of_initiation_of_drinking_uncertainty",
               "p_type_of_alcohol_usually_consumed_bottled_wine", "p_type_of_alcohol_usually_consumed_bottled_wine_uncertainty",
               "p_type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "p_type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol_uncertainty",
               "p_type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "p_type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol_uncertainty",
               "p_type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol", "p_type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol_uncertainty",
               "p_type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "p_type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can_uncertainty", 
               "p_type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "p_type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs_uncertainty",
               "p_type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle", "p_type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle_uncertainty", 
               "p_type_of_alcohol_usually_consumed_cider", "p_type_of_alcohol_usually_consumed_cider_uncertainty",
               "p_type_of_alcohol_usually_consumed_other", "p_type_of_alcohol_usually_consumed_other_uncertainty")

drugs1 <- c('n_ever_used_illicit_drugs_yes',
             'n_ever_used_illicit_drugs_yes_uncertainty',
             'n_ever_used_illicit_drugs_yes',
             'n_ever_used_illicit_drugs_yes_uncertainty',
             'n_ever_used_illicit_drugs_no',
             'n_ever_used_illicit_drugs_no_uncertainty',
             'n_ever_used_illicit_drugs_no',
             'n_ever_used_illicit_drugs_no_uncertainty',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_yes',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_yes_uncertainty',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_yes',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_yes_uncertainty',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_no',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_no_uncertainty',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_no',
             'n_ever_used_pharmaceuticals_for_non_medical_purposes_no_uncertainty',
             'n_recently_used_illicit_drugs_yes',
             'n_recently_used_illicit_drugs_yes_uncertainty',
             'n_recently_used_illicit_drugs_yes',
             'n_recently_used_illicit_drugs_yes_uncertainty',
             'n_recently_used_illicit_drugs_no',
             'n_recently_used_illicit_drugs_no_uncertainty',
             'n_recently_used_illicit_drugs_no',
             'n_recently_used_illicit_drugs_no_uncertainty',
             'n_recently_used_cannabis_yes',
             'n_recently_used_cannabis_yes_uncertainty',
             'n_recently_used_cannabis_yes',
             'n_recently_used_cannabis_yes_uncertainty',
             'n_recently_used_cannabis_no',
             'n_recently_used_cannabis_no_uncertainty',
             'n_recently_used_cannabis_no',
             'n_recently_used_cannabis_no_uncertainty')


drugs2 <- c("n_age_of_initiation_of_illicit_drug_use_lifetime", "n_age_of_initiation_of_illicit_drug_use_lifetime_uncertainty",
          "n_age_of_initiation_of_illicit_drug_use_recent", "n_age_of_initiation_of_illicit_drug_use_recent_uncertainty",
          "p_cannabis_use_frequency_every_day", "p_cannabis_use_frequency_once_a_week_or_more",
          "p_cannabis_use_frequency_about_once_a_month", "p_cannabis_use_frequency_every_few_months", "p_cannabis_use_frequency_once_or_twice_a_year")
          

cigs1 <- c("n_current_vaper", "n_current_vaper_uncertainty",
              "p_current_vaper", "p_current_vaper_uncertainty",
              "n_ex_vaper", "n_ex_vaper_uncertainty",
              "p_ex_vaper", "p_ex_vaper_uncertainty",                                             
              "n_never_vaped", "n_never_vaped_uncertainty",                                          
              "p_never_vaped", "p_never_vaped_uncertainty")

ste <- c("STE_CODE16","calendar_year")
nat <- c("Australia", "calendar_year")
age <- c("age_group")
sex <- c("sex")
irsd <- c("irsd_quintile","calendar_year")


# 1.9.1 substance abuse: SMOKING -----------------------------------------------

#reading in new data frames for smoking variables only -------------------------

#STATE -------------------------------------------------------------------------
a191 <- df1[,c(ste, smoking1)]
b191 <- df2[,c(ste, smoking2)]

ste191 <- merge(a191,b191, by = intersect(names(a191), names(b191)))
  
#NATIONAL ----------------------------------------------------------------------

#CREATE SUB DATA SET BY INDICATOR
c191 <- df3[,c(nat, age, smoking1)]
d191 <- df4[,c(nat, sex, smoking1)]
f191 <- df6[,c(nat, age, smoking2)]
g191 <- df7[,c(nat, sex, smoking2)]

#ADDING A AGE COL SO THE TWO DATA FRAMES MATCH
d191$age_group <- NA
c191$sex <- NA
f191$sex <- NA
g191$age_group <- NA

#RBIND TOGETHER
cd191 <- rbind(c191,d191)
fg191 <- rbind(f191,g191)

#CREATING FULL DATA SET
nat191 <- merge(fg191,cd191, by = intersect(names(fg191), names(cd191)), all.x = T)

# 1.9.2 substance abuse: DRINKING ----------------------------------------------

#reading in new data frames for drinking variables only ------------------------

#STATE -------------------------------------------------------------------------
a192 <- df1[,c(ste, drinking1)]
b192 <- df2[,c(ste, drinking2)]

ste192 <- merge(a192,b192, by = intersect(names(a192), names(b192)))

#NATIONAL ----------------------------------------------------------------------

#CREATE SUB DATA SET BY INDICATOR
c192 <- df3[,c(nat, age, drinking1)]
d192 <- df4[,c(nat, sex, drinking1)]
f192 <- df6[,c(nat, age, drinking2)]
g192 <- df7[,c(nat, sex, drinking2)]

#ADDING A AGE COL SO THE TWO DATA FRAMES MATCH
d192$age_group <- NA
c192$sex <- NA
f192$sex <- NA
g192$age_group <- NA

#RBIND TOGETHER
cd192 <- rbind(c192,d192)
fg192 <- rbind(f192,g192)

#CREATING FULL DATA SET
nat192 <- merge(fg192,cd192, by = intersect(names(fg192), names(cd192)), all.x = T)


# 1.9.3 substance abuse: Drugs -------------------------------------------------

#reading in new data frames for smoking variables only -------------------------


# STATE ------------------------------------------------------------------------
a193 <- df1[,c(ste, drugs1)]
b193 <- df2[,c(ste, drugs2)]

ste193 <- merge(a193,b193, by = intersect(names(a193), names(b193)))

# NATIONAL ---------------------------------------------------------------------

#CREATE SUB DATA SET BY INDICATOR
c193 <- df3[,c(nat, age, drugs1)]
d193 <- df4[,c(nat, sex, drugs1)]
f193 <- df6[,c(nat, age, drugs2)]
g193 <- df7[,c(nat, sex, drugs2)]

#ADDING A AGE COL SO THE TWO DATA FRAMES MATCH
d193$age_group <- NA
c193$sex <- NA
f193$sex <- NA
g193$age_group <- NA

#RBIND TOGETHER
cd193 <- rbind(c193,d193)
fg193 <- rbind(f193,g193)

#CREATING FULL DATA SET
nat193 <- merge(fg193,cd193, by = intersect(names(fg193), names(cd193)), all.x = T)

# 1.9.4 substance abuse: e-cigarettes -------------------------------------------------

#reading in new data frames for smoking variables only -------------------------

# STATE ------------------------------------------------------------------------

a194 <- df1[,c(ste, cigs1)]

# RENAMING DF
ste194 <- a194


# NATIONAL ---------------------------------------------------------------------

#CREATE SUB DATA SET BY INDICATOR
c194 <- df3[,c(nat, age, cigs1)]
d194 <- df4[,c(nat, sex, cigs1)]

#ADDING A AGE COL SO THE TWO DATA FRAMES MATCH
d194$age_group <- NA
c194$sex <- NA

#RBIND TOGETHER
cd194 <- rbind(c194,d194)

#CREATING FULL DATA SET
nat194 <- cd194


#CHANGING COL ORDER

corder <- c("Australia", "calendar_year", "age_group", "sex","n_current_vaper", "n_current_vaper_uncertainty", "p_current_vaper", "p_current_vaper_uncertainty", "n_ex_vaper", "n_ex_vaper_uncertainty", "p_ex_vaper", "p_ex_vaper_uncertainty", "n_never_vaped", "n_never_vaped_uncertainty", "p_never_vaped", "p_never_vaped_uncertainty")
nat194 <- nat194[,corder]



# WRITING CSVS -----------------------------------------------------------------

write.csv(ste191, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_191_smoking_STE.csv", row.names = F)
write.csv(ste192, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_192_alcohol_STE.csv", row.names = F)
write.csv(ste193, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_193_drugs_STE.csv", row.names = F)
write.csv(ste194, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_194_drugs_STE.csv", row.names = F)

write.csv(nat191, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_191_smoking_National.csv", row.names = F)
write.csv(nat192, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_192_alcohol_National.csv", row.names = F)
write.csv(nat193, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_193_drugs_National.csv", row.names = F)
write.csv(nat194, file = "../../../Data_Collections_READY_FOR_QA/NDSHS/NDSHS_194_drugs_National.csv", row.names = F)



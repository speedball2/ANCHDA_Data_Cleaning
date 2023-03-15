#------------------------------------------------------------------------------#
#--------------------------------NDSHS Cleaning--------------------------------#
#------------------------------------------------------------------------------#



#dfnames-----------------------------------------------------------------------

#df1 = AOD status by state, Sheet 2
#df2 = AOD Qs by state, Sheet 3
#df3 = AOD Status - disaggs (national, age), Sheet 4
#df4 = AOD Status - disaggs (national, sex), Sheet 4
#df5 = AOD Status - disaggs (national, IRSD), Sheet 4
#df6 = AOD Status - disaggs (national, age), Sheet 5
#df7 = AOD Status - disaggs (national, sex), Sheet 5
#df8 = AOD Status - disaggs (national, IRSD), Sheet 5



# Before running, set working directory to source file location.
# Session > Set Working Directory > To Source File Location



# Libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


#-------------------------#
#---AOD Status by State---# #Sheet 2
#-------------------------#



# Open file --------------------------------------------------------------------
df1 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 2, #AOD State by Status
                 range = "A6:AJ50")

# Remove NA and NP strings -----------------------------------------------------
df1[df1[,] == "n.a."] <- NA
df1[df1[,] == "n.p."] <- NA

# seperating asterisks and adding new column -----------------------------------

#testing code 
#substr(df1[,6],1,1)
#which(substr(df1[,6],1,1)=="**")
#which(substr(df1[,6],1,1)=="*")
#which(substr(df1[,6],1,2)=="**")

#ready to implement ---------------------

#df1$uncertainty <- NA #new column for uncertainty 

#df1[c(which(substr(df1[, 6],1,1) == "*")), "uncertainty"] = "*"
#df1[c(which(substr(df1[, 6],1,2)=="**")), "uncertainty"] = "**"


#col_order <- c(37, 1:36) #used numbers because column names have not been assigned yet

#df1 <- df1[col_order]


# Remove asterisks -------------------------------------------------------------

for(i in 2:ncol(df1)){
  df1[,i] <- str_replace_all(df1[,i],"[*]","")
  df1[,i] <- as.numeric(df1[,i])
  df1[,i] <- round(df1[,i],2)
}

# Change columns ---------------------------------------------------------------
colnames(df1) <- c("STE_NAME16", "calendar_year", 
                   "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                   "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                   "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                   "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                   "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                   "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                   "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

#for(i in 2:ncol(df1)){
#  df1[,i] <- str_replace_all(df1[,i],["ever_used"],"")

#  for(i in 2:ncol(df1)){
#    df1[,i] <- str_replace_all(df1[,i],["recently_used"],"")



# Fill state names -------------------------------------------------------------
for (i in 2:nrow(df1)){
  if(is.na(df1[i,1])){
    df1[i,1] <- df1[i-1,1]
  }
}

# Rename state names -----------------------------------------------------------
df1[,1][which(df1[,1] == "Qld")] <- "QLD"
df1[,1][which(df1[,1] == "Vic")] <- "VIC"
df1[,1][which(df1[,1] == "Tas")] <- "TAS"
df1[,1][which(df1[,1] == "NT(l)")] <- "NT"
df1[,1][which(df1[,1] == "National")] <- "Australia"

# Add state codes --------------------------------------------------------------

dummy <- c()
dummy$STE_NAME16 <- unique(df1$STE_NAME16)
dummy$STE_CODE16 <- c(1,2,3,5,4,6,8,7,10) # AUS set to 10, changed to 0 later.
dummy <- as.data.frame(dummy)

# Sort by state code -----------------------------------------------------------

df1 <- merge(dummy,df1)
df1 <- df1[order(df1$STE_CODE16),]
rownames(df1) <- 1:nrow(df1)
df1[,2][which(df1[,2] == 10)] <- 0 # Changing AUS code to 0.

df1$STE_NAME16 <- NULL




















#---------------------#
#---AOD Qs by state---# #Sheet 3
#---------------------#



df2 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 3, #AOD State by Status 
                 range = "A6:T50")

# Remove NA and NP strings -----------------------------------------------------
df2[df2[,] == "n.a."] <- NA
df2[df2[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------
for(i in 2:ncol(df2)){
  df2[,i] <- str_replace_all(df2[,i],"[*]","")
  df2[,i] <- str_replace_all(df2[,i],"[`]","")
  df2[,i] <- as.numeric(df2[,i])
  df2[,i] <- round(df2[,i],2)
}

# name columns -----------------------------------------------------------------
colnames(df2) <- c("STE_NAME16","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                   "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                   "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                   "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                   "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a_month", 
                   "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")



#fill down to fill in missing---------------------------------------------------
df2 <- df2 %>% fill("STE_NAME16", .direction = "down")


# Rename state names -----------------------------------------------------------

df2[,1][which(df2[,1] == "NT(h)")] <- "NT"
df2[,1][which(df2[,1] == "National")] <- "Australia"



# Add state codes --------------------------------------------------------------
dummy2 <- c()
dummy2$STE_NAME16 <- unique(df1$STE_NAME16)
dummy2$STE_CODE16 <- c(1,2,3,5,4,6,8,7,0) # AUS set to 10, changed to 0 later.
dummy2 <- as.data.frame(dummy2)

# Harriette to implement cleaner.
df2 <- merge(dummy2,df2)
df2$STE_NAME16 <- NULL


#---------------------------#
#---AOD Status - disaggs ---# #Sheet 4
#---    national, age    ---#
#---------------------------#

df3 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 4, #AOD State by Status 
                 range = "A7:AJ16")

# Remove NA and NP strings -----------------------------------------------------
df3[df3[,] == "n.a."] <- NA
df3[df3[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------
for(i in 2:ncol(df3)){
  df3[,i] <- str_replace_all(df3[,i],"[*]","")
  df3[,i] <- as.numeric(df3[,i])
  df3[,i] <- round(df3[,i],2)
}

# name columns -----------------------------------------------------------------
colnames(df3) <- c("age_group","calendar_year",
                   "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                   "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                   "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                   "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                   "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                   "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                   "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

#fill down to fill in missing---------------------------------------------------

df3 <- df3 %>% fill("age_group", .direction = "down")

# adding national "0" column ---------------------------------------------------

df3$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","age_group","calendar_year",
                "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%") 

df3 <- df3[, col_order]  #flipping columns 




#---------------------------#
#---AOD Status - disaggs ---# #Sheet 4
#---    national, sex    ---#
#---------------------------#

df4 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 4, #AOD State by Status 
                 range = "A44:AJ53")

# Remove NA and NP strings -----------------------------------------------------
df4[df4[,] == "n.a."] <- NA
df4[df4[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------


for(i in 2:ncol(df4)){
  df4[,i] <- str_replace_all(df4[,i],"[*]","")
  df4[,i] <- as.numeric(df4[,i])
  df4[,i] <- round(df4[,i],2)
}


# name columns -----------------------------------------------------------------
colnames(df4) <- c("sex","calendar_year",
                   "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                   "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                   "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                   "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                   "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                   "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                   "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")


#fill down to fill in missing---------------------------------------------------
df4 <- df4 %>% fill("sex", .direction = "down")

# adding national column -------------------------------------------------------

df4$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","sex","calendar_year",
                "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

df4 <- df4[, col_order]  #flipping columns 


#---------------------------#
#---AOD Status - disaggs ---# #Sheet 4
#---    national, IRSD    ---#
#---------------------------#

df5 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 4, #AOD State by Status 
                 range = "A62:AJ86")

# Remove NA and NP strings -----------------------------------------------------
df5[df5[,] == "n.a."] <- NA
df5[df5[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------

for(i in 2:ncol(df5)){
  df5[,i] <- str_replace_all(df5[,i],"[*]","")
  df5[,i] <- as.numeric(df5[,i])
  df5[,i] <- round(df5[,i],2)
}


# name columns -----------------------------------------------------------------
colnames(df5) <- c("irsd_quintile","calendar_year", 
                   "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                   "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                   "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                   "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                   "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                   "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                   "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

# Rename IRSD quintiles --------------------------------------------------------
df5[,1][which(df5[,1] == "Lowest socioeconomic quintile")] <- "1"
df5[,1][which(df5[,1] == "Highest socioeconomic quintile")] <- "5"

#fill down to fill in missing --------------------------------------------------
df5 <- df5 %>% fill("irsd_quintile", .direction = "down") 

# adding national column -------------------------------------------------------

df5$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","irsd_quintile","calendar_year",
                "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
                "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
                "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
                "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
                "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
                "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
                "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

df5 <- df5[, col_order]  #flipping columns 

#---------------------------#
#---AOD Status - disaggs ---# #Sheet 5
#---    national, age    ---#
#---------------------------#

df6 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 5, #AOD State by Status 
                 range = "A7:T16")

# Remove NA and NP strings -----------------------------------------------------
df6[df6[,] == "n.a."] <- NA
df6[df6[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------

for(i in 2:ncol(df6)){
  df6[,i] <- str_replace_all(df6[,i],"[*]","")
  df6[,i] <- as.numeric(df6[,i])
  df6[,i] <- round(df6[,i],2)
}


# name columns -----------------------------------------------------------------
colnames(df6) <- c("age_group","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                   "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                   "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                   "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                   "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                   "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

#fill down to fill in missing---------------------------------------------------
df6 <- df6 %>% fill("age_group", .direction = "down") 


# adding national column -------------------------------------------------------

df6$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","age_group","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

df6 <- df6[, col_order]  #flipping columns 
#---------------------------#
#---AOD Status - disaggs ---# #Sheet 5
#---    national,sex    ---#
#---------------------------#

df7 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 5, #AOD State by Status 
                 range = "A39:T48")

# Remove NA and NP strings -----------------------------------------------------
df7[df7[,] == "n.a."] <- NA
df7[df7[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------

for(i in 2:ncol(df7)){
  df7[,i] <- str_replace_all(df7[,i],"[*]","")
  df7[,i] <- as.numeric(df7[,i])
  df7[,i] <- round(df7[,i],2)
}


# name columns -----------------------------------------------------------------
colnames(df7) <- c("sex","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                   "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                   "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                   "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                   "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                   "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

#fill down to fill in missing---------------------------------------------------

df7 <- df7 %>% fill("sex", .direction = "down") 

# adding national column -------------------------------------------------------

df7$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","sex","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

df7 <- df7[, col_order]  #flipping columns 


#---------------------------#
#---AOD Status - disaggs ---# #Sheet 5
#---    national,IRSD    ---#
#---------------------------#


df8 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 5, #AOD State by Status 
                 range = "A57:T81")

# Remove NA and NP strings -----------------------------------------------------
df8[df8[,] == "n.a."] <- NA
df8[df8[,] == "n.p."] <- NA

# Remove asterisks -------------------------------------------------------------

for(i in 2:ncol(df8)){
  df8[,i] <- str_replace_all(df8[,i],"[*]","")
  df8[,i] <- as.numeric(df8[,i])
  df8[,i] <- round(df8[,i],2)
}

# name columns -----------------------------------------------------------------
colnames(df8) <- c("irsd_quintile","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                   "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                   "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                   "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                   "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                   "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

# Rename IRSD quintiles --------------------------------------------------------
df8[,1][which(df8[,1] == "Lowest SES")] <- "1"
df8[,1][which(df8[,1] == "Highest SES")] <- "5"

#fill down to fill in missing---------------------------------------------------

df8 <- df8 %>% fill("irsd_quintile", .direction = "down")

# adding national column -------------------------------------------------------

df8$STE_CODE16 <- 0

col_order <- c ("STE_CODE16","irsd_quintile","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
                "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
                "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
                "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
                "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
                "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

df8 <- df8[, col_order]  #flipping columns 



#-----------------------------------------#
#--- combining for VISER: by indicator ---#
#-----------------------------------------#


# 1.9.1 substance abuse: SMOKING -----------------------------------------------

#reading in new data frames for smoking variables only -------------------------

#calendar_year
#age_group
#sex
#irsd_quintiles


#old code 

#a191 <- df1[,c("STE_CODE16","calendar_year",
#               "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
#               "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%")]

#c191 <- df3[,c("STE_CODE16","age_group","calendar_year",
#               "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
#               "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%")]

#d191 <- df4[,c("STE_CODE16","sex","calendar_year",
#               "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
#               "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%")]

#e191 <- df5[,c("STE_CODE16","irsd_quintile","calendar_year",
#              "current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
#               "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%")]

#b191 <- df2[,c("STE_CODE16","calendar_year","age_of_initiation_of_smoking")]

#f191 <- df6[,c("STE_CODE16","age_group","calendar_year","age_of_initiation_of_smoking")]

#g191 <- df7[,c("STE_CODE16","sex","calendar_year","age_of_initiation_of_smoking")]

#h191 <- df8[,c("STE_CODE16","irsd_quintile","calendar_year","age_of_initiation_of_smoking")]


smoking1 <- c("current_smoker_N","current_smoker_%", "ex-smoker_N","ex-smoker_%","never_smoked_N", "never_smoked_%",
              "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%")
smoking2 <- c("age_of_initiation_of_smoking")
calendar <- c("STE_CODE16","calendar_year")
age <- c("STE_CODE16","age_group","calendar_year")
sex <- c("STE_CODE16","sex","calendar_year")
irsd <- c("STE_CODE16","irsd_quintile","calendar_year")


a191 <- df1[,c(calendar, smoking1)]
c191 <- df3[,c(age, smoking1)]
d191 <- df4[,c(sex, smoking1)]
e191 <- df5[,c(irsd, smoking1)]

b191 <- df2[,c(calendar, smoking2)]
f191 <- df6[,c(age, smoking2)]
g191 <- df7[,c(sex, smoking2)]
h191 <- df8[,c(irsd, smoking2)]


#merging the data frames together ----------------------------------------------

#letter combining 
# a
#      ac
# c       acde
# d
#     de
# e
#---      ---     full
# b
#     bf
# f
# g       bfgh
#     gh
# h

ac191 <- merge(a191,c191,by = intersect(names(a191), names(c191)), all.x = T)
de191 <- merge(d191,e191,by = intersect(names(d191), names(b191)), all.x = T)
bf191 <- merge(b191,f191,by = intersect(names(b191), names(f191)), all.x = T)
gh191 <- merge(g191,h191,by = intersect(names(b191), names(f191)), all.x = T)

acde191 <- merge(ac191,de191,by = intersect(names(ac191), names(de191)), all.x = T)
bfgh191 <- merge(bf191,gh191,by = intersect(names(bf191), names(gh191)), all.x = T)

full191 <-  merge(acde191,bfgh191,by = intersect(names(acde191), names(bfgh191)), all.x = T)



# 1.9.1 substance abuse: DRINKING ----------------------------------------------

#reading in new data frames for drinking variables only ------------------------


#calendar_year
#age_group
#sex
#irsd_quintiles


drinking1 <- c("current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%")
drinking2 <- c("age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
               "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
               "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
               "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other")
calendar <- c("STE_CODE16","calendar_year")
age <- c("STE_CODE16","age_group","calendar_year")
sex <- c("STE_CODE16","sex","calendar_year")
irsd <- c("STE_CODE16","irsd_quintile","calendar_year")

a192 <- df1[,c(calendar, drinking1)]
c192 <- df3[,c(age, drinking1)]
d192 <- df4[,c(sex, drinking1)]
e192 <- df5[,c(irsd, drinking1)]

b192 <- df2[,c(calendar, drinking2)]
f192 <- df6[,c(age, drinking2)]
g192 <- df7[,c(sex, drinking2)]
h192 <- df8[,c(irsd, drinking2)]


#merging the data frames together ----------------------------------------------

#letter combining 
# a
#      ac
# c       acde
# d
#     de
# e
#---      ---     full
# b
#     bf
# f
# g       bfgh
#     gh
# h

ac192 <- merge(a192,c192,by = intersect(names(a192), names(c192)), all.x = T)
de192 <- merge(d192,e192,by = intersect(names(d192), names(b192)), all.x = T)
bf192 <- merge(b192,f192,by = intersect(names(b192), names(f192)), all.x = T)
gh192 <- merge(g192,h192,by = intersect(names(b192), names(f192)), all.x = T)

acde192 <- merge(ac192,de192,by = intersect(names(ac192), names(de192)), all.x = T)
bfgh192 <- merge(bf192,gh192,by = intersect(names(bf192), names(gh192)), all.x = T)

full192 <-  merge(acde192,bfgh192,by = intersect(names(acde192), names(bfgh192)), all.x = T)


# 1.9.3 substance abuse: Drugs -------------------------------------------------

#reading in new data frames for smoking variables only -------------------------


drugs1 <- c("ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
            "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
            "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
            "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")


drugs2 <- c("age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
            "illicit_use_of_drugs_cannabis_use_frequency_every_day", "illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more", "illicit_use_of_drugs_cannabis_use_frequency_about_once_a
                   _month", 
            "illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")

drugs3 <- c("age_of_initiation_of_illicit_drug_use_lifetime","age_of_initiation_of_illicit_drug_use_recent","illicit_use_of_drugs_cannabis_use_frequency_every_day","illicit_use_of_drugs_cannabis_use_frequency_once_a_week_or_more",
            "illicit_use_of_drugs_cannabis_use_frequency_about_once_a_month","illicit_use_of_drugs_cannabis_use_frequency_every_few_months", "illicit_use_of_drugs_cannabis_use_frequency_once_or_twice_a_year")


calendar <- c("STE_CODE16","calendar_year")
age <- c("STE_CODE16","age_group","calendar_year")
sex <- c("STE_CODE16","sex","calendar_year")
irsd <- c("STE_CODE16","irsd_quintile","calendar_year")

a193 <- df1[,c(calendar, drugs1)]
c193 <- df3[,c(age, drugs1)]
d193 <- df4[,c(sex, drugs1)]
e193 <- df5[,c(irsd, drugs1)]

b193 <- df2[,c(calendar, drugs3)]
f193 <- df6[,c(age, drugs2)]
g193 <- df7[,c(sex, drugs2)]
h193 <- df8[,c(irsd, drugs2)]

# merging the data frames together ----------------------------------------------

#letter combining 
# a
#      ac
# c       acde
# d
#     de
# e
#---      ---     full
# b
#     bf
# f
# g       bfgh
#     gh
# h

ac193 <- merge(a193,c193,by = intersect(names(a193), names(c193)), all.x = T)
de193 <- merge(d193,e193,by = intersect(names(d193), names(b193)), all.x = T)
bf193 <- merge(b193,f193,by = intersect(names(b193), names(f193)), all.x = T)
gh193 <- merge(g193,h193,by = intersect(names(b193), names(f193)), all.x = T)

acde193 <- merge(ac193,de193,by = intersect(names(ac193), names(de193)), all.x = T)
bfgh193 <- merge(bf193,gh193,by = intersect(names(bf193), names(gh193)), all.x = T)

full193 <-  merge(acde193,bfgh193,by = intersect(names(acde193), names(bfgh193)), all.x = T)


#----------------------------------#
#--- writing csv : by indicator ---#
#----------------------------------#


# writing csv by indicator for fully merged data frames ------------------------



write.csv(full191, file = "NDSHS_191_smoking.csv", row.names = F)
write.csv(full192, file = "NDSHS_192_alcohol.csv", row.names = F)
write.csv(full193, file = "NDSHS_193_drugs.csv", row.names = F)






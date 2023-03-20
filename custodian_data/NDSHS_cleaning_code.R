#------------------------------------------------------------------------------#
#--------------------------------NDSHS Cleaning--------------------------------#
#------------------------------------------------------------------------------#


# Before running, set working directory to source file location.
# Session > Set Working Directory > To Source File Location


setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/from_custodians/NDSHS_STE_National")

#dfnames-----------------------------------------------------------------------

#df1 = AOD status by state, Sheet 2
#df2 = AOD Qs by state, Sheet 3
#df3 = AOD Status - disaggs (national, age), Sheet 4
#df4 = AOD Status - disaggs (national, sex), Sheet 4
#df5 = AOD Status - disaggs (national, IRSD), Sheet 4
#df6 = AOD Status - disaggs (national, age), Sheet 5
#df7 = AOD Status - disaggs (national, sex), Sheet 5
#df8 = AOD Status - disaggs (national, IRSD), Sheet 5



# Libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

coln1 <- c("STE_NAME16", "calendar_year", 
           "current_smoker_N","current_smoker_%", "ex_smoker_N","ex_smoker_%","never_smoked_N", "never_smoked_%",
           "current_vaper_N", "current_vaper_%","ex_vaper_N","ex_vaper_%", "never_vaped_N", "never_vaped_%", 
           "current_drinker_N","current_drinker_%", "ex_drinker_N","ex_drinker_%", "never_drinker_N", "never_drinker_%",
           "ever_used_illicit_drugs_YES_N","ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_%",
           "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N","ever_used_pharmaceuticals_for_non_medical_purposes_YES_%","ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", 
           "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_%", 
           "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_%", "recently_used_cannabis_NO_N","recently_used_cannabis_NO_%")

coln1.1 <- c("age_group",coln1[2:length(coln1)])
coln1.2 <- c("sex",coln1[2:length(coln1)])
coln1.3 <- c("irsd_quintile",coln1[2:length(coln1)])

coln2 <- c("STE_NAME16","calendar_year","age_of_initiation_of_smoking","age_of_initiation_of_drinking","type_of_alcohol_usually_consumed_bottled_wine",
           "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol",
           "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle",
           "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_other", "age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_recent",
           "cannabis_use_frequency_every_day", "cannabis_use_frequency_once_a_week_or_more", "cannabis_use_frequency_about_once_a_month", 
           "cannabis_use_frequency_every_few_months", "cannabis_use_frequency_once_or_twice_a_year")

coln2.1 <- c("age_group",coln2[2:length(coln2)])
coln2.2 <- c("sex",coln2[2:length(coln2)])
coln2.3 <- c("irsd_quintile",coln2[2:length(coln2)])



# Read in excel files ----------------------------------------------------------

xlxs <- function(x,y,nv){
  # x: Spreadsheet file.
  # y: Spreadsheet range.
  # nv: Names vector.
  
  # READ SHEET:
  df <- as.data.frame(read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                                sheet = x, 
                                range = y))
  
  # REMOVE NAs & NPs:
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  
  # NEW COLUMN NAMES:
  names(df) <- nv
  
  # ADD AND CALCULATE UNCERTAINTY
  for(i in ncol(df):3){
    # Insert column using cursed new notation: https://stackoverflow.com/questions/60311773/mutate-with-paste0
    df <- add_column(df, !!paste0(names(df)[i],"_uncertainty") := NA, .after = i)
    df[which(substr(df[, i],1,1) == "*"),i+1] <- "*"
    df[which(substr(df[, i],1,2) == "**"),i+1] <- "**"
    df[which(substr(df[, i],1,1) == "`"),i+1] <- "`"
    df[which(substr(df[, i],1,2) == "``"),i+1] <- "``"
    df[,i] <- str_replace_all(df[,i],"[*]","")
    df[,i] <- str_replace_all(df[,i],"[`]","")
  }
  
  # RECODING STATE NAMES TO CODES:
  if(names(df)[1] == "STE_NAME16"){
    df$STE_NAME16 <- recode(df$STE_NAME16,
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
                            "National" = 0) # TO BE FINISHED BY HARRIETTE.
  
  }
  
  # RECODING IRSD NAMES TO CODES:
  if(names(df)[1] == "irsd_quintile"){
    df$irsd_quintile <- recode(df$irsd_quintile,
                            "Lowest socioeconomic quintile" <- 1,
                            "Highest socioeconomic quintile" <- 5,
                            "Lowest SES" <- 1,
                            "Highest SES" <- 5) 
  
  
  
  
  
  # FILLING FIRST COLUMN:
  df <- df %>% fill(names(df)[1], .direction = "down")
  
  # ROUNDING:
  for(i in seq(3,ncol(df),2)){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
  
  # OUTPUT:
  return(df)
}

df1 <- xlxs(x = 2, y = "A6:AJ50", nv = coln1) #AOD status by state
df2 <- xlxs(x = 3, y = "A6:T50", nv = coln2) #AOD Qs by Status 
df3 <- xlxs(x = 4, y = "A7:AJ16", nv = coln1.1) #AOD Status - disaggs (national, age)
df4 <- xlxs(x = 4, y = "A44:AJ53", nv = coln1.2) #AOD Status - disaggs (national, sex) 
df5 <- xlxs(x = 4, y = "A62:AJ86", nv = coln1.3) #AOD Status - disaggs (national, IRSD) 
df6 <- xlxs(x = 5, y = "A7:T16", nv = coln2.1) #AOD Qs - disaggs (national, age)
df7 <- xlxs(x = 5, y = "A39:T48", nv = coln2.2) #AOD Qs - disaggs (national, sex)
df8 <- xlxs(x = 5, y = "A57:T81", nv = coln2.3) #AOD Qs - disaggs (national, IRSD)

# TO BE DONE
# Any column/row rearranging.
# Merging by indicators.
# Check old code for missed steps.
# Check guidelines for any other missed steps.
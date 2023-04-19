#------------------------------------------------------------------------------#
#--------------------------------NDSHS Cleaning--------------------------------#
#------------------------------------------------------------------------------#

#Aiden to review 
#renaming IRSD codes to names 

#TO DO
# add aus column to national data w/ 0 
#Seperate sheets for national and State 
#TOP 3 for main alcohol consumed 



# Before running, set working directory to source file location.
# Session > Set Working Directory > To Source File Location


#Harriette's WD
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NDSHS_STE_National")

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

cleaning <- function(x,y,nv){
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
                            "National" = 0) 
  
  }
  
  # RECODING IRSD NAMES TO CODES:
  # if(names(df)[1] == "irsd_quintile"){
  #   df$irsd_quintile <- recode(df$irsd_quintile,
  #                           "Lowest socioeconomic quintile" <- 1,
  #                           "Highest socioeconomic quintile" <- 5,
  #                           "Lowest SES" <- 1,
  #                           "Highest SES" <- 5,
  #                           "2" <- 2,
  #                           "3" <- 3, 
  #                           "4" <- 4) 
  #   
  # }
  
  
  
  # FILLING FIRST COLUMN:
  df <- df %>% fill(names(df)[1], .direction = "down")
  
  # RENAMING FIRST COLUMN TO CODE
  names(df1)[names(df1) == "STE_NAME16"] <- "STE_CODE16"
  names(df2)[names(df2) == "STE_NAME16"] <- "STE_CODE16"
  

  
  # ROUNDING:
  for(i in seq(3,ncol(df),2)){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
  

  
  # OUTPUT:
  return(df)
}

df1 <- xlxs(x = 2, y = "A6:AJ50", nv = coln1) #AOD status by state
df2 <- xlxs(x = 3, y = "A6:T50", nv = coln2) #AOD Qs by State 
df3 <- xlxs(x = 4, y = "A7:AJ16", nv = coln1.1) #AOD Status - disaggs (national, age)
df4 <- xlxs(x = 4, y = "A44:AJ53", nv = coln1.2) #AOD Status - disaggs (national, sex) 
df5 <- xlxs(x = 4, y = "A62:AJ86", nv = coln1.3) #AOD Status - disaggs (national, IRSD) 
df6 <- xlxs(x = 5, y = "A7:T16", nv = coln2.1) #AOD Qs - disaggs (national, age)
df7 <- xlxs(x = 5, y = "A39:T48", nv = coln2.2) #AOD Qs - disaggs (national, sex)
df8 <- xlxs(x = 5, y = "A57:T81", nv = coln2.3) #AOD Qs - disaggs (national, IRSD)

# TO BE DONE
# Any column/row rearranging. - does states need to be listed in correct order 1,2,3, etc
# Merging by indicators.
# Check old code for missed steps.-done 
# Check guidelines for any other missed steps.- done


# most common alcohol consumed top 3 - don't include others in final data set 


smoking1 <- c("current_smoker_N", "current_smoker_N_uncertainty",
              "current_smoker_%", "current_smoker_%_uncertainty",
              "ex_smoker_N", "ex_smoker_N_uncertainty",
              "ex_smoker_%", "ex_smoker_%_uncertainty",                                             
              "never_smoked_N", "never_smoked_N_uncertainty",                                          
               "never_smoked_%","never_smoked_%_uncertainty",                                          
              "current_vaper_N", "current_vaper_N_uncertainty",                                         
               "current_vaper_%", "current_vaper_%_uncertainty",                                         
               "ex_vaper_N", "ex_vaper_N_uncertainty",                                              
               "ex_vaper_%","ex_vaper_%_uncertainty",                                              
               "never_vaped_N","never_vaped_N_uncertainty",                                          
               "never_vaped_%",  "never_vaped_%_uncertainty" )

smoking2 <- c("age_of_initiation_of_smoking", "age_of_initiation_of_smoking_uncertainty")

drinking1 <- c("current_drinker_N","current_drinker_N_uncertainty",
               "current_drinker_%", "current_drinker_%_uncertainty",
               "ex_drinker_N", "ex_drinker_N_uncertainty",
               "ex_drinker_%", "ex_drinker_%_uncertainty",
               "never_drinker_N", "never_drinker_N_uncertainty",
               "never_drinker_%", "never_drinker_%_uncertainty")


drinking2 <- c("age_of_initiation_of_drinking", "age_of_initiation_of_drinking_uncertainty",
               "type_of_alcohol_usually_consumed_bottled_wine", "type_of_alcohol_usually_consumed_bottled_wine_uncertainty",
               "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol", "type_of_alcohol_usually_consumed_regular_strength_beer_greater_than_4%_alcohol_uncertainty",
               "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol", "type_of_alcohol_usually_consumed_mid_strength_beer_3%_to3.9%_alcohol_uncertainty",
               "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol", "type_of_alcohol_usually_consumed_low_alcohol_beer_1%_to_2.9%_alcohol_uncertainty",
               "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can", "type_of_alcohol_usually_consumed_pre-mixed_spirits_in_a_can_uncertainty", 
               "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs", "type_of_alcohol_usually_consumed_bottled_spirits_and_liqueurs_uncertainty",
               "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle", "type_of_alcohol_usually_consumed_pre_mixed_spirits_in_a_bottle_uncertainty", 
               "type_of_alcohol_usually_consumed_cider", "type_of_alcohol_usually_consumed_cider_uncertainty",
               "type_of_alcohol_usually_consumed_other", "type_of_alcohol_usually_consumed_other_uncertainty")

drugs1 <- c("ever_used_illicit_drugs_YES_N", "ever_used_illicit_drugs_YES_N_uncertainty",
            "ever_used_illicit_drugs_YES_%", "ever_used_illicit_drugs_YES_%_uncertainty",
            "ever_used_illicit_drugs_NO_N", "ever_used_illicit_drugs_NO_N_uncertainty",
            "ever_used_illicit_drugs_NO_%", "ever_used_illicit_drugs_NO_%_uncertainty",
            "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N",  "ever_used_pharmaceuticals_for_non_medical_purposes_YES_N_uncertainty",
            "ever_used_pharmaceuticals_for_non_medical_purposes_YES_%",  "ever_used_pharmaceuticals_for_non_medical_purposes_YES_%_uncertainty",
            "ever_used_pharmaceuticals_for_non_medical_purposes_NO_N", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_N_uncertainty", 
            "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%", "ever_used_pharmaceuticals_for_non_medical_purposes_NO_%_uncertainty", 
            "recently_used_illicit_drugs_YES_N", "recently_used_illicit_drugs_YES_N_uncertainty",
            "recently_used_illicit_drugs_YES_%", "recently_used_illicit_drugs_YES_%_uncertainty",
            "recently_used_illicit_drugs_NO_N", "recently_used_illicit_drugs_NO_N_uncertainty",
            "recently_used_illicit_drugs_NO_%", "recently_used_illicit_drugs_NO_%_uncertainty",
            "recently_used_cannabis_YES_N", "recently_used_cannabis_YES_N_uncertainty", 
            "recently_used_cannabis_YES_%", "recently_used_cannabis_YES_%_uncertainty", 
            "recently_used_cannabis_NO_N","recently_used_cannabis_NO_N_uncertainty",
            "recently_used_cannabis_NO_%", "recently_used_cannabis_NO_%_uncertainty")


drugs2 <- c("age_of_initiation_of_illicit_drug_use_lifetime", "age_of_initiation_of_illicit_drug_use_lifetime_uncertainty",
            "age_of_initiation_of_illicit_drug_use_recent", "age_of_initiation_of_illicit_drug_use_recent_uncertainty",
            "cannabis_use_frequency_every_day", 
            "cannabis_use_frequency_once_a_week_or_more",
            "cannabis_use_frequency_about_once_a_month", 
            "cannabis_use_frequency_every_few_months", 
            "cannabis_use_frequency_once_or_twice_a_year")


calendar <- c("STE_CODE16","calendar_year")
age <- c("age_group","calendar_year")
sex <- c("sex","calendar_year")
irsd <- c("irsd_quintile","calendar_year")

#MERGING BY INDICATOR FUCTION


# merging <- function(filter1, filter2){
#   
#   a <- df1[,c(calendar, filter1)]
#   c <- df3[,c(age, filter1)]
#   d <- df4[,c(sex, filter1)]
#   e <- df5[,c(irsd, filter1)]
#   
#   b <- df2[,c(calendar, filter2)]
#   f <- df6[,c(age, filter2)]
#   g <- df7[,c(sex, filter2)]
#   h <- df8[,c(irsd, filter2)]
#   
#   ac <- merge(a,c,by = intersect(names(a), names(c)), all.x = T)
#   de <- merge(d,e,by = intersect(names(d), names(b)), all.x = T)
#   bf <- merge(b,f,by = intersect(names(b), names(f)), all.x = T)
#   gh <- merge(g,h,by = intersect(names(b), names(f)), all.x = T)
#   
#   acde <- merge(ac,de,by = intersect(names(ac), names(de)), all.x = T)
#   bfgh <- merge(bf,gh,by = intersect(names(bf), names(gh)), all.x = T)
#   
#   full <-  merge(acde,bfgh,by = intersect(names(acde), names(bfgh)), all.x = T)
#   
#   return(df)
# }




# 1.9.1 substance abuse: SMOKING -----------------------------------------------

#reading in new data frames for smoking variables only -------------------------


  a191 <- df1[,c(calendar, smoking1)]
  c191 <- df3[,c(age, smoking1)]
  d191 <- df4[,c(sex, smoking1)]
  e191 <- df5[,c(irsd, smoking1)]
  
  b191 <- df2[,c(calendar, smoking2)]
  f191 <- df6[,c(age, smoking2)]
  g191 <- df7[,c(sex, smoking2)]
  h191 <- df8[,c(irsd, smoking2)]
  
  ac191 <- merge(a191,c191,by = intersect(names(a191), names(c191)), all.x = T)
  de191 <- merge(d191,e191,by = intersect(names(d191), names(b191)), all.x = T)
  bf191 <- merge(b191,f191,by = intersect(names(b191), names(f191)), all.x = T)
  gh191 <- merge(g191,h191,by = intersect(names(b191), names(f191)), all.x = T)
  
  acde191 <- merge(ac191,de191,by = intersect(names(ac191), names(de191)), all.x = T)
  bfgh191 <- merge(bf191,gh191,by = intersect(names(bf191), names(gh191)), all.x = T)
  
  full191 <-  merge(acde191,bfgh191,by = intersect(names(acde191), names(bfgh191)), all.x = T)
  
  # 1.9.1 substance abuse: DRINKING ----------------------------------------------
  
  #reading in new data frames for drinking variables only ------------------------
  
  
  a192 <- df1[,c(calendar, drinking1)]
  c192 <- df3[,c(age, drinking1)]
  d192 <- df4[,c(sex, drinking1)]
  e192 <- df5[,c(irsd, drinking1)]
  
  b192 <- df2[,c(calendar, drinking2)]
  f192 <- df6[,c(age, drinking2)]
  g192 <- df7[,c(sex, drinking2)]
  h192 <- df8[,c(irsd, drinking2)]
  
  ac192 <- merge(a192,c192,by = intersect(names(a192), names(c192)), all.x = T)
  de192 <- merge(d192,e192,by = intersect(names(d192), names(b192)), all.x = T)
  bf192 <- merge(b192,f192,by = intersect(names(b192), names(f192)), all.x = T)
  gh192 <- merge(g192,h192,by = intersect(names(b192), names(f192)), all.x = T)
  
  acde192 <- merge(ac192,de192,by = intersect(names(ac192), names(de192)), all.x = T)
  bfgh192 <- merge(bf192,gh192,by = intersect(names(bf192), names(gh192)), all.x = T)
  
  full192 <-  merge(acde192,bfgh192,by = intersect(names(acde192), names(bfgh192)), all.x = T)
  
  # 1.9.3 substance abuse: Drugs -------------------------------------------------
  
  #reading in new data frames for smoking variables only -------------------------
  
 
  a193 <- df1[,c(calendar, drugs1)]
  c193 <- df3[,c(age, drugs1)]
  d193 <- df4[,c(sex, drugs1)]
  e193 <- df5[,c(irsd, drugs1)]
  
  b193 <- df2[,c(calendar, drugs2)]
  f193 <- df6[,c(age, drugs2)]
  g193 <- df7[,c(sex, drugs2)]
  h193 <- df8[,c(irsd, drugs2)]
  
  ac193 <- merge(a193,c193,by = intersect(names(a193), names(c193)), all.x = T)
  de193 <- merge(d193,e193,by = intersect(names(d193), names(b193)), all.x = T)
  bf193 <- merge(b193,f193,by = intersect(names(b193), names(f193)), all.x = T)
  gh193 <- merge(g193,h193,by = intersect(names(b193), names(f193)), all.x = T)
  
  acde193 <- merge(ac193,de193,by = intersect(names(ac193), names(de193)), all.x = T)
  bfgh193 <- merge(bf193,gh193,by = intersect(names(bf193), names(gh193)), all.x = T)
  
  full193 <-  merge(acde193,bfgh193,by = intersect(names(acde193), names(bfgh193)), all.x = T)
  

  # writing csv by indicator for fully merged data frames ------------------------
  
  
  
  write.csv(full191, file = "NDSHS_191_smoking_STE_National.csv", row.names = F)
  write.csv(full192, file = "NDSHS_192_alcohol_STE_National.csv", row.names = F)
  write.csv(full193, file = "NDSHS_193_drugs_STE_National.csv", row.names = F)
  
  
  
  
  
  
  
  

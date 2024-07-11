
# Harriette's WD

setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/Immunisation_SA3")

#---------------#
#---libraries---#
#---------------#

library(readxl)
library(dplyr)
library(tidyr)

#-------------------------------#
#---reading in excel function---#
#-------------------------------#

cleaning <- function(path, sht, range, col){
  #path = file path, sht = Sheet number, range = column range, col = col names (T/F) 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  #REMOVING NP AND # 
  df[df[,] == "NP"] <- NA
  df[df[,] == "#"] <- "1"
  
  
  #Note for QA:
  #  as per sheet one of per raw data "*" in this instance means Interpret with caution:
  #This area’s eligible population is between 26 and 100 registered children. 
  #I have just left as the end column, as in raw data but changed the symbol from # to 1 
  
  
  #REMOVING "YEARS" IN AGE COLUMN


  names(df)[names(df) == "Age group" ] <- "age"
  
  df$age <- as.numeric(substr(df$age,1,1))

  
  #ROUNDING TO TWO DECIMAL PLACES

  # ROUNDING:
  
  # ROUNDING:
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("Reporting Year"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  #ADDING FILTER COLUMNS
  df$sex <- "all"
  df$age_group <- "0-5"
  
  
  #CHANGING YEAR RANGE TO FULL NUMBERS
  
  # RENAMING VALUES IN A SPECIFIC COL
  names(df)[names(df) == "Reporting Year"] <- "year_range"
  
  # apply gsub to the year column
  df <- mutate(df, year_range = gsub("–", "-", year_range))
  df <- mutate(df, year_range = gsub("(\\d{4})-(\\d{2})", "\\1-20\\2", year_range))
  
  
  return(df)
  
}
#National Data -----------------------------------------------------------------
  df1 <- cleaning("AIHWMEDICARE_Immunisationratesforchildren_SA4.xlsx", 2, "A16:F34", T)
  
#RENAMING COLUMNS  
  names(df1) <- c("year_range", "age_fully_immunised_AIR_public", "n_of_children_registered_immunisation_AIR_public", 
                  "n_fully_immunised_AIR_public",
                  "n_not_fully_immunised_AIR_public", 
                  "p_full_immunised_AIR_public","sex", "age_group")
  
  
 # ADDING NATIONAL COLUMN   
df1$Australia <- 0




#BRINGING NATIONAL COLUMN TO FRONT 
corder <- c("Australia", "year_range", "age_group", "sex","age_fully_immunised_AIR_public",  "n_of_children_registered_immunisation_AIR_public", 
            "n_fully_immunised_AIR_public",
            "n_not_fully_immunised_AIR_public", 
            "p_full_immunised_AIR_public")

df1 <- df1[,corder]
  
  
#SA4 Data ----------------------------------------------------------------------
  df2 <- cleaning("AIHWMEDICARE_Immunisationratesforchildren_SA4.xlsx", 7, "B17:J281", T)


#REMOVE STATE NAMES COLUMN 

df2 <- df2[,-2]
  
#RENAMING COLUMNS 

names(df2) <- c("SA4_CODE16", "year_range", "age_fully_immunised_AIR_public", "n_of_children_registered_immunisation_AIR_public", "n_fully_immunised_AIR_public","n_not_fully_immunised_AIR_public", 
            "p_full_immunised_AIR_public", "uncertainty_AIR_public", "sex", "age_group")


#REORDERING COLS
corder <- c("SA4_CODE16", "year_range", "age_group", "sex", "age_fully_immunised_AIR_public", "n_of_children_registered_immunisation_AIR_public", 
            "n_fully_immunised_AIR_public", "n_not_fully_immunised_AIR_public", "p_full_immunised_AIR_public", "uncertainty_AIR_public")

df2 <- df2[,corder]


#SA3 Data 
df3 <- cleaning("AIHWMEDICARE_Immunisationratesforchildren_SA4.xlsx", 4, "B16:J1018", T)
  
#REMOVE STATE NAMES COLUMN 
df3 <- df3[,-2]
  
#RENAMING COLUMNS 

names(df3) <- c("SA3_CODE16", "year_range", "age_fully_immunised_AIR_public", "n_of_children_registered_immunisation_AIR_public", "n_fully_immunised_AIR_public","n_not_fully_immunised_AIR_public", 
                  "p_full_immunised_AIR_public", "uncertainty_AIR_public", "sex", "age_group")

#REORDERING COLS
corder <- c("SA3_CODE16", "year_range", "age_group", "sex", "age_fully_immunised_AIR_public",  "n_of_children_registered_immunisation_AIR_public", 
            "n_fully_immunised_AIR_public", "n_not_fully_immunised_AIR_public", "p_full_immunised_AIR_public", "uncertainty_AIR_public")

df3 <- df3[,corder]
  
  
  #----------------#
  #---write csvs---#
  #----------------#

  
 write.csv(df1, "../../../Data_Collections_READY_FOR_QA/AIR_public/AIR_121_fully_immunised_National.csv", row.names = F)
 write.csv(df2, "../../../Data_Collections_READY_FOR_QA/AIR_public/AIR_121_fully_immunised_SA4.csv", row.names = F)
 write.csv(df3, "../../../Data_Collections_READY_FOR_QA/AIR_public/AIR_121_fully_immunised_SA3.csv", row.names = F)

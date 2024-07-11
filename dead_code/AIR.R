

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

cleaning <- function(sht, range){
  #path = file path, sht = Sheet number, range = column range, col = col names (T/F) 
  
  df <- as.data.frame(read_xlsx("AIHWMEDICARE_Immunisationratesforchildren_SA4.xlsx",
                                sht,
                                range,
                                T))
  
  #REMOVING NP AND # 
  df[df[,] == "NP"] <- NA
  df[df[,] == "#"] <- "*"
  
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
  
  
  return(df)
  
}
#National Data -----------------------------------------------------------------
  df1 <- cleaning(
          sht = 2, 
          range = "A16:F34")
  
#RENAMING COLUMNS  
  names(df1) <- c("calendar_year", "age_group", "N_of_children_registered_immunisation", 
                  "N_fully_immunised",
                  "N_not_fully_immunised", 
                  "%_full_immunised")
  
  
 # ADDING NATIONAL COLUMN   
df1$Australia <- 0

#BRINGING NATIONAL COLUMN TO FRONT 
corder <- c("Australia", "calendar_year", "age_group", "N_of_children_registered_immunisation", 
            "N_fully_immunised",
            "N_not_fully_immunised", 
            "%_full_immunised")

df1 <- df1[,corder]
  
  
#SA4 Data ----------------------------------------------------------------------
  df2 <- cleaning(sht = 7, 
                  range = "B17:J281")


#REMOVE STATE NAMES COLUMN 

df2 <- df2[,-2]
  
#RENAMING COLUMNS 

names(df2) <- c("SA4_CODE16", "calendar_year", "age_group", "N_of_children_registered_immunisation", "N_fully_immunised","N_not_fully_immunised", 
            "%_full_immunised", "area_uncertainty_immunisation")

#Note for Aiden
#  as per sheet one of per raw data "*" in this instance means Interpret with caution:
#This area’s eligible population is between 26 and 100 registered children. 
#I have just left as the end column, as in raw data but changed the symbol from # to * 


#SA3 Data 
df3 <- cleaning(sht = 4, 
                  range = "B16:J1018")
  
#REMOVE STATE NAMES COLUMN 
df3 <- df3[,-2]
  
#RENAMING COLUMNS 

names(df3) <- c("SA3_CODE16", "calendar_year", "age_group", "N_of_children_registered_immunisation", "N_fully_immunised","N_not_fully_immunised", 
                  "%_full_immunised", "area_uncertainty_immunisation")
  
  
  #----------------#
  #---write csvs---#
  #----------------#

  
 write.csv(df1, "../../../Data_Collections_INTERIM/AIR_121_fully_immunised_National.csv", row.names = F)
 write.csv(df2, "../../../Data_Collections_INTERIM/AIR_121_fully_immunised_SA4.csv", row.names = F)
 write.csv(df3, "../../../Data_Collections_INTERIM/AIR_121_fully_immunised_SA3.csv", row.names = F)

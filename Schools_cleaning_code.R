

#HARRIETTES WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/TableBuilder_Data/Schools/for ANCHDA")

# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)
library(dplyr)


# ------------------------------ #
# --- reading in excel files --- #
# ------------------------------ #

cleaning <- function(path, sht, range, col, nems, corder){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  names(df)[names(df) == 'State/Territory'] <- 'STE'
  
  
  if(names(df)[1] == "STE"){
    df$STE <- recode(df$STE,
                            "a NSW" = 1)
    
    }


  
  return(df)
  
}
  
  df1 <- cleaning (path = "Table 42bN_FT_andPT_Students, 2006-2022.xlsx",
                   sht = 3,
                   range = "A5:M77085",
                   col = T)
  
  #REMOVING COLUMNS

  df1 <- df1[, -c(3:4,6:9)]
  
  

 
  
  
  #COLUMNS REMOVED: Affiliation (Gov/Non-gov), Affiliation (Gov/Cath/Ind), Aboriginal and Torres Strait Islander Status,  
  #School Level, National Report on Schooling (ANR) School Level, Year (Grade) 
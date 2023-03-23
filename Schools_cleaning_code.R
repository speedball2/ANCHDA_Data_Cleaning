

#HARRIETTES WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/TableBuilder_Data/Schools/for ANCHDA")

# TO DO:
# 
# STUDENTS - GOV, CATH, INDEP COL ONL, REMOVE GOV/NON GOV COLUMN
# RENTION RATES - ADD AGE GROUP AND KEEP YEAR LEVEL AS WELL
# FIX EXISTING CODE
# FIND SOLUTION TO LESS THAN 4 AND GREATER THAN 21
#TOP 3 for main alcohol consumed 









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
  
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("Affiliation (Gov/Non-gov)","Affiliation (Gov/Cath/Ind)","Age", "Sex", "Affiliation", "Year Range"))){
      
      #NAS introduced in other columns but these are deleted anyway ;O
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  
  
 #CHANGE NAMES TO CODES

if("STE" %in% names(df)){
   df$STE <- recode(df$STE,
                     "NSW" = 1,
                    "Vic." = 2,
                     "Qld" = 3,
                     "SA" = 4,
                     "WA" = 5,
                     "Tas." = 6,
                     "NT" = 7,
                     "ACT" = 8)

   #unique(df1$col) <- check above worked, col = col want checked
   }


  
  if("Age" %in% names(df)){
    df$Age <- recode(df$Age,
                      "a 4 years and under" = "> 4",
                      "b 5 years" = "5",
                      "c 6 years" = "6",
                     "d 7 years" = "7",
                      "e 8 years" = "8",
                     "f 9 years" = "9",
                     "g 10 years" = "10",
                      "h 11 years" = "11",
                      "i 12 years" = "12",
                      "j 13 years" = "13",
                      "k 14 years" = "14",
                      "l 15 years" = "15",
                      "m 16 years" = "16",
                      "n 17 years" = "17",
                      "o 18 years" = "18",
                      "p 19 years" = "19",
                      "q 20 years" = "20",
                      "r 21 years and over" = "21 <")

     #unique(df1$col) <- check above worked, col = col want checked
  }
  
  
  
  return(df)
  
}

# School Attendance Rates ------------------------------------------------------
  
  df1 <- cleaning (path = "Table 42bN_FT_andPT_Students, 2006-2022.xlsx",
                   sht = 3,
                   range = "A5:M77085",
                   col = T)

  #REMOVING COLUMNS

  df1 <- df1[, -c(6:9)]
  
  
  #COLUMNS REMOVED: Aboriginal and Torres Strait Islander Status,  
  #School Level, National Report on Schooling (ANR) School Level, Year (Grade) 
  
  
  names(df1) <- c("calendar_year", "STE_CODE16","affiliation_gov_non_gov","affiliation_gov_cath_ind","sex",
                  "age_group", "full_time_student_count", "part_time_student_count", "total_abs_schools")
  
  
  # School continuation Rates --------------------------------------------------
  
  df2 <- cleaning (path = "Table 62a Capped Apparent Continuation Rates, 2011-2022.xlsx",
                   sht = 2,
                   range = "A5:E1625",
                   col = T)
  
  names(df2) <- c("calendar_year", "STE_CODE16", "sex", "age_group", "apparent_continuation_rate")
  
  
  # School retention Rates -----------------------------------------------------
  
  df3 <- cleaning(path = "Table 63a_ARetention Rates_Single Year_grade.xlsx",
                  sht = 2,
                  range = "A5:H8105",
                  col = T)
  
  #REMOVING COLUMNS
  
  df3 <- df3[, -c(3,6)]
  
  #COLUMNS REMOVED: "Affiliation" , "Aboriginal and Torres Strait Islander ARR"
  
  names(df3) <- c("calendar_year", "STE_CODE16", "sex", "age_group", "apparent_retention_rate", "total_rentention_rate")
  
  
  # * age is by grade not years 
  
  
  
 # _____________________________________________________________________
  
  
  
  #RENAME STATE COLUMN TO MAKE CODING EASIER 
  
  names(df)[names(df) == 'State/Territory'] <- 'STE'
  
  
  
  
  
  #REVOMING UNWANTED CHARACTERS FROM WITHIN COLUMNS
  
  # df$STE <- substr(df$STE,3,nchar(df$STE))
  # 
  # df$Sex <- substr(df$Sex,3,nchar(df$Sex)) 
  # 
  # names(df1)[names(df1) == 'Affiliation (Gov/Non-gov)'] <- "gov"
  # 
  # names(df1)[names(df1) == 'Affiliation (Gov/Cath/Ind)'] <- "cath"
  # 
  # df1$gov <- substr(df1$gov,3,nchar(df1$gov)) 
  # 
  # df1$cath <- substr(df1$cath,3,nchar(df1$cath)) 
  # 
  # 
    
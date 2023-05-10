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
  
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("Affiliation (Gov/Non-gov)","Affiliation (Gov/Cath/Ind)","Age", "Sex", "Affiliation", "year Range", "year (Grade)"))){
      # Check if column is not already of character type
      if(!is.character(df[,i])){
        df[,i] <- as.numeric(df[,i])
        df[,i] <- round(df[,i],1)
      }
    }
  }
  
  
  
  #CHANGE NAMES TO CODES
  
  names(df)[names(df) == 'State/Territory'] <- 'STE'
  
  if("STE" %in% names(df)){
    df$STE <- recode(df$STE,
                     "a NSW" = 1,
                     "b Vic." = 2,
                     "c Qld" = 3,
                     "d SA" = 4,
                     "e WA" = 5,
                     "f Tas." = 6,
                     "g NT" = 7,
                     "h ACT" = 8,
                     "f Tas." = 6,
                     "i Aust." = 0)
    
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  
  #CHANGE TO NOT AGE PREFIX/SUFFIX
  if("Age" %in% names(df)){
    df$Age <- recode(df$Age,
                     #DF1
                     "a 4 years and under" = "0-4",
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
                     "r 21 years and over" = "21-24",
                     
                     #DF2
                     "a 14 Turning 15" = "14-15",
                     "b 15 Turning 16" = "15-16",
                     "c 16 Turning 17" = "16-17",
                     "d 17 Turning 18" = "17-18",
                     "e 18 Turning 19" = "18-19")
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  #CHANGE NAMES TO CODES
  
  names(df)[names(df) == 'Affiliation (Gov/Cath/Ind)'] <- "gov"
  
  if("gov" %in% names(df)){
    df$gov <- recode(df$gov,
                     "a Government" = "Government" ,
                     "b Catholic" = "Catholic" ,
                     "c Independent" = "Independent")
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  #CHANGE Sex to not have prefix
  
  
  if("Sex" %in% names(df)){
    df$Sex <- recode(df$Sex,
                     "a Male" = "Male",
                     "b Female" = "Female",
                     "c Persons" = "Persons")
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  
  # MAKE VALUES W/IN CELLS LOWER CASE 
  
  df[] <- lapply(df, tolower)
  
  
  return(df)
  
}

# ------------------------------- #
# --- cleaning individual dfs --- #
# ------------------------------- #

# School Attendance Rates ------------------------------------------------------

df1 <- cleaning (path = "Table 42bN_FT_andPT_Students, 2006-2022.xlsx",
                 sht = 3,
                 range = "A5:M77085",
                 col = T)

#REMOVING COLUMNS

df1 <- df1[, -c(3,6:9,13)]


#COLUMNS REMOVED: affiliation_gov_non_gov, Aboriginal and Torres Strait Islander Status,  
#School Level, National Report on Schooling (ANR) School Level, year (Grade), total 


names(df1) <- c("calendar_year", "STE_CODE16","affiliation_abs_schools","sex",
                "age_group", "n_full_time_student", "n_part_time_student")

corder <- c("STE_CODE16", "calendar_year", "age_group", "sex", "affiliation_abs_schools", "n_full_time_student", "n_part_time_student")

df1 <- df1[,corder]


# School continuation Rates --------------------------------------------------

df2 <- cleaning (path = "Table 62a Capped Apparent Continuation Rates, 2011-2022.xlsx",
                 sht = 2,
                 range = "A5:E1625",
                 col = T)

names(df2) <- c("calendar_year", "STE_CODE16", "sex", "age_group", "p_apparent_continuation_rate")

corder <- c("STE_CODE16","calendar_year", "age_group","sex", "p_apparent_continuation_rate")

df2 <- df2[,corder]

# School retention Rates -----------------------------------------------------

df3 <- cleaning(path = "Table 63a_ARetention Rates_Single year_grade.xlsx",
                sht = 2,
                range = "A5:H8105",
                col = T)

#REMOVING COLUMNS

df3 <- df3[, -c(3,6)]

#COLUMNS REMOVED: "Affiliation" , "Aboriginal and Torres Strait Islander ARR"

names(df3) <- c("calendar_year", "STE_CODE16", "sex", "school_grade", "apparent_retention_rate", "total_rentention_rate")

df3$age_group <- NA

corder <- c("STE_CODE16", "calendar_year","sex", "age_group", "school_grade", "apparent_retention_rate", "total_rentention_rate")

df3 <- df3[,corder]

if("school_grade" %in% names(df3)){
  df3$school_grade <- recode(df3$school_grade,
                             "a year 7 - year 8" = "year 7 - year 8",
                             "b year 8 - year 9" = "year 8 - year 9",
                             "c year 9 - year 10" = "year 9 - year 10",
                             "d year 10 - year 11" = "year 10 - year 11",
                             "e year 11 - year 12" = "year 11 - year 12")
  
  
  #unique(df1$col) <- check above worked, col = col want checked
}


# Adding column based on other column:

df3 <- df3 %>%
  mutate(age_group = case_when(
    endsWith(school_grade, "8") ~ "13-14",
    endsWith(school_grade, "9") ~ "14-15",
    endsWith(school_grade, "10") ~ "15-16",
    endsWith(school_grade, "11") ~ "16-17",
    endsWith(school_grade, "12") ~ "17-18",
  ))


# -------------------------------------- #
# --- year 12 and year 5 attendance  --- #
# -------------------------------------- #


# year 12 ---------------------------------------------------------------------

df4 <- cleaning (path = "Table 42bN_FT_andPT_Students, 2006-2022.xlsx",
                 sht = 3,
                 range = "A5:M77085",
                 col = T)

#REMOVING COLUMNS

df4 <- df4[, -c(3,6:8,13)]


#COLUMNS REMOVED: affiliation_gov_non_gov, Aboriginal and Torres Strait Islander Status,  
#School Level, National Report on Schooling (ANR) School Level, Total

names(df4) <- c("calendar_year", "STE_CODE16","affiliation_abs_schools","sex", "school_grade", 
                "age_group", "n_full_time_student", "n_part_time_student")

corder <- c("STE_CODE16", "calendar_year","sex", "age_group" , "school_grade", "affiliation_abs_schools", "n_full_time_student", "n_part_time_student")

df4 <- df4[,corder]

# ONLY year 12 DATA 

df4 <- df4[which(df4$school_grade == "o year 12"),names(df4) %in% c("STE_CODE16", "calendar_year","sex", "age_group" , "school_grade", "affiliation_abs_schools", "n_full_time_student", "n_part_time_student", "total_abs_schools")]

# unique(df4$school_grade) - confirm only grade level 

if("school_grade" %in% names(df4)){
  df4$school_grade <- recode(df4$school_grade,
                             "o year 12" = "year 12"
  )
}


# year 5 ---------------------------------------------------------------------

df5 <- cleaning (path = "Table 42bN_FT_andPT_Students, 2006-2022.xlsx",
                 sht = 3,
                 range = "A5:M77085",
                 col = T)

#REMOVING COLUMNS

df5 <- df5[, -c(3,6:8,13)]


#COLUMNS REMOVED: affiliation_gov_non_gov, Aboriginal and Torres Strait Islander Status,  
#School Level, National Report on Schooling (ANR) School Level

names(df5) <- c("calendar_year", "STE_CODE16","affiliation_abs_schools","sex", "school_grade", 
                "age_group", "n_full_time_student", "n_part_time_student")

corder <- c("STE_CODE16", "calendar_year","sex", "age_group" , "school_grade", "affiliation_abs_schools", "n_full_time_student", "n_part_time_student")

df5 <- df5[,corder]

# ONLY year 5 DATA 

df5 <- df5[which(df5$school_grade == "f year 5"),names(df5) %in% c("STE_CODE16", "calendar_year","sex", "age_group" , "school_grade", "affiliation_abs_schools", "n_full_time_student", "n_part_time_student", "total_abs_schools")]

# unique(df5$school_grade) - confirm only grade level 


if("school_grade" %in% names(df5)){
  df5$school_grade <- recode(df5$school_grade,
                             "f year 5" = "year 5"
  )
}

# -----------------------------------------------------------------------------

#REMOVING TOTALS FROM DATA 

df2 <- df2[!grepl("Persons", df2$sex),]
df3 <- df3[!grepl("Persons", df3$sex),]


# ----------------- #
# --- write csv --- #
# ----------------- #

write.csv(df1, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_473_full_time_and_part_time_students_STE.csv", row.names = F)
write.csv(df2, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_463_continuation_rates_STE.csv", row.names = F)
write.csv(df3, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_461_retention_rate_STE.csv", row.names = F)
write.csv(df4, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_462_school_completion_year_12.csv.csv", row.names = F)
write.csv(df5, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_Attendance_at_primary_school_year_5_STE.csv", row.names = F)

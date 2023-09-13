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

cleaning <- function(path, sht, range, col, df){
  #path = file path, sht = Sheet number, range = column range, col = col names T/F, df = dataframen number
  
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
                     "b 5 years" = "5-5",
                     "c 6 years" = "6-6",
                     "d 7 years" = "7-7",
                     "e 8 years" = "8-8",
                     "f 9 years" = "9-9",
                     "g 10 years" = "10-10",
                     "h 11 years" = "11-11",
                     "i 12 years" = "12-12",
                     "j 13 years" = "13-13",
                     "k 14 years" = "14-14",
                     "l 15 years" = "15-15",
                     "m 16 years" = "16-16",
                     "n 17 years" = "17-17",
                     "o 18 years" = "18-18",
                     "p 19 years" = "19-19",
                     "q 20 years" = "20-20",
                     "r 21 years and over" = "21-24",
                     
                     #DF2
                     "a 14 Turning 15" = "14-15",
                     "b 15 Turning 16" = "15-16",
                     "c 16 Turning 17" = "16-17",
                     "d 17 Turning 18" = "17-18",
                     "e 18 Turning 19" = "18-19")
    
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  #CHANGING SCHOOL GRADE NAME
  
  names(df)[names(df) == 'Year (Grade)'] <- "school_grade"
  
  
    # unique(df4$school_grade) - confirm only grade level 
  
  #CHANGE NAMES TO CODES
  
  names(df)[names(df) == 'Affiliation (Gov/Cath/Ind)'] <- "gov"
  names(df)[names(df) == 'Affiliation'] <- "gov"
  
  
  if("gov" %in% names(df)){
    df$gov <- recode(df$gov,
                     "a Government" = "Government" ,
                     "b Catholic" = "Catholic" ,
                     "c Independent" = "Independent",
                     
                     #df3
                     
                     "b Non-Government" = "non-government",
                     "c Catholic" = "catholic",
                     "d Independent" = "independent",
                     "e All affiliations" = "All affiliations")
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  #CHANGE Sex to not have prefix
  
  
  if("Sex" %in% names(df)){
    df$Sex <- recode(df$Sex,
                     "a Male" = "Male",
                     "b Female" = "Female",
                     "c Persons" = "all")
    
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


# Convert columns to numeric
df1$`Full-time Student count` <- as.numeric(df1$`Full-time Student count`)
df1$`Part-time Student count` <- as.numeric(df1$`Part-time Student count`)


# Grouping by Year, STE, gov, Sex, and Age and calculating the sums
df1 <- df1 %>%
  group_by(Year, STE, gov, Sex, Age, school_grade) %>%
  summarise(
    n_full_time_student = sum(`Full-time Student count`),
    n_part_time_student = sum(`Part-time Student count`)
  ) 



names(df1) <- c("calendar_year", "STE_CODE16","affiliation_abs_schools","sex",
                "age_group", "school_grade", "n_full_time_student", "n_part_time_student")

corder <- c("STE_CODE16", "calendar_year", "age_group", "sex", "affiliation_abs_schools", "school_grade", "n_full_time_student", "n_part_time_student")

df1 <- df1[,corder]


df1 <-subset(df1, school_grade!="i ungraded primary" & school_grade!="p ungraded secondary")


if("school_grade" %in% names(df1)){
  df1$school_grade <- recode(df1$school_grade,
                            
                            
                            "a pre-year 1 (foundation year)" = "pre year 1",
                            "b year 1" = "year 1",
                            "c year 2" = "year 2",
                            "d year 3" = "year 3",
                            "e year 4" = "year 4",
                            "f year 5" = "year 5",
                            "g year 6" = "year 6",
                            "h year 7 primary" = "year 7 primary",
                            "j year 7 secondary" = "year 7 secondary",
                            "k year 8" = "year 8",
                            "l year 9" = "year 9",
                            "m year 10" = "year 10",
                            "n year 11" = "year 11",
                            "o year 12" = "year 12"
                            
                          
  )
}


df1.1 <- df1 %>% group_by(STE_CODE16, calendar_year, age_group, affiliation_abs_schools, school_grade) %>%
  summarise(n_full_time_student = sum (`n_full_time_student`),
            n_part_time_student = sum (`n_part_time_student`)) %>% mutate(sex = "all")

df1 <- rbind(df1.1, df1)


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

# Convert columns to numeric
df3$`Non-Indigenous ARR` <- as.numeric(df3$`Non-Indigenous ARR`)
df3$`All ARR` <- as.numeric(df3$`All ARR`)


names(df3)[names(df3) == 'Year Range'] <- "school_grade"

# Grouping by Year, STE, gov, Sex, and Age and calculating the sums

df3 <- df3 %>%
  group_by( STE, Year , gov, Sex, school_grade) %>%
  summarise(
    apparent_retention_rate = sum(`Non-Indigenous ARR`),
    total_retention_rate = sum(`All ARR`)
  ) 




names(df3) <- c("STE_CODE16","calendar_year","affiliation_abs_schools", "sex", "school_grade", "apparent_retention_rate", "total_retention_rate") #fixed typo here

df3$age_group <- NA

corder <- c("STE_CODE16", "calendar_year","sex", "age_group", "affiliation_abs_schools", "school_grade", "apparent_retention_rate", "total_retention_rate") #fixed typo here


df3 <- df3[,corder]


  
  #unique(df1$col) <- check above worked, col = col want checked



if("school_grade" %in% names(df3)){
  df3$school_grade <- recode(df3$school_grade,
                            
                            
                            
                            #DF3
                            "a year 7 - year 8" = "year 7 - year 8",
                            "b year 8 - year 9" = "year 8 - year 9",
                            "c year 9 - year 10" = "year 9 - year 10",
                            "d year 10 - year 11" = "year 10 - year 11",
                            "e year 11 - year 12" = "year 11 - year 12",
                            
                            
  )
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

# SUBSET DATASET OF YEAR 12
df4 <- subset(df1, (school_grade %in% c("year 12")))


# SUBSET DATASET OF YEAR 5
df5 <- subset(df1, (school_grade %in% c("year 5")))

# -----------------------------------------------------------------------------

#REMOVING TOTALS FROM DATA 

# df2 <- df2[!grepl("Persons", df2$sex),]
# df3 <- df3[!grepl("Persons", df3$sex),]
# df3 <- df3[!grepl("e all affiliations", df3$affiliation_abs_schools),]


#COL ORDER ---------------------------------------------------------------------

df1 <- df1[,c(1:3,8,4:7)]
df3 <- df3[,c(1:2,4,3,5:8)]
df4 <- df4[,c(1:3,8,4:7)]
df5 <- df5[,c(1:3,8,4:7)]


# testing SA bugs --------------------------------------------------------------


sa.only <- df3

sa.only <- select(filter(sa.only, STE_CODE16 == 4), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))

sa.only.all <- select(filter(sa.only, affiliation_abs_schools == "all affiliations"), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))
sa.only.cath <- select(filter(sa.only, affiliation_abs_schools == "catholic"), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))
sa.only.gov <- select(filter(sa.only, affiliation_abs_schools == "government"), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))
sa.only.indep <- select(filter(sa.only, affiliation_abs_schools == "independent"), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))
sa.only.non.gov <- select(filter(sa.only, affiliation_abs_schools == "non-government"), c("STE_CODE16","calendar_year","age_group","sex","affiliation_abs_schools","school_grade","apparent_retention_rate","total_retention_rate"))

# ----------------- #
# --- write csv --- #
# ----------------- #

# write.csv(df1, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_473_full_time_and_part_time_students_STE.csv", row.names = F)
# write.csv(df2, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_463_continuation_rates_STE.csv", row.names = F)
# write.csv(df3, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_461_retention_rate_STE.csv", row.names = F)
# write.csv(df4, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_462_school_completion_year_12_STE.csv.csv", row.names = F)
# write.csv(df5, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/SCHOOLS/ABS_schools_411_attendance_at_primary_school_year_5_STE.csv", row.names = F)

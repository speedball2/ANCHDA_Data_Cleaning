#HARRIETTES WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NMD_SA2_SA3")

#---------------------------#
#--------libraries----------#
#---------------------------#

library(readxl)
library(dplyr)
library(stringr)

#-------------------------------#
#-------- data frames ----------#
#-------------------------------#

#excel doc: 202211_ANCHDA_suppressed_cells_final.xlsx
# df1 = NMD_SA3_infant_mortality - 4 year rolling , Sheet 3
# df2 = NMD_national_infant_mortality - irsd, sheet 4
# df3 = NMD_SA2_infant_mortality - 10 year rolling , Sheet 5
# df4 = NMD_SA3_child_mortality - age, 4 year rolling, Sheet 6
# df5 = NMD_National_child_mortality - IRSD, sheet 7
# df6 = NMD_SA2_child_mortality - sex, 10 year rolling, sheet 8
# df7 = NMD_national_youth_mortality - sheet 10
# df8 = NMD_SA2_youth_mortality_10_year_rolling - sheet 11

#excel doc:202211_ANCHDA_suppressed_cells_SA3_persons.xlsx
#df9 = NMD_SA3_child_mortality_4_year_rolling, sheet 3
#df10 = NMD_SA3_child_mortality_4_year_rolling, sheet 4


# functionalised code - doesn't work: 
# - removing sa2&3 name 
# -substring to remove "SA3' from code column
#calender reading in as NAs
#adding national column breaks column order code

colnems1.1 <- c("SA3_CODE16", "SA3_NAME16", "calendar_year", "total_deaths", "total_births", 
                "crude_rate_per_1000")

colnems1.2 <- c("irsd_quintiles","calendar_year", "total_deaths", 
                "total_births", "crude_rate_per_1000")


colnems1.3 <- c("SA2_CODE16", "SA2_NAME16", "calendar_year", "total_deaths", "total_births", 
                "crude_rate_per_1000")

colnems1.4 <- c("SA3_CODE16", "SA3_NAME16", "calendar_year", "age_group","total_deaths", 
                "total_population_NMD")

colnems1.5 <- c("irsd_quintiles","calendar_year", "total_deaths", 
                "total_population_NMD", "crude_rate_per_100,000")

colnems1.6 <- c("SA2_CODE16", "SA2_NAME16", "calendar_year", "total_deaths", 
                "total_population_NMD","crude_rate_per_100,000")

colnems1.7 <- c("irsd_quintiles","calendar_year", "total_deaths", 
                "total_population_NMD", "crude_rate_per_100,000")
#8 same as 6

colnems1.9 <- c("SA3_CODE16", "SA3_NAME16", "calendar_year", "sex", "total_deaths", 
                "total_population_NMD","crude_rate_per_100,000")


colnems1.10 <- c("SA3_CODE16", "SA3_NAME16","calendar_year", "sex", "total_deaths", 
                 "total_population_NMD","crude_rate_per_100,000")


#for col order function -------------------------------------------------------

colorder1.1 <- c("SA3_CODE16", "calendar_year", "total_deaths", "total_births", 
                 "crude_rate_per_1000")
colorder1.2 <- c("Australia", "irsd_quintiles","calendar_year", "total_deaths", 
                 "total_births", "crude_rate_per_1000")

colorder1.3 <- c("SA2_CODE16", "calendar_year", "total_deaths", "total_births", 
                 "crude_rate_per_1000")

colorder1.4 <- c("SA3_CODE16", "calendar_year", "age_group","total_deaths", 
                 "total_population_NMD")

colorder1.5 <- c("Australia","irsd_quintiles","calendar_year", "total_deaths", 
                 "total_population_NMD", "crude_rate_per_100,000")

colorder1.6 <- c("SA2_CODE16", "calendar_year", "total_deaths", 
                 "total_population_NMD","crude_rate_per_100,000")

colorder1.7 <- c("Australia","irsd_quintiles","calendar_year", "total_deaths", 
                 "total_population_NMD", "crude_rate_per_100,000")


colorder1.9 <- c("SA3_CODE16",  "calendar_year", "sex", "total_deaths", 
                 "total_population_NMD","crude_rate_per_100,000")

colorder1.10 <- c("SA3_CODE16", "calendar_year", "sex", "total_deaths", 
                  "total_population_NMD","crude_rate_per_100,000")

# ------------------------------------------------------------------------------

# CLEANING DATA FUNCTION 

cleaning <- function(path, sht, range, col, nems, corder){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  #REMOVING NAS AND NPS FROM DATA, CHANGE IRSD TO NUMBERS
  
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  df[df[,] == "Quintile 1 (lowest)"] <- 1
  df[df[,] == "Quintile 2"] <- 2
  df[df[,] == "Quintile 3"] <- 3
  df[df[,] == "Quintile 4"] <- 4
  df[df[,] == "Quintile 5 (highest)"] <- 5
  
  
  # ROUNDING:
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("SA3_code","SA3_name","Period"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  
  # REMOVING SA- FROM WITHIN COLUMN 
  
  # str_sub(df1$SA3_code,4,nchar(df1$SA3_code)) FROM AIDEN :)
  
  
  # ADDING NATIONAL COLUMN TO DATA SETS
  
  
  
  #RENAMING COLUMNS
  
  #names(df) <- nems
  
  # CHANGING COLUMN ORDER
  
  #df2 <- df2[,corder]
  
  
  return(df)
}


# df <- df[,-SA2_NAME16]
# df <- df[,-SA3_NAME16]  


df1 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 3, range = "A2:F3080", col = T)

df2 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 4, range = "A30:E89", col = F)

#ADDING NATIONAL COLUMN 
df2$Australia <- 0



df3 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 5, range = "A2:F6965", col = T)
df4 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 6, range = "A2:F6159", col = T)

#ADDING NATIONAL COLUMN  
df4$Australia <- 0

df5 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 7, range = "A30:E89", col = F)
df6 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 8, range = "A2:F6980", col = T)

#ADDING NATIONAL COLUMN  
df6$Australia <- 0

df7 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 10, range = "A30:E89", col = F)
df8 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 11, range =  "A2:F6980", col = T)


df9 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 3, range = "A2:G3080", col = T )
df10 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 4, range = "A2:G3080", col = T)
  
  
  
  
  
  
# Harriette's WD


# libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, new_name){
  #path = file path, sht = Sheet number, range = column range 
  #code = SA4/LGA region for pivoting wide to long
  #new_name = col name to change
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
           sht,
           range,
           col_names = T))
  #REMOVING 1ST COL (BLANK)
  
  df <- df[-1,]
  
  # REMOVING ATSI STATUS
  
  df <- select(df, -c (Aboriginality))
  
  
 # CONCISTENT NAMING CONVENTIONS W/ DATA DICTIONARY
  
  df[df[,] == 0] <- NA
  df[df[,] == "1 to 4"] <- "1-4"
  
  
  #RENAMING COLUMNS FOR ALL DFS
  names(df)[names(df) == "Age of victim (in years)"] <- "age_group"
  names(df)[names(df) == "Gender"] <- "sex"
  names(df)[names(df) == "SA4 of Incident"] <- "SA4_NAME16"
  names(df)[names(df) == "LGA of Incident"] <- "LGA_NAME21"


  # REMOVING RANDOM "Y" FROM DFs
# df 8 has a 'y' at the end of the age column
  
  df$age_group <-gsub("y","",as.character(df$age_group))
  
  
  # PIVOTING DATA FROM WIDE TO LONG
  df <- gather(df, calendar_year, indicator, gathercol <- c("2006", "2007", "2008", "2009","2010","2011","2012", "2013","2014","2015","2016","2017", "2018","2019","2020","2021")) 
  
  #DF = DATA FRAME 
  #INDICATOR = COUNTS FOR RATES IN NEW COL (a new col)
  #CALENDAR_YEAR = NEW COLUMN FOR "GATHERCOL" VARIABLES (a new col)
  #OTHER COLUMNNS FOLLOW
  
  
  #RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
  names(df)[names(df) == "indicator"] <- new_name
  
  
  # #CHANGING NAMES SO NEXT FUNC WORKS
  
  if ("SA4_NAME16" %in% names(df)) {
    rep_str <- c("And" = "and", "Exc" = "exc")
    df$SA4_NAME16 <- stringr::str_replace_all(df$SA4_NAME16, rep_str)
  }
  
  
return(df)
   
}

df1 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 1, "A5:T415","victims_domestic_violence_related_assault")
df2 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 2, "A5:T1571", "victims_domestic_violence_related_assault")
df3 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 3, "A5:T90", "victims_domestic_violence_related_murder")
df4 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 4, "A5:T114", "victims_domestic_violence_related_murder")
df5 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 5, "A5:T393", "victims_sexual_assault")
df6 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 6, "A5:T1325", "victims_sexual_assault")
df7 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 7, "A5:T392", "victims_sexual_touching")
df8 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 8, "A5:T1337", "victims_sexual_touching")


# reading in geographies for codes ---------------------------------------------


sa4 <- read.csv("SA2_2016_AUST_no_geom.csv")


lga <- read_xlsx("LGA_2021_AUST (2).xlsx", 1, cell_limits(c(1, 1), c(NA, 3)), T)

lga <- lga[,-1]

lga <- lga[!duplicated(lga),]

#JUST LGA NAMES AND CODES

write.csv(lga, "lga_names_codes.csv", F)



# ------------------------------------------------------------------------------

# MATCH FUNCTION FOR SA CODES AND NAMES 

#confirm which ASGS is being used


sa4_codes <- function(df){
  
  copy <- df
  
  # REMOVING JUNK COLS
  
  copy <- copy[,-(1:2)]
  
  copy$code <- NA
  
  copy <- copy[,-(2:3)]
  
  # REMOVING DUPLICATE NAMES - SA names only for NSW
  
  copy <- copy[!duplicated(copy),]
  
  #ONLY SA4 FROM ASGS
  
  sa4 <- sa4[,-c(1:5,8:12)]
  sa4 <- sa4[!duplicated(sa4),]
  sa4 <- sa4[sa4$SA4_CODE_2016<200,]
  
  #MERGING TWO DATA FRAMES TOGETHER (CUSTODIAN + ABS ASGS)
  
  dummy <- merge(copy,sa4,by.y="SA4_NAME_2016",by.x="SA4_NAME16",all=T)
  
  #REMOVE COLS FROM ASGS FILES
  
  dummy <- dummy[,-2]
  
  #CBIND BACK WITH OTHER DATASET
  
  new <- merge(df, dummy, by = "SA4_NAME16")
  
  return(new)
}


lga_codes <- function(df){
  
  
  copy2 <- df2
  # REMOVING JUNK COLS
  
  copy2 <- copy2[,-(1:2)]
  
  copy2$code <- NA
  
  copy2 <- copy2[,-(2:3)]
  
  # REMOVING DUPLICATE NAMES - SA names only for NSW
  
  copy2 <- copy2[!duplicated(copy2),]
  
  #MERGING TWO DATA FRAMES TOGETHER (CUSTODIAN + ABS ASGS)
  
  dummy2 <- merge(lga, copy2)
  
  # MATCHING
  
  dummy2$code <- dummy2$LGA_CODE_2021[match(dummy2$LGA_NAME21, dummy2$LGA_NAME_2021)]
  
  
  #REMOVE COLS FROM ASGS FILES
  
  dummy2 <- dummy2[,-(1:2)]
  
  #CBIND BACK WITH OTHER DATASET
  
  new <- merge(df, dummy2, by = "LGA_NAME21")
  new <- new[!duplicated(new),]
  
  return(new)
  
}
#SA4
df1_new <- sa4_codes(df1)
df3_new <- sa4_codes(df3)
df5_new <- sa4_codes(df5)
df7_new <- sa4_codes(df7)


#LGA
# df2_new <- lga_codes(df2)
# df4_new <- lga_codes(df4)
# df6_new <- lga_codes(df6)
# df8_new <- lga_codes(df8)


concistency <- function(df){
  
  #REMOVE VARIABLES IN DATA 
  df <- df[!grepl("Unknown/missing", df$sex),]
  
  #REMOVING CAPITALS FOR M/F
  
  if("sex" %in% names(df)){
    df$sex <- recode(df$sex,
                     "Female" = "female",
                     "Male" = "male",)
    
  }
  
  return(df)
}


df1_new <- concistency(df1_new)


#REMOVE IN CUSTODY DATA 
# df <- df[!grepl("In Custody", df$SA4_NAME16),]
#df <- df[!grepl("In Custody", df$LGA_NAME21),]
 
#Rename code as proper data dictionary code name 

#write csvs 


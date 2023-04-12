

# Harriette's WD

setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/BOSCAR_SA4_LGA")



# TO DO ------------------------------------------------------------------------

# what to do w/ unknown/missing data for m/f

# libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)

# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range){
  #path = file path, sht = Sheet number, range = column range 
  #code = SA4/LGA region for pivoting wide to long
  
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
  df[df[,] == "1 to 4"] <- "<5"
  
  
  #RENAMING COLUMNS FOR ALL DFS
  names(df)[names(df) == "Age of victim (in years)"] <- "age_group"
  names(df)[names(df) == "Gender"] <- "sex"
  names(df)[names(df) == "SA4 of Incident"] <- "SA4_NAME16"
  names(df)[names(df) == "LGA of Incident"] <- "LGA_NAME16"


  # REMOVING RANDOM "Y" FROM DFs
# df 8 has a 'y' at the end of the age column
  
  df$age_group <-gsub("y","",as.character(df$age_group))
  
  
  # PIVOTING DATA FROM WIDE TO LONG
  df <- gather(df, calendar_year, indicator, gathercol <- c("2006", "2007", "2008", "2009","2010","2011","2012", "2013","2014","2015","2016","2017", "2018","2019","2020","2021")) 
  
  #DF = DATA FRAME 
  #INDICATOR = COUNTS FOR RATES IN NEW COL (a new col)
  #CALENDAR_YEAR = NEW COLUMN FOR "GATHERCOL" VARIABLES (a new col)
  #OTHER COLUMNNS FOLLOW
 
return(df)
   
}

# SPECIFIC TO EACH DF ----------------------------------------------------------

df1 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 1, "A5:T415")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df1)[names(df1) == "indicator"] <- "victims_domestic_violence_related_assault"

# ------------------------------------------------------------------------------

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
df2 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 2, "A5:T1571")

names(df2)[names(df2) == "indicator"] <- "victims_domestic_violence_related_assault"

# ------------------------------------------------------------------------------


df3 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 3, "A5:T90")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df3)[names(df3) == "indicator"] <- "victims_domestic_violence_related_murder"

# ------------------------------------------------------------------------------

df4 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 4, "A5:T114")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df4)[names(df4) == "indicator"] <- "victims_domestic_violence_related_murder"

# ------------------------------------------------------------------------------

df5 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 5, "A5:T393")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df5)[names(df5) == "indicator"] <- "victims_sexual_assault"

# ------------------------------------------------------------------------------

df6 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 6, "A5:T1325")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df6)[names(df6) == "indicator"] <- "victims_sexual_assault"

# ------------------------------------------------------------------------------

df7 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 7, "A5:T392")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df7)[names(df7) == "indicator"] <- "victims_sexual_touching"

# ------------------------------------------------------------------------------

df8 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 8, "A5:T1337")

#RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
names(df8)[names(df8) == "indicator"] <- "victims_sexual_touching"


# reading in geographies for codes ---------------------------------------------


# ../ from where R script is saved 

sa4 <- read_xlsx("SA4_2021_AUST (1).xlsx", 1, "A1:B109", T)


lga <- read_xlsx("LGA_2021_AUST (1).xlsx", 1, cell_limits(c(1, 1), c(NA, 3)), T)

lga <- lga[,-1]
 
lga <- lga[!duplicated(lga),]

# ------------------------------------------------------------------------------

# MATCH

#print(match(test(c()), sa4)) this but for the name cols 

s <- c("SA4_CODE_2021","SA4_NAME_2021")
c <- c("SA4_NAME16")

copy <- df1

copy <- copy[!duplicated(copy),]

dummy <- merge(sa4, copy)

copy <- copy %>%
  arrange(SA4_NAME16, -victims_domestic_violence_related_assault) %>%
  filter(!duplicated(SA4_NAME16))

data$test = ifesle(data$)
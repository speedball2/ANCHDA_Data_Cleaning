

#HARRIETTE'S WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/TableBuilder_Data/Births")



# TO DO LIST 
# fix np code not working - done 
# fix rounding code not working - SA4 
# col names - half 
# combine sheets for states, then make data long with year in sep col?
# NSW totl (aka 1) change to "total" ? - done 
#remove empty columns that are present in the raw data ;-;
# 0 at end of SA region being removed (SA2)

#combine data and split years into new column : ) 


# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)
library(dplyr)



# SA4 --------------------------------------------------------------------------


df8 <- as.data.frame(read_xlsx("births_SA4_2011-2021.xlsx", 2, "A7:EC128", T))  # ACTUAL END OF DOC IS EC BUT DOESNT READ IN FULL DF

#REMOVE FIRST ROW
df8 <- df8[-1,]



#MAKE NAs CONCISTENT
df8[df8[,] == "np"] <- NA


# REMOVING ERP COLUMN
df8 = df8[,!grepl("^Estimated",names(df8))]

# #ROUNDING
for(i in seq(2,ncol(df8))){
  df8[,i] <- as.numeric(df8[,i])
  df8[,i] <- round(df8[,i],2)
}

# REMOVING SA NAMES + OTHER JUNK COLUMNS
df8 <- df8[,-c(2,13,24,35,46)]

#REMOVE BLANK ROWS

#  BLANK ROW FOR EACH STATE


# # SA4 --------------------------------------------------------------------------

full <- data.frame(matrix(ncol = 11, nrow = 0))
namecol <- c("SA4_CODE16", "births_abs","15-19", "20-24","25-29","30-34", "40-44", "45-49", "total_fertility_rate", "median_age_of_mother","calendar_year")
names(full) <- namecol
for (i in seq(11,ncol(df8),11)){
  test <- df8[,c(1,i:(i+1))] %>% mutate(Year = 2010+(i/2))
  names(test) <- namecol
  full <- rbind(full,test)
}




# LGA --------------------------------------------------------------------------

# ------------------------------ #
# --- reading in excel files --- #
# ------------------------------ #

cleaning_lga <- function(path, sht, range){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col_names = T))
  
  df = df[,!grepl("^person",names(df))] # REMOVING ERP COLUMN 
  
  df <- df[,-c(2)] # REMOVING SA NAMES 
  
  df <- df[, -c(4,7,10,13,16,19,22,25,28,31)] #REMOVING BLANK COLUMNS (SEE RAW DATA)
  
  
  #CHANGING NAME FOR 1ST COL FOR EASE IN FUNCTION USE
  names(df)[names(df) == 'LGA Code 2021'] <- "code"
  
  #MAKE NAs CONCISTENT 
  df[df[,] == "np"] <- NA
  
  # #ROUNDING
  for(i in seq(3,ncol(df))){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
    
  return(df)
  
}

# ------------------------------------- #
# --- CLEANING INDIVIDUAL DATA SETS --- #
# ------------------------------------- #

# NSW 

df1 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 2, range = "A8:AS138")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df1["code"][df1["code"] == 1] <- "total"

#VIC
df2 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 3, range = "A8:AS89")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df2["code"][df2["code"] == 2] <- "total"


#QLD
df3 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 4, range = "A8:AS87")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df3["code"][df3["code"] == 3] <- "total"

#SA
df4 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 5, range = "A8:AS80")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df4["code"][df4["code"] == 4] <- "total"


#WA
df5 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 6, range = "A8:AS148")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df5["code"][df5["code"] == 5] <- "total"

#TAS
df6 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 7, range = "A8:AS39")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df6["code"][df6["code"] == 6] <- "total"

#NT
df7 <- cleaning_lga(path = "births_LGA_2011_2021.xlsx", sht = 8, range = "A8:AS28")

#CHANGE CODES TO "TOTAL" TO STANDARDISE W/ DATA DICTIONARY
df7["code"][df7["code"] == 7] <- "total"




# SA2 --------------------------------------------------------------------------



cleaning_sa <- function(path, sht, range){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                  sht,
                  range,
                  col_names = T))
  
  df = df[,!grepl("^Estimated",names(df))] # REMOVING ERP COLUMN 
  
  #MAKE NAs CONCISTENT 
  df[df[,] == "np"] <- NA
  

  df <- df[-1,]
  
  df <- df[,-c(2,5,8,11,14,17,20,23,26,29,32)]
  
  #ROUNDING
  for(i in seq(3,ncol(df))){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }
  
  names(df)[names(df) == '2021 ASGS Code'] <- "code"
  
  
 return(df)
  
}



#NSW
df9 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 2 , range = "A7:AS773")

#VIC
df10 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 3, range = "A7:AS616")

#QLD
df11 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 4, range = "A7:AS658")

#SA
df12 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 5, range = "A7:AS220")

#WA
df13 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 6, range = "A7:AS320")

#TAS
df14 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 7, range = "A7:AS129")

#NT
df15 <- cleaning_sa(path = "births_SA2_2011-2021.xlsx", sht = 8, range = "A7:AS90")


# ------------------------------------------------------------------------------

# MAKING DATA LONG 

long <- function(df,geoname){
  full <- data.frame(matrix(ncol = 4, nrow = 0))
  namecol <- c(geoname, "births_abs","total_fertility_rate_abs", "calendar_year")
  names(full) <- namecol
  for (i in seq(2,ncol(df),2)){
    test <- df[,c(1,i:(i+1))] %>% mutate(Year = 2010+(i/2))
    names(test) <- namecol
    full <- rbind(full,test)
    }
  return(full)
}


#SA2 & LGA ---------------------------------------------------------------------

df1 <- long(df1,"LGA_CODE16")
df2 <- long(df2,"LGA_CODE16")
df3 <- long(df3,"LGA_CODE16")
df4 <- long(df4,"LGA_CODE16")
df5 <- long(df5,"LGA_CODE16")
df6 <- long(df6,"LGA_CODE16")
df7 <- long(df7,"LGA_CODE16")
#SKIP DF 8, FOLLOWS DIFFERENT PATTERN
df9 <- long(df9,"SA2_CODE16")
df10 <- long(df10,"SA2_CODE16")
df11 <- long(df11,"SA2_CODE16")
df12 <- long(df12,"SA2_CODE16")
df13 <- long(df13,"SA2_CODE16")
df14 <- long(df14,"SA2_CODE16")
df15 <- long(df15,"SA2_CODE16")

#  R BIND DFS BY ASGS ORDER ----------------------------------------------------


LGA <- rbind(df1,df2,df3,df4,df5,df6,df7)
SA2 <- rbind(df9, df10, df11, df12, df13, df14, df15)

# SEPERATING BY INDICATOR ------------------------------------------------------

l <- c("LGA_CODE16", "calendar_year")
s <- c("SA2_CODE16", "calendar_year")
b <- c("births_abs")
f <- c("total_fertility_rate_abs")

# SA2 --------------------------------------------------------------------------

#LGA BIRTH RATES
lbirth <- LGA[,c(l,b)]

#LGA BIRTH RATES
lfert <- LGA[,c(l,b)]



# LGA --------------------------------------------------------------------------

#SA2 FERTILITY RATES
sbirth <- SA2[,c(s,f)]

#SA2 FERTILITY RATES
sfert <- SA2[,c(s,f)]

# WRITE CSVS -------------------------------------------------------------------

# 1.1.10 births ----------------------------------------------------------------

write.csv(lbirth, "../../../../Data_Collections_INTERIM/ABS_births_1110_births_LGA.csv", row.names = F)
write.csv(sbirth, "../../../../Data_Collections_INTERIM/ABS_births_1110_births_SA2.csv", row.names = F)


# 1.1.14 fertility rates -------------------------------------------------------


write.csv(lfert, "../../../../Data_Collections_INTERIM/ABS_births_1114_fertility_rates_LGA.csv", row.names = F)
write.csv(sfert, "../../../../Data_Collections_INTERIM/ABS_births_1114_fertility_rates_SA2.csv", row.names = F)








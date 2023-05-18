
# libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, new_name, geography_field){
  # path = file path, sht = Sheet number, range = column range 
  # new_name = col name to change
  # geography_field = field indicating the geography (SA4_NAME16 or LGA_NAME16)
  
  # READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col_names = TRUE))
  
  # REMOVING 1ST COL (BLANK)
  df <- df[-1,]
  
  # CONCISE NAMING CONVENTIONS W/ DATA DICTIONARY
  df[df[,] == 0] <- NA
  df[df[,] == "1 to 4"] <- 0
  
  # RENAMING COLUMNS FOR ALL DFS
  names(df)[names(df) == "Age of victim (in years)"] <- "age_group"
  names(df)[names(df) == "Gender"] <- "sex"
  names(df)[names(df) == "SA4 of Incident"] <- "SA4_NAME16"
  names(df)[names(df) == "LGA of Incident"] <- "LGA_NAME16"
  
  # REMOVING RANDOM "Y" FROM DFs
  df$age_group <- gsub("y", "", as.character(df$age_group))
  
  # PIVOTING DATA FROM WIDE TO LONG
  df <- gather(df, calendar_year, indicator, gathercol <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) 
  
  # RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
  names(df)[names(df) == "indicator"] <- new_name
  
  # CHANGING NAMES SO NEXT FUNC WORKS
  if (geography_field %in% names(df)) {
    rep_str <- c("And" = "and", "Exc" = "exc")
    df[[geography_field]] <- stringr::str_replace_all(df[[geography_field]], rep_str)
  }
  
  # REMOVE VARIABLES IN DATA 
  df <- df[!grepl("Unknown/missing", df$sex),]
  
  # REMOVING CAPITALS FOR M/F
  if ("sex" %in% names(df)) {
    df$sex <- recode(df$sex, "Female" = "female", "Male" = "male")
  }
  
  # CONVERTING 6TH COLUMN TO NUMERIC
  df[[new_name]] <- as.numeric(df[[new_name]])
  
  # GROUP BY age_group, sex, geography_field, calendar_year and calculate total_victims
  result <- df %>%
    group_by(age_group, sex, !!as.name(geography_field), calendar_year) %>%
    summarize(!!as.name(new_name) := sum(!!as.name(new_name), na.rm = TRUE))
  
  
  # Return the result
  return(result)
}

df1 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 1, "A5:T415","n_victims_domestic_violence_related_assault", "SA4_NAME16")
df2 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 2, "A5:T1571", "n_victims_domestic_violence_related_assault", "LGA_NAME16")
df3 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 3, "A5:T90", "n_victims_domestic_violence_related_murder", "SA4_NAME16")
df4 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 4, "A5:T114", "n_victims_domestic_violence_related_murder", "LGA_NAME16")
df5 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 5, "A5:T393", "n_victims_sexual_assault", "SA4_NAME16")
df6 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 6, "A5:T1325", "n_victims_sexual_assault", "LGA_NAME16")
df7 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 7, "A5:T392", "n_victims_sexual_touching", "SA4_NAME16")
df8 <- cleaning("ab23-22205 Victims by Aboriginality Gender SA4 LGA.xlsx", 8, "A5:T1337", "n_victims_sexual_touching", "LGA_NAME16")



# reading in geographies for codes ---------------------------------------------
sa4 <- read.csv("SA2_2016_AUST_no_geom.csv")
lga <- read.csv("LGA_2016_AUST.csv")
lga <- lga[, c(3, 4)]
lga <- lga[!duplicated(lga),]
lga$LGA_NAME_2016 <- gsub("\\s+\\((C|A|S|RC|R|DC|AC|T|M|B)\\)", "", lga$LGA_NAME_2016)
head(lga)




# MATCH FUNCTION FOR SA CODES AND NAMES ----------------------------------------
sa4_codes <- function(df, corder, indicator){
  
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
  
  # clean up afterwards ---
  
  #REMOVING IN CUSTODY - CANNOT BE GEO CODED 
  new <- new[!grepl("In Custody", new$SA4_NAME16),]
  
  # REMOVING SA4 NAME (NOT NEEDED)
  new <- new[ , !names(new) %in% 
                c("SA4_NAME16")]
  
  # RENAMING CODE COLUMNS
  colnames(new)[colnames(new) == "SA4_CODE_2016"] = "SA4_CODE16"
  
  #CHANGING COLUMN ORDER 
  corder <- c("SA4_CODE16", "calendar_year", "age_group", "sex", indicator)
  new <- new[,corder]
  
  return(new)
}
lga_codes <- function(df, indicator){
  
  
  
  copy2 <- df
  # REMOVING JUNK COLS
  
  copy2 <- copy2[,-(1:2)]
  
  copy2$code <- NA
  
  copy2 <- copy2[,-(2:3)]
  
  # REMOVING DUPLICATE NAMES - SA names only for NSW
  
  copy2 <- copy2[!duplicated(copy2),]
  
  #MERGING TWO DATA FRAMES TOGETHER (CUSTODIAN + ABS ASGS)
  
  dummy2 <- merge(copy2,lga, by.y="LGA_NAME_2016",by.x="LGA_NAME16",all=T)
  
  
  #CBIND BACK WITH OTHER DATASET
  
  new <- merge(df, dummy2, by = "LGA_NAME16")
  
  # clean up afterwards ---
  
  #REMOVING IN CUSTODY - CANNOT BE GEO CODED 
  new <- new[!grepl("In Custody", new$LGA_NAME16),]
  
  test <<- new
  
  # REMOVING SA4 NAME (NOT NEEDED)
  new <- new[ , !names(new) %in% 
                
                c("LGA_NAME16",
                  "code")]
  
  # RENAMING CODE COLUMNS
  colnames(new)[colnames(new) == "LGA_CODE_2016"] = "LGA_CODE16"
  
  corder <- c("LGA_CODE16", "calendar_year", "age_group", "sex", indicator)
  new <- new[,corder]
  
  return(new)
  
}
#SA4
df1_new <- sa4_codes(df1, indicator = "n_victims_domestic_violence_related_assault")
df3_new <- sa4_codes(df3, indicator = "n_victims_domestic_violence_related_murder")
df5_new <- sa4_codes(df5, indicator = "n_victims_sexual_assault")
df7_new <- sa4_codes(df7,indicator = "n_victims_sexual_touching")



#LGA
df2_new <- lga_codes(df2, indicator = "n_victims_domestic_violence_related_assault")
df4_new <- lga_codes(df4, indicator = "n_victims_domestic_violence_related_murder")
df6_new <- lga_codes(df6, indicator = "n_victims_sexual_assault")
df8_new <- lga_codes(df8, indicator = "n_victims_sexual_touching")


#WRITE CSVS --------------------------------------------------------------------
write.csv(df1_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3131_victims_domestic_violence_related_assault_SA4.csv", row.names = FALSE)
write.csv(df2_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3131_victims_domestic_violence_related_assault_LGA.csv", row.names = F)

write.csv(df3_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3132_victims_domestic_violence_related_murder_SA4.csv", row.names = F)
write.csv(df4_new, file = "C:/Users/00095998/OneDrive - The University of Wes
tern Australia/acwa_temp/BOCSAR/BOCSAR_3132_victims_domestic_violence_related_murder_LGA.csv", row.names = F)

write.csv(df5_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3133_victims_sexual_assault_SA4.csv",row.names = F)
write.csv(df6_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3133_victims_sexual_assault_LGA.csv",row.names = F)

write.csv(df7_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3134_victims_sexual_touching_SA4.csv",row.names = F)
write.csv(df8_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_3134_victims_sexual_touching_LGA.csv",row.names = F)
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
  # df[df[,] == "Quintile 1 (lowest)"] <- 1
  # df[df[,] == "Quintile 2"] <- 2
  # df[df[,] == "Quintile 3"] <- 3
  # df[df[,] == "Quintile 4"] <- 4
  # df[df[,] == "Quintile 5 (highest)"] <- 5
  
  
  # ROUNDING:
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("SA3_code","SA3_name","Period", "Age group (years)", "Sex"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  
  df$age_group <- 0
  
  df$sex <- "all"
  
  
  return(df)
}

#DF 1---------------------------------------------------------------------------

df1 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 3, range = "A2:F3080", col = T)
#REMOVING SA2_NAME  
  df1 <- df1[,-2] 

#RENAMING COLUMNS
  names(df1) <- c("SA3_CODE16", "year_range", "totals_deaths_NMD", "totals_births_NMD", 
                  "crude_rate_per_1000_NMD", "age_group", "sex")
  
#ADDING AGE
  
  df1["age_group"][df1["age_group"] == 0] <- "0-1"
  
# REMOVE "SA3" FROM W/IN DATA
  
  df1$SA3_CODE16 <-gsub("SA3","",as.character(df1$SA3_CODE16))


  
# DF2 --------------------------------------------------------------------------
# df2 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 4, range = "A30:E89", col = F)
# 
#   #ADDING NATIONAL COLUMN   
#   df2$Australia <- 0 
#   
# #RENAMING COLUMNS
#   
#   names(df2) <- c("irsd_quintiles","year_range", "totals_deaths_NMD", 
#                    "totals_births_NMD", "crude_rate_per_1000_NMD", "australia", "sex", "age_group")
#   
#   #ADDING AGE
#   
#   df2["age_group"][df2["age_group"] == 0] <- "0-1"



  
# DF 3 -------------------------------------------------------------------------
  
df3 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 5, range = "A2:F6965", col = T)

#REMOVING SA2_NAME  
  df3 <- df3[,-2] 
  
#RENAMING COLUMNS
  
  names(df3) <- c("SA2_CODE16", "year_range", "totals_deaths_NMD", "totals_births_NMD", 
                  "crude_rate_per_1000_NMD", "age_group", "sex")
  
#ADDING AGE
  
  df3["age_group"][df3["age_group"] == 0] <- "0-1"
  

#DF 4 --------------------------------------------------------------------------
  
df4 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 6, range = "A2:G6159", col = T)

#REMOVING SA2_NAME
  df4 <- df4[,-2] 

#RENAMING COLUMNS
  
  names(df4) <- c("SA3_CODE16", "year_range", "age_group","totals_deaths_NMD", 
  "total_population_NMD","age_specific_rate_per_100,000_NMD", "sex")
  
# REMOVE ADDED AGE GROUP COLUMN (ALREADY DISAGG IN THIS DF)
  df4 <- df4[,-7]
    
  # REMOVE "SA3" FROM W/IN DATA
  
  df4$SA3_CODE16 <-gsub("SA3","",as.character(df4$SA3_CODE16))
  
# DF 5 -------------------------------------------------------------------------

# df5 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 7, range = "A30:E89", col = F)
# 
# #ADDING NATIONAL COLUMN   
#   df5$Australia <- 0
#   
# #RENAMING COLUMNS
#   
#   names(df5) <- c("irsd_quintiles","year_range", "totals_deaths_NMD", 
#   "total_population_NMD", "crude_rate_per_100,000_NMD", "australia", "age_group", "sex")
#   
#   #ADDING AGE
#   
#   df5["age_group"][df5["age_group"] == 0] <- "0-17"
  
  
# DF 6 -------------------------------------------------------------------------

df6 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 8, range = "A2:F6980", col = T)

#REMOVING SA2_NAME
  df6 <- df6[,-2] 

#RENAMING COLUMNS
  names(df6) <- c("SA2_CODE16", "year_range", "totals_deaths_NMD", 
  "total_population_NMD","crude_rate_per_100,000_NMD", "age_group", "sex")
  
  #ADDING AGE
  
  df6["age_group"][df6["age_group"] == 0] <- "0-17"
  
  
  
#DF 7 --------------------------------------------------------------------------

# df7 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 10, range = "A30:E89", col = F)
# 
#   #ADDING NATIONAL COLUMN 
#   df7$Australia <- 0 
# 
# #RENAMING COLUMNS
#   
#  names(df7) <- c("irsd_quintiles","year_range", "totals_deaths_NMD", 
#   "total_population_NMD", "crude_rate_per_100,000_NMD", "australia", "age_group", "sex")
#  
#  
# #ADDING AGE
#  
#  df7["age_group"][df7["age_group"] == 0] <- "18-24"
#  
 
 
# DF 8 -------------------------------------------------------------------------

df8 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 11, range =  "A2:F6980", col = T)
#REMOVING SA2_NAME
  df8 <- df8[,-2] 

#RENAMING COLUMNS
  
  names(df8) <- c("SA2_CODE16", "year_range", "totals_deaths_NMD", 
  "total_population_NMD","crude_rate_per_100,000_NMD", "age_group", "sex")
  
  #ADDING AGE
  
  df8["age_group"][df8["age_group"] == 0] <- "18-24"
  

# DF 9 -------------------------------------------------------------------------

df9 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 3, range = "A2:G3080", col = T )
#REMOVING SA2_NAME
  df9 <- df9[,-2] 
  
#REMOVING PERSONS COL
  df9 <- df9[,-3]

#RENAMING COLUMNS
  
 names(df9) <- c("SA3_CODE16",  "year_range", "totals_deaths_NMD", 
    "total_population_NMD","crude_rate_per_100,000_NMD", "age_group", "sex")
 
 #ADDING AGE
 
 df9["age_group"][df9["age_group"] == 0] <- "0-17"

 
 # REMOVE "SA3" FROM W/IN DATA
 
 df9$SA3_CODE16 <-gsub("SA3","",as.character(df9$SA3_CODE16))

# DF 10 ------------------------------------------------------------------------
  
df10 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 4, range = "A2:G3080", col = T)
#REMOVING SA2_NAME
  df10 <- df10[,-2] 

#REMOVE PERSONS COL
  df10 <- df10[,-3]
  
#RENAMING COLUMNS
  
 names(df10) <- c("SA3_CODE16", "year_range", "totals_deaths_NMD", 
    "total_population_NMD","age_specific_rate_per_100,000_NMD", "age_group", "sex")
 
 
 #ADDING AGE
 
 df10["age_group"][df10["age_group"] == 0] <- "18-24"
 
 
 # REMOVE "SA3" FROM W/IN DATA
 
 df10$SA3_CODE16 <-gsub("SA3","",as.character(df10$SA3_CODE16))
 
# ---------------------------- # 
# --- MERGING BY INDICATOR --- #
# ---------------------------- #
 
# FILTERS ----------------------------------------------------------------------
 
# as per data dictionary, other NMD  
 
SA3 <- c("SA3_CODE16", "year_range")
SA2 <- c("SA2_CODE16", "year_range")
Aus <- c("australia", "year_range" )

age <- c("age_group")
sex <- c("sex")
#irsd <- c("irsd_quintiles")
 

# 1.1.2 INFANT MORTALITY -------------------------------------------------------
 
 infantmort <- c("totals_deaths_NMD", "crude_rate_per_1000_NMD")

#infant mortality SA3
a <- df1[,c(SA3, age, sex, infantmort)]

#infant mortality national, irsd
#b <- df2[,c(Aus, age, sex, irsd, infantmort)]

# infant mortality SA2
c <- df3[,c(SA2, age, sex, infantmort)]
 
# 1.1.1O BIRTHS ----------------------------------------------------------------
 
 
 birth <- c("totals_births_NMD", "crude_rate_per_1000_NMD")
 
#births SA3
 d <- df1[,c(SA3, age, sex, birth)]
 
 #infant births national, irsd
# e <- df2[,c(Aus, age, sex, irsd, birth)]
 
 #infant mortality SA2
 f <- df3[,c(SA2, age, sex, birth)]

 

# 1.21.1 MORTALITY 0-17 --------------------------------------------------------
# 1.21.2 YOUNG PEOPLE MORTALITY 18-24 ------------------------------------------ 
 
mort1.1 <- c("totals_deaths_NMD", "total_population_NMD","age_specific_rate_per_100,000_NMD")

mort1.2 <- c("totals_deaths_NMD", "total_population_NMD", "crude_rate_per_100,000_NMD")

#SA3 0-17 mortality, age 
g <- df4[,c(SA3, age, sex, mort1.1)]

#SA3 mortality 0-17, sex
m <- df9[,c(SA3, age, sex, mort1.2)]

#SA3 mortality 18-24, age group 
n <- df10[,c(SA3, age, sex, mort1.1)]

# ------------------------------------------------------------------------------

#national mortality, irsd, 0-17
#h <- df5[,c(Aus, age, sex, irsd, mort1.2)]

# RENAMING CALENDAR COLUMNS
#colnames(h)[colnames(h) == "year_range"] ="calendar_year"

#national, irsd 18-24 

#j <- df7[,c(Aus, age, sex,irsd, mort1.2)]

# RENAMING CALENDAR COLUMNS
#colnames(j)[colnames(j) == "year_range"] ="calendar_year"

# ------------------------------------------------------------------------------

#SA2 mortality 0-17

i <- df6[,c(SA2, age, sex, mort1.2)]

#SA2 mortality 18-24

 k <- df8[,c(SA2, age, sex, mort1.2)]


 # MERGING TOGETHER ------------------------------------------------------------

 # - 0 - 17 SA3
 
 gm <- merge(g,m,by = intersect(names(g), names(m)), all.x = T) 
 
 
 
 
 
 # ----------------- # 
 # --- WRITE CSV --- #   RUN BY AIDEN AFTER FINAL CHECK WILL NEED BE MOVED INTO INTERIM FOLDER (WILL SAVE INTO RAW DATA)
 # ----------------- #
 
# 1.1.2 INFANT MORTALITY CSVS 
 
 write.csv(a, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_112_infant_mortality_SA3.csv", row.names = F)
 #write.csv(b, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_112_infant_mortality_National.csv", row.names = F)
 write.csv(c, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_112_infant_mortality_SA2.csv", row.names = F)
 
 
# 1.1.1O BIRTHS CSVS
 
 write.csv(d, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1110_infant_births_SA3.csv", row.names = F)
 #write.csv(e, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1110_infant_births_National.csv", row.names = F)
 write.csv(f, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1110_infant_births_SA2.csv", row.names = F)
 
 
# 1.21.1 MORTALITY CSVS 

 write.csv(gm, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1211_mortality_SA3.csv", row.names = F)
 #write.csv(h, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1211_mortality_national.csv", row.names = F)
 write.csv(i, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1211_mortality_SA2.csv", row.names = F)
 
 # 1.21.2 YOUNG PEOPLE MORTALITY CSVS 
 
 write.csv(df10, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1212_young_people_mortality_SA3.csv", row.names = F)
 #write.csv(j, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1212_young_people_mortality_national.csv", row.names = F)
 write.csv(df8, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1212_young_people_mortality_SA2.csv", row.names = F)
 
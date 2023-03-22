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
                 "total_population_NMD","crude_rate_per_100,000") # old column function alt, shift o to expand 

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
    if(!(names(df)[i] %in% c("SA3_code","SA3_name","Period", "Age group (years)", "Sex"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  
  # REMOVING SA- FROM WITHIN COLUMN 
  
  # str_sub(df1$SA3_code,4,nchar(df1$SA3_code)) FROM AIDEN  :)
  
  #TBD NEED TO EMAIL OWEN
  
  return(df)
}

#DF 1---------------------------------------------------------------------------
df1 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 3, range = "A2:F3080", col = T)
#REMOVING SA2_NAME  
  df1 <- df1[,-2] 

#RENAMING COLUMNS
  names(df1) <- c("SA3_CODE16", "calendar_year", "total_deaths", "total_births", 
                  "crude_rate_per_1000")
# DF2 --------------------------------------------------------------------------
df2 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 4, range = "A30:E89", col = F)
#ADDING NATIONAL COLUMN   
  df2$Australia <- 0 
  
#RENAMING COLUMNS
  
  names(df2) <- c("irsd_quintiles","calendar_year", "total_deaths", 
                   "total_births", "crude_rate_per_1000", "Australia")

#CHANGING COLUMN ORDER - AUS AT FRONT
  
  corder <- c("Australia", "irsd_quintiles","calendar_year", "total_deaths", 
              "total_births", "crude_rate_per_1000")
  
  df2 <- df2[,corder]
  
# DF 3 -------------------------------------------------------------------------
  
df3 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 5, range = "A2:F6965", col = T)
#REMOVING SA2_NAME  
  df3 <- df3[,-2] 
  
#RENAMING COLUMNS
  
  names(df3) <- c("SA2_CODE16", "calendar_year", "total_deaths", "total_births", 
                  "crude_rate_per_1000")

#DF 4 --------------------------------------------------------------------------
  
df4 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 6, range = "A2:G6159", col = T)

#REMOVING SA2_NAME
  df4 <- df4[,-2] 

#RENAMING COLUMNS
  
  names(df4) <- c("SA3_CODE16", "calendar_year", "age_group","total_deaths", 
  "total_population_NMD","age_specific_rate_per_100,000")
  
# DF 5 -------------------------------------------------------------------------

df5 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 7, range = "A30:E89", col = F)

#ADDING NATIONAL COLUMN   
  df5$Australia <- 0
  
#RENAMING COLUMNS
  
  names(df5) <- c("irsd_quintiles","calendar_year", "total_deaths", 
  "total_population_NMD", "crude_rate_per_100,000", "Australia")
  
#CHANGING COLUMN ORDER 
  
  corder <- c("Australia", "irsd_quintiles","calendar_year", "total_deaths", 
              "total_population_NMD", "crude_rate_per_100,000")
  
  df5 <- df5[,corder]
  
# DF 6 -------------------------------------------------------------------------

df6 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 8, range = "A2:F6980", col = T)

#REMOVING SA2_NAME
  df6 <- df6[,-2] 

#RENAMING COLUMNS
  names(df6) <- c("SA2_CODE16", "calendar_year", "total_deaths", 
  "total_population_NMD","crude_rate_per_100,000")
  
  
#DF 7 --------------------------------------------------------------------------

df7 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 10, range = "A30:E89", col = F)

  #ADDING NATIONAL COLUMN 
  df7$Australia <- 0 

#RENAMING COLUMNS
  
 names(df7) <- c("irsd_quintiles","calendar_year", "total_deaths", 
  "total_population_NMD", "crude_rate_per_100,000", "Australia")
 
 
 #REORDERING COLUMNS
 
 corder <- c("Australia", "irsd_quintiles","calendar_year", "total_deaths", 
             "total_population_NMD", "crude_rate_per_100,000")
 
 df7 <- df7[,corder]
 
# DF 8 -------------------------------------------------------------------------

df8 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", sht = 11, range =  "A2:F6980", col = T)
#REMOVING SA2_NAME
  df8 <- df8[,-2] 

#RENAMING COLUMNS
  
  names(df8) <- c("SA2_CODE16", "calendar_year", "total_deaths", 
  "total_population_NMD","crude_rate_per_100,000")
  
# DF 9 -------------------------------------------------------------------------

df9 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 3, range = "A2:G3080", col = T )
#REMOVING SA2_NAME
  df9 <- df9[,-2] 

#RENAMING COLUMNS
  
 names(df9) <- c("SA3_CODE16",  "calendar_year", "sex", "total_deaths", 
    "total_population_NMD","crude_rate_per_100,000")

# DF 10 ------------------------------------------------------------------------
  
df10 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", sht = 4, range = "A2:G3080", col = T)
#REMOVING SA2_NAME
  df10 <- df10[,-2] 
  
#RENAMING COLUMNS
  
 names(df10) <- c("SA3_CODE16", "calendar_year", "sex", "total_deaths", 
    "total_population_NMD","age_specific_rate_per_100,000")
 
 
# ---------------------------- # 
# --- MERGING BY INDICATOR --- #
# ---------------------------- #
 
# FILTERS ----------------------------------------------------------------------
 
# as per data dictionary, other NMD  
 
SA3 <- c("SA3_CODE16", "calendar_year")
SA2 <- c("SA2_CODE16", "calendar_year")
Aus <- c("Australia", "calendar_year" )

age <- c("age_group")
sex <- c("sex")
irsd <- c("irsd_quintiles")
 
# 1.1.2 INFANT MORTALITY -------------------------------------------------------
 
 infantmort <- c("total_deaths", "crude_rate_per_1000")

#infant mortality SA3
a <- df1[,c(SA3, infantmort)]

#infant mortality national, irsd
b <- df2[,c(Aus, irsd, infantmort)]

# infant mortality SA2
c <- df3[,c(SA2, infantmort)]
 
# 1.1.1O BIRTHS ----------------------------------------------------------------
 
 
 birth <- c("total_births", "crude_rate_per_1000")
 
#births SA3
 d <- df1[,c(SA3, birth)]
 
 #infant births national, irsd
 e <- df2[,c(Aus, irsd, birth)]
 
 #infant mortality SA2
 f <- df3[,c(SA2, birth)]
 
 

 
 
# 1.21.1 MORTALITY -------------------------------------------------------------
 
mort1.1 <- c("total_deaths", "total_population_NMD","age_specific_rate_per_100,000")

mort1.2 <- c("total_deaths", "total_population_NMD", "crude_rate_per_100,000")

#SA3 0-17 mortality, age 
g <- df4[,c(SA3, age, mort1.1)]

#SA3 mortality 0-17, sex
m <- df9[,c(SA3, sex, mort1.2)]

#SA3 mortality 18-24, age group 
n <- df10[,c(SA3, sex, mort1.1)]

# ------------------------------------------------------------------------------

#national mortality, irsd
h <- df5[,c(Aus, irsd, mort1.2)]

#national, irsd 18-24 

j <- df7[,c(Aus, irsd, mort1.2)]

# ------------------------------------------------------------------------------

#SA2 mortality 

i <- df6[,c(SA2, mort1.2)]

#SA2 mortality 18-24

 k <- df8[,c(SA2, mort1.2)]


 # MERGING TOGETHER ------------------------------------------------------------

 #national mortality (0-24: 0-17, 18-24)
 
 hj <- merge(h,j,by = intersect(names(h), names(j)), all.x = T) 

#SA3 mortality (0-24: 0-17, 18-24)
 gm <- merge(g,m,by = intersect(names(g), names(m)), all.x = T) 
 gmn <- merge(gm,n,by = intersect(names(gm), names(n)), all.x = T) 
 
#SA2 mortality (0-24: 0-17, 18-24)
 
 ik <- merge(i,k,by = intersect(names(i), names(k)), all.x = T) 
 
 
 # ----------------- # 
 # --- WRITE CSV --- #   RUN BY AIDEN AFTER FINAL CHECK WILL NEED BE MOVED INTO INTERIM FOLDER (WILL SAVE INTO RAW DATA)
 # ----------------- #
 
 # 1.1.2 INFANT MORTALITY CSVS 
 
 # write.csv(a, "NMD_112_infant_mortality_SA3", row.names = F)
 # write.csv(b, "NMD_112_infant_mortality_National", row.names = F)
 # write.csv(c, "NMD_112_infant_mortality_SA2", row.names = F)
 # 
 # 
 # # 1.1.1O BIRTHS CSVS
 # 
 # write.csv(d, "NMD_1110_infant_births_SA3", row.names = F)
 # write.csv(e, "NMD_1110_infant_births_National", row.names = F)
 # write.csv(f, "NMD_1110_infant_births_SA2", row.names = F)
 # 
 # 
 # # 1.21.1 MORTALITY CSVS 
 # 
 # write.csv(hj, "NMD_1211_mortality_National", row.names = F)
 # write.csv(gmn, "NMD_1211_mortality_SA3", row.names = F)
 # write.csv(ik, "NMD_1211_mortality_SA2", row.names = F)
 
 
 
 
  
  
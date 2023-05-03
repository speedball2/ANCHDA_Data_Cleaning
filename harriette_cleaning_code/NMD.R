
#HARRIETTES WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NMD_SA2_SA3")

# LIBARIES ---------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)

# DF LIST ----------------------------------------------------------------------
  # IRSD NOT USED IN PROTOTYPE 

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

# READING IN DATA --------------------------------------------------------------


cleaning <- function(path, sht, range, col, age = NULL, sex, round_col){

  #PATH = FILE PATH
  #SHT = SHEET NUMBER
  #RANGE = COLUMN RANGE
  #COL = COL NAMES T/F
  #SEX = SEX VALUE IN NEW COL
  #AGE = AGE VALUE IN NEW COL
  #ROUND_COL = WHICH COL TO START THE ROUNDING FROM
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  #REMOVING NAS AND NPS FROM DATA
  
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  
  
  # ROUNDING:
  
  #specify round col when reading in data
  for(i in seq(round_col,ncol(df),1)){
    if(!(names(df)[i] %in% c(ncol))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],2)
    }
  }
  
  #ADDING FILTER COLUMNS
  if(!is.null(age)){
    df$age_group <- age
  }
  
  df$sex <- sex
  
  
  #REMOVING NAME COL
  df <- df[ , !names(df) %in% 
              c("SA3_name", "SA2_name")]
  
  
  
  #UPDATING COLUMN NAMES
  
  if ("SA2_code" %in% names(df)) {
    df <- df %>% rename("SA2_CODE16" = "SA2_code",
                        "n_total_deaths" = "Total number of deaths",
                        "n_total_births" = "Total births",
                        "p_crude_rate_per_1000" = "Crude rate (per 1,000 live births)"
    )
  } else if ("SA3_code" %in% names(df)) {
    df <- df %>% rename("SA3_CODE16" = "SA3_code",
                        "n_total_deaths" = "Total deaths",
                        "n_total_births" = "Total births", 
                        "p_crude_rate_per_1000 " = "Crude rate (per 1,000 live births)",
    )
  } else if("Age group (years)" %in% names(df)){
    df <- df %>% rename("SA3_CODE16" = "SA3_code",
                        "age_group" = "Age group (years)",
                        "n_total_deaths" = "Total number of deaths",
                        "n_total_births" = "Total population", 
                        "p_crude_rate_per_1000 " = "Crude rate (per 1,000 live births)")
  }
  
  
  df <- df %>% 
    rename("year_range" = "Period",
           "age_group" = "age_group",
           "sex" = "sex")
  
  

  # #UPDATING COLUMN NANMES  
  # df <- df %>% 
  #   rename("SA3_CODE16" = "SA3_code",
  #          "SA2_CODE16" = "SA2_code",
  #          "year_range" = "Period",
  #          "n_total_deaths" = "Total deaths",
  #          "n_total_births" = "Total births", 
  #          "p_crude_rate_per_1000 " = "Crude rate (per 1,000 live births)",
  #          "age_group" = "age_group", 
  #          "sex" = "sex")
  # 

  
  
  return(df)
}

df1 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 3, "A2:F3080", T, "0-1", "all", round_col = 4)

#REMOVE SA3 FROM SA3 CODE COLUMNS
df1$SA3_CODE16 <-gsub("SA3","",as.character(df1$SA3_CODE16))

df2 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 5, "A2:F6965", T, "0-1", "all", round_col = 4)
df3 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 6, "A2:G6159", T, sex = "all", round_col = 5)
#REMOVE SA3 FROM SA3 CODE COLUMNS
df3$SA3_CODE16 <-gsub("SA3","",as.character(df3$SA3_CODE16))

df4 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 8, "A2:F6980", T, "0-17", "all", round_col = 4)
df5 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 11, "A2:F6980", T, "18-24", "all", round_col = 4)

df6 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", 4, "A2:G3080", T, sex =  "all", round_col = 5)
#REMOVE SA3 FROM SA3 CODE COLUMNS
df6$SA3_CODE16 <-gsub("SA3","",as.character(df6$SA3_CODE16))

#FIXING ERROR IN DATA, UPDATING TO AGE RANGE

df6[df6[,] == "Persons"] <- "18-24"

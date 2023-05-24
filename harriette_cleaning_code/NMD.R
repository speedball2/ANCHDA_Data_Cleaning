
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

# COLUMN NAMES -----------------------------------------------------------------

coln1 <- c("SA3_CODE16","year_range","total_deaths_nmd","total_births_nmd","crude_rate_per_1000_nmd","age_group","sex")
coln2 <- c("SA2_CODE16", "year_range", "total_deaths_nmd", "total_births_nmd","crude_rate_per_1000_nmd", "age_group", "sex")
coln3 <- c("SA3_CODE16", "year_range", "age_group", "total_deaths_nmd", "total_population_nmd","age_specific_rate_per_100,000_nmd", "sex")
coln4 <- c("SA2_CODE16", "year_range", "total_deaths_nmd", "total_population_nmd","crude_rate_per_100,000_nmd", "age_group", "sex")
coln5 <- c("SA2_CODE16", "year_range", "total_deaths_nmd", "total_population_nmd","crude_rate_per_100,000_nmd", "age_group", "sex")
coln6 <- c("SA3_CODE16", "year_range", "age_group", "total_population_nmd","age_specific_rate_per_100,000_nmd","total_deaths_nmd", "sex")

# READING IN DATA --------------------------------------------------------------


cleaning <- function(path, sht, range, col, age = NULL, sex, round_col, coln){

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
  
  #RENAMING COLUMNS
  names(df) <- coln
  
  
  
  
  return(df)
}

df1 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 3, "A2:F3080", T, "0-1", "all", round_col = 4, coln1)

#REMOVE SA3 FROM SA3 CODE COLUMNS
df1$SA3_CODE16 <-gsub("SA3","",as.character(df1$SA3_CODE16))

df2 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 5, "A2:F6965", T, "0-1", "all", round_col = 4, coln2)
df3 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 6, "A2:G6159", T, sex = "all", round_col = 5, coln = coln3)
#REMOVE SA3 FROM SA3 CODE COLUMNS
df3$SA3_CODE16 <-gsub("SA3","",as.character(df3$SA3_CODE16))

df4 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 8, "A2:F6980", T, "0-17", "all", round_col = 4, coln4)
df5 <- cleaning(path = "202211_ANCHDA_suppressed_cells_final.xlsx", 11, "A2:F6980", T, "18-24", "all", round_col = 4, coln5)

df6 <- cleaning(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx", 4, "A2:G3080", T, sex =  "all", round_col = 5, coln =coln6)
#REMOVE SA3 FROM SA3 CODE COLUMNS
df6$SA3_CODE16 <-gsub("SA3","",as.character(df6$SA3_CODE16))

#FIXING ERROR IN DATA, UPDATING TO AGE RANGE

df6[df6[,] == "Persons"] <- "18-24"


#GEO SHAPEFILES FROM ABS (SA2 only) --------------------------------------------

asgs_merge <- function(df){
  
  asgs <- read.csv("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/ASGS2016_SA2_SA3_SA4_code_name_matching_ref_csv/SA2_2016_AUST_no_geom.csv")
  
  asgs <- select(asgs, "SA2_MAINCODE_2016","SA2_5DIGITCODE_2016","SA2_NAME_2016")
  
  colnames(asgs)[colnames(asgs) == "SA2_MAINCODE_2016"] = "SA2_CODE16"
  
  df <- merge(x= asgs, y= df, by = "SA2_CODE16", all.x = T)
  
  #REMOVING NAME COL
  df <- df[ , !names(df) %in% 
              c("SA2_5DIGITCODE_2016","SA2_NAME_2016")]
  
  return(df)
}

df2 <- asgs_merge(df2)
df4 <- asgs_merge(df4)
df5 <- asgs_merge(df5)

# SAVING DATA BY INDICATOR -----------------------------------------------------


# FILTER VARIABLES (SEE DATA DICTIONARY)  

SA3 <- c("SA3_CODE16", "year_range")
SA2 <- c("SA2_CODE16", "year_range")

age <- c("age_group")
sex <- c("sex")



# 1.1.2 INFANT MORTALITY -------------------------------------------------------

infantmort <- c("total_deaths_nmd", "crude_rate_per_1000_nmd")

#infant mortality SA3
a <- df1[,c(SA3, age, sex, infantmort)]

# infant mortality SA2
b <- df2[,c(SA2, age, sex, infantmort)]

# 1.1.1O BIRTHS ----------------------------------------------------------------


birth <- c("total_births_nmd", "crude_rate_per_1000_nmd")

#births SA3
c <- df1[,c(SA3, age, sex, birth)]

#infant mortality SA2
d <- df2[,c(SA2, age, sex, birth)]

# 1.21.1 MORTALITY 0-17 --------------------------------------------------------
mort1.1 <- c("total_deaths_nmd", "total_population_nmd","age_specific_rate_per_100,000_nmd")

mort1.2 <- c("total_deaths_nmd", "total_population_nmd", "crude_rate_per_100,000_nmd")

#SA3 0-17 mortality, age 
e <- df3[,c(SA3, age, sex, mort1.1)]

#SA2 mortality 0-17

f <- df4[,c(SA2, age, sex, mort1.2)]

# 1.21.2 YOUNG PEOPLE MORTALITY 18-24 ------------------------------------------ 

#SA3 mortality 18-24, age group 
g <- df6[,c(SA3, age, sex, mort1.1)]


#SA2 mortality 18-24

h <- df5[,c(SA2, age, sex, mort1.2)]

# WRITE CSVS -------------------------------------------------------------------


# # 1.1.2 INFANT MORTALITY CSVS 

write.csv(a, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_112_infant_mortality_SA3.csv", row.names = F)
write.csv(b, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_112_infant_mortality_SA2.csv", row.names = F)


# 1.1.1O BIRTHS CSVS

write.csv(c, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1110_births_SA3.csv", row.names = F)
write.csv(d, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1110_births_SA2.csv", row.names = F)


# 1.21.1 MORTALITY CSVS

write.csv(e, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1211_mortality_SA3.csv", row.names = F)
write.csv(f, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1211_mortality_SA2.csv", row.names = F)

# 1.21.2 YOUNG PEOPLE MORTALITY CSVS

write.csv(g, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1212_young_people_mortality_SA3.csv", row.names = F)
write.csv(h, "../../../Data_Collections_READY_FOR_QA/NMD//NMD_1212_young_people_mortality_SA2.csv", row.names = F)








options(max.print=999999)


#HARRIETTE'S WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/TableBuilder_Data/Births")

# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)

# READ IN EXCEL ----------------------------------------------------------------


cleaning <- function(path, sht, range){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col_names = F))
  
  # REMOVE COLUMNs 
  df <- df[,-(3:12)]
  df <- df[,-c(2,8)]
  df <- df[,-(5:6)]
  
  
  # #ROUNDING
  for(i in seq(3,ncol(df),2)){
    df[,i] <- as.numeric(df[,i])
    df[,i] <- round(df[,i],2)
  }

  colnames(df) <- c("country_of_birth",  "born_in_Australia", "born_in_same_country", "other_country")
  
  df$National <- 0
  
  return(df)
  
  
}

# OUTPUTS ---------------------------------------------------------------------- 
  
# MOTHER 
df1 <- cleaning("parents_country_of_birth_2021.xlsx", 2, "A8:R95")

df1$sex <- "female"

#FATHER
df2 <- cleaning("parents_country_of_birth_2021.xlsx", 3, "A8:R95")

df2$sex <- "male"


#MJOINING TOGETHER 

full <- rbind(df1,df2)

full$calendar_year <- 2021


#CHANGING COL ORDER 

corder <- c("National","calendar_year", "sex",  "born_in_Australia", "born_in_same_country", "other_country", "country_of_birth")
full <- full[,corder]


# WRITING CSVS -----------------------------------------------------------------

write.csv(full, "../../../../Data_Collections_INTERIM/ABS_births_662_country_of_birth_parents_national.csv", row.names = F)


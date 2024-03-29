
#Harriette's WD
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/ABS_ERP_SA2_LGA")

#LIBARIES

library(readxl)
library(tidyr)
library(reshape2)

#CLEANING CODE -----------------------------------------------------------------

cleaning <- function(file_path, sheet, range, col) {
  # READING IN EXCEL 
  df <- read_xlsx(file_path, sheet, range, col)
  
  # REMOVING SA NAME 
  df <- df[,-3]
  
  #RENAME COLUMNS 
  names(df)[1:2] <- c("calendar_year", "SA2_CODE16")
  
  
  return(df)
  
}


# READING IN FILES -------------------------------------------------------------

#SA2
df1 <- cleaning("Client File-ERP-LS005201 - Copy.xlsx", 2, "A9:BA36681", T)

#2017-2021 NEEDS CORRESPONDANCE LGA
df2 <- cleaning("LS005302 - Client File.xlsx", 2, "A6:BA2741", T)

# RENAMING CODE COLUMNS
colnames(df2)[colnames(df2) == "SA2_CODE16"] ="LGA_CODE21"

#2006-2016 LGA
df3<- cleaning("LS005302 - Client File.xlsx", 3, "A6:BA6199", T)

# RENAMING CODE COLUMNS
colnames(df3)[colnames(df3) == "SA2_CODE16"] ="LGA_CODE16"

# PIVOT WIDE TO LONG -----------------------------------------------------------

# Define columns for splitting genders
# id_cols <- c("SA2_CODE16", "calendar_year")
# m_cols <- paste0("m", 0:24)
# f_cols <- paste0("f", 0:24)


wide_to_long <- function(df, id_cols, m_cols, f_cols) {
  

# Split genders into separate data frames
  male <- df[, c(id_cols, m_cols)]
  female <- df[, c(id_cols, f_cols)]
  
# Remove leading letter for gender column names
  colnames(male) <- gsub("m", "", colnames(male))
  colnames(female) <- gsub("f", "", colnames(female))
  
# Combine age columns into one
  male_long <- reshape2::melt(male, id.vars = id_cols, variable.name = "age_group", value.name = "estimated_regional_population")
  female_long <- reshape2::melt(female, id.vars = id_cols, variable.name = "age_group", value.name = "estimated_regional_population")
  
# Add sex column
  male_long$sex <- "male"
  female_long$sex <- "female"
  
# Merge male and female data frames
  df_long <- rbind(female_long, male_long)
  

  
  return(df_long)
  
}




# ------------------------------------------------------------------------------


df1_full <- wide_to_long(df1, id_cols = c("SA2_CODE16", "calendar_year"), 
                         m_cols = paste0("m", 0:24), f_cols = paste0("f", 0:24))

df2_full <- wide_to_long(df2, id_cols = c("LGA_CODE21", "calendar_year"), 
                         m_cols = paste0("m", 0:24), f_cols = paste0("f", 0:24))

df3_full <- wide_to_long(df3, id_cols = c("LGA_CODE16", "calendar_year"), 
                         m_cols = paste0("m", 0:24), f_cols = paste0("f", 0:24))


#CHANGING ADD TO VISER SPECIFICATIONS ------------------------------------------

change_age <- function(df){
  
  #ADDING ADD NEW COL TO VISER SPECIFICATIONS
  df$age_range <- paste0(df$age_group, "-", df$age_group)
  
  # REMOVE COLUMNS BASED ON NAME
  df = df[,!grepl("age_group", names(df))]
  
  #CHANING COL NAME
  colnames(df)[colnames(df) == "age_range"] ="age_group"
  
  return(df)
}

df1_full <- change_age(df1_full)
df2_full <- change_age(df2_full)
df3_full <- change_age(df3_full)

# REORDERING COLUMNS -----------------------------------------------------------

reorder <- function(df, code) {
  
  col_order <- c(code, "calendar_year", "age_group", "sex", "estimated_regional_population")
  df <- df[, col_order]
  
  return(df)
}

df1_full <- reorder(df1_full, "SA2_CODE16")
df2_full <- reorder(df2_full, "LGA_CODE21")
df3_full <- reorder(df3_full, "LGA_CODE16")

# WRITE CSVS -------------------------------------------------------------------

 write.csv(df1_full, "../../../Data_Collections_INTERIM/ERP/ABS_ERP_181_ERP_SA2.csv", row.names = F)


 write.csv(df2_full, "../../../Data_Collections_INTERIM/ERP/ABS_ERP_181_ERP_LGA_2017_2021_ASGS2021_single_year.csv", row.names = F)
 write.csv(df3_full, "../../../Data_Collections_INTERIM/ERP/ABS_ERP_181_ERP_LGA_2006_2016_ASGS2016_single_year.csv", row.names = F)




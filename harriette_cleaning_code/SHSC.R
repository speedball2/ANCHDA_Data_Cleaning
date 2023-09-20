
#SET WD


#---------------------------#
#--------libraries----------#
#---------------------------#



library(readxl)
library(dplyr)

#-------------------------------------#
#--------S-HSC_age_sex (df1)----------#
#-------------------------------------#



df1 <-read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                 sheet = 2,
                 range = "A3:F20106")

#CHANING COLUMN NAMES
colnames(df1) <- c("SA3_CODE16","SA3_NAME16", "age_group","sex", "year_range", "client_count_SHSC")

#UPDATING COLUMNS
df1$SA3_NAME16 <- NULL

#REORDING COLUMNS
corder <-  c("SA3_CODE16","year_range", "age_group", "sex", "client_count_SHSC")
df1 <- df1[,corder]

#--------------------------------------------------#
#--------S-HSC_presenting_unit_type (df2)----------#
#--------------------------------------------------#

df2 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 3,
                  range = "A3:F60314")

#CHANGING COLUMN NAMES
colnames(df2) <- c("SA3_CODE16","SA3_NAME16","age_group", "year_range", "presenting_unit_type_SHSC", "client_count_SHSC")

#UPDATING COLUMNS
df2$SA3_NAME16 <- NULL
df2$sex <- "all"

#CHANGING COLUMN ORDER
corder <-  c("SA3_CODE16","year_range", "age_group", "sex", "presenting_unit_type_SHSC", "client_count_SHSC")
df2 <- df2[,corder]


#CHANGING CELL VALUES TO A BETTER FORMAT

df2["presenting_unit_type_SHSC"][df2["presenting_unit_type_SHSC"] == "Alone/not part of family"] <- "alone or not apart of a family"
df2["presenting_unit_type_SHSC"][df2["presenting_unit_type_SHSC"] == "Couple with child/ren"] <- "couple with one or more children"
df2["presenting_unit_type_SHSC"][df2["presenting_unit_type_SHSC"] == "Single with child/ren"] <- "couple with one or more children"

#CHANGING TO LOWER CASE FOR CONCISTENCY
df2$presenting_unit_type_SHSC <- tolower(df2$presenting_unit_type_SHSC)




#-------------------------------------------------------------#
#--------S-HSC_main_reasons_seeking_assistance (df3)----------#
#-------------------------------------------------------------#

df3 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 4,
                  range = "A3:F60314")

#CHANGING COLUMN NAMES
colnames(df3) <- c("SA3_CODE16","SA3_NAME16","age_group", "year_range", "main_reasons_seeking_assistance_SHSC", "client_count_SHSC")

#UPDATING COLUMNS
df3$SA3_NAME16 <- NULL
df3$sex <- "all"

#CHANGING COLUMN ORDER
corder <-  c("SA3_CODE16","year_range", "age_group", "sex", "main_reasons_seeking_assistance_SHSC", "client_count_SHSC")
df3 <- df3[,corder]

#LOWERCASE FOR CONCISTENCY
df3$main_reasons_seeking_assistance_SHSC <- tolower(df3$main_reasons_seeking_assistance_SHSC)



#-------------------------------------------------------------------------------

cleaning <- function(df){
  
  #REMOVING CAPITALS FOR M/F
  
  if("sex" %in% names(df)){
    df$sex <- recode(df$sex,
                       "Female" = "female",
                       "Male" = "male",)
    
  }

  
  df <- df[!grepl("Total", df$age_group),]
  
  
  df <- mutate(df, year_range = gsub("(\\d{4})-(\\d{2})", "\\1-20\\2", year_range))
  
  
  return(df)
  
}

df1 <- cleaning(df1)
df2 <- cleaning(df2)
df3 <- cleaning(df3)



#---------------------------#
#--------write csv----------#
#---------------------------#
# 
write.csv(df1, file = "../SHSC_292_specialist_services_client_count_SA3.csv", row.names = F)
write.csv(df2, file = "../SHSC_292_specialist_services_presenting_unit_type_count_SA3.csv", row.names = F)
write.csv(df3, file = "../SHSC_292_specialist_services_main_reason_seeking_assistance_SA3.csv", row.names = F)
#  


#cell suppression


# Define a function to process each dataframe
process_dataframe <- function(df) {
  # Replace values 1, 2, 3, or 4 in client_count_SHSC with 9999999
  df$client_count_SHSC[df$client_count_SHSC %in% c(1, 2, 3, 4)] <- 9999999
  
  # Delete rows where SA3_CODE16 is 99999999
  df <- df[df$SA3_CODE16 != 99999999, ]
  
  return(df)
}

# Process df1
df1 <- process_dataframe(df1)

# Process df2
df2 <- process_dataframe(df2)

# Process df3
df3 <- process_dataframe(df3)

#replace 9999999
df2$client_count_SHSC[df2$age_group == "0-9" & df2$presenting_unit_type_SHSC == "alone or not apart of a family"] <- 9999999


# Assuming df1, df2, and df3 are your processed dataframes
output_dir <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/SHSC/cell_suppressed/"

# Save df1 with the specified filename
write.csv(df1, file.path(output_dir, "SHSC_292_specialist_services_client_count_SA3.csv"), row.names = FALSE)

# Save df2 with the specified filename
write.csv(df2, file.path(output_dir, "SHSC_292_specialist_services_presenting_unit_type_count_SA3.csv"), row.names = FALSE)

# Save df3 with the specified filename
write.csv(df3, file.path(output_dir, "SHSC_292_specialist_services_main_reason_seeking_assistance_SA3.csv"), row.names = FALSE)


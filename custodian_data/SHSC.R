#set working directory ---------------------------------------------------------
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/SHSC_SA3")

#---------------------------#
#--------libraries----------#
#---------------------------#



library(readxl)
library(dplyr)




#df1 = df1


#-------------------------------------#
#--------S-HSC_age_sex (df1)----------#
#-------------------------------------#



df1 <-read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                 sheet = 2,
                 range = "A3:F20106")

colnames(df1) <- c("SA3_CODE16","SA3_NAME16","age_group","sex", "year_range", "client_count_SHSC")

df1$SA3_NAME16 <- NULL

#--------------------------------------------------#
#--------S-HSC_presenting_unit_type (df2)----------#
#--------------------------------------------------#

df2 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 3,
                  range = "A3:F60314")

colnames(df2) <- c("SA3_CODE16","SA3_NAME16","age_group", "year_range", "presenting_unit_type_SHSC", "client_count_SHSC")

df2$SA3_NAME16 <- NULL

#-------------------------------------------------------------#
#--------S-HSC_main_reasons_seeking_assistance (df3)----------#
#-------------------------------------------------------------#

df3 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 4,
                  range = "A3:F60314")

colnames(df3) <- c("SA3_CODE16","SA3_NAME16","age_group", "year_range", "main_reasons_seeking_assistance_SHSC", "client_count_SHSC")

df3$SA3_NAME16 <- NULL


#-------------------------------------------------------------------------------

cleaning <- function(df){
  
  #REMOVING CAPITALS FOR M/F
  
  if("sex" %in% names(df)){
    df$sex <- recode(df$sex,
                       "Female" = "female",
                       "Male" = "male",)
    
  }

  
  df <- df[!grepl("Total", df$age_group),]
  
  return(df)
  
}

df1 <- cleaning(df1)
df2 <- cleaning(df2)
df3 <- cleaning(df3)



#---------------------------#
#--------write csv----------#
#---------------------------#

 write.csv(df1, file = "../../../Data_Collections_READY_FOR_QA/SHSC/SHSC_191_homelessness_client_count_SA3", row.names = F)
 write.csv(df2, file = "../../../Data_Collections_READY_FOR_QA/SHSC/SHSC_191_homelessness_presenting_unit_type_count_SA3", row.names = F)
 write.csv(df3, file = "../../../Data_Collections_READY_FOR_QA/SHSC/SHSC_191_homelessness_main_reason_seeking_assistance_SA3", row.names = F)
 


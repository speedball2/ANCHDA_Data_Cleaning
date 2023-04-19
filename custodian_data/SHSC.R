#set working directory ---------------------------------------------------------


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

colnames(df1) <- c("SA3_CODE16","SA3_NAME16","age_group","sex", "calendar_year", "client_count_SHSC")

df1$SA3_NAME16 <- NULL

#--------------------------------------------------#
#--------S-HSC_presenting_unit_type (df2)----------#
#--------------------------------------------------#

df2 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 3,
                  range = "A3:F60314")

colnames(df2) <- c("SA3_CODE16","SA3_NAME16","age_group", "calendar_year", "presenting_unit_type_SHSC", "client_count_SHSC")

df2$SA3_NAME16 <- NULL

#-------------------------------------------------------------#
#--------S-HSC_main_reasons_seeking_assistance (df3)----------#
#-------------------------------------------------------------#

df3 <- read_excel(col_names = F, path = "Request 4958 SHSC ANCHDA Atlas ad hoc (1).xlsx",
                  sheet = 4,
                  range = "A3:F60314")

colnames(df3) <- c("SA3_CODE16","SA3_NAME16","age_group", "calendar_year", "main_reasons_seeking_assistance_SHSC", "client_count_SHSC")

df3$SA3_NAME16 <- NULL


#----------------------------------------#
#-------- merging by indicator ----------#
#----------------------------------------#

# merging by indicator ---------------------------------------------------------

# 2.9.1 housing amenity: homelessness ------------------------------------------

half <- merge(df1,df2,by = intersect(names(df1), names(df2)), all.x = T)
full <- merge(half,df3,by = intersect(names(half), names(df3)), all.x = T)

col_order <- c("SA3_CODE16","calendar_year", "age_group", "client_count_SHSC", "main_reasons_seeking_assistance_SHSC" , "presenting_unit_type_SHSC")
full <- full[,col_order]

#---------------------------#
#--------write csv----------#
#---------------------------#

 write.csv(full, file = "SHSC_191_homelessness_SA3", row.names = F)



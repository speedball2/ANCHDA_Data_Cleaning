
#---------------------------#
#--------libraries----------#
#---------------------------#

library(readxl)
library(dplyr)

#----------------------------#
#--------incidence-----------#
#----------------------------#

# read in excel ----------------------------------------------------------------
df1 <- read_excel(col_names = T, path = "NDR Type 1 incidence and prevalence by SA4[58].xlsx",
                  sheet = 1,
                  range = "A1:E4258")

#ADDING AGE
df1$age_group <- "0-24"

#naming columns ----------------------------------------------------------------
colnames(df1) <- c("calendar_year","SA4_NAME16","SA4_CODE16","sex","n_number_cases", "age_group")

#reordering columns ------------------------------------------------------------
col_order <- c("SA4_CODE16","calendar_year", "age_group","sex","n_number_cases")

df1 <- df1[,col_order]

#----------------------------#
#--------prevalence----------#
#----------------------------#

# read in excel ----------------------------------------------------------------
df2 <- read_excel(col_names = T, path = "NDR Type 1 incidence and prevalence by SA4[58].xlsx",
                  sheet = 2,
                  range = "A1:F24031")

#naming columns ----------------------------------------------------------------
colnames(df2) <- c("calendar_year","SA4_NAME16","SA4_CODE16","age_group","sex","n_number_of_new_cases")


#reordering columns ------------------------------------------------------------

col_order2 <- c("SA4_CODE16","calendar_year","age_group","sex","n_number_of_new_cases")

df2 <- df2[,col_order2] 


# CONCISTENCY ACROSS DATASETS ----------------------------------------------------------------



concistency <- function(df){
  # REMOVE TOTAL COLS
df <- df[!grepl("Total", df$age_group),]

df <- df[!grepl("Persons", df$sex),]

#REMOVING CAPITALS FOR M/F

if("sex" %in% names(df)){
  df$sex <- recode(df$sex,
                   "Female" = "female",
                   "Male" = "male",)
  
  }

return(df)
}

df1 <- concistency(df1)
df2 <- concistency(df2)

#---------------------------#
#--------write csv----------#
#---------------------------#

write.csv(df1, file = "../../../Data_Collections_READY_FOR_QA/NDR/NDR_192_chronic_conditions_diabetes_incidence.csv", row.names = F)
write.csv(df1, file = "../../../Data_Collections_READY_FOR_QA/NDR/NDR_192_chronic_conditions_diabetes_prevalence.csv", row.names = F)


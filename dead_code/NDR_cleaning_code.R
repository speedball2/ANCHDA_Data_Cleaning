
#---------------------------#
#--------libraries----------#
#---------------------------#

library(readxl)
library(dplyr)

#----------------------------#
#--------incidence-----------#
#----------------------------#

# read in excel ----------------------------------------------------------------
df1 <- cbind(
  read_excel(col_names = T, path = "NDR Type 1 incidence and prevalence by SA4[58].xlsx",
             sheet = 1,
             range = "A1:E4258"))

#naming columns ----------------------------------------------------------------
colnames(df1) <- c("calendar_year","SA4_NAME16","SA4_CODE16","sex","diabetes_indicence_population")

#reordering columns ------------------------------------------------------------
col_order <- c("SA4_CODE16","calendar_year","sex","diabetes_indicence_population")

df1 <- df1[,col_order]

#----------------------------#
#--------prevalence----------#
#----------------------------#

# read in excel ----------------------------------------------------------------
df2 <- cbind(
  read_excel(col_names = T, path = "NDR Type 1 incidence and prevalence by SA4[58].xlsx",
             sheet = 2,
             range = "A1:F24031"))

#naming columns ----------------------------------------------------------------
colnames(df2) <- c("calendar_year","SA4_NAME16","SA4_CODE16","age_group","sex","diabetes_prevelance_population")


#reordering columns ------------------------------------------------------------

col_order2 <- c("SA4_CODE16","calendar_year","sex","age_group","diabetes_prevelance_population")

df2 <- df2[,col_order2] 

# merge together ---------------------------------------------------------------

# 1.12.2 Chronic conditions including asthma, allergies and diabetes -----------

full <- merge(df1,df2,by = intersect(names(df1), names(df2)), all.x = T)


col_order <- c("SA4_CODE16","calendar_year","sex","age_group","diabetes_prevelance_population", "diabetes_indicence_population")
full <- full[,col_order]

#---------------------------#
#--------write csv----------#
#---------------------------#

write.csv(full, file = "NDR_192_chronic_conditions_diabetes.csv", row.names = F)


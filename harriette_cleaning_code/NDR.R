
#Harriette's WD:
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/NDR_SA4")


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

#naming columns ----------------------------------------------------------------
colnames(df1) <- c("calendar_year","SA4_NAME16","SA4_CODE16","sex","diabetes_incidence_population")

#reordering columns ------------------------------------------------------------
col_order <- c("SA4_CODE16","calendar_year","sex","diabetes_incidence_population")

df1 <- df1[,col_order]

#----------------------------#
#--------prevalence----------#
#----------------------------#

# read in excel ----------------------------------------------------------------
df2 <- read_excel(col_names = T, path = "NDR Type 1 incidence and prevalence by SA4[58].xlsx",
                  sheet = 2,
                  range = "A1:F24031")

#naming columns ----------------------------------------------------------------
colnames(df2) <- c("calendar_year","SA4_NAME16","SA4_CODE16","age_group","sex","diabetes_prevalence_population")


#reordering columns ------------------------------------------------------------

col_order2 <- c("SA4_CODE16","calendar_year","sex","age_group","diabetes_prevalence_population")

df2 <- df2[,col_order2] 

# merge together ---------------------------------------------------------------

# 1.12.2 Chronic conditions including asthma, allergies and diabetes -----------

full <- merge(df1,df2,by = intersect(names(df1), names(df2)), all.x = T)


col_order <- c("SA4_CODE16","calendar_year","sex","age_group","diabetes_prevalence_population", "diabetes_incidence_population")
full <- full[,col_order]


# CONCISTENCY ACROSS DATASETS ----------------------------------------------------------------

# REMOVE TOTAL COLS

full <- full[!grepl("Total", full$age_group),]

full <- full[!grepl("Persons", full$sex),]

#REMOVING CAPITALS FOR M/F

if("sex" %in% names(full)){
  full$sex <- recode(full$sex,
                   "Female" = "female",
                   "Male" = "male",)
  
  }

#---------------------------#
#--------write csv----------#
#---------------------------#

write.csv(full, file = "../../../Data_Collections_READY_FOR_QA/NDR/NDR_192_chronic_conditions_diabetes.csv", row.names = F)

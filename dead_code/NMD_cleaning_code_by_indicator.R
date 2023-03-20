

#set working directory 


#---------------------------#
#--------libraries----------#
#---------------------------#

library(readxl)
library(dplyr)

#-------------------------------#
#-------- data frames ----------#
#-------------------------------#

#excel doc: 202211_ANCHDA_suppressed_cells_final.xlsx
# df1 = NMD_SA3_infant_mortality - 4 year rolling , Sheet 3
# df2 = NMD_SA2_infant_mortality - 10 year rolling , Sheet 5
# df3 = NMD_SA3_child_mortality - age, 4 year rolling, Sheet 6
# df4 = NMD_National_child_mortality - IRSD, sheet 7
# df5 = NMD_SA2_child_mortality - sex, 10 year rolling, sheet 8
# df6 = NMD_national_youth_mortality - sheet 10
# df7 = NMD_SA2_youth_mortality_10_year_rolling - sheet 11

#excel doc:202211_ANCHDA_suppressed_cells_SA3_persons.xlsx
#df8 = NMD_SA3_child_mortality_4_year_rolling, sheet 3
#df9 = NMD_SA3_child_mortality_4_year_rolling, sheet 4




#--------------------------------------------------------- #
#--------NMD_SA3_infant_mortality 4 year rolling ----------#
#--------------------------------------------------------- #

# reading in data --------------------------------------------------------------

df1<- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                        sheet = 3,
                        range = "A2:F3080",
                        col_names = T)

# removing NAs and NPs from data -----------------------------------------------

df1[df1[,] == "n.a."] <- NA
df1[df1[,] == "n.p."] <- NA

# removing SA3 Names -----------------------------------------------------------

df1 <- df1[,-2]

# renaming columns -------------------------------------------------------------

colnames(df1) <- c("SA3_CODE16", "calendar_year", "total_deaths", "total_births", 
              "crude_rate_per_1000")

# rounding to two decimal places -----------------------------------------------

df1$crude_rate_per_1000 <- round(as.numeric(df1$crude_rate_per_1000), 2)

#error:  $ operator is invalid for atomic vectors

#----------------------------------------------------------#
#--------NMD_SA2_infant_mortality 10 year rolling----------#
#----------------------------------------------------------#

# reading in data --------------------------------------------------------------

df2 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 5,
                  range = "A2:F6965",
                  col_names = T)

# removing NAs and NPs ---------------------------------------------------------

df2[df2[,] == "n.a."] <- NA
df2[df2[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df2 <- df2[,-2]

# renaming columns -------------------------------------------------------------

colnames(df2) <- c("SA2_CODE16", "calendar_year", "total_deaths", "total_births", 
                   "crude_rate_per_1000")


#----------------------------------------------------------------#
#--------NMD_SA3_child_mortality - age, 4 year rolling ----------#
#----------------------------------------------------------------#

# reading in data --------------------------------------------------------------

df3 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 6,
                  range = "A2:F6159",
                  col_names = T)

# removing NAs and NPs ---------------------------------------------------------

df3[df3[,] == "n.a."] <- NA
df3[df3[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df3 <- df3[,-2]

# renaming columns -------------------------------------------------------------

colnames(df3) <- c("SA3_CODE16", "calendar_year", "age_group","total_deaths", 
                   "total_population_NMD")


#------------------------------------------------------#
#--------NMD_National_child_mortality - IRSD ----------#
#------------------------------------------------------#

# reading in data --------------------------------------------------------------

df4 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 7,
                  range = "A30:E89",
                  col_names = F)

# removing NAs, NPs and renaming IRSD quintiles to numbers ---------------------

df4[df4[,] == "n.a."] <- NA
df4[df4[,] == "n.p."] <- NA

df4[df4[,] == "Quintile 1 (lowest)"] <- "1"
df4[df4[,] == "Quintile 2"] <- "2"
df4[df4[,] == "Quintile 3"] <- "3"
df4[df4[,] == "Quintile 4"] <- "4"
df4[df4[,] == "Quintile 5 (highest)"] <- "5"


# adding national column -------------------------------------------------------

df4$national <- 0

# renaming columns and re ordering columns---------------------------------------

colnames(df4) <- c("irsd_quintiles","calendar_year", "total_deaths", 
                   "total_population_NMD", "crude_rate_per_100,000",
  "STE_CODE16")

col_order <-  c("STE_CODE16", "calendar_year", "irsd_quintiles", "total_deaths", 
  "total_population_NMD", "crude_rate_per_100,000")

df4 <- df4[,col_order]

#----------------------------------------------------------------#
#--------NMD_SA2_child_mortality - sex, 10 year rolling----------#
#----------------------------------------------------------------#

# reading in data --------------------------------------------------------------

df5 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 8,
                  range = "A2:F6980",
                  col_names = T)

# removing NAs, NPs ------------------------------------------------------------

df5[df5[,] == "n.a."] <- NA
df5[df5[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df5 <- df5[,-2]

# renaming columns -------------------------------------------------------------

colnames(df5) <- c("SA2_CODE16", "calendar_year", "total_deaths", 
                   "total_population_NMD","crude_rate_per_100,000")


#------------------------------------------------#
#-------- NMD_national_youth_mortality ----------#
#------------------------------------------------#

# reading in data --------------------------------------------------------------

df6 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 10,
                  range = "A30:E89",
                  col_names = F)

# removing NAs, NPs and renaming IRSD quintiles to numbers ---------------------

df6[df6[,] == "n.a."] <- NA
df6[df6[,] == "n.p."] <- NA

df6[df6[,] == "Quintile 1 (lowest)"] <- "1"
df6[df6[,] == "Quintile 2"] <- "2"
df6[df6[,] == "Quintile 3"] <- "3"
df6[df6[,] == "Quintile 4"] <- "4"
df6[df6[,] == "Quintile 5 (highest)"] <- "5"


# adding national column -------------------------------------------------------

df6$national <- 0


# renaming columns and re ordering columns---------------------------------------

colnames(df6) <- c("irsd_quintiles","calendar_year", "total_deaths", 
                   "total_population_NMD", "crude_rate_per_100,000",
                   "STE_CODE16")

col_order <-  c("STE_CODE16", "calendar_year", "irsd_quintiles", "total_deaths", 
                "total_population_NMD", "crude_rate_per_100,000")

df6 <- df6[,col_order]

#-----------------------------------------------------------#
#-------- NMD_SA2_youth_mortality_10_year_rolling ----------#
#-----------------------------------------------------------#

# reading in data --------------------------------------------------------------

df7 <- read_excel(path = "202211_ANCHDA_suppressed_cells_final.xlsx",
                  sheet = 11,
                  range = "A2:F6980",
                  col_names = T)

# removing NAs, NPs and renaming IRSD quintiles to numbers ---------------------

df7[df7[,] == "n.a."] <- NA
df7[df7[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df7 <- df7[,-2]

# renaming columns -------------------------------------------------------------

colnames(df7) <- c("SA2_CODE16", "calendar_year", "total_deaths", 
                   "total_population_NMD","crude_rate_per_100,000")

#----------------------------------------------------------#
#-------- NMD_SA3_child_mortality_4_year_rolling ----------#
#----------------------------------------------------------#

# reading in data --------------------------------------------------------------

df8<- read_excel(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx",
              sheet = 3,
              range = "A2:G3080",
              col_names = T)

# removing NAs, NPs and renaming IRSD quintiles to numbers ---------------------

df8[df8[,] == "n.a."] <- NA
df8[df8[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df8 <- df8[,-2]

# renaming columns -------------------------------------------------------------

colnames(df8) <- c("SA3_CODE16", "calendar_year", "sex", "total_deaths", 
                   "total_population_NMD","crude_rate_per_100,000")

#----------------------------------------------------------#
#-------- NMD_SA3_child_mortality_4_year_rolling ----------#
#----------------------------------------------------------#

# reading in data --------------------------------------------------------------

df9 <- read_excel(path = "202211_ANCHDA_suppressed_cells_SA3_persons.xlsx",
                  sheet = 4,
                  range = "A2:G3080",
                  col_names = T)

# removing NAs, NPs and renaming IRSD quintiles to numbers ---------------------

df9[df9[,] == "n.a."] <- NA
df9[df9[,] == "n.p."] <- NA

# removing SA2 Names -----------------------------------------------------------

df9 <- df9[,-2]

# renaming columns -------------------------------------------------------------

colnames(df9) <- c("SA3_CODE16", "calendar_year", "sex", "total_deaths", 
                   "total_population_NMD","crude_rate_per_100,000")

#------------------------------------------------#
#-------- merging data frames together ----------#
#------------------------------------------------#

#excel doc: 202211_ANCHDA_suppressed_cells_final.xlsx
# df1 = NMD_SA3_infant_mortality - 4 year rolling , Sheet 3
# df2 = NMD_SA2_infant_mortality - 10 year rolling , Sheet 5
# df3 = NMD_SA3_child_mortality - age, 4 year rolling, Sheet 6
# df4 = NMD_National_child_mortality - IRSD, sheet 7
# df5 = NMD_SA2_child_mortality - sex, 10 year rolling, sheet 8
# df6 = NMD_national_youth_mortality - sheet 10
# df7 = NMD_SA2_youth_mortality_10_year_rolling - sheet 11

#excel doc:202211_ANCHDA_suppressed_cells_SA3_persons.xlsx
#df8 = NMD_SA3_child_mortality_4_year_rolling, sheet 3
#df9 = NMD_SA3_child_mortality_4_year_rolling, sheet 4



#1.1.2 infant mortality
#1.1.10 births
#1.21.1  mortality (changed from child 1-24)


  
# 1.1.2 infant mortality -------------------------------------------------------
########################

# infant mortality columns -----------------------------------------------------
infant_mortality <- c("total_deaths", "crude_rate_per_1000")
infant_filters1 <- c("SA3_CODE16", "calendar_year")
infant_filters2 <- c("SA2_CODE16", "calendar_year")

# infant mortality data frames -------------------------------------------------

a112 <- df1[,c(infant_filters1, infant_mortality)]
b112 <- df2[,c(infant_filters2, infant_mortality)]

# 1.1.10 infant births ---------------------------------------------------------
######################

# infant births columns --------------------------------------------------------
infant_births <- c("total_births", "crude_rate_per_1000")
infant_filters1 <- c("SA3_CODE16", "calendar_year")
infant_filters2 <- c("SA2_CODE16", "calendar_year")

# infant births data frames ----------------------------------------------------
a1110 <- df1[,c(infant_filters1, infant_births)]
b1110 <- df2[,c(infant_filters2, infant_births)]


# merging infant data frames ---------------------------------------------------

ab112 <- merge(a112,b112,by = intersect(names(b112), names(a112)), all.x = T)


ab112 <- merge(x=a112, y=b112, all.x = T,
               by=c("calendar_year","total_deaths","crude_rate_per_1000"))



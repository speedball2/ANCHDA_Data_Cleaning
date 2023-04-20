


#Harriette's WD: 
setwd("c:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/YJMDS_SA4")


# ------------------------------------------------------------------------------

library(readxl)
library(tidyr)
library(dplyr)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, col, rename){
  #path = file path, sht = Sheet number, range = column range, col = col names t/f
  #rename indicator col specific to each sheet in raw data 
  
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  #ROUNDING
  for(i in seq(3,ncol(df))){
    if(!(names(df)[i] %in% c("SA4_CODE_2016"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  # REMOVE NAs, NPs, & —:
  df[df[,] == "—"] <- NA
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
 
  
  #REMOVING NAME COL
  df <- df[ , !names(df) %in% 
              c("SA4_NAME_2016")]
  
  # PIVOTING DATA FROM WIDE TO LONG
  
  df <- gather(df, year_range, indicator, gathercol <- c("2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20","2020-21"))
  
  #DF = DATA FRAME 
  #INDICATOR = COUNTS FOR RATES IN NEW COL (a new col)
  #YEAR_RANGE = NEW COLUMN FOR "GATHERCOL" VARIABLES (a new col)
  #OTHER COLUMNNS FOLLOW
  
  
  # RENAMING VALUES IN A SPECIFIC COL (match data dic)
  
  if("year_range" %in% names(df)){
    df$year_range <- recode(df$year_range,
                     "2006-07" = "2006-2007",
                     "2007-08" = "2007-2008",
                     "2008-09" = "2008-2009",
                     "2009-10" = "2009-2010",
                     "2010-11" = "2010-2011",
                     "2011-12" = "2011-2012",
                     "2012-13" = "2012-2013", 
                     "2013-14" = "2013-2014",
                     "2014-15" = "2014-2015",
                     "2015-16" = "2015-2016",
                     "2016-17" = "2016-2017",
                     "2017-18" = "2017-2018",
                     "2018-19" = "2018-2019",
                     "2019-20" = "2019-2020", 
                     "2020-21" = "2020-2021")
    
  }
  
  #RENAMING COLUMNS NAMES TO SUITABLE COL NAMES
  
  colnames(df)[colnames(df) == "indicator"] = rename
  colnames(df)[colnames(df) == "SA4_CODE_2016"] = "SA4_CODE16"
  
  #ADD FILTER COL (SEX)
  df$sex <- "ALL"
  df$age_group <- "10-17"
  
  
return(df)
  
 
}


# cleaning function for disaggrations ------------------------------------------

cleaning2 <- function(path, sht, range, col, filter, rename){
  #path = file path, sht = Sheet number, range = column range, col = col names t/f
  
  #rename = rename new column added for filter
  #filter = renaming filter col 
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  
  #ROUNDING
  for(i in seq(3,ncol(df))){
    if(!(names(df)[i] %in% c("SA4_CODE_2016"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  #REMOVE EMPTY COL (SEE RAW DATA)
  df <- df[,-3]
  
  colnames(df) <- c("SA4_CODE_2016","SA4_NAME_2016", "2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")
  
  #REMOVING NAME COL
  df <- df[ , !names(df) %in% 
              c("SA4_NAME_2016")]
  
  
  
  df$column <- filter
  
  colnames(df)[colnames(df) == "column"] = rename
  
  return(df)
  
}

# reading in data --------------------------------------------------------------

df1 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 1, "A4:Q111", T, "n_community_based_supervision_on_an_average_day")



df2 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 2, "A4:Q111", T, "n_detention_an_average_day")


#-------------------------------------------------------------------------------

# sex break down ---------------------------------------------------------------
# community based supervision 

#MALES
df3 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 3, "A4:R112", T, "Male", "sex")

df3 <- df3[-1,]

#FEMALES
df4  <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 3, "A114:R220", F, "Feale", "sex")

#detention

#MALES

df5 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 4, "A4:R112", T, "Male", "sex")

df5 <- df5[-1,]


#FEMALES

df6  <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 4, "A114:R220", F, "Female", "sex")


# sentencing breakdown ---------------------------------------------------------

#COMMUNITY SUPERVISION

#SENTENCED

df7 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 7, "A4:R112", T, "Sentenced", "sentencing_status_yjnmds")

df7 <- df7[-1,]

#UNSENTENCED

df8 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 7, "A114:R220", T, "Unsentenced", "sentencing_status_yjnmds")

#IN DETENTION 

#SENTENCED

df9 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 8, "A4:R112", T, "Sentenced", "sentencing_status_yjnmds")

df9 <- df9[-1,]

#UNSENTENCED

df10 <- cleaning2("ATLAS_data_SA4_AYJA_March_2023.xlsx", 8, "A114:R220", T, "Unsentenced", "sentencing_status_yjnmds")


# pivoting data wide to long + cleaning it more! -------------------------------

combine <- function(year_range, indicator, gathercol, dataframe1, dataframe2, rename){
#GATHER COL FUNCTION
  #DF = DATA FRAME 
  #INDICATOR = COUNTS FOR RATES IN NEW COL (a new col)
  #CALENDAR_YEAR = NEW COLUMN FOR "GATHERCOL" VARIABLES (a new col)
  #OTHER COLUMNNS FOLLOW'
  
# DATAFRAME 1&2: dfs rbinded together
  
# rename: indicator column that needs to be renamed

df <- rbind(dataframe1, dataframe2)

df <- gather(df, year_range, indicator, gathercol <- c("2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20","2020-21"))

if("year_range" %in% names(df)){
  df$year_range <- recode(df$year_range,
                          "2006-07" = "2006-2007",
                          "2007-08" = "2007-2008",
                          "2008-09" = "2008-2009",
                          "2009-10" = "2009-2010",
                          "2010-11" = "2010-2011",
                          "2011-12" = "2011-2012",
                          "2012-13" = "2012-2013", 
                          "2013-14" = "2013-2014",
                          "2014-15" = "2014-2015",
                          "2015-16" = "2015-2016",
                          "2016-17" = "2016-2017",
                          "2017-18" = "2017-2018",
                          "2018-19" = "2018-2019",
                          "2019-20" = "2019-2020", 
                          "2020-21" = "2020-2021")
  
}

df$age_group <- "10-17"

colnames(df)[colnames(df) == "indicator"] = rename
colnames(df)[colnames(df) == "SA4_CODE_2016"] = "SA4_CODE16"

return(df)


}

# ------------------------------------------------------------------------------

#COMBINING SUPERVISION M/F

a <- df1

b <- df2

c <- combine(year_range, indicator, gathercol, df3, df4, "n_community_based_supervision_on_an_average_day")

d <- combine(year_range, indicator, gathercol, df5, df6, "n_detention_an_average_day")

e <- combine(year_range, indicator, gathercol, df7, df8, "n_community_based_supervision_on_an_average_day")

c$sex <- "ALL"

f <- combine(year_range, indicator, gathercol, df9, df10, "n_detention_an_average_day")

c$sex <- "ALL"




# changing column order --------------------------------------------------------

column_order <- function(corder, columns){
  #corder = col order changing function
  
  
  corder <- c(columns)
  df <- df[,corder]
  
  return(df)
  
  
  
}


corder <- c("SA4_CODE16", "year_range", "age_group", "sex", "n_community_based_supervision_on_an_average_day")
a <- a[,corder]
c <- c[,corder]

corder.1 <- c("SA4_CODE16", "year_range", "age_group", "sex", "n_detentionon_an_average_day")
b <- b[,corder]







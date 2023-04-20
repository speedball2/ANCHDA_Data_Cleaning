

#Harriette's WD: 
setwd("c:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/YJMDS_SA4")


#to do:
# deal w/ ATSI and m/f disagg 

# ------------------------------------------------------------------------------

library(readxl)
library(tidyr)
library(dplyr)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, col){
  #path = file path, sht = Sheet number, range = column range, col = col names t/f
  
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
  
  df$age_group <- "10-17"
 
return(df)
  
 
}

# reading in data --------------------------------------------------------------

df1 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 1, "A4:Q111", T)

#RENAME INDICATOR COL SPECIFIC PER SHEET
colnames(df1)[colnames(df1) == "indicator"] ="n_community_based_supervision_on_an_average_day"

df2 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 2, "A4:Q111", T)

#RENAME INDICATOR COL SPECIFIC PER SHEET
colnames(df2)[colnames(df2) == "indicator"] ="n_detentionon_an_average_day"

df3 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 3, "A4:R220", T)




df4 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 4, "A4:R220", T)
df5 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 5, "A4:R220", T)
df6 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 6, "A4:R220", T)
df7 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 7, "A4:R220", T)
df8 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 8, "A4:R220", T)


# tidy up/ clean data more -----------------------------------------------------

tidy <- function(df){
  
  df <- df[-1,]
  
  
  
  return(df)
}

df3<- tidy(df3)


#Harriette's WD: 
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/YJMDS_SA4")

# ------------------------------------------------------------------------------

library(readxl)
library(tidyr)
library(dplyr)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, col){
  #path = file path, sht = Sheet number, range = column range, col = col names t/f
  #rename indicator col specific to each sheet in raw data 
  
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  # REMOVE NAs, NPs, & —:
  df[df[,] == "—"] <- 0
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  
  #ROUNDING
  for(i in seq(3,ncol(df))){
    if(!(names(df)[i] %in% c("SA4_CODE_2016"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],2)
    }
  }
  
  #REMOVING NAME COL
  df <- df[ , !names(df) %in% 
              c("SA4_NAME_2016")]
  
  return(df)
  
}

#                 --- COMMUNITY BASED SUPERVISION ---

df1 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 1, "A4:Q111", T)

#                         --- DETENTION ---

df2 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 2, "A4:Q111", T)

#                 --- COMMUNITY BASED SUPERVISION ---
#--- MALES ---
df3 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 3, "A4:R112", T)

#REMOVING FIRST ROW, AND BLANK COL (SEE RAW DATA FOR FORMATTING)
df3 <- df3[-1,]
df3 <- df3[,-2]

#--- FEMALES ---
df4  <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 3, "A114:R220", F)

#REMOVE EMPTY COL (SEE RAW DATA) +SA4 NAME COL
df4 <- df4[,-c(2:3)]


#                         --- DETENTION ---
#--- MALES ---
df5 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 4, "A4:R112", T)

#REMOVING FIRST ROW, AND BLANK COL (SEE RAW DATA FOR FORMATTING)
df5 <- df5[-1,]
df5 <- df5[,-2]

#--- FEMALES ---
df6  <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 4, "A114:R220", F)

#REMOVE EMPTY COL (SEE RAW DATA) +SA4 NAME COL
df6 <- df6[,-c(2:3)]


#                 --- COMMUNITY BASED SUPERVISION ---
#--- SENTENCED ---
df7 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 7, "A4:R112", T) 

#REMOVING FIRST ROW, AND BLANK COL (SEE RAW DATA FOR FORMATTING)
df7 <- df7[-1,]
df7 <- df7[,-2]

                  
#--- UNSENTENCED ---

df8 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 7, "A114:R220", F)

#REMOVE EMPTY COL (SEE RAW DATA) +SA4 NAME COL
df8 <- df8[,-c(2:3)]



#                         --- DETENTION ---

#--- SENTENCED ---

df9 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 8, "A4:R112", T)

#REMOVING FIRST ROW, AND BLANK COL (SEE RAW DATA FOR FORMATTING)
df9 <- df9[-1,]
df9 <- df9[,-2]


#--- UNSENTENCED ---

df10 <- cleaning("ATLAS_data_SA4_AYJA_March_2023.xlsx", 8, "A114:R220", F)

#REMOVE EMPTY COL (SEE RAW DATA) +SA4 NAME COL
df10 <- df10[,-c(2:3)]

# PIVOTING WIDE TO LONG --------------------------------------------------------

wide_to_long <- function(df, rename, sex, status){
  
  
  # CHANGING COLUMN NAMES TO BE CONCISIENT FOR NEXT FUNCTION
  colnames(df) <- c("SA4_CODE_2016", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21")
  
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
    
  
  #ADD FILTER COL (SEX)
  df$sex <- sex
  df$age_group <- "10-17"
  
  corder <- c("SA4_CODE_2016", "year_range", "age_group", "sex", "indicator")
  df <- df[,corder]
    
  #RENAMING COLUMNS NAMES TO SUITABLE COL NAMES
    
    colnames(df)[colnames(df) == "indicator"] = rename
    colnames(df)[colnames(df) == "SA4_CODE_2016"] = "SA4_CODE16"
    

    df$sententing_status <- status
    
    
  
  return(df)
}
                  
 a <- wide_to_long(df1, "n_community_based_supervision_on_an_average_day", "all", NA)  
 b <- wide_to_long(df2, "n_detention_an_average_day", "all", NA)
 c <- wide_to_long(df3, "n_community_based_supervision_on_an_average_day", "male", NA)
 d <- wide_to_long(df4, "n_community_based_supervision_on_an_average_day", "female", NA)
 e <- wide_to_long(df5, "n_detention_an_average_day", "male", NA)
 f <- wide_to_long(df6, "n_detention_an_average_day", "female", NA)
 g <- wide_to_long(df7, "n_community_based_supervision_on_an_average_day", "all", "sentenced")
 h <- wide_to_long(df8, "n_community_based_supervision_on_an_average_day", "all", "unsentenced")
 i <- wide_to_long(df9, "n_detention_an_average_day", "all", "sentenced")
 j <- wide_to_long(df10, "n_detention_an_average_day", "all", "unsentenced")
 
 
 # COMBINE DATA FRAMES TOGETHER ------------------------------------------------
 
 community <- rbind(a,c,d,g,h)
 detention <- rbind(b,e,f,i,j)
                
                  
 # WRITE CSVS ------------------------------------------------------------------                 
                  
 write.csv(community, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/YJNMDS/YJNMDS_3101_juvenile_offenders_community_based_supervision_SA4.csv", row.names = F)
 write.csv(detention, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_READY_FOR_QA/YJNMDS/YJNMDS_3101_juvenile_offenders_in_detention_SA4.csv", row.names = F)
 
                  
                  
                  
#rm(list = ls()) 

#Harriette's WD: 
#insert WD

# Checklist
# 1. use snake case
# 2. append data set collection to end of col name if there is another data collection that covers the same indicators
# 3. no special characters (% -> p)
# 4. use filters defined in data dictionary: https://connectqutedu.sharepoint.com/:u:/r/teams/FOS_PRO_ANCHDA/Shared%20Documents/General/Data_Cleaning_Links/data_dictionary_LINK.url?csf=1&web=1&e=At4u83
#   - add filter cols if not present (e.g, add col with given age, if no age then use "0-24", no sex parameters use "all")
# 5. No geo name needed
# 6. round to two decimal places
# 7. filter cols needs to be present 
# 8. supress values 1-4 (tbd)
# 9. add n_ , p_ to define count or percentage
# 10. save in a folder by data set in QA
# 11. check with asgs is used, note for when Owen's correspondance code works 
#12. concistent cols: code, year, age, sex, indicator, filters
#13. percentage as a .00



library(readxl)

# READING IN/CLEANING DATA -----------------------------------------------------


cleaning <- function(path, sht, range, col){
  #path = file path, sht = Sheet number, range = column range, col = col names t/f
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  # ROUNDING (pulled from AIR):
  for(i in seq(3,ncol(df),1)){
    if(!(names(df)[i] %in% c("Reporting Year"))){
      df[,i] <- as.numeric(df[,i])
      df[,i] <- round(df[,i],1)
    }
  }
  
  
  
  #abs schools -----------------------------------------------------------------
  
  # RENAMING VALUES IN A SPECIFIC COL
  names(df)[names(df) == 'State/Territory'] <- 'STE'
  
  library(dplyr)
  
  if("STE" %in% names(df)){
    df$STE <- recode(df$STE,
                     "a NSW" = 1,
                     "b Vic." = 2,
                     "c Qld" = 3,
                     "d SA" = 4,
                     "e WA" = 5,
                     "f Tas." = 6,
                     "g NT" = 7,
                     "h ACT" = 8,
                     "f Tas." = 6,
                     "i Aust." = 0)
    
    
    #unique(df1$col) <- check above worked, col = col want checked
  }
  
  
  # Adding column based on other column:
  
  df3 <- df3 %>%
    mutate(age_group = case_when(
      endsWith(school_grade, "8") ~ "13-14",
      endsWith(school_grade, "9") ~ "14-15",
      endsWith(school_grade, "10") ~ "15-16",
      endsWith(school_grade, "11") ~ "16-17",
      endsWith(school_grade, "12") ~ "17-18",
    ))
  
  
  # MAKE VALUES W/IN CELLS LOWER CASE 
  
  df[] <- lapply(df, tolower)
  
  
  # assad ----------------------------------------------------------------------
  
  # RENAMING CODE COLUMNS
  colnames(df)[colnames(df) == "National_Code"] ="Australia"
  
  # CHANGING VALUES W/IN DATA 
  df["Age_and_Sex"][df["Age_and_Sex"] == "Overall Total (M/F)"] <- "total"
  
  #FIX SPELLING ERRORS IN RAW DATA 
  df <- df %>% 
    rename_with(~ gsub("cigerette", "cigarette", .x, fixed = TRUE))
  
  # REMOVE COLUMNS BASED ON NAME
  
  df = df[,!grepl("*year", names(df))]
  
  #CHANGING SPECIAL CHATACTERS TO BE NORMAL
  df <- df %>% 
    rename_with(~ gsub("%_", "p_", .x, fixed = TRUE))
  
  
  #SEPERATING AGE AND SEX INTO DIFFERENT COLUMNS (WAS NOT OG IN A FUNC)
  
  df0 <- df0 %>% 
    separate(`Age_and_Sex`, into = c("age_group", "sex"), 
             sep = "(?<=[0-9])(?=\\s?[A-Z])", remove = FALSE) %>% 
    mutate(sex = trimws(sex))
  
  # REMOVE NAs & NPs:
  df[df[,] == "n.a."] <- NA
  df[df[,] == "n.p."] <- NA
  
  return(df)
  
}
  
# ------------------------------------------------------------------------------
    
    
    # country of birth
    
    #CHANGING COL ORDER 
    
    corder <- c("National","calendar_year", "sex",  "born_in_Australia", "born_in_same_country", "other_country", "country_of_birth")
    full <- full[,corder]
    


# ------------------------------------------------------------------------------
    
    # BOCSAR 
    
    library(tidyr)
    
    # PIVOTING DATA FROM WIDE TO LONG
    df <- gather(df, calendar_year, indicator, gathercol <- c("2006", "2007", "2008", "2009","2010","2011","2012", "2013","2014","2015","2016","2017", "2018","2019","2020","2021")) 
    
    #DF = DATA FRAME 
    #INDICATOR = COUNTS FOR RATES IN NEW COL (a new col)
    #CALENDAR_YEAR = NEW COLUMN FOR "GATHERCOL" VARIABLES (a new col)
    #OTHER COLUMNNS FOLLOW


# ------------------------------------------------------------------------------
    
    #ERP 
    
      #single year rangle to year range - create new column 
    
    df$age_range <- paste0(df$age_group, "-", df$age_group)
    










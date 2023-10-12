
# libraries --------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)


# cleaning function ------------------------------------------------------------

cleaning <- function(path, sht, range, new_name, geography_field){
  # path = file path, sht = Sheet number, range = column range 
  # new_name = col name to change
  # geography_field = field indicating the geography (SA4_NAME21 or LGA_NAME21)
  
  # READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col_names = TRUE))
  
  # REMOVING 1ST COL (BLANK)
  df <- df[-1,]
  
  # CONCISE NAMING CONVENTIONS W/ DATA DICTIONARY
  df[df[,] == 0] <- NA
  df[df[,] == "1 to 4"] <- 0
  
  # RENAMING COLUMNS FOR ALL DFS
  names(df)[names(df) == "Age of victim (in years)"] <- "age_group"
  names(df)[names(df) == "Gender"] <- "sex"
  names(df)[names(df) == "Victim's SA4 of residence"] <- "SA4_NAME21"
  names(df)[names(df) == "Victim's LGA of residence"] <- "LGA_NAME21"
  
  # REMOVING RANDOM "Y" FROM DFs
  df$age_group <- gsub("y", "", as.character(df$age_group))
  
  # PIVOTING DATA FROM WIDE TO LONG
  df <- gather(df, calendar_year, indicator, gathercol <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) 
  
  # RENAME INDICATOR COLUMN TO BE SHEET SPECIFIC
  names(df)[names(df) == "indicator"] <- new_name
  
  # CHANGING NAMES SO NEXT FUNC WORKS
  if (geography_field %in% names(df)) {
    rep_str <- c("And" = "and", "Exc" = "exc")
    df[[geography_field]] <- stringr::str_replace_all(df[[geography_field]], rep_str)
  }
  
  # REMOVE VARIABLES IN DATA 
  df <- df[!grepl("Unknown/missing", df$sex),]
  
  # REMOVING CAPITALS FOR M/F
  if ("sex" %in% names(df)) {
    df$sex <- recode(df$sex, "Female" = "female", "Male" = "male")
  }
  
  # CONVERTING 6TH COLUMN TO NUMERIC
  df[[new_name]] <- as.numeric(df[[new_name]])
  
  # GROUP BY age_group, sex, geography_field, calendar_year and calculate total_victims
  result <- df %>%
    group_by(age_group, sex, !!as.name(geography_field), calendar_year) %>%
    summarize(!!as.name(new_name) := sum(!!as.name(new_name), na.rm = TRUE))
  
  
  # Return the result
  return(result)
}

df1 <- cleaning("st23-22883 Victims of DV assault and sexual offences by gender, SA4 and LGA.xlsx", 1, "A15:T186","n_victims_domestic_violence_related_assault", "SA4_NAME21") #DV Assault SA4
df2 <- cleaning("st23-22883 Victims of DV assault and sexual offences by gender, SA4 and LGA.xlsx", 2, "A16:T617", "n_victims_domestic_violence_related_assault", "LGA_NAME21") #DV ASSAULT LGA
df3 <- cleaning("st23-22883 Victims of DV assault and sexual offences by gender, SA4 and LGA.xlsx", 3, "A14:T193", "n_victims_sexual_assault", "SA4_NAME21")
df4 <- cleaning("st23-22883 Victims of DV assault and sexual offences by gender, SA4 and LGA.xlsx", 4, "A14:T619", "n_victims_sexual_assault", "LGA_NAME21")



# reading in geographies for codes ---------------------------------------------
sa4 <- read.csv("C:/Users/00095998/OneDrive - The University of Western Australia/The Mothership/Data_Collections_RAW/public_data/ASGS_2021/SA2_2021_AUST_GDA2020.csv")[, c(8, 9)]


lga <- read_xlsx("LGA_2021_AUST (2).xlsx", 1, cell_limits(c(1, 1), c(NA, 3)), T)

lga <- lga[,-1]

lga <- lga[!duplicated(lga),]

lga$LGA_NAME_2021 <- gsub("\\s+\\(NSW\\)", "", lga$LGA_NAME_2021)
head(lga)

head(sa4)


# MATCH FUNCTION FOR SA CODES AND NAMES ----------------------------------------
sa4_codes <- function(df, corder, indicator){
  
  copy <- df
  
  # REMOVING JUNK COLS
  
  copy <- copy[,-(1:2)]
  
  copy$code <- NA
  
  copy <- copy[,-(2:3)]
  
  # REMOVING DUPLICATE NAMES - SA names only for NSW
  
  copy <- copy[!duplicated(copy),]
  
  #ONLY SA4 FROM ASGS
  
  sa4 <- sa4[!duplicated(sa4),]
  sa4 <- sa4[sa4$SA4_CODE_2021<200,]
  
  #MERGING TWO DATA FRAMES TOGETHER (CUSTODIAN + ABS ASGS)
  
  dummy <- merge(copy,sa4,by.y="SA4_NAME_2021",by.x="SA4_NAME21",all=T)
  
  #REMOVE COLS FROM ASGS FILES
  
  dummy <- dummy[,-2]
  
  #CBIND BACK WITH OTHER DATASET
  
  new <- merge(df, dummy, by = "SA4_NAME21")
  
  # clean up afterwards ---
  
  #REMOVING IN CUSTODY - CANNOT BE GEO CODED 
  new <- new[!grepl("In Custody", new$SA4_NAME21),]
  
  # REMOVING SA4 NAME (NOT NEEDED)
  new <- new[ , !names(new) %in% 
                c("SA4_NAME21")]
  
  # RENAMING CODE COLUMNS
  colnames(new)[colnames(new) == "SA4_CODE_2021"] = "SA4_CODE21"
  
  #CHANGING COLUMN ORDER 
  corder <- c("SA4_CODE21", "calendar_year", "age_group", "sex", indicator)
  new <- new[,corder]
  
  return(new)
}

lga_codes <- function(df, indicator){
  
  
  
  copy2 <- df
  # REMOVING JUNK COLS
  
  copy2 <- copy2[,-(1:2)]
  
  copy2$code <- NA
  
  copy2 <- copy2[,-(2:3)]
  
  # REMOVING DUPLICATE NAMES - SA names only for NSW
  
  copy2 <- copy2[!duplicated(copy2),]
  
  #MERGING TWO DATA FRAMES TOGETHER (CUSTODIAN + ABS ASGS)
  
  dummy2 <- merge(copy2,lga, by.y="LGA_NAME_2021",by.x="LGA_NAME21",all=T)
  
  
  #CBIND BACK WITH OTHER DATASET
  
  new <- merge(df, dummy2, by = "LGA_NAME21")
  
  # clean up afterwards ---
  
  #REMOVING IN CUSTODY - CANNOT BE GEO CODED 
  new <- new[!grepl("In Custody", new$LGA_NAME21),]
  
  test <<- new
  
  # REMOVING SA4 NAME (NOT NEEDED)
  new <- new[ , !names(new) %in% 
                
                c("LGA_NAME21",
                  "code")]
  
  # RENAMING CODE COLUMNS
  colnames(new)[colnames(new) == "LGA_CODE_2021"] = "LGA_CODE21"
  
  corder <- c("LGA_CODE21", "calendar_year", "age_group", "sex", indicator)
  new <- new[,corder]
  
  return(new)
  
}


#SA4
df1_new <- sa4_codes(df1, indicator = "n_victims_domestic_violence_related_assault")
df3_new <- sa4_codes(df3, indicator = "n_victims_sexual_assault")

#LGA
df2_new <- lga_codes(df2, indicator = "n_victims_domestic_violence_related_assault")
df4_new <- lga_codes(df4, indicator = "n_victims_sexual_assault")



# Define a function for data manipulation
process_data <- function(df, indicator_col, group_col) {
  # Get unique combinations of columns 1, 2, and 3
  unique_combinations <- unique(df[, 1:3])
  
  # Create a new data frame with "sex" set to "all" and calculate the sum
  new_rows <- df %>%
    group_by({{group_col}}, calendar_year, age_group) %>%
    summarize(
      sex = "all",
      {{indicator_col}} := sum(as.numeric({{indicator_col}}))
    ) %>%
    ungroup()
  
  # Add the new rows to the original data frame
  df <- rbind(df, new_rows)
  
  # Filter out rows where sex is not "Unknown"
  df <- df %>%
    filter(sex != "Unknown")
  
  return(df)
}

# Process df1_new
df1_new <- process_data(df1_new, indicator_col = n_victims_domestic_violence_related_assault, group_col = SA4_CODE21)

# Process df3_new
df3_new <- process_data(df3_new, indicator_col = n_victims_sexual_assault, group_col = SA4_CODE21)

# Process df2_new and replace SA4_CODE21 with LGA_CODE21
df2_new <- process_data(df2_new, indicator_col = n_victims_domestic_violence_related_assault, group_col = LGA_CODE21)

# Process df4_new and replace SA4_CODE21 with LGA_CODE21
df4_new <- process_data(df4_new, indicator_col = n_victims_sexual_assault, group_col = LGA_CODE21)


#bring in ERP data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##----------------------------------------------------------------------------LGA ERP MUST BE UPDATED TO 2021 !!!!!  -----------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#ERP_LGA <- read_csv("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/abs_erp/erp_sa4_lga/ABS_ERP_181_ERP_LGA.csv")


ERP_SA4 <- read_csv("C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/abs_erp/erp_sa4_lga/ABS_ERP_181_ERP_SA4.csv")
# For ERP_SA4
ERP_SA4 <- ERP_SA4 %>%
  mutate(age = sub("-.*", "", age_group))

# For ERP_LGA
ERP_LGA <- ERP_LGA %>%
  mutate(age = sub("-.*", "", age_group))

# Remove the "age_group" column from ERP_SA4
ERP_SA4 <- ERP_SA4 %>%
  select(-age_group)

# Remove the "age_group" column from ERP_LGA
ERP_LGA <- ERP_LGA %>%
  select(-age_group, -estimated_regional_population_uncertainty_correspondence)

# For ERP_LGA
ERP_LGA <- ERP_LGA %>%
  mutate(age = as.numeric(age),  # Convert "age" column to numeric
         age_group = ifelse(age >= 0 & age <= 17, "0-17", "18-24")) %>%
  group_by(LGA_CODE21, sex, calendar_year, age_group) %>%
  summarise(estimated_regional_population = sum(estimated_regional_population))
# For ERP_LGA
ERP_SA4 <- ERP_SA4 %>%
  mutate(age = as.numeric(age),  # Convert "age" column to numeric
         age_group = ifelse(age >= 0 & age <= 17, "0-17", "18-24")) %>%
  group_by(SA4_CODE21, sex, calendar_year, age_group) %>%
  summarise(estimated_regional_population = sum(estimated_regional_population))


# Convert "calendar_year" to character in ERP_SA4 data frame
ERP_SA4$calendar_year <- as.character(ERP_SA4$calendar_year)
ERP_LGA$calendar_year <- as.character(ERP_LGA$calendar_year)
# Join df1_new with ERP_SA4 to add the "estimated_regional_population" column
df1_new <- left_join(df1_new, ERP_SA4[, c("SA4_CODE21", "sex", "calendar_year", "age_group", "estimated_regional_population")], 
                     by = c("SA4_CODE21", "sex", "calendar_year", "age_group"))


# Join df3_new with ERP_SA4 to add the "estimated_regional_population" column
df3_new <- left_join(df3_new, ERP_SA4[, c("SA4_CODE21", "sex", "calendar_year", "age_group", "estimated_regional_population")], 
                     by = c("SA4_CODE21", "sex", "calendar_year", "age_group"))


# Join df2_new with ERP_LGA to add the "estimated_regional_population" column
df2_new <- left_join(df2_new, ERP_LGA[, c("LGA_CODE21", "sex", "calendar_year", "age_group", "estimated_regional_population")], 
                     by = c("LGA_CODE21", "sex", "calendar_year", "age_group"))

# Join df4_new with ERP_LGA to add the "estimated_regional_population" column
df4_new <- left_join(df4_new, ERP_LGA[, c("LGA_CODE21", "sex", "calendar_year", "age_group", "estimated_regional_population")], 
                     by = c("LGA_CODE21", "sex", "calendar_year", "age_group"))


# Calculate p_victims_domestic_violence_related_assault per 100,000 in df1_new
df1_new <- df1_new %>%
  mutate(per_100000_victims_domestic_violence_related_assault_rate = round((n_victims_domestic_violence_related_assault / estimated_regional_population) * 100000, 2))

# Calculate p_victims_domestic_violence_related_assault per 100,000 in df2_new
df2_new <- df2_new %>%
  mutate(per_100000_victims_domestic_violence_related_assault_rate = round((n_victims_domestic_violence_related_assault / estimated_regional_population) * 100000, 2))

# Calculate p_victims_sexual_assault per 100,000 in df3_new
df3_new <- df3_new %>%
  mutate(p_100000_victims_sexual_assault_rate = round((n_victims_sexual_assault / estimated_regional_population) * 100000, 2))

# Calculate p_victims_sexual_assault per 100,000 in df4_new
df4_new <- df4_new %>%
  mutate(p_100000_victims_sexual_assault_rate = round((n_victims_sexual_assault / estimated_regional_population) * 100000, 2))



# Remove rows with NA values in the first column for df1_new
df1_new <- df1_new %>%
  filter(!is.na(SA4_CODE21))

# Remove rows with NA values in the first column for df2_new
df2_new <- df2_new %>%
  filter(!is.na(LGA_CODE21))

# Remove rows with NA values in the first column for df3_new
df3_new <- df3_new %>%
  filter(!is.na(SA4_CODE21))

# Remove rows with NA values in the first column for df4_new
df4_new <- df4_new %>%
  filter(!is.na(LGA_CODE21))


df1_new <- df1_new %>%
  select(-estimated_regional_population)
df2_new <- df2_new %>%
  select(-estimated_regional_population)
df3_new <- df3_new %>%
  select(-estimated_regional_population)
df4_new <- df4_new %>%
  select(-estimated_regional_population)




#WRITE CSVS --------------------------------------------------------------------
write.csv(df1_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_335_victims_domestic_violence_related_assault_SA4.csv", row.names = FALSE)
write.csv(df2_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_335_victims_domestic_violence_related_assault_LGA.csv", row.names = F)

write.csv(df3_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_337_victims_sexual_assault_SA4.csv",row.names = F)
write.csv(df4_new, file = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/BOCSAR/BOCSAR_337_victims_sexual_assault_LGA.csv",row.names = F)

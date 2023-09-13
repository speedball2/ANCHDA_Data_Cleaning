

# Set WE


# LIBRARIES --------------------------------------------------------------------



library(readxl)
library(dplyr)
library(stringr)


# 

read <- function(path, sht, range, col){
  
  # path = file path
  # sht = Sheet number
  # range = column range
  # col = col names t/f
  
  #READING IN SPREADSHEETS FROM EXCEL
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col))
  
  return(df)
  
  }

#HOSPITALS (lat,long not inc)
df1 <- read("ANCHDA_2923-04-26 data extract.xlsx", 2, "A1:Z22282", T)


#OTHER LOCATIONS (lat, long inc)

df2 <- read.csv("../../../Data_Collections_INTERIM/Service Location Data/NHSD/services_physical.csv")


#CLEANING UP DATA --------------------------------------------------------------

# HOSPITALS --------------------------------------------------------------------

# KEEPING ONLY WANTED COLUMNS

df1_new <- select(df1, "Organisation Name","Healthcare Service Identifier (NHSD)","Healthcare Service Type", "Location Address line_3","Location City","Location Postcode","Location State", "Location Type")

# SUBSET DATASET OF HOSPITALS
df1_new <- subset(df1_new, `Healthcare Service Type` == "Hospital service")
# SUBSET DATASET OF HOSPITALS
df1_new <- subset(df1_new, `Location Type` == "PHYSICAL")


#GEO CODING FILE (NOT FOR FINAL OUT)

write.csv(df1_new, "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_INTERIM/Service Location Data/NHSD/hospitals_to_be_geo_coded.csv", row.names = F)

#CREATING EMPT LAT AND LONG COLS
df1_new$latitude <- NA
df1_new$longitude <- NA

#RE-ORDERING COLUMNS
colorder <- c("latitude", "longitude",  "Organisation Name","Healthcare Service Type","Healthcare Service Identifier (NHSD)")

df1_new <- df1_new[,colorder]

#RENAMING COLUMNS
new_names1 <- c("organisation_name_nhsd", "healthcare_service_identifier_nhsd", "healthcare_service_type_nhsd")

old_names1 <- c("Organisation Name", "Healthcare Service Identifier (NHSD)", "Healthcare Service Type")

colnames(df1_new)[colnames(df1_new) %in% old_names1] <- new_names1

#CONVERTING TO LOWER CASE
df1_new$organisation_name_nhsd = str_to_lower(df1_new$organisation_name_nhsd)
df1_new$healthcare_service_identifier_nhsd = str_to_lower(df1_new$healthcare_service_identifier_nhsd)

#GPS ---------------------------------------------------------------------------
# KEEPING ONLY WANTED COLUMNS
df2_new <- select(df2, "X", "Y", "Name", "Healthcare.Service.Identifier..NHSD.", "Healthcare.Service.Type")

# SUBSET DATASET OF GPS
df2_new <- subset(df2_new, `Healthcare.Service.Type` %in% c("General medical service", "General practice service"))

#RE-ORDERING COLUMNS
colorder <- c("Y", "X", "Name", "Healthcare.Service.Type", "Healthcare.Service.Identifier..NHSD.")

df2_new <- df2_new[,colorder]


#RENAMING COLUMNS
colnames(df2_new)[colnames(df2_new) == "Name"] = "organisation_name_nhsd"
colnames(df2_new)[colnames(df2_new) == "Y"] = "latitude"
colnames(df2_new)[colnames(df2_new) == "X"] = "longitude"
colnames(df2_new)[colnames(df2_new) == "Healthcare.Service.Type"] = "healthcare_service_type_nhsd"
colnames(df2_new)[colnames(df2_new) == "Healthcare.Service.Identifier..NHSD."] = "healthcare_service_identifier_nhsd"

#CONVERTING TO LOWER CASE
df2_new$organisation_name_nhsd = str_to_lower(df2_new$organisation_name_nhsd)
df2_new$healthcare_service_identifier_nhsd = str_to_lower(df2_new$healthcare_service_identifier_nhsd)



# OTHER SERVICES ---------------------------------------------------------------

# KEEPING ONLY WANTED COLUMNS
df3_new <- select(df2, "X", "Y", "Name", "Healthcare.Service.Identifier..NHSD.", "Healthcare.Service.Type")

# SUBSET DATASET OF OTHER SERVICES
df3_new <- subset(df3_new, !(Healthcare.Service.Type %in% c("General medical service", "General practice service")))


#RE-ORDERING COLUMNS
colorder <- c("Y", "X", "Name", "Healthcare.Service.Type", "Healthcare.Service.Identifier..NHSD.")

df3_new <- df3_new[,colorder]


#RENAMING COLUMNS
colnames(df3_new)[colnames(df3_new) == "Name"] = "organisation_name_nhsd"
colnames(df3_new)[colnames(df3_new) == "Y"] = "latitude"
colnames(df3_new)[colnames(df3_new) == "X"] = "longitude"
colnames(df3_new)[colnames(df3_new) == "Healthcare.Service.Type"] = "healthcare_service_type_nhsd"
colnames(df3_new)[colnames(df3_new) == "Healthcare.Service.Identifier..NHSD."] = "healthcare_service_identifier_nhsd"

#CONVERTING TO LOWER CASE
df3_new$organisation_name_nhsd = str_to_lower(df3_new$organisation_name_nhsd)
df3_new$healthcare_service_identifier_nhsd = str_to_lower(df3_new$healthcare_service_identifier_nhsd)


# WRITE CSVS -------------------------------------------------------------------


write.csv(df1_new, "../../../Data_Collections_INTERIM/Service Location Data/NHSD/NHSD_711_hospital_service_locations.csv", row.names = F)
write.csv(df2_new, "../../../Data_Collections_INTERIM/Service Location Data/NHSD/NHSD_712_general_practice_locations.csv", row.names = F)
write.csv(df3_new, "../../../Data_Collections_INTERIM/Service Location Data/NHSD/NHSD_713_healthcare_service_locations.csv", row.names = F)








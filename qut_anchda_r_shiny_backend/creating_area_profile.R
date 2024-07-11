###################################################################
#Title: AREA PROFILE CREATION
#Goal : To create area profile for each SA2, SA3, SA4 and LGA 
#       with STATE AVERAGE and National AVERAGE for each indicators

#Author: Dr.Nishani Musafer



#--------------------------------------------
library(dplyr)

start <- Sys.time()

#reading the R object - all the VISER database
df <- readRDS("viserDB2.RDS")

#creating new age_group indicator which will be easy to get state avg and national avg
df$age_group <- paste0(df$ageRangeMin, "-", df$ageRangeMax)

#creating indicator using all the filterers and indicator name
df$full_indicator <- paste0(df$Year, "-", df$Sex, "-", df$age_group, "-", df$dataItemName)

#remove all STATE level and Australia level data
new_df <- df[-which(df$ResG == "STATE" | df$ResG == "NATIONAL"),]


#stat and national data sva in sperate data frames
STATE_data <- df[which(df$ResG == "STATE"),]

NATIONAL_data <- df[which(df$ResG == "NATIONAL"),]

#######################################


View(new_df[which(new_df$ste_unique_identifier == "2016-FEMALE-5-9-Homeless children and young people (%)-SA4-1"),])
#-----------------------

#creating STATE AVG column & NATIONAL AVG columns

#create new column to identify unique indicator level based on age, year, sex,  geo resolution and state
#Let's say we are calculating STARE AVERAGE for an indicator based on SA3 _. we'll get the averag of that indicator across the all SA3s in corresponding STAE

new_df$ste_unique_identifier <- paste0(new_df$full_indicator, "-", new_df$ResG, "-", substr(new_df$Code,1,1))

#Let's say we are calculating STARE AVERAGE for an indicator based on SA3 _. we'll get the average of that indicator across the all SA3s  acroos the AUSTRALIA
new_df$aus_unique_identifier <- paste0(new_df$full_indicator, "-", new_df$ResG)

#remove any suppression raw from the STATE/NATIONAl AVG calculation
sup_index <- which(new_df$dataItemValue == 9999999)

if(length(sup_index) > 0){
  
  new_df[sup_index, "IndicatorValue"] <- NA
}

#obtaing the STATE and NATIONAL AVG
new_df<- new_df %>% group_by(ste_unique_identifier) %>% mutate(state_avg = round(mean(dataItemValue, na.rm = TRUE),4))

new_df <- new_df %>% group_by(aus_unique_identifier) %>% mutate(national_avg = round(mean(dataItemValue, na.rm = TRUE), 4))
  
if(length(which(is.nan(new_df$state_avg) == TRUE)) > 0){
  
  new_df$state_avg[which(is.nan(new_df$state_avg) == TRUE)] <- NA
}

if(length(which(is.nan(new_df$national_avg) == TRUE)) > 0){
  
  new_df$national_avg[which(is.nan(new_df$national_avg) == TRUE)] <- NA
}


  
###############################
#get unique area code
u_area_code <- unique(new_df$Code)

#path to the output folder 
path <- "C:/Users/Nishani/Queensland University of Technology/ACWA_QUT - General/Data_Shiny_Area_Profile/new_shiny_area_profiles/"


#First for given area code, data will be filtered, then remove the additional columns that have been created to calculate the STATE AVG and NATIONAL AVG.
for(i in 1 :length(u_area_code)){
  
  print(paste0(i,"/", length(u_area_code)))
  area_data <- new_df[which(new_df$Code == u_area_code[i], ),]
  
  area_data <- area_data[, -which(names(area_data) %in% c("full_indicator","ste_unique_identifier","aus_unique_identifier"))]

  #file name contains the unique area code and the geo resolution
  write.csv(area_data, paste0(path,"area_profile_" ,u_area_code[i],"_" ,unique(area_data$ResG), ".csv"), row.names = FALSE)
  
}

print( Sys.time() - start )

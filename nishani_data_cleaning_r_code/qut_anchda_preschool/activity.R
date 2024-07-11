#---------------------------

library(readxl)
library(reshape2)
library(tidyxl)
library(dplyr)
library(openxlsx)
library(tidyr)
#-----------

source("./functions/read_files.R")
source("./functions/creading_correspondence_sheet_list.R")
source("./functions/applying_temporal_correspondance_to_SA2_2016.R")
source("./functions/extract_SA_data_general.R")
source("./functions/join_SA2_2016_to_other_stat_area.R")
source("./functions/rounding_function.R")
source("./functions/rare_edge_cases_treatment.R")

#------------------

filenames <- list.files(path = "./data/", "*.csv", full.names=TRUE)

index <- c(13403 - 11, 13403 - 11, 13877 - 11, 13877 - 11,13877 - 11,20810 -11 )

table_name_pre <- "preschool_attendance_sa2_"

not_all_na <- function(x) any(!is.na(x))

full_data <- NULL

for( i in 1:length(filenames)){
  print(i)
 
 #extracting year from file name
 year <- substr(filenames[i], nchar(filenames[i]) - 7 , nchar(filenames[i]) - 4)
 
 
 #reading csv file
 SA2_data <- read.csv(filenames[i], skip = 11, header = FALSE, check.names = FALSE)  
 
 #remove columns with all na
 SA2_data <- SA2_data %>% select(where(not_all_na))
 
 #extracted required rows
 SA2_data <- SA2_data[1:index[i],]
 
 #new column names
 colnames(SA2_data) <-  c("SA2_CODE16", "sex", "attended", "n_children_attending_preschool")
 
 #select the required "Attended in reference week
 SA2_data <- SA2_data[which(SA2_data$attended == "Attended in reference week" ),]
 
 #remove the unwanted column
 SA2_data <- SA2_data[ , -which(names(SA2_data) == "attended")]

 SA2_data$age_group <- "3-6"
 SA2_data$calendar_year <- year
 SA2_data$sex <- tolower(SA2_data$sex)
 
 print(unique(paste(year, "- ", SA2_data$sex)))
 SA2_data$sex <- ifelse(SA2_data$sex == "male" | SA2_data$sex == "female", SA2_data$sex , NA)
 
 SA2_data <- SA2_data[-which(is.na(SA2_data$sex) == TRUE),]
                      
 #filling the SA2 code
 for(j in 1:nrow(SA2_data)){
   
   if(j %% length(unique(SA2_data$sex))   == 1 ){
     
     SA2_data$SA2_CODE16[j + 1] <- SA2_data$SA2_CODE16[j]
     
     #SA2_data$SA2_CODE16[j + 2] <- SA2_data$SA2_CODE16[j]
     
   }
 }
  
 SA2_data <- SA2_data[, c("SA2_CODE16", "age_group", "sex", "calendar_year", "n_children_attending_preschool" )]
 
 full_data <-  rbind(full_data, SA2_data)
 
}


if(length(which(is.na(full_data$sex) == TRUE) > 0)){
  
  full_data <- full_data[-which(is.na(full_data$sex) == TRUE),]
}


#################################

#creating temporal correspondence data frame for different SA
correspondence_sheet_list <- creading_correspondence_sheet_list()

GEO_TYPE <- "SA2"

GEO_TO <- paste0(GEO_TYPE, "_CODE_2016")

GEO_TO_REQUIRED <- paste0(GEO_TYPE, "_CODE16")

GEO_FROM <- paste0(GEO_TYPE, "_CODE_2011")

#------------------------------------------
#read data


df <- full_data
df$calendar_year <- as.numeric(df$calendar_year)

df_other <- df[which(df$calendar_year > 2017 ),]
df_2011 <- df[which(df$calendar_year <= 2017),]
names(df_2011)[1] <- GEO_FROM

#----------------------------------------


#applying temporal correspondence 2021 ASGS -> 2016 ASGS

#------------------------------------------
corr_year_from = c(rep(c(2006,2011,2021),each=4))
corr_geo_to = c(rep(c("SA2","SA3", "SA4", "LGA"),4))
select_correspondence_indices <- paste0(corr_year_from,corr_geo_to)

df_corr_2016 <- correspondence_sheet_list[[match(paste0(2011,GEO_TYPE),select_correspondence_indices)]]

#updated_df_corr_2021 <- rare_edge_cases_treatment(df_corr_2021, SA2_2016_AUST, GEO_FROM, GEO_TO)

#----------------------------------

filter_names <- names(df_2011)[2:(ncol(df_2011) - 1)]

var_name <- names(df_2011)[ncol(df_2011)]

#--------------------------------------

#applying temporal correspondence on 2021-2022 data

out_df_2011 <- applying_temporal_correspondance (df_2011,GEO_FROM, df_corr_2016, GEO_FROM,  var_name, filter_names, "foward", "n" , GEO_TO, paste0(var_name, "_uncertainty_correspondence"))

names(out_df_2011)[1] <- GEO_TO_REQUIRED

#joint stat area information to  data < 2021
df_other$n_children_attending_preschool_uncertainty_correspondence <- "Good"

new_data <- rbind(out_df_2011, df_other)

new_data <- new_data %>%  drop_na(.data[[GEO_TO_REQUIRED]])%>% drop_na(calendar_year)

new_data[[var_name]]<-   unlist(rounding_fun(new_data[[var_name]]))

new_data[[var_name]] <- round(new_data[[var_name]])

new_data <-  arrange(new_data, eval(parse(text = GEO_TO_REQUIRED)), calendar_year)

new_data <- as.data.frame(new_data)

full_data <- new_data[, 1:5]



##################################

write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_SA2.csv", row.names = FALSE)

#----------------------------------------------------

#geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

#SA3

#SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","SA3_CODE_2016")]

#names(SA3_geom_data) <- c("SA2_CODE16", "SA3_CODE16")

#SA3_geom_data$SA2_CODE16 <- as.character(SA3_geom_data$SA2_CODE16)

#new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )

#new_data <- left_join(full_data, SA3_geom_data, by = "SA2_CODE16" )

#write.csv((new_data[which(is.na(new_data$SA3_CODE16) == TRUE),]), "no_match_SA2_code.csv", row.names = FALSE)

new_data <- full_data

new_data$SA3_CODE16 <- substr(new_data$SA2_CODE16,1,5)

new_data <- new_data %>% group_by(SA3_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("SA3_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]

if(length(which(is.na(new_data$sex) == TRUE) > 0)){
  
  new_data <- new_data[-which(is.na(new_data$sex) == TRUE),]
}
write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_SA3.csv", row.names = FALSE)

#-----------------------

#SA4


# geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)
# 
# 
# SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","SA4_CODE_2016")]
# 
# names(SA3_geom_data) <- c("SA2_CODE16", "SA4_CODE16")
# 
# SA3_geom_data$SA2_CODE16 <- as.character(SA3_geom_data$SA2_CODE16)
# 
# #new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )
#new_data <- left_join(full_data, SA3_geom_data, by = "SA2_CODE16" )

new_data <- full_data

new_data$SA4_CODE16 <- substr(new_data$SA2_CODE16,1,3)

new_data <- new_data %>% group_by(SA4_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("SA4_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]
unique(new_data$SA4_CODE16)

if(length(which(is.na(new_data$sex) == TRUE) > 0)){
  
  new_data <- new_data[-which(is.na(new_data$sex) == TRUE),]
}
write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_SA4.csv", row.names = FALSE)
#-----------------

#STATE


# geom_data <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)
# 
# 
# SA3_geom_data <- geom_data[, c("SA2_MAINCODE_2016","STATE_CODE_2016")]
# 
# names(SA3_geom_data) <- c("SA2_CODE16", "STE_CODE16")
# 
# new_data <- merge(full_data,SA3_geom_data, by = "SA2_CODE16" )

new_data <- full_data

new_data$STE_CODE16 <- substr(new_data$SA2_CODE16,1,1)

new_data <- new_data %>% group_by(STE_CODE16, calendar_year, sex) %>% mutate(new_n_children_attending_preschool = sum(n_children_attending_preschool, na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function

new_data <- new_data[, c("STE_CODE16", "age_group", "sex", "calendar_year", "new_n_children_attending_preschool" )]

names(new_data)[5] <- "n_children_attending_preschool"

new_data <- new_data[!duplicated(new_data), ]

if(length(which(is.na(new_data$sex) == TRUE) > 0)){
  
  new_data <- new_data[-which(is.na(new_data$sex) == TRUE),]
}

write.csv(new_data, "./output/abs_preschools_411_preschool_attendance_STE.csv", row.names = FALSE)


#-------------------------

#cell suppression



filenames <- list.files(path = "./output/", "*.csv", full.names=TRUE)

for(i in 1:length(filenames)){
  
  data <-read.csv(filenames[i], header = TRUE, check.names = FALSE)
  
  index1 <- which(data$n_children_attending_preschool > 0 & data$n_children_attending_preschool < 5 )
  
  if(names(data)[1] == "SA2_CODE16"){
    
    index2 <- which(data$n_children_attending_preschool_uncertainty_correspondence == "Poor" )
    
    index <- unique(c(index1, index2))
    
  }else{
    
    index <- index1
  }
  

  if(length(index) > 0){
    
    
    data[index, "n_children_attending_preschool"] <- 9999999
    
  }
  
  data <- data[, 1:5]
  
  write.csv(data, paste0("./output/cell_suppressed/", basename(filenames[i])), row.names = FALSE)
  
}



#---------------------------
#######NOT TO RUN - RE DIID THE CLEANISNG AND CELL SUPRESSION"#################

#removing rows contain NA for sex ( QA DONE FILES)


filenames <- list.files(path = "./data/QA_DONE/", "*.csv", full.names=TRUE)

for(i in 1:length(filenames)){
  
  data <- read.csv(filenames[i], header = TRUE, check.names = FALSE)
  
  if(length(which(is.na(data$sex) == TRUE) > 0)){
    
    data <- data[-which(is.na(data$sex) == TRUE),]
  }

  
  if(names(data)[1] == "SA2_CODE16"){
    
    print(length(which(data$SA2_CODE16 == 101011001)))
    data[which(data$SA2_CODE16 == 101011001), "SA2_CODE16"] <- 101051539
  }
  
  
  write.csv(data, paste0("./data/QA_DONE/DATA_PROBLEM_FIXED/", basename(filenames[i])), row.names = FALSE )
  
}



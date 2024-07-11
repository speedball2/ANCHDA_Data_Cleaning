


# Temporal concordance function for census data - drafting/testing


library(tidyverse) #for tidyr::fill()
library(readxl)
#---------------------------------

num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}


rounding_fun <- function(x){
  
  
  
  lapply(1:length(x), function(i){
    
    if(is.na(x[i]) == FALSE){
      
      if(num.decimals(x[i]) > 2){
        return((ifelse(round(x[i], 1) %% 1 == 0.5, ceiling(x[i] * 10) / 10, round(x[i], 2))))
      }else{
        return(round(x[i], 2))
      }
    }else{
      
      return(NA)
    }
    
  })
}
#--------------------------------------


#########################################################################################################################################################


#################
### 2006 Files ##
#################

SLA_2006_SA2_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F3858", col_names = TRUE)

SLA_2006_SA2_2016 <-  SLA_2006_SA2_2016[-1,] #remove empty first row

names(SLA_2006_SA2_2016) <- c("SLA_CODE_2006", "SLA_NAME_2006",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",            "PERCENTAGE")


SLA_2006_SA2_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2286", col_names = TRUE)

SLA_2006_SA2_2016_quality <-  SLA_2006_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SLA_2006_SA2_2016 <-  dplyr::left_join(SLA_2006_SA2_2016,SLA_2006_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SSD_2006_SA3_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F644", col_names = TRUE)

SSD_2006_SA3_2016 <-  SSD_2006_SA3_2016[-1,] #remove empty first row


SSD_2006_SA3_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                        sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SSD_2006_SA3_2016_quality <-  SSD_2006_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SSD_2006_SA3_2016 <-  dplyr::left_join(SSD_2006_SA3_2016,SSD_2006_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SD_2006_SA4_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F161", col_names = TRUE)

SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row


SD_2006_SA4_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                                       sheet = 3,
                                       range = "A6:C96", col_names = TRUE)

SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SD_2006_SA4_2016 <-  dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2006_LGA_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                sheet = 4,
                                range = "A6:F906", col_names = TRUE)

LGA_2006_LGA_2016 <-  LGA_2006_LGA_2016[-1,] #remove empty first row  



LGA_2006_LGA_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                        sheet = 3,
                                        range = "A6:C552", col_names = TRUE)

LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016_quality[-1,] #remove empty first row  

# Join quality info onto correspondence sheet
LGA_2006_LGA_2016 <-  dplyr::left_join(LGA_2006_LGA_2016,LGA_2006_LGA_2016_quality,by="LGA_CODE_2016") %>% as.data.frame()



#########################################################################################################################################################

#################
### 2011 Files ##
#################


SA2_2011_SA2_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                sheet = 4,
                                range = "A6:F2433", col_names = TRUE)


names(SA2_2011_SA2_2016) <- c("SA2_CODE_2011", "SA2_NAME_2011",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",           "PERCENTAGE")       

SA2_2011_SA2_2016 <-  SA2_2011_SA2_2016[-1,] #remove empty first row


SA2_2011_SA2_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                        sheet = 3,
                                        range = "A6:C2298", col_names = TRUE)

SA2_2011_SA2_2016_quality <-  SA2_2011_SA2_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA2_2011_SA2_2016 <-  dplyr::left_join(SA2_2011_SA2_2016,SA2_2011_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()

#------------


SA3_2011_SA3_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                sheet = 4,
                                range = "A6:F376", col_names = TRUE)

SA3_2011_SA3_2016 <-  SA3_2011_SA3_2016[-1,] #remove empty first row


SA3_2011_SA3_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                        sheet = 3,
                                        range = "A6:C346", col_names = TRUE)

SA3_2011_SA3_2016_quality <-  SA3_2011_SA3_2016_quality[-1,] #remove empty first row


# Join quality info onto correspondence sheet
SA3_2011_SA3_2016 <-  dplyr::left_join(SA3_2011_SA3_2016,SA3_2011_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()

#-------------

SA4_2011_SA4_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                sheet = 4,
                                range = "A6:F117", col_names = TRUE)

SA4_2011_SA4_2016 <-  SA4_2011_SA4_2016[-1,] #remove empty first row


SA4_2011_SA4_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                        sheet = 3,
                                        range = "A6:C96", col_names = TRUE)

SA4_2011_SA4_2016_quality <-  SA4_2011_SA4_2016_quality[-1,] #remove empty first row

# Join quality info onto correspondence sheet
SA4_2011_SA4_2016 <-  dplyr::left_join(SA4_2011_SA4_2016,SA4_2011_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

#------------

LGA_2011_LGA_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_LGA_2011_LGA_2016.csv")


# quality info already included



#########################################################################################################################################################

################
## 2021 Files ##
################

SA2_2021_SA2_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA2_2016_SA2_2021.csv")
SA3_2021_SA3_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA3_2016_SA3_2021.csv")
SA4_2021_SA4_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA4_2016_SA4_2021.csv")
LGA_2021_LGA_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_LGA_2016_LGA_2021.csv")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Create named list of correspondence DFs to select from in loop below

corr_year_from = c(rep(c(2006,2011,2021),each=4))
corr_geo_to = c(rep(c("SA2","SA3", "SA4", "LGA"),4))
select_correspondence_indices <- paste0(corr_year_from,corr_geo_to)
correspondence_sheet_list = list(SLA_2006_SA2_2016,SSD_2006_SA3_2016,SD_2006_SA4_2016,LGA_2006_LGA_2016,
                                 SA2_2011_SA2_2016,SA3_2011_SA3_2016,SA4_2011_SA4_2016,LGA_2011_LGA_2016,
                                 SA2_2021_SA2_2016,SA3_2021_SA3_2016,SA4_2021_SA4_2016,LGA_2021_LGA_2016)

#---------------------------------------
GEO_TYPE <- "LGA"

origin_folder_path_base <- paste0("./output/",GEO_TYPE ,"/")

destination_folder_path_base <- paste0("./output/",GEO_TYPE ,"_TC/") 

base_name <- "ABS_Births_1110_n_birth"

VALUE_TYPE <- "n"

FILTER_VARS <- c("calendar_year", "sex", "age_group")

VAR_NAME <-"n_births"

GEO_TO <- paste0(GEO_TYPE,"_CODE_2016")

GEO_TO_REQUIRED <- paste0(GEO_TYPE,"_CODE16")

GEO_FROM <- paste0(GEO_TYPE,"_CODE_2021") 


# Select appropriate correspondence sheet for 2021
df_corr_2021 <- correspondence_sheet_list[[match(paste0(2021,GEO_TYPE),select_correspondence_indices)]]

duplicated_code <- df_corr_2021[which(duplicated(df_corr_2021[,GEO_FROM])== TRUE),GEO_FROM]

for(i in 1:length(duplicated_code)){
  
  sub_data <- df_corr_2021[df_corr_2021[,GEO_FROM] == duplicated_code[i],]
  unique_2016 <- unique(sub_data[, GEO_TO])
  print(duplicated_code[i])
  df_corr_2021[df_corr_2021[,GEO_TO] %in% unique_2016 ,"QI_INDICATOR"] <- "Poor"
  
}

#write.csv(df_corr_2021, "duplicated_LGS.csv", row.names = FALSE)
# Read in 2021 data
df_2021_name <- paste0(origin_folder_path_base, base_name,"_",GEO_TYPE,".csv")

df_2021 <- read.csv(df_2021_name, na.strings=c("","NA"), check.names=FALSE)  



df_2021 <- df_2021 %>% as.data.frame()

names(df_2021)[1] <- GEO_FROM

# filters combo
if(length(FILTER_VARS) == 1){
  df_2021$filters_combo <- df_2021[,FILTER_VARS[1]]
} else if(length(FILTER_VARS) == 2){
  df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],sep="_")
} else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
  df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],df_2021[,FILTER_VARS[3]],sep="_")
}else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
  df_2021$filters_combo <- paste(df_2021[,FILTER_VARS[1]],df_2021[,FILTER_VARS[2]],df_2021[,FILTER_VARS[3]],df_2021[,FILTER_VARS[4]],sep="_")
} else {print("check number of filter variables in FILTER_VARS argument")}


#df_2021$calendar_year <- rep(2021,length(df_2021[,1]))

# Outer join
merging_df_2021 <- merge(df_2021, df_corr_2021, by = {{paste0(GEO_TYPE,"_CODE_2021")}},all=T)

#removing obs which are below minimum output size
merging_df_2021 <- merging_df_2021[-which(merging_df_2021$BMOS_NULL_FLAG != 0),]


merging_df_2021$new_vals_raw <- as.numeric(merging_df_2021[,VAR_NAME])

if(VALUE_TYPE == "rate"){
  
  merging_df_2021$new_vals_raw <- merging_df_2021$new_vals_raw  * as.numeric(merging_df_2021$RATIO)
  
}

# group by filter variables and new_geography, find new correspondence calculated vals as sum over multiple entries for target geom (grouped by filter vars and target geom)
merging_df_2021 <- merging_df_2021 %>% 
  group_by(filters_combo,.data[[GEO_TO]]) %>% mutate(new_vals = (sum(eval(parse (text = new_vals_raw )))))


merging_df_2021$new_vals <- unlist(rounding_fun(merging_df_2021$new_vals))



#View(merging_df_2021[which(merging_df_2021$SA2_CODE_2016 == 102011034 ),])


# Rename uncertainty indicator

uncertainty_colname <- paste0(VAR_NAME, "_uncertainty_correspondence")
merging_df_2021[[uncertainty_colname]] <- merging_df_2021$QI_INDICATOR


# ungroup
merging_df_2021 <- merging_df_2021 %>% ungroup()

# Keep only necessary columns
if(length(FILTER_VARS) == 1){
  out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
} else if(length(FILTER_VARS) == 2){
  out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
} else if(length(FILTER_VARS) == 3){ # If there is an additional filter column in the data, use the following line instead
  out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
}else if(length(FILTER_VARS) == 4){ # If there is an additional filter column in the data, use the following line instead
  out_df_2021 <- merging_df_2021 %>% dplyr::select(.data[[FILTER_VARS[1]]], .data[[FILTER_VARS[2]]], .data[[FILTER_VARS[3]]],.data[[FILTER_VARS[4]]], .data[[GEO_TO]], new_vals, .data[[uncertainty_colname]], calendar_year)
} else {print("check number of filter variables in FILTER_VARS argument")}

#rename values column after temporal correspondence
out_df_2021 <- rename(out_df_2021, {{VAR_NAME}} := new_vals)
 
# Remove duplicate rows and NA for geom
out_df_2021 <- distinct(out_df_2021, .keep_all = T) %>% drop_na(.data[[GEO_TO]])

#View(out_df_2021[which(out_df_2021$SA2_CODE_2016 == 102011034 ),])

out_df_2021 <- out_df_2021[, c(GEO_TO, "calendar_year", "sex", "age_group",VAR_NAME, uncertainty_colname)]

# if(length(FILTER_VARS) <= 2){
#   out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]])
# } else if(length(FILTER_VARS) >= 3){
#   out_df_all_years <- rbind(out_df_2006,out_df_2011,out_df_2016,out_df_2021) %>% arrange(calendar_year,.data[[GEO_TO]],age_group,sex)
# } else {print("check number of filter variables in FILTER_VARS argument - arranging out_df_all_years")}


names(out_df_2021)[1] <- GEO_TO_REQUIRED

out_df_2021 <- out_df_2021 %>% drop_na(calendar_year)

if(GEO_TYPE == "LGA"){

  if(length(which(out_df_2021[,1] == 69399)) > 0){

    out_df_2021 <- out_df_2021[-which(out_df_2021[,1] == 69399),]
  }

}

out_df_2021 <- as.data.frame(out_df_2021)

print(length(which(is.na(out_df_2021[,VAR_NAME]) == TRUE)))

# if(length(which(is.na(out_df_2021[,VAR_NAME]) == TRUE))> 0){
#   
#   out_df_2021[which(is.na(out_df_2021[,VAR_NAME]) == TRUE), VAR_NAME] <- "NULL"
#   
# }
write.csv(out_df_2021, paste0(destination_folder_path_base, base_name,"_",GEO_TYPE,".csv"), row.names = FALSE)

#out_df_2021$SA2_CODE16[which(is.na(out_df_2021$n_births) == TRUE)]


#--------------------------------------------


#combined files
GEO_TYPE <- "SA4"
destination_folder_path_base <- paste0("./output/",GEO_TYPE ,"_TC/") 

base_name_n <- "ABS_Births_1110_n_birth"
base_name_p <- "ABS_Births_1114_fertility_rate"



GEO_TO_REQUIRED <- paste0(GEO_TYPE,"_CODE16")

data_3_1_full_birth <- read.csv(paste0(destination_folder_path_base, base_name_n,"_",GEO_TYPE,".csv"), header = TRUE, check.names = FALSE)
  
data_3_1_full_fertility <-  read.csv(paste0(destination_folder_path_base, base_name_p,"_",GEO_TYPE,".csv"), header = TRUE, check.names = FALSE)


combined_data <- left_join(data_3_1_full_birth,data_3_1_full_fertility, by = c(GEO_TO_REQUIRED, "calendar_year", "sex","age_group")) 

write.csv(combined_data, paste0(destination_folder_path_base, "ABS_Births_1110_1114_n_birth_fertility_rate_",GEO_TYPE,".csv"), row.names = FALSE)

#---------------

#cell suppression

source("./functions/cell_suppression.R")



geo_type <- c("SA2", "SA4", "LGA")

for(i in 1:length(geo_type)){
  
  input_folder <- paste0("./output/",geo_type[i],"_TC/" )
  count_file <- paste0(input_folder,"ABS_Births_1110_n_birth_",geo_type[i], ".csv" )
  rate_file <- paste0(input_folder,"ABS_Births_1114_fertility_rate_",geo_type[i], ".csv" )
  
  cell_suppression(count_file, rate_file, "n_births", "fertility_rate", "n_births_uncertainty_correspondence", 3, "ABS_Births_1110_1114_n_birth_fertility_rate_")
  
  
  
    
    
}


  
  


#2021 -> 2016
# Temporal concordance function for census data - drafting/testing

library(tidyverse) #for tidyr::fill()
library(readxl)

#------------------------------
source("./functions/creading_correspondence_sheet_list.R")
source("./functions/applying_temporal_correspondance_to_SA2_2016.R")
source("./functions/extract_SA_data_general.R")
source("./functions/join_SA2_2016_to_other_stat_area.R")
source("./functions/rounding_function.R")
source("./functions/rare_edge_cases_treatment.R")
#-------------------------------

#creating temporal correspondence data frame for different SA
correspondence_sheet_list <- creading_correspondence_sheet_list()

#------------------------

#reading geographical area details
SA2_2016_AUST <- read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)

#---------------------------------------
origin_folder_path_base <- "./data/ABS_Labour_Force_Survey/"

destination_folder_path_base <- "./data/ABS_Labour_Force_Survey_TC/"

base_name <- c("ABS_Labour_Force_Survey_281_households_earning_less_than_1000_SA2")

GEO_TYPE <- c("SA2")

#var_name <- c("n_victims_domestic_violence_related_assault", "n_victims_domestic_violence_related_murder", "n_victims_sexual_assault", "n_victims_sexual_touching")

for(i in 1:length(base_name)){
  
  for(j in 1:length(GEO_TYPE)){
    
    GEO_TO <- paste0(GEO_TYPE[j], "_CODE_2016")
    
    GEO_TO_REQUIRED <- paste0(GEO_TYPE[j], "_CODE16")
    
    GEO_FROM <- paste0(GEO_TYPE[j], "_CODE_2021")
    
    #------------------------------------------
    #read data
    
    df_name <- paste0(origin_folder_path_base, base_name[i],".csv")
    
    df <- read.csv(df_name, na.strings=c("","NA"), check.names = FALSE)  
    df$calendar_year <- as.numeric(df$calendar_year)
    df_2021 <- df
    names(df_2021)[1] <- GEO_FROM
    
    #----------------------------------------
    
    
    #applying temporal correspondence 2021 ASGS -> 2016 ASGS
    
    #------------------------------------------
    corr_year_from = c(rep(c(2006,2011,2021),each=4))
    corr_geo_to = c(rep(c("SA2","SA3", "SA4", "LGA"),4))
    select_correspondence_indices <- paste0(corr_year_from,corr_geo_to)
    
    df_corr_2021 <- correspondence_sheet_list[[match(paste0(2021,GEO_TYPE[j]),select_correspondence_indices)]]
    
    updated_df_corr_2021 <- rare_edge_cases_treatment(df_corr_2021, SA2_2016_AUST, GEO_FROM, GEO_TO)
    
    #----------------------------------
    
    filter_names <- names(df_2021)[2:(ncol(df_2021) - 2)]
    
    var_name1 <- names(df_2021)[ncol(df_2021)]
    
    
    var_name2 <- names(df_2021)[ncol(df_2021) - 1]
    #--------------------------------------
    
    #applying temporal correspondence on 2021-2022 data
    
    out_df_2021_p <- applying_temporal_correspondance (df_2021[, c(names(df_2021)[1],filter_names, var_name1)],GEO_FROM, updated_df_corr_2021, GEO_FROM, var_name1, filter_names, "backward", "p" , GEO_TO, paste0(var_name1, "_uncertainty_correspondence"))
    out_df_2021_n <- applying_temporal_correspondance (df_2021[, c(names(df_2021)[1],filter_names, var_name2)],GEO_FROM, updated_df_corr_2021, GEO_FROM, var_name2, filter_names, "backward", "n" , GEO_TO, paste0(var_name2, "_uncertainty_correspondence"))
    
    out_df_2021 <- left_join(out_df_2021_n,out_df_2021_p, by = c(names(out_df_2021_p)[1],filter_names))
    
    
    names(out_df_2021)[1] <- GEO_TO_REQUIRED
    
    new_data <- out_df_2021
    
    new_data <- new_data %>%  drop_na(.data[[GEO_TO_REQUIRED]])%>% drop_na(calendar_year)
    
    new_data[[var_name1]]<-   unlist(rounding_fun(new_data[[var_name1]]))
    new_data[[var_name2]]<-   unlist(rounding_fun(new_data[[var_name2]]))
    
    new_data <-  arrange(new_data, eval(parse(text = GEO_TO_REQUIRED)), calendar_year)
    
    new_data <- as.data.frame(new_data)
    
    # if(length(which(is.na(new_data[,which(names(new_data) == var_name)]) == TRUE) > 0)){
    #   
    #   new_data[which(is.na(new_data[,which(names(new_data) == var_name)]) == TRUE), var_name] <- "NULL"
    # }
    
    
    new_data$age_group <- gsub(" ", "",  new_data$age_group)
    
    print(paste0("orginal - ", nrow(df), ", new - ", nrow(new_data)))
    
    write.csv(new_data, paste0(destination_folder_path_base, base_name, ".csv"), row.names = FALSE)
    
    #-----------------------------
  }
}


rare_edge_cases_treatment <- function(df_corr_2021, SA2_2016_AUST, GEO_FROM, GEO_TO){
  
  # names(df_corr_2021)[1]<- key_col
  # 
  # new_df_corr_2021 <- merge(df_corr_2021, SA2_2016_AUST, by = key_col, all = T )
  # 
  # new_df_corr_2021$area <- new_df_corr_2021$RATIO * new_df_corr_2021$AREA_ALBERS_SQKM
  # 
  # #calculating the sum area for 2021 SA2
  # new_df_corr_2021 <- new_df_corr_2021 %>%  group_by(eval(parse(text = GEO_FROM))) %>%   mutate(new_total_area = sum(area)) 
  # 
  # new_df_corr_2021$new_ratio <- new_df_corr_2021$area/new_df_corr_2021$new_total_area
  # 
  # updated_df_corr_2021 <- new_df_corr_2021[, c(1:8,ncol(new_df_corr_2021))]
  # 
  # names(updated_df_corr_2021)[1:8] <- names(df_corr_2021)
  
  updated_df_corr_2021 <- df_corr_2021
  
  duplicated_code <- (updated_df_corr_2021[which(duplicated(updated_df_corr_2021[,GEO_FROM])== TRUE),  GEO_FROM ])
  
  names(updated_df_corr_2021)[1] <- GEO_TO
  
  for(i in 1:length(duplicated_code)){
    
    sub_data <- updated_df_corr_2021[updated_df_corr_2021[,GEO_FROM] == as.numeric(duplicated_code[i]),]
    
    unqiue_geom_to <- unique(sub_data[, GEO_TO])
    
      
    print(duplicated_code[i])
      
    updated_df_corr_2021[updated_df_corr_2021[,GEO_TO] %in% unqiue_geom_to[i],"QI_INDICATOR"] <- "Poor"
    
  }
  
  return(updated_df_corr_2021)
}
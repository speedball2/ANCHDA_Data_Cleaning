library(dplyr)
cell_suppression_value <- 9999999

cut_off_value <- 5
#----------------------
cell_suppression_func <- function(data){
  #checking for "n" column
  if("n" %in% names(data)){
    
    #checking for uncertainty column
    if ("uncertainty" %in% names(data)){
      
      #if the uncertainty == 2, do the suppression
      if(length(which(data[,"uncertainty" ] == 2)) > 0){
        
        data[which(data[,"uncertainty" ] == 2), "n" ] <- cell_suppression_value
      }
    }else{
      cat("Warning: 'uncertainty' column is missing in data", "\n")
    }
    
    #if the data < do cut_off_value the suppression
    if(any(data[,"n"] < cut_off_value)){
      
      data[which(data[,"n"] < cut_off_value), "n"] <-  cell_suppression_value
      
    }
  }else{
    
    cat("Error: 'n' column is missing in data", "\n")
  }
  
  return(data)
}
#-----------------------------

#read "n" file
count_data <- read.csv("./output/psychological_n_p/ABS_NHS_151_n_young_people_18_to_24_psychological_distress_STE.csv",header = TRUE, check.names = FALSE)

#read "p" file
propotion_data <- read.csv("./output/psychological_n_p/ABS_NHS_151_p_young_people_18_to_24_psychological_distress_STE.csv",header = TRUE, check.names = FALSE)

#calling suppression 
sup_count_data <- cell_suppression_func(count_data)

#left join the proportion table to count table & remove uncertainty columns
merge_DATA <- left_join(sup_count_data,propotion_data, by = c("STE_CODE16", "age_group", "sex","year_range", names(count_data)[5])) %>%
  select(-contains("uncertainty"))

#perform suppression in p column if there suppression was performed in n column 
if(length(which(merge_DATA$n== cell_suppression_value)) > 0){
  merge_DATA[which(merge_DATA$n== cell_suppression_value), "p"] <- cell_suppression_value
}

write.csv(merge_DATA,"./output/psychological_n_p/ABS_NHS_151_young_people_18_to_24_psychological_distress_STE.csv", row.names = FALSE)


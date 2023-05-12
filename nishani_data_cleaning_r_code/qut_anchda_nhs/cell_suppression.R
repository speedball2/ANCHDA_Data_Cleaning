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
    if(length(which(data[,"n"] < cut_off_value)) > 0){
      
      data[which(data[,"n"] < cut_off_value), "n"] <-  cell_suppression_value
      
    }
  }else{
    
    cat("Error: 'n' column is missing in data", "\n")
  }
  
  return(data)
}
#-----------------------------

#if we have seprate n file and p file
#read "n" file
count_data <- read.csv("./output/psychological_n_p/ABS_Births_1110_n_birth_LGA.csv",header = TRUE, check.names = FALSE)

#read "p" file
propotion_data <- read.csv("./output/psychological_n_p/ABS_Births_1114_fertility_rate_LGA.csv",header = TRUE, check.names = FALSE)

#Apply "NULL" fo the proportion with #prefix (uncertainty == 1)


if(length(which(propotion_data$uncertainty == 1)) > 0){
  
  propotion_data$p[which(propotion_data$uncertainty == 1)] <- "NULL"
  
}

#calling suppression 
sup_count_data <- cell_suppression_func(count_data)


#left join the proportion table to count t able & remove uncertainty columns
merge_DATA <- left_join(sup_count_data,propotion_data, by = c("STE_CODE16", "age_group", "sex","year_range", names(count_data)[5])) %>%
  select(-contains("uncertainty"))

#perform suppression in p column if there suppression was performed in n column 
if(length(which(merge_DATA$n== cell_suppression_value)) > 0){
  merge_DATA[which(merge_DATA$n== cell_suppression_value), "p"] <- cell_suppression_value
}

#check any value for p > 1

which(merge_DATA$p > 1 & merge_DATA$p != 9999999 & merge_DATA$p != "NULL" )

if(length(which(merge_DATA$p > 1 & merge_DATA$p != 9999999 & merge_DATA$p != "NULL" )) > 0){
  
  merge_DATA$p[which(merge_DATA$p > 1 & merge_DATA$p != 9999999 & merge_DATA$p != "NULL")] <- 1
}


write.csv(merge_DATA,"./output/psychological_n_p/ABS_Births_1114_fertility_rate_LGA", row.names = FALSE)



#----------------

#if both n and p in same file

data <- read.csv("./output/AIR_121_fully_immunised_SA4.csv",header = TRUE, check.names = FALSE)

names(data)

#calling suppression 
data <- cell_suppression_func(data)

merge_DATA <- data
#perform suppression in p column if there suppression was performed in n column 
if(length(which(merge_DATA$n== cell_suppression_value)) > 0){
  merge_DATA[which(merge_DATA$n== cell_suppression_value), "p"] <- cell_suppression_value
}


merge_DATA <- merge_DATA %>% select(-contains("uncertainty"))
which(merge_DATA$p > 1 & merge_DATA$p != 9999999 & merge_DATA$p != "NULL" )
write.csv(merge_DATA,"./output/AIR_121_fully_immunised_SA4.csv", row.names = FALSE)



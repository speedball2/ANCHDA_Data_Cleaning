
library(dplyr)

#------------------------

#This function calculates the state average by year for given indicator in the dataset.

calculation_state_average <- function(new_data, inicator_name, year){
  
  data <- new_data
  
  if(!is.null(data)){
    if(!is.null(inicator_name)){
      
       #extract the firs digit if the site code
       data$state <- substr(data[,1],1,1)
       data <- data %>%
        group_by(state, eval(parse(text = year)))%>% #calculate the mean by grouping the state and year
        mutate(state_avg = mean(eval(parse(text = inicator_name)), na.rm=TRUE)) %>%
        mutate(across(where(is.numeric), function(x) ifelse(round(x, 1) %% 1 == 0.5, ceiling(x * 10) / 10, round(x, 2)))) #rounding function
      
        names(data)[which(names(data)== "state_avg")] <- paste("state_avg_", inicator_name, sep = "") #change the name of the average 
        data <- data[ ,c(names(new_data), paste("state_avg_", inicator_name, sep = ""))] #remove the state column 
       return(data)
    }else{
      
      cat("Error: inicator_name is not given", "\n")
    }
  }else{
    
    cat("Error: data set is not given", "\n")
  }

}

#################################
#read files

filenames <- list.files("./data/sa3", pattern="*.csv", full.names=TRUE)



for(i in filenames){
  
  

  input_data <- read.csv(i, header = TRUE, check.names = FALSE)
  
  if( "naplan_score_acara" %in% names(input_data)){
    
    if("calendar_year"  %in% names(input_data)){
      
      output_data <- calculation_state_average(input_data, "naplan_score_acara", "calendar_year")
    }else{
      
      cat("file: ", i, " doesen't contain ", "calendar_year", "\n")
    }
  }else{
    
     cat("file: ", i, " doesen't contain ", "naplan_score_acara", "\n")
  }
  
  write.csv(output_data, paste("./data/sa3_state_avg/", substr(i,12, nchar(i))), row.names = FALSE )
  
}
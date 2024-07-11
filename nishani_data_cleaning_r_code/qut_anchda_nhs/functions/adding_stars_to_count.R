#This function combines uncertainty character to the value column
adding_stars_to_count <- function(table_old, star_data){
  
  #adding uncertainty level 1 character to the value
  if(nrow(star_data$one_data) > 0){
    for(i in 1:nrow(star_data$one_data)){
      if((star_data$one_data[i,2] <= ncol(table_old)) & (star_data$one_data[i,1] <= nrow(table_old))){
        
        table_old[star_data$one_data[i,1], star_data$one_data[i,2]] <- paste(table_old [star_data$one_data[i,1], star_data$one_data[i,2]], "one", sep = "")
      }
    }
  }
  
  #adding uncertainty level 2 character to the value
  if(nrow(star_data$two_data) > 0 ){
    
    for(j in 1:nrow(star_data$two_data)){
      
      if((star_data$two_data[j,2] <= ncol(table_old))& (star_data$two_data[j,1] <= nrow(table_old))){
        
        table_old[star_data$two_data[j,1], star_data$two_data[j,2]] <- paste(table_old [star_data$two_data[j,1], star_data$two_data[j,2]], "two", sep = "")
      }
    }
  }
  return(table_old)
}
 
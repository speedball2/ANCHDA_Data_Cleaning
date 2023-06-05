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
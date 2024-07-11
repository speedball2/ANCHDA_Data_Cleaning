#This function creates the required output table for ABS NHS data sets 
output_table_creation_household_income <- function( table_old, site_code,value_name, sex_cat, calendar_year){
  
 
  #change the name of the first column 
  names(table_old)[1] <- site_code
  
  #extract the 9 font 9 digts 
  table_old[, site_code] <- substr(table_old[, site_code],1,9)
  
  
  #gender column creation
  
  table_old$sex <- NA
  #if we just need to extract one sex category
  if(is.null(sex_cat) == FALSE){
    table_old$sex <- sex_cat
   
  }
  
  #creating long table
  names(table_old) <- gsub("–", "-",names(table_old))
  long_table_old <- melt(table_old, id.vars = c(site_code, "sex"), value.name = value_name )
  long_table_old$age_group <- NA
  long_table_old$age_group <- as.character(long_table_old$variable)
  
  #removing variable column as we created age_group column in previous step
  long_table_old <- long_table_old[, -which(names(long_table_old) == "variable")]
  
  #removing rows with at least one NA
  long_table_old <- long_table_old[rowSums(is.na(long_table_old)) == 0,]
  
  #covert total column to numeric and get the actual count 
  long_table_old[,value_name] <- as.numeric(long_table_old[,value_name])
  
  #calendar_year creation
  long_table_old$calendar_year  <- calendar_year

  #change the order of the columns
  long_table_old <-long_table_old[,c(site_code ,"sex", "age_group", "calendar_year", value_name)]
  
  return(long_table_old)
}


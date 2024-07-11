#This function creates the required output table for ABS NHS data sets 
output_table_creation <- function(table_old, indicator_name,filename,  multipy, bmi_categories, star_data, value_name, sex_cat){
  
  
  #change the name of the first column & change the strings the column to lower case
  #indicator_name <- tolower(indicator_name)
  names(table_old)[1] <- indicator_name
  table_old[1:nrow(table_old),indicator_name] <-unlist( lapply(1:nrow(table_old), function(x)tolower(table_old[x,indicator_name])))
  
  
  #combine the uncertainty level character
  table_old <- adding_stars_to_count(table_old, star_data)
  
  
  #gender column creation
  #gender_index <- which((table_old[,indicator_name]) %in% sex) # this not working, need
  
  if(length(sex_cat) > 1){
    gender_index <- unlist(lapply(1:length(sex), function(y) which(table_old[,indicator_name] == sex[y]) ))
    s_gender_index <- sort(gender_index)
    s_sex <- sex[order(gender_index)]
    table_old$sex <- NA
    for(j in 1:(length(s_gender_index))){
      
      if( j < length(s_gender_index) ){
        table_old$sex[s_gender_index[j]:(s_gender_index[j+1]- 1)] <- substr(s_sex[j],1, nchar(s_sex[j])- 1)
      }else{
        table_old$sex[(s_gender_index[j]):length(table_old$sex)] <- substr(s_sex[j],1, nchar(s_sex[j])- 1)
      }
    }
  }
  
  #if we just need to extract one sex category
  if(length(sex_cat)== 1){
    
    table_old$sex <- sex_cat
    table_old$sex <- substr(table_old$sex,1, nchar(table_old$sex)- 1)
  }
  
  
  #creating long table
  names(table_old) <- gsub("–", "-",names(table_old))
  long_table_old <- melt(table_old, id.vars = c(indicator_name, "sex"), value.name = value_name )
  long_table_old$age_group <- NA
  long_table_old$age_group <- as.character(long_table_old$variable)
  
  #removing variable column as we created age_group column in previous step
  long_table_old <- long_table_old[, -which(names(long_table_old) == "variable")]
  
  #removing rows with at least one NA
  long_table_old <- long_table_old[rowSums(is.na(long_table_old)) == 0,]
  
  #adding uncertainty column
  long_table_old$uncertainty <- 0
  
  #creating uncertainty column 
  if(length(grep("one", long_table_old[, value_name])) > 0){
    long_table_old$uncertainty[grep("one", long_table_old[,value_name])] <- 1
    long_table_old[, value_name] <- gsub("one","",long_table_old[,value_name])
  }
  if(length(grep("two", long_table_old[, value_name])) > 0){
    long_table_old$uncertainty[grep("two", long_table_old[,value_name])] <- 2
    long_table_old[,value_name] <- gsub('two',"",long_table_old[,value_name])
  }
  
  #covert total column to numeric and get the actual count 
  long_table_old[,value_name] <- as.numeric(long_table_old[,value_name])
  long_table_old[,value_name] <- long_table_old[,value_name] *multipy
  
  #selected row with bmi_categories
  long_table_old <- long_table_old[long_table_old[, indicator_name] %in% bmi_categories,]
  
  #year range extraction
  long_table_old$year_range <- NA
  combined_year_range <- substr(filename,nchar(filename) - 12, nchar(filename) - 5 )
  long_table_old$year_range <- paste(substr(combined_year_range,1,4), substr(combined_year_range,5,8), sep = "-")
  
  
  #site code extraction
  long_table_old$STE_CODE16 <- NA
  site_value <- lapply(site_code, function(x){
    if(length(grep(x,filename)) == 1){
      return(site_cat[which(site_code == x)])
    }
  })
  long_table_old$STE_CODE16 <- unlist(site_value)
  
  #change the order of the columns
  long_table_old <-long_table_old[,c("STE_CODE16" ,"age_group", "sex", "year_range",indicator_name, value_name, "uncertainty")]
  return(long_table_old)
}


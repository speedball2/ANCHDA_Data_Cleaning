#this is the main function which combines all the output files from seperate states
main_function <- function(bmi_categories,indicator_name, filenames, sheet_name, data_range, starting_row, row_removed, format_1, format_2, multiply, value_name, further_modi_bmi, sex_cat){
  
  output_list <- lapply(1:length(filenames), function(x){
    
    #reading table 
    table_input<- read_files(filenames[x], sheet_name , data_range)
    #removing empty row in the input table
    table_input <- table_input[-1 * (1:row_removed), ]
    #get the row and column number for cells with uncertain values
    stars_output <- finding_cell_with_one_and_two_stars(filenames[x], sheet_name, starting_row, row_removed, format_1,  format_2)
    #output table
    table_output <- output_table_creation(table_input,indicator_name, filenames[x], multiply, bmi_categories, stars_output, value_name, sex_cat)
    
    #if the user want to remove the any special character from the catergories
    if(further_modi_bmi == "YES"){
      table_output[, indicator_name] <- gsub("\\s*\\([^\\)]+\\)","",as.character(table_output[,indicator_name]))
    }
    
    table_output[, indicator_name] <- unlist(lapply(1:nrow(table_output), function(x)gsub(" ", "_", table_output[x,indicator_name])))
    
    table_output
  })
  
  return(do.call("rbind", output_list))
}

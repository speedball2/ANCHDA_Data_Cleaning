
#reading excel files
read_files <- function(file_name, sheet_name, data_range){
  
  return(read_excel(file_name, sheet_name, data_range, col_names = TRUE, col_types = "text"))
  
}

#finding the cell with uncertainty character
finding_cell_with_one_and_two_stars <- function(filename, sheet_name, starting_row_number, number_rows_removed, format_1, format_2){
  
  xlfile <- filename
  
  #obtain the all the format in the excel
  formats <- xlsx_formats(xlfile)
  #read the cells related to the given sheet
  cells   <- xlsx_cells(xlfile, sheets = sheet_name)
  #store the row and column number of cells for uncertainty level1
  one_star_cells <- NULL
  #store the row and column number of cells for uncertainty level2
  two_star_cells <- NULL
  
  #finding row and column number for uncertainty level1
  if(is.null(format_1) == FALSE){
    one_star <- which(formats$local$numFmt == format_1 )
    one_star_cells<-cells[ cells$local_format_id %in% one_star, c("row", 'col' ) ]
    one_star_cells$row <- one_star_cells$row - starting_row_number - number_rows_removed
  }
  
  #finding row and column number for uncertainty level2
  if(is.null(format_2) == FALSE){
    two_star <- which(formats$local$numFmt == format_2)
    two_star_cells<-cells[ cells$local_format_id %in% two_star, c("row", 'col' ) ]
    two_star_cells$row <- two_star_cells$row - starting_row_number - number_rows_removed
  }
  
  one_star_cells <- one_star_cells[one_star_cells$row > 0,]
  
  two_star_cells <- two_star_cells[two_star_cells$row > 0,]
  return(list(one_data = as.data.frame(one_star_cells), two_data = as.data.frame(two_star_cells) ))
}



#btesting fuction 

library(readxl)

setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/from_custodians/NDSHS_STE_National")

df1 <- read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
                 sheet = 2, #AOD State by Status
                 range = "A6:AJ50")

xlxs <- function(x,y){
 
   read_xlsx(col_names = F, path = "NDSHS_Final data tables.xlsx",
            sheet = x, 
            range = y)
  
}

df2 <- xlxs(x = 2, y = "A6:AJ50")


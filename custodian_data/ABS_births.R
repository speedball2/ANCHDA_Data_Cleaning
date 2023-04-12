

#HARRIETTE'S WD:

setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/public_data/TableBuilder_Data/Births")

# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)
library(dplyr)


# ------------------------------ #
# --- reading in excel files --- #
# ------------------------------ #

cleaning <- function(path, sht, range){
  #path = file path, sht = Sheet number, range = column range, col = col names 
  
  df <- as.data.frame(read_xlsx(path,
                                sht,
                                range,
                                col_names = T))
  
  #df[df[,] == "np"] <- NA
  
  # for(i in seq(3,ncol(df),2)){
  #   df[,i] <- as.numeric(df[,i])
  #   df[,i] <- round(df[,i],2)
  # }
  
}



# ------------------------------------- #
# --- CLEANING INDIVIDUAL DATA SETS --- #
# ------------------------------------- #


df1 <- cleaning(path = "births_LGA_2011_2021.xlsx", sht = 2, range = "A8:AS138")


# library(dplyr)
# df <- data.frame(V1 = seq(26), V2 = letters)
# 
# df %>% filter(row_number() %% 2 != 0) ## Delete even-rows
# df %>% filter(row_number() %% 2 != 1) ## Delete odd-rows
# df %>% filter(row_number() %% 3 != 1)



# ------------------------------------------------------------------------------














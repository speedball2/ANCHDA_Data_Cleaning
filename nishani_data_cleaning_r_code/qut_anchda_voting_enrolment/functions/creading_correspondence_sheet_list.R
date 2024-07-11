creading_correspondence_sheet_list <- function(){
  
  
  #########################################################################################################################################################
  
  
  #################
  ### 2006 Files ##
  #################
  
  SLA_2006_SA2_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                  sheet = 4,
                                  range = "A6:F3858", col_names = TRUE)
  
  SLA_2006_SA2_2016 <-  SLA_2006_SA2_2016[-1,] #remove empty first row
  
  names(SLA_2006_SA2_2016) <- c("SLA_CODE_2006", "SLA_NAME_2006",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",            "PERCENTAGE")
  
  
  SLA_2006_SA2_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SLA_2006_SA2_2016.xls",
                                          sheet = 3,
                                          range = "A6:C2286", col_names = TRUE)
  
  SLA_2006_SA2_2016_quality <-  SLA_2006_SA2_2016_quality[-1,] #remove empty first row
  
  
  # Join quality info onto correspondence sheet
  SLA_2006_SA2_2016 <-  dplyr::left_join(SLA_2006_SA2_2016,SLA_2006_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()
  
  #------------
  
  
  SSD_2006_SA3_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                  sheet = 4,
                                  range = "A6:F644", col_names = TRUE)
  
  SSD_2006_SA3_2016 <-  SSD_2006_SA3_2016[-1,] #remove empty first row
  
  
  SSD_2006_SA3_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SSD_2006_SA3_2016.xls",
                                          sheet = 3,
                                          range = "A6:C346", col_names = TRUE)
  
  SSD_2006_SA3_2016_quality <-  SSD_2006_SA3_2016_quality[-1,] #remove empty first row
  
  
  # Join quality info onto correspondence sheet
  SSD_2006_SA3_2016 <-  dplyr::left_join(SSD_2006_SA3_2016,SSD_2006_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()
  
  #-------------
  
  SD_2006_SA4_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                                 sheet = 4,
                                 range = "A6:F161", col_names = TRUE)
  
  SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row
  
  
  SD_2006_SA4_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                                         sheet = 3,
                                         range = "A6:C96", col_names = TRUE)
  
  SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row
  
  # Join quality info onto correspondence sheet
  SD_2006_SA4_2016 <-  dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()
  
  #------------
  
  LGA_2006_LGA_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                  sheet = 4,
                                  range = "A6:F906", col_names = TRUE)
  
  LGA_2006_LGA_2016 <-  LGA_2006_LGA_2016[-1,] #remove empty first row  
  
  
  
  LGA_2006_LGA_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_LGA_2006_LGA_2016.xlsx",
                                          sheet = 3,
                                          range = "A6:C552", col_names = TRUE)
  
  LGA_2006_LGA_2016_quality <-  LGA_2006_LGA_2016_quality[-1,] #remove empty first row  
  
  # Join quality info onto correspondence sheet
  LGA_2006_LGA_2016 <-  dplyr::left_join(LGA_2006_LGA_2016,LGA_2006_LGA_2016_quality,by="LGA_CODE_2016") %>% as.data.frame()
  
  
  
  #########################################################################################################################################################
  
  #################
  ### 2011 Files ##
  #################
  
  
  SA2_2011_SA2_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                  sheet = 4,
                                  range = "A6:F2433", col_names = TRUE)
  
  
  names(SA2_2011_SA2_2016) <- c("SA2_CODE_2011", "SA2_NAME_2011",     "SA2_CODE_2016", "SA2_NAME_2016",     "RATIO",           "PERCENTAGE")       
  
  SA2_2011_SA2_2016 <-  SA2_2011_SA2_2016[-1,] #remove empty first row
  
  
  SA2_2011_SA2_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA2_2011_SA2_2016.xls",
                                          sheet = 3,
                                          range = "A6:C2298", col_names = TRUE)
  
  SA2_2011_SA2_2016_quality <-  SA2_2011_SA2_2016_quality[-1,] #remove empty first row
  
  
  # Join quality info onto correspondence sheet
  SA2_2011_SA2_2016 <-  dplyr::left_join(SA2_2011_SA2_2016,SA2_2011_SA2_2016_quality,by="SA2_CODE_2016") %>% as.data.frame()
  
  #------------
  
  
  SA3_2011_SA3_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                  sheet = 4,
                                  range = "A6:F376", col_names = TRUE)
  
  SA3_2011_SA3_2016 <-  SA3_2011_SA3_2016[-1,] #remove empty first row
  
  
  SA3_2011_SA3_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA3_2011_SA3_2016.xls",
                                          sheet = 3,
                                          range = "A6:C346", col_names = TRUE)
  
  SA3_2011_SA3_2016_quality <-  SA3_2011_SA3_2016_quality[-1,] #remove empty first row
  
  
  # Join quality info onto correspondence sheet
  SA3_2011_SA3_2016 <-  dplyr::left_join(SA3_2011_SA3_2016,SA3_2011_SA3_2016_quality,by="SA3_CODE_2016") %>% as.data.frame()
  
  #-------------
  
  SA4_2011_SA4_2016 <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                  sheet = 4,
                                  range = "A6:F117", col_names = TRUE)
  
  SA4_2011_SA4_2016 <-  SA4_2011_SA4_2016[-1,] #remove empty first row
  
  
  SA4_2011_SA4_2016_quality <- read_excel("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_SA4_2011_SA4_2016.xls",
                                          sheet = 3,
                                          range = "A6:C96", col_names = TRUE)
  
  SA4_2011_SA4_2016_quality <-  SA4_2011_SA4_2016_quality[-1,] #remove empty first row
  
  # Join quality info onto correspondence sheet
  SA4_2011_SA4_2016 <-  dplyr::left_join(SA4_2011_SA4_2016,SA4_2011_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()
  
  #------------
  
  LGA_2011_LGA_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2011_2016/CG_LGA_2011_LGA_2016.csv")
  
  
  # quality info already included
  
  
  
  #########################################################################################################################################################
  
  ################
  ## 2021 Files ##
  ################
  
  SA2_2021_SA2_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA2_2016_SA2_2021.csv")
  SA3_2021_SA3_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA3_2016_SA3_2021.csv")
  SA4_2021_SA4_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_SA4_2016_SA4_2021.csv")
  LGA_2021_LGA_2016 <- read.csv("C:/Users/Nishani/Documents/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_LGA_2016_LGA_2021.csv")
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Create named list of correspondence DFs to select from in loop below
  
  corr_year_from = c(rep(c(2006,2011,2021),each=4))
  corr_geo_to = c(rep(c("SA2","SA3", "SA4", "LGA"),4))
  select_correspondence_indices <- paste0(corr_year_from,corr_geo_to)
  correspondence_sheet_list = list(SLA_2006_SA2_2016,SSD_2006_SA3_2016,SD_2006_SA4_2016,LGA_2006_LGA_2016,
                                   SA2_2011_SA2_2016,SA3_2011_SA3_2016,SA4_2011_SA4_2016,LGA_2011_LGA_2016,
                                   SA2_2021_SA2_2016,SA3_2021_SA3_2016,SA4_2021_SA4_2016,LGA_2021_LGA_2016)
  
  return(correspondence_sheet_list)
}
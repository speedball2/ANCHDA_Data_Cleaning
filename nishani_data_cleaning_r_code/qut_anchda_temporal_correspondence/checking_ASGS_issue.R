
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


View(SA2_2011_SA2_2016)

data_code <- unique(full_data$SA2_CODE16)

sa2_code16 <- unique(SA2_2011_SA2_2016$SA2_CODE_2016)

diff <- setdiff(data_code, sa2_code16)

sa2_code11 <- unique(SA2_2011_SA2_2016$SA2_CODE_2011)

diff2 <- setdiff(data_code, sa2_code11)

diff <- as.data.frame(diff)
names(diff) <- "SA2_CODE_IN_DATA_NOT_MATCH_WITH_SA2_2016"

diff$match_with_SA2_CODE11 <- "YES"

diff$match_with_SA2_CODE11[which(diff$SA2_CODE_IN_DATA_NOT_MATCH_WITH_SA2_2016 %in% diff2 )] <- "NO"

diff$year <- as.character(unlist(lapply(1:length(diff$SA2_CODE_IN_DATA_NOT_MATCH_WITH_SA2_2016), function(x){
  
  paste(unique(full_data$calendar_year[which(full_data$SA2_CODE16 == diff$SA2_CODE_IN_DATA_NOT_MATCH_WITH_SA2_2016[x])]), collapse = "-")
  
})))

write.csv(diff, "SA2_codes_not_match_with_SA2_CODE16.csv", row.names = FALSE)

View(full_data[which(full_data$SA2_CODE16 %in% diff),])


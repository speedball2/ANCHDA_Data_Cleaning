
#apply cell suppression



fn <- "./output/BITRE_172_children_17_25_motor_vehicle_accidents_STE.csv"

old <- read.csv(fn, header = TRUE, check.names = FALSE)

names(old)
df <- old
# Check if the columns exist in the data frame
if ("n_fatally_injured_in_road_accident" %in% colnames(df) || "rolling_sum_fatally_injured_in_road_accident" %in% colnames(df)) {
  suppressed_cols <- c("n_fatally_injured_in_road_accident", "rolling_sum_fatally_injured_in_road_accident")
  
  # Apply cell suppression and count affected rows
  affected_rows <- 0
  for (col in suppressed_cols) {
    if (col %in% colnames(df)) {
      suppressed_rows <- df[, col] < 5 & df[, col] > 0 & !is.na(df[, col])
      df[suppressed_rows, col] <- 9999999
      affected_rows <- affected_rows + sum(suppressed_rows)
    }
  }
  
}

dim(old)
dim(df)




#17-25
#which(duplicated(df[, c("STE_CODE16", "sex", "age_group", "calendar_year","type_of_road_user")]) == TRUE)


#0-16

which(duplicated(df[, c("STE_CODE16", "sex", "age_group", "calendar_year")]) == TRUE)


write.csv(df, paste0("./output/cell_suppression/", basename(fn)), row.names = FALSE)

library(readxl)
library(tidyxl)
library(dplyr)
library(purrr)
library(janitor)
library(tidyr)
library(stringr)
library(zoo)
library(openxlsx)


`%notin%` <- Negate(`%in%`)

# Set the file path
file_path <- "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aihw_child_protection/AIHW-CWS-87-Data-tables.xlsx"
path_out = "C:/Users/00095998/OneDrive - The University of Western Australia/acwa_temp/aihw_child_protection/"


# Read all sheets except the first three

sheets <- readxl::excel_sheets(file_path)[-c(1, 2, 3)]



df_list <- lapply(sheets, function(sheet_name) {
  xlsx::read.xlsx(file_path, sheet = sheet_name, col_names = TRUE, na = "", encoding="UTF-8")
})


# Print the first column name of each data frame in df_list
for (df in df_list) {
  print(names(df)[1])
}



##---------------------------------------------------------------------- out of home care table 5.18 ----------------------------------------------------------------------
out_of_home_care <- NULL


for (df in df_list) {
  if (names(df)[1] == "Table S5.18: Children admitted to out-of-home care, by age group and state or territory, 2016–17 to 2020–21") {
    out_of_home_care <- df
    break
  }
}

#rename colnames
colnames(out_of_home_care) <- as.character(out_of_home_care[1, ])
#remove the last 6 rows
out_of_home_care <- head(out_of_home_care, -6)



#slice out the rows where value is N
out_of_home_care_number <- out_of_home_care %>%
  slice(3:37)
out_of_home_care_number$Year <- na.locf(out_of_home_care_number$Year)
out_of_home_care_number$Year <- str_replace(out_of_home_care_number$Year, "–", "-")
out_of_home_care_number$Year <- str_replace(out_of_home_care_number$Year, "(\\d{4})-(\\d{2})", "\\1-20\\2")
out_of_home_care_number$`Age group (years)` <- gsub("<1", "0-1", out_of_home_care_number$`Age group (years)`)


out_of_home_care_number_long <- out_of_home_care_number %>%
  pivot_longer(cols = -c(Year, `Age group (years)`),
               names_to = "STE_CODE16",
               values_to = "Number") %>%
  mutate(STE_CODE16 = sub("\\(a\\)", "", STE_CODE16)) # remove (a) from Tas column name



out_of_home_care_n <- out_of_home_care_number_long %>%
  rename(year_range = Year, age_group = `Age group (years)`, n_children_admitted_out_of_home_care = Number) %>%
  select(STE_CODE16, everything()) %>%

  mutate(STE_CODE16 = recode(STE_CODE16, 
                             "NSW" = "1", 
                             "Vic" = "2", 
                             "Qld" = "3", 
                             "SA" = "4", 
                             "WA" = "5", 
                             "Tas" = "6", 
                             "NT" = "7", 
                             "ACT" = "8", 
                             "Total" = "0")) %>%
  filter(age_group %notin% c("Total", "Unknown")) %>%
  select(STE_CODE16, age_group, year_range, n_children_admitted_out_of_home_care)
  

# remove 0 from state and create a national dataset
out_of_home_care_n_aus <- out_of_home_care_n %>%
  filter(STE_CODE16 == "0")


out_of_home_care_n <- out_of_home_care_n %>%
  filter(STE_CODE16 != "0")

##---------------------------------------------------------------------- part 2-----------------------------------------------------------------------


#slice out the rows where value is rate
out_of_home_care_rate <- out_of_home_care %>%
  slice(39:62)


out_of_home_care_rate$Year <- na.locf(out_of_home_care_rate$Year)
out_of_home_care_rate$Year <- str_replace(out_of_home_care_rate$Year, "–", "-")
out_of_home_care_rate$Year <- str_replace(out_of_home_care_rate$Year, "(\\d{4})-(\\d{2})", "\\1-20\\2")
out_of_home_care_rate$`Age group (years)` <- gsub("<1", "0-1", out_of_home_care_rate$`Age group (years)`)


out_of_home_care_rate_long <- out_of_home_care_rate %>%
  pivot_longer(cols = -c(Year, `Age group (years)`),
               names_to = "STE_CODE16",
               values_to = "Number") %>%
  mutate(STE_CODE16 = sub("\\(a\\)", "", STE_CODE16), 
         Number = as.numeric(Number),
         Number = sprintf("%.2f", Number))




out_of_home_care_p <- out_of_home_care_rate_long %>%
  rename(year_range = Year, age_group = `Age group (years)`, per_1000_children_admitted_out_of_home_care = Number) %>%
  select(STE_CODE16, everything()) %>%
  mutate(STE_CODE16 = recode(STE_CODE16, 
                             "NSW" = "1", 
                             "Vic" = "2", 
                             "Qld" = "3", 
                             "SA" = "4", 
                             "WA" = "5", 
                             "Tas" = "6", 
                             "NT" = "7", 
                             "ACT" = "8", 
                             "Total" = "0")) %>%
  filter(age_group %notin% c("Total", "Unknown"))%>%
  select(STE_CODE16, age_group, year_range, per_1000_children_admitted_out_of_home_care)


# remove 0 from state and create a national dataset
out_of_home_care_p_aus <- out_of_home_care_p %>%
  filter(STE_CODE16 == "0")


out_of_home_care_p <- out_of_home_care_p %>%
  filter(STE_CODE16 != "0")





#----------------------------------------------------------------Table 5.20 discharged ------------------------------------------------------------------------
dis_from_home_care <- NULL


for (df in df_list) {
  if (names(df)[1] == "Table S5.20: Children discharged from out-of-home care, by age group and state or territory, 2016–17 to 2020–21") {
    dis_from_home_care <- df
    break
  }
}

#rename colnames
colnames(dis_from_home_care) <- as.character(dis_from_home_care[1, ])
#remove the last 6 rows
dis_from_home_care <- head(dis_from_home_care, -8)






#slice out the rows where value is N
dis_from_home_care_number <- dis_from_home_care %>%
  slice(3:37)
dis_from_home_care_number$Year <- na.locf(dis_from_home_care_number$Year)
dis_from_home_care_number$Year <- str_replace(dis_from_home_care_number$Year, "–", "-")
dis_from_home_care_number$Year <- str_replace(dis_from_home_care_number$Year, "(\\d{4})-(\\d{2})", "\\1-20\\2")
dis_from_home_care_number$`Age group (years)` <- gsub("<1", "0-1", dis_from_home_care_number$`Age group (years)`)


dis_from_home_care_number_long <- dis_from_home_care_number %>%
  pivot_longer(cols = -c(Year, `Age group (years)`),
               names_to = "STE_CODE16",
               values_to = "Number") %>%
  mutate(STE_CODE16 = sub("\\(a\\)", "", STE_CODE16)) # remove (a) from Tas column name



dis_from_home_care_n <- dis_from_home_care_number_long %>%
  rename(year_range = Year, age_group = `Age group (years)`, n_children_discharged_from_out_of_home_care = Number) %>%
  select(STE_CODE16, everything()) %>%

  mutate(STE_CODE16 = recode(STE_CODE16, 
                             "NSW" = "1", 
                             "Vic" = "2", 
                             "Qld" = "3", 
                             "SA" = "4", 
                             "WA" = "5", 
                             "Tas" = "6", 
                             "NT" = "7", 
                             "ACT" = "8", 
                             "Total" = "0")) %>%
  filter(age_group %notin% c("Total", "Unknown")) %>%
  select(STE_CODE16, age_group, year_range, n_children_discharged_from_out_of_home_care)


# remove 0 from state and create a national dataset
dis_from_home_care_n_aus <- dis_from_home_care_n %>%
  filter(STE_CODE16 == "0")


dis_from_home_care_n <- dis_from_home_care_n %>%
  filter(STE_CODE16 != "0")

##---------------------------------------------------------------------- part 2-----------------------------------------------------------------------


#slice out the rows where value is rate
dis_from_home_care_rate <- dis_from_home_care %>%
  slice(39:68)


dis_from_home_care_rate$Year <- na.locf(dis_from_home_care_rate$Year)
dis_from_home_care_rate$Year <- str_replace(dis_from_home_care_rate$Year, "–", "-")
dis_from_home_care_rate$Year <- str_replace(dis_from_home_care_rate$Year, "(\\d{4})-(\\d{2})", "\\1-20\\2")
dis_from_home_care_rate$`Age group (years)` <- gsub("<1", "0-1", dis_from_home_care_rate$`Age group (years)`)


out_of_home_care_rate_d_long <- dis_from_home_care_rate %>%
  pivot_longer(cols = -c(Year, `Age group (years)`),
               names_to = "STE_CODE16",
               values_to = "Number") %>%
  mutate(STE_CODE16 = sub("\\(a\\)", "", STE_CODE16), 
         Number = as.numeric(Number),
         Number = sprintf("%.2f", Number))




dis_from_home_care_p <- out_of_home_care_rate_d_long %>%
  rename(year_range = Year, age_group = `Age group (years)`, per_1000_children_discharged_from_out_of_home_care = Number) %>%
  select(STE_CODE16, everything()) %>%
  mutate(STE_CODE16 = recode(STE_CODE16, 
                             "NSW" = "1", 
                             "Vic" = "2", 
                             "Qld" = "3", 
                             "SA" = "4", 
                             "WA" = "5", 
                             "Tas" = "6", 
                             "NT" = "7", 
                             "ACT" = "8", 
                             "Total" = "0")) %>%
  filter(age_group %notin% c("Total", "Unknown")) %>%
  select(STE_CODE16, age_group, year_range, per_1000_children_discharged_from_out_of_home_care)


# remove 0 from state and create a national dataset
dis_from_home_care_p_aus <- dis_from_home_care_p %>%
  filter(STE_CODE16 == "0")


dis_from_home_care_p <- dis_from_home_care_p %>%
  filter(STE_CODE16 != "0")







#----------------------------------------------------------------Table S4.11: Trends in children admitted to care and protection orders ------------------------------------------------------------------------

care_and_protection_orders <- NULL


for (df in df_list) {
  if (names(df)[1] == "Table S4.11: Trends in children admitted to care and protection orders, by state or territory, 2016–17 to 2020–21 (number)") {
    care_and_protection_orders <- df
    break
  }
}

#rename colnames
colnames(care_and_protection_orders) <- as.character(care_and_protection_orders[1, ])
#remove the last 6 rows
care_and_protection_orders <- head(care_and_protection_orders, -6)






#slice out the rows where value is N
care_and_protection_orders_number <- slice(care_and_protection_orders, -1)

care_and_protection_orders_number$Year <- na.locf(care_and_protection_orders_number$Year)
care_and_protection_orders_number$Year <- str_replace(care_and_protection_orders_number$Year, "–", "-")
care_and_protection_orders_number$Year <- str_replace(care_and_protection_orders_number$Year, "(\\d{4})-(\\d{2})", "\\1-20\\2")



care_and_protection_orders_number_long <- care_and_protection_orders_number %>%
  pivot_longer(cols = -c(Year),
               names_to = "STE_CODE16",
               values_to = "Number") %>%
  mutate(STE_CODE16 = gsub("\\(a\\)|\\(b\\)", "", STE_CODE16)) # remove (a) and (b) from ste names



care_and_protection_orders_n <- care_and_protection_orders_number_long %>%
  rename(year_range = Year, n_care_and_protection_orders = Number) %>%
  mutate(age_group = "0-24",
         STE_CODE16 = recode(STE_CODE16, 
                             "NSW" = "1", 
                             "Vic" = "2", 
                             "Qld" = "3", 
                             "SA" = "4", 
                             "WA" = "5", 
                             "Tas" = "6", 
                             "NT" = "7", 
                             "ACT" = "8", 
                             "Total" = "0")) %>%
  select(STE_CODE16, year_range, age_group, n_care_and_protection_orders) %>%
  mutate(sex = "all",
         .before = 2)


# remove 0 from state and create a national dataset
care_and_protection_orders_n_aus <- care_and_protection_orders_n %>%
  filter(STE_CODE16 == "0")


care_and_protection_orders_n <- care_and_protection_orders_n %>%
  filter(STE_CODE16 != "0")

#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------

# join the two dataframes
out_of_home_care <- left_join(out_of_home_care_n, out_of_home_care_p, 
                              by = c("STE_CODE16", "year_range", "age_group"))





out_of_home_care_aus <- left_join(out_of_home_care_n_aus, out_of_home_care_p_aus, 
                              by = c("STE_CODE16", "year_range", "age_group"))





dis_from_home_care <- left_join(dis_from_home_care_n, dis_from_home_care_p, 
                              by = c("STE_CODE16", "year_range", "age_group"))


dis_from_home_care_aus <- left_join(dis_from_home_care_n_aus, dis_from_home_care_p_aus, 
                                by = c("STE_CODE16", "year_range", "age_group"))


out_of_home_care <- out_of_home_care %>% 
  mutate(sex = "all") %>% 
  select(STE_CODE16, sex, year_range, age_group, everything())

out_of_home_care_aus <- out_of_home_care_aus %>% 
  mutate(sex = "all") %>% 
  select(STE_CODE16, sex, year_range, age_group, everything())

dis_from_home_care <- dis_from_home_care %>% 
  mutate(sex = "all") %>% 
  select(STE_CODE16, sex, year_range, age_group, everything())

dis_from_home_care_aus <- dis_from_home_care_aus %>% 
  mutate(sex = "all") %>% 
  select(STE_CODE16, sex, year_range, age_group, everything())

head(out_of_home_care)
head(out_of_home_care_aus)

head(dis_from_home_care)
head(dis_from_home_care_aus)


head(care_and_protection_orders_n_aus)
head(care_and_protection_orders_n)


#--------------------------------------------------------------------------------------------------------------------------write out csvs
#--------------------------------------------------------------------------------------------------------------------------write out csvs
#--------------------------------------------------------------------------------------------------------------------------write out csvs
#--------------------------------------------------------------------------------------------------------------------------write out csvs
#--------------------------------------------------------------------------------------------------------------------------write out csvs
#--------------------------------------------------------------------------------------------------------------------------write out csvs

write.csv(out_of_home_care, file = file.path(path_out, "aihw_361_children_admitted_out_of_home_care_STE.csv"), row.names = FALSE)
write.csv(out_of_home_care_aus, file = file.path(path_out, "aihw_361_children_admitted_out_of_home_care_national.csv"), row.names = FALSE)

write.csv(dis_from_home_care, file = file.path(path_out, "aihw_361_children_discharged_from_out_of_home_care_STE.csv"), row.names = FALSE)
write.csv(dis_from_home_care_aus, file = file.path(path_out, "aihw_361_children_discharged_from_out_of_home_care__national.csv"), row.names = FALSE)

write.csv(care_and_protection_orders_n, file = file.path(path_out, "aihw_3121_child_protection_substantiations_STE.csv"), row.names = FALSE)
write.csv(care_and_protection_orders_n_aus, file = file.path(path_out, "aihw_3121_child_protection_substantiations_national.csv"), row.names = FALSE)

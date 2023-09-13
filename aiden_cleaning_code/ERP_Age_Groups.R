
#Harriette edits 01.05.23

#adding root.dir
#creating totals - sa2, 3, 4, STE, aus
#library dplyr


#root.dir <- "C:/Users/pricea4/OneDrive - Queensland University of Technology/ACWA_QUT/"

root.dir <- "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/"
file.path <- "Data_Collections_INTERIM/ERP/"
save.path <- "Data_Collections_READY_FOR_QA/ERP/"

if (system.file(package='ASGS') == ""){
  install.packages("ASGS.foyer")
  ASGS.foyer::install_ASGS()
}
library(ASGS)
library(tidyverse)
library(sf)
library(readr)
library(dplyr)

df.base <- st_drop_geometry(st_as_sf(SA2_2016))#[,c(1,2)]
names(df.base)[2] <- "SA2_CODE16"
df.base$SA2_CODE16 <- as.numeric(as.character(df.base$SA2_CODE16))

df.erp <- read_csv(paste0(root.dir,file.path,"ABS_ERP_181_ERP_SA2.csv"))

df.new <- inner_join(df.erp,df.base)#[,c(6,2,3,4,5)]
names(df.new)[5] <- "x"

df.SA3 <- aggregate(df.new$x,
                    by=list(SA3_CODE16=df.new$SA3_CODE16,
                            calendar_year=df.new$calendar_year,
                            age_group=df.new$age_group,
                            sex=df.new$sex), FUN=sum)

df.SA4 <- aggregate(df.new$x,
                    by=list(SA4_CODE16=df.new$SA4_CODE16,
                            calendar_year=df.new$calendar_year,
                            age_group=df.new$age_group,
                            sex=df.new$sex), FUN=sum)

df.STE <- aggregate(df.new$x,
                    by=list(STE_CODE16=df.new$STE_CODE16,
                            calendar_year=df.new$calendar_year,
                            age_group=df.new$age_group,
                            sex=df.new$sex), FUN=sum)

df.AUS <- aggregate(df.new$x,
                    by=list(calendar_year=df.new$calendar_year,
                            age_group=df.new$age_group,
                            sex=df.new$sex), FUN=sum)
df.AUS <- cbind(Australia = as.factor(0), df.AUS)
#df.STE <- rbind(df.STE, df.AUS)
#df.STE <- df.STE[order(df.STE$sex,df.STE$calendar_year,df.STE$age_group,df.STE$STE_CODE16),]

# Age groups:
df.erp.ag <- df.erp %>%
  mutate(ag = recode(age_group,
                     "0-0" = "0-4","1-1" = "0-4","2-2" = "0-4","3-3" = "0-4","4-4" = "0-4",
                     "5-5" = "5-9","6-6" = "5-9","7-7" = "5-9","8-8" = "5-9","9-9" = "5-9",
                     "10-10" = "10-14","11-11" = "10-14","12-12" = "10-14","13-13" = "10-14","14-14" = "10-14",
                     "15-15" = "15-19","16-16" = "15-19","17-17" = "15-19","18-18" = "15-19","19-19" = "15-19",
                     "20-20" = "20-24","21-21" = "20-24","22-22" = "20-24","23-23" = "20-24","24-24" = "20-24"))

df.new.ag <- inner_join(df.erp.ag,df.base)

df.ag <- inner_join(aggregate(df.new.ag$estimated_regional_population,
                              by=list(SA2_CODE16=df.new.ag$SA2_CODE16,
                                      calendar_year=df.new.ag$calendar_year,
                                      age_group=df.new.ag$ag,
                                      sex=df.new.ag$sex), FUN=sum),df.base)

df.ag.SA3 <- aggregate(df.ag$x,
                       by=list(SA3_CODE16=df.ag$SA3_CODE16,
                               calendar_year=df.ag$calendar_year,
                               age_group=df.ag$age_group,
                               sex=df.ag$sex), FUN=sum)

df.ag.SA4 <- aggregate(df.ag$x,
                       by=list(SA4_CODE16=df.ag$SA4_CODE16,
                               calendar_year=df.ag$calendar_year,
                               age_group=df.ag$age_group,
                               sex=df.ag$sex), FUN=sum)

df.ag.STE <- aggregate(df.ag$x,
                       by=list(STE_CODE16=df.ag$STE_CODE16,
                               calendar_year=df.ag$calendar_year,
                               age_group=df.ag$age_group,
                               sex=df.ag$sex), FUN=sum)

df.ag.AUS <- aggregate(df.ag$x,
                       by=list(calendar_year=df.ag$calendar_year,
                               age_group=df.ag$age_group,
                               sex=df.ag$sex), FUN=sum)

df.ag.AUS <- cbind(Australia = as.factor(0), df.ag.AUS)
#df.ag.STE <- rbind(df.ag.STE, df.ag.AUS)
#df.ag.STE <- df.ag.STE[order(df.ag.STE$sex,df.ag.STE$calendar_year,df.ag.STE$age_group,df.ag.STE$STE_CODE16),]

# Cleaning:
df.SA2 <- df.new[,c(6,2:5)]
names(df.SA2)[1] <- "SA2_CODE16"
df.ag.SA2 <- df.ag[,c(6,2:5)]
names(df.ag.SA2)[1] <- "SA2_CODE16"

#SA2 ---------------------------------------------------------------------------

erp.SA2 <- rbind(df.SA2,df.ag.SA2)

#Harriette/Aiden edit: Creating Totals (01/06/23)

erp.totalage.SA2 <- erp.SA2 %>% group_by(SA2_CODE16, calendar_year, age_group) %>%
  summarise(x=sum(x)) %>% mutate(sex = "all")
erp.totalage.SA2 <-  erp.totalage.SA2[,c(1,2,3,5,4)]

erp.SA2 <- rbind(erp.SA2, erp.totalage.SA2)
erp.SA2 <- erp.SA2[with(erp.SA2, order(sex, age_group, calendar_year, SA2_CODE16)),]
names(erp.SA2)[5] <- "estimated_regional_population"

write.csv(erp.SA2,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA2.csv"), row.names=F)

# SA3 --------------------------------------------------------------------------

erp.SA3 <- rbind(df.SA3,df.ag.SA3)

#Harriette/Aiden edit: Creating Totals (01/06/23)

erp.totalage.SA3 <- erp.SA3 %>% group_by(SA3_CODE16, calendar_year, age_group) %>%
  summarise(x=sum(x)) %>% mutate(sex = "all")
erp.totalage.SA3 <-  erp.totalage.SA3[,c(1,2,3,5,4)]

erp.SA3 <- rbind(erp.SA3, erp.totalage.SA3)
erp.SA3 <- erp.SA3[with(erp.SA3, order(sex, age_group, calendar_year, SA3_CODE16)),]
names(erp.SA3)[5] <- "estimated_regional_population"

names(erp.SA3)[5] <- "estimated_regional_population"
write.csv(erp.SA3,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA3.csv" ), row.names = F)

#SA4 ---------------------------------------------------------------------------

erp.SA4 <- rbind(df.SA4,df.ag.SA4)

erp.totalage.SA4 <- erp.SA4 %>% group_by(SA4_CODE16, calendar_year, age_group) %>%
  summarise(x=sum(x)) %>% mutate(sex = "all")
erp.totalage.SA4 <-  erp.totalage.SA4[,c(1,2,3,5,4)]

erp.SA4 <- rbind(erp.SA4, erp.totalage.SA4)
erp.SA4 <- erp.SA4[with(erp.SA4, order(sex, age_group, calendar_year, SA4_CODE16)),]
names(erp.SA4)[5] <- "estimated_regional_population"

write.csv(erp.SA4,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA4.csv" ), row.names = F)

#STE ---------------------------------------------------------------------------
erp.STE <- rbind(df.STE,df.ag.STE)

erp.totalage.STE <- erp.STE %>% group_by(STE_CODE16, calendar_year, age_group) %>%
  summarise(x=sum(x)) %>% mutate(sex = "all")
erp.totalage.STE <-  erp.totalage.STE[,c(1,2,3,5,4)]

erp.STE <- rbind(erp.STE, erp.totalage.STE)
erp.STE <- erp.STE[with(erp.STE, order(sex, age_group, calendar_year, STE_CODE16)),]
names(erp.STE)[5] <- "estimated_regional_population"

write.csv(erp.STE,paste0(root.dir,save.path,"ABS_ERP_181_ERP_STE.csv" ), row.names = F)

# AUS --------------------------------------------------------------------------
erp.AUS <- rbind(df.AUS,df.ag.AUS)

erp.totalage.AUS <- erp.AUS %>% group_by(Australia, calendar_year, age_group) %>%
  summarise(x=sum(x)) %>% mutate(sex = "all")
erp.totalage.AUS <-  erp.totalage.AUS[,c(1,2,3,5,4)]

erp.AUS <- rbind(erp.AUS, erp.totalage.AUS)
erp.AUS <- erp.AUS[with(erp.AUS, order(sex, age_group, calendar_year, Australia)),]
names(erp.AUS)[5] <- "estimated_regional_population"

write.csv(erp.AUS,paste0(root.dir,save.path,"ABS_ERP_181_ERP_Australia.csv" ), row.names = F)

# LGA --------------------------------------------------------------------------

#Harriette/Aiden Edit: 07.06.23

df.lga.base <- st_drop_geometry(st_as_sf(LGA_2016))[,c(1,2)]
df.lga.base$LGA_CODE16 <- as.numeric(as.character(df.lga.base$LGA_CODE16))


df.lga.erp1 <- read_csv(paste0(root.dir,file.path,"ABS_ERP_181_ERP_LGA_TC_done.csv"))
df.lga.erp2 <- read_csv(paste0(root.dir,file.path,"ABS_ERP_181_ERP_LGA_2006_2016_ASGS2016_single_year.csv")) %>% mutate("estimated_regional_population_uncertainty_correspondence" = "Good")

df.lga.erp <- rbind(df.lga.erp1,df.lga.erp2)
df.lga.erp$LGA_CODE16 <- as.numeric(as.character(df.lga.erp$LGA_CODE16))

df.lga.new <- inner_join(df.lga.erp,df.lga.base)

# Age groups:
df.lga.new.ag <- df.lga.new %>%
  mutate(ag = recode(age_group,
                     "0-0" = "0-4","1-1" = "0-4","2-2" = "0-4","3-3" = "0-4","4-4" = "0-4",
                     "5-5" = "5-9","6-6" = "5-9","7-7" = "5-9","8-8" = "5-9","9-9" = "5-9",
                     "10-10" = "10-14","11-11" = "10-14","12-12" = "10-14","13-13" = "10-14","14-14" = "10-14",
                     "15-15" = "15-19","16-16" = "15-19","17-17" = "15-19","18-18" = "15-19","19-19" = "15-19",
                     "20-20" = "20-24","21-21" = "20-24","22-22" = "20-24","23-23" = "20-24","24-24" = "20-24"))

df.lga.ag <- aggregate(df.lga.new.ag$estimated_regional_population,
                              by=list(LGA_CODE16=df.lga.new.ag$LGA_CODE16,
                                      calendar_year=df.lga.new.ag$calendar_year,
                                      age_group=df.lga.new.ag$ag,
                                      sex=df.lga.new.ag$sex,
                                      estimated_regional_population_uncertainty_correspondence = df.lga.new.ag$estimated_regional_population_uncertainty_correspondence), FUN=sum)

df.lga.ag <- df.lga.ag[,c(1:4,6,5)]
names(df.lga.ag)[5] <- "estimated_regional_population"

erp.LGA <- rbind(df.lga.new[,1:6],df.lga.ag)


erp.totalage.LGA <- erp.LGA %>% group_by(LGA_CODE16, calendar_year, age_group) %>%
  summarise(estimated_regional_population =sum (`estimated_regional_population`)) %>% mutate(sex = "all")

erp.totalage.LGA$estimated_regional_population_uncertainty_correspondence <- "Good"

erp.LGA <- rbind(erp.LGA, erp.totalage.LGA)

write.csv(erp.LGA, paste0(root.dir,save.path,"ABS_ERP_181_ERP_LGA.csv" ), row.names = F)


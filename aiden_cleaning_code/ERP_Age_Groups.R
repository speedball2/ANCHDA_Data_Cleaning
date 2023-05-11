root.dir <- "C:/Users/pricea4/OneDrive - Queensland University of Technology/ACWA_QUT/"
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

df.base <- st_drop_geometry(st_as_sf(SA2_2016))
names(df.base)[2] <- "SA2_CODE16"
df.base$SA2_CODE16 <- as.numeric(as.character(df.base$SA2_CODE16))

df.erp <- read_csv(paste0(root.dir,file.path,"ABS_ERP_181_ERP_SA2.csv"))

df.new <- inner_join(df.erp,df.base)
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
df.SA2 <- df.new[,1:5]
df.ag.SA2 <- df.ag[,1:5]

erp.SA2 <- rbind(df.SA2,df.ag.SA2)
names(erp.SA2)[5] <- "estimated_regional_population"
write.csv(erp.SA2,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA2.csv"))

erp.SA3 <- rbind(df.SA3,df.ag.SA3)
names(erp.SA3)[5] <- "estimated_regional_population"
write.csv(erp.SA3,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA3.csv"))

erp.SA4 <- rbind(df.SA4,df.ag.SA4)
names(erp.SA4)[5] <- "estimated_regional_population"
write.csv(erp.SA4,paste0(root.dir,save.path,"ABS_ERP_181_ERP_SA4.csv"))

erp.STE <- rbind(df.STE,df.ag.STE)
names(erp.STE)[5] <- "estimated_regional_population"
write.csv(erp.STE,paste0(root.dir,save.path,"ABS_ERP_181_ERP_STE.csv"))

erp.AUS <- rbind(df.AUS,df.ag.AUS)
names(erp.AUS)[5] <- "estimated_regional_population"
write.csv(erp.AUS,paste0(root.dir,save.path,"ABS_ERP_181_ERP_Australia.csv"))












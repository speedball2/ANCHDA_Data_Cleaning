################################################################################
#              ACWA Data Clean 22-MAY-23 | HD Health Service Data              #
################################################################################


#Harriette Edit 25.05.23
# Updating Column Names
# Updating Csv names (from interim folder and output csvs)
# adding root dir
# change latitude and longitude to be leading column (original order of column)
# output of new csvs



# ------------------------------------------------------------------------------
# Save and Load Directories:
# Change root directory to appropriate file path.
#root.dir <- "C:/Users/Mudki/OneDrive - Queensland University of Technology/ACWA_QUT/"

#Harriette changing names - new root.dir
root.dir <- "C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/"
file.dir <- "Data_Collections_INTERIM/Service Location Data/NHSD/"
save.dir <- "Data_Collections_READY_FOR_QA/Service Location Data/"




# ------------------------------------------------------------------------------
# General Practice Locations:
df1 <- read.csv(paste0(root.dir, file.dir, "NHSD_712_general_practice_locations.csv"))[,c(1,2,3,4,5)]
names(df1) <- c("latitude", "longitude","organisation_name_nhsd", "healthcare_service_type_nhsd", "healthcare_service_identifier_nhsd")
# healthcare_service_identifier_nhsd: healthcare_service_healthcare_service_identifier_nhsdentifier_nhsd
write.csv(df1,
          paste0(root.dir,save.dir,"NHSD_712_general_practice_locations.csv"),
          row.names=F)





# ------------------------------------------------------------------------------
# Healthcare Service Locations:
df2 <- read.csv(paste0(root.dir, file.dir, "NHSD_713_healthcare_service_locations.csv"))[,c(1,2,3,4,5)]
names(df2) <- c("latitude", "longitude","organisation_name_nhsd", "healthcare_service_type_nhsd", "healthcare_service_identifier_nhsd")
# healthcare_service_identifier_nhsd: healthcare_service_healthcare_service_identifier_nhsdentifier_nhsd
write.csv(df2,
          paste0(root.dir,save.dir,"NHSD_713_healthcare_service_locations.csv"),
          row.names=F)





# ------------------------------------------------------------------------------
# Hospital Service Locations:
df3 <- read.csv(paste0(root.dir, file.dir, "NHSD_711_hospital_service_locations.csv"))[,c(1,2,3,4,5)]
names(df3) <- c("latitude", "longitude","organisation_name_nhsd", "healthcare_service_type_nhsd", "healthcare_service_identifier_nhsd")
# healthcare_service_identifier_nhsd: healthcare_service_healthcare_service_identifier_nhsdentifier_nhsd

df4 <- read.csv(paste0(root.dir, file.dir, "Hospitals_formatted.csv"))
df3$latitude <- df4$Y
df3$longitude <- df4$X

write.csv(df3,
          paste0(root.dir,save.dir,"NHSD_711_hospital_service_locations.csv"),
          row.names=F)


# ------------------------------------------------------------------------------
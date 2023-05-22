################################################################################
#              ACWA Data Clean 22-MAY-23 | HD Health Service Data              #
################################################################################





# ------------------------------------------------------------------------------
# Save and Load Directories:
# Change root directory to appropriate file path.
root.dir <- "C:/Users/Mudki/OneDrive - Queensland University of Technology/ACWA_QUT/"
file.dir <- "Data_Collections_INTERIM/Service Location Data/NHSD/"
save.dir <- "Data_Collections_READY_FOR_QA/Service Location Data/"





# ------------------------------------------------------------------------------
# General Practice Locations:
df1 <- read.csv(paste0(root.dir, file.dir, "NHSD_712_general_practices_service_locations.csv"))[,c(5,3,1,2,4)]
names(df1) <- c("ID", "Service_Name", "Latitude", "Longitude", "Healthcare_Service_Type")
# ID: healthcare_service_identifier_nhsd
write.csv(df1,
          paste0(root.dir,save.dir,"NHSD_712_general_practice_service_locations.csv"),
          row.names=F)





# ------------------------------------------------------------------------------
# Healthcare Service Locations:
df2 <- read.csv(paste0(root.dir, file.dir, "NHSD_713_healthcare_services_locations.csv"))[,c(5,3,1,2,4)]
names(df2) <- c("ID", "Service_Name", "Latitude", "Longitude", "Healthcare_Service_Type")
# ID: healthcare_service_identifier_nhsd
write.csv(df2,
          paste0(root.dir,save.dir,"NHSD_713_healthcare_service_locations.csv"),
          row.names=F)





# ------------------------------------------------------------------------------
# Hospital Service Locations:
df3 <- read.csv(paste0(root.dir, file.dir, "NHSD_711_hospitals_service_locations_new.csv"))[,c(5,3,1,2,4)]
names(df3) <- c("ID", "Service_Name", "Latitude", "Longitude", "Healthcare_Service_Type")
# ID: healthcare_service_identifier_nhsd

df4 <- read.csv(paste0(root.dir, file.dir, "Hospitals_formatted.csv"))
df3$Latitude <- df4$Y
df3$Longitude <- df4$X

write.csv(df3,
          paste0(root.dir,save.dir,"NHSD_711_hospital_service_locations.csv"),
          row.names=F)










# ------------------------------------------------------------------------------
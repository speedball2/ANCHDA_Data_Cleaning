################################################################################
#            ACWA Data Clean 18-APR-23 | ACARA School Location Data            #
################################################################################





# ------------------------------------------------------------------------------
# Save and Load Directories:
# Change root directory to appropriate file path.
root.dir <- "C:/Users/Mudki/OneDrive - Queensland University of Technology/ACWA_QUT/"
file.dir <- "Data_Collections_RAW/public_data/Service Location Data/ACARA/school-location-2021e23a2f404c94637ead88ff00003e0139.xlsx"
save.dir <- "Data_Collections_READY_FOR_QA/Service Location Data/"





# ------------------------------------------------------------------------------
# Packages:
packages <- c("ggmap", "readxl", "sf", "spatialEco")
install.packages(setdiff(packages, rownames(installed.packages())))
library(ggmap)
library(readxl)
library(sf)
library(spatialEco)

if (system.file(package='ASGS') == ""){
  install.packages("ASGS.foyer")
  ASGS.foyer::install_ASGS()
}
library(ASGS)

# Clean:
rm(packages)





# ------------------------------------------------------------------------------
# Data:
df.scan <- read_xlsx(path = paste0(root.dir,file.dir),
                sheet = 2)

# Identify columns of interest:
names(df.scan)
file.cols <- which(names(df.scan) %in% c("ACARA SML ID", "School Name", "Latitude", "Longitude"))

# Read appropriate columns:
df <- as.data.frame(df.scan[,file.cols])

# Clean:
rm(df.scan);rm(file.cols)





# ------------------------------------------------------------------------------
# Convert Lat-Lon to LGA:
LGA.poly <- LGA_2016
LGA.sf <- st_as_sf(LGA.poly)

df.sf <- st_transform(st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326),
                      st_crs(LGA.sf))

df.LGA <- st_intersection(df.sf, LGA.sf, sp = TRUE, duplicate = TRUE)
LGA.cols <- which(names(df.LGA) %in% c("ACARA.SML.ID", "School.Name", "LGA_CODE16"))
df.LGA.premerge <- st_drop_geometry(df.LGA[,LGA.cols])
names(df.LGA.premerge)[1:2] <- c("ACARA SML ID", "School Name")

# Clean:
rm(LGA.poly);rm(LGA.sf);rm(df.sf);rm(LGA.cols);rm(df.LGA)





# ------------------------------------------------------------------------------
# Convert Lat-Lon to SA2/SA3/SA4:
SA2.poly <- SA2_2016
SA2.sf <- st_as_sf(SA2.poly)

df.sf <- st_transform(st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326),
                      st_crs(SA2.sf))

df.SA2 <- st_intersection(df.sf, SA2.sf, sp = TRUE, duplicate = TRUE)
SA2.cols <- which(names(df.SA2) %in% c("ACARA.SML.ID", "School.Name", "SA2_MAIN16", "SA3_CODE16", "SA4_CODE16", "STE_CODE16"))
df.SA2.premerge <- st_drop_geometry(df.SA2[,SA2.cols])
names(df.SA2.premerge)[1:2] <- c("ACARA SML ID", "School Name")

# Clean:
rm(SA2.poly);rm(SA2.sf);rm(df.sf);rm(SA2.cols);rm(df.SA2)





# ------------------------------------------------------------------------------
# Merge Codes:
df.base.LGA <- merge(df,df.LGA.premerge,
                     by = c("ACARA SML ID","School Name"))

df.base.LGA.SA2 <- merge(df.base.LGA,df.SA2.premerge,
                     by = c("ACARA SML ID","School Name"))

df.output <- df.base.LGA.SA2

# Clean:
rm(df.base.LGA);rm(df.base.LGA.SA2);rm(df.LGA.premerge);rm(df.SA2.premerge)





# ------------------------------------------------------------------------------
# Save:
write.csv(df.output,
          paste0(root.dir,save.dir,"School_Locations.csv"),
          row.names=F)





# ------------------------------------------------------------------------------
# Quality Control Skip for Minor Edits:
# Set working directory to source file location and load in final environment.
load("ACARA_Environment.RData")










################################################################################
#                                End of Script                                 #
################################################################################
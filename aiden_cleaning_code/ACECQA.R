################################################################################
#      ACWA Data Clean 22-MAY-23 | ACECQA Childcare Centre Location Data       #
################################################################################





# ------------------------------------------------------------------------------
# Save and Load Directories:
# Change root directory to appropriate file path.
root.dir <- "C:/Users/Mudki/OneDrive - Queensland University of Technology/ACWA_QUT/"
file.dir <- "Data_Collections_RAW/public_data/Service Location Data/ACECQA/Education-services-au-export.csv"
file.dir.interim <- "Data_Collections_INTERIM/Service Location Data/ACECQA/"
save.dir <- "Data_Collections_READY_FOR_QA/Service Location Data/"

library(ggmap)
# register_google(key = "INSERT KEY HERE") # https://console.cloud.google.com/google/maps-apis/credentials?project=opportune-cairn-137723

df <- read.csv(paste0(root.dir,file.dir))[,c(1,3,6,7,8,9)]

df.string <- paste0(df$ServiceAddress,
                    ", ",
                    df$Suburb,
                    ", ",
                    df$State,
                    ", ",
                    df$Postcode)

# df.lonlat <- geocode(df.string) # Commented out to avoid accidentally running.
# write.csv(df.lonlat, "../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_lonlat.csv") # Saved as separate file to avoid having to re-run geocode().
df.lonlat <- read.csv(paste0(root.dir,file.dir.interim,"edu_lonlat.csv"))[,2:3] # Load file.

# Contains missing addresses, filled in later using ESRI geocoding.

#-------------------------------------------------------------------------------
# Install ASGS Package:
# install.packages("ASGS.foyer")
# ASGS.foyer::install_ASGS()
#-------------------------------------------------------------------------------


# Coding to SA level no longer needed.
# library(ASGS)

#df.code <- c()
#for(i in 1:nrow(df.lonlat)){
#  if(is.na(df.lonlat[i,2]) | is.na(df.lonlat[i,1])){
#    df.code[i] <- NA
#  } else {
#    df.code[i] <- as.character(latlon2SA(df.lonlat[i,2],
#                         df.lonlat[i,1],
#                         to = "SA3",
#                         yr = "2016",
#                         return = c("v","sp"),
#                         NAME = FALSE))
#  }
#}

# write.csv(data.frame(df.code), "../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_code.csv")
# df.code <- read.csv(paste0(root.dir,file.dir.interim,"edu_code.csv"))[,2] # Load file.

#-------------------------------------------------------------------------------
# Merge:
# df.full <- cbind(df,df.lonlat,df.code)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Merge:
df.full <- na.omit(cbind(df,df.lonlat))[,c(1,2,8,7)]
names(df.full) <- c("ID", "Service_Name", "Latitude", "Longitude")
# ID: ACECQA Service Approval Number
write.csv(df.full, paste0(root.dir, save.dir, "ACECQA_721_childcare_centre_locations.csv"),row.names = FALSE)
#-------------------------------------------------------------------------------










#-------------------------------------------------------------------------------
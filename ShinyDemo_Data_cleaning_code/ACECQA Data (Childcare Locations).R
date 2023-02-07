################################################################################
################################################################################
##                                                                            ##
##                    ____            _   _                                   ##
##                   / ___|__ _ _   _| |_(_) ___  _ __                        ##
##                  | |   / _` | | | | __| |/ _ \| '_ \                       ##
##                  | |__| (_| | |_| | |_| | (_) | | | |                      ##
##                   \____\__,_|\__,_|\__|_|\___/|_| |_|                      ##
##                                                                            ##
##                                                                            ##
################################################################################
################################################################################

################################################################################
# DO NOT RUN THIS SCRIPT FOR FUN - Code relies on a quota-based Google API key.#
################################################################################

library(ggmap)
# register_google(key = "INSERT KEY HERE") # https://console.cloud.google.com/google/maps-apis/credentials?project=opportune-cairn-137723

df <- read.csv("../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/Education-services-au-export.csv")[,c(3,6,7,8,9)]

df.string <- paste0(df$ServiceAddress,
                    ", ",
                    df$Suburb,
                    ", ",
                    df$State,
                    ", ",
                    df$Postcode)

# df.lonlat <- geocode(df.string) # Commented out to avoid accidentally running.
# write.csv(df.lonlat, "../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_lonlat.csv") # Saved as separate file to avoid having to re-run geocode().
df.lonlat <- read.csv("../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_lonlat.csv")[,2:3] # Load file.

#-------------------------------------------------------------------------------
# Install ASGS Package:
# install.packages("ASGS.foyer")
# ASGS.foyer::install_ASGS()
#-------------------------------------------------------------------------------

library(ASGS)

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
df.code <- read.csv("../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_code.csv")[,2] # Load file.

#-------------------------------------------------------------------------------
# Merge:
df.full <- cbind(df,df.lonlat,df.code)
write.csv(df.full, "../../Data_Collections_Raw/public_data/Service Location Data/ACECQA/edu_clean.csv")
#-------------------------------------------------------------------------------


















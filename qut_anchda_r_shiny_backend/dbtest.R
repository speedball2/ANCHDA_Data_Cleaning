# ############################################################################ #
#                                                                              #
#       _______ _______ ___         _______ ___ ___ ___ ______   _______       #
#      |   _   |   _   |   |       |   _   |   Y   |   |   _  \ |   _   |      #
#      |   1___|.  |   |.  |       |.  |___|.  |   |.  |.  |   \|.  1___|      #
#      |____   |.  |   |.  |___    |.  |   |.  |   |.  |.  |    |.  __)_       #
#      |:  1   |:  1   |:  1   |   |:  1   |:  1   |:  |:  1    |:  1   |      #
#      |::.. . |::..   |::.. . |   |::.. . |::.. . |::.|::.. . /|::.. . |      #
#      `-------`----|:.`-------'   `-------`-------`---`------' `-------'      #
#                   `--'                                                       #
#                                                                              #
# ---------------------------------------------------------------------------- #
#                                                                              #
#                                                                              #
# Overview:   Guide detailing the database build in R using existing VISER DB  #
#             schema as of 19-June-2023. This script requires the user to      #
#             download external software (XAMPP) to enable the hosting of a    #
#             local SQL database. While SQL code is required to query the      #
#             database, the supplied code will continue to function without    #
#             the need for edits so long as VISER or UWA do not edit the DB    #
#             schema. Note: Skip to step 5 if using existing database.         #
#                                                                              #
#                                                                              #
# ---------------------------------------------------------------------------- #
#                                                                              #
#                                                                              #
# Author:     Dr Aiden Price                                                   #
# Date:       21-June-2023                                                     #
# Version:    1.0                                                              #
#                                                                              #
#                                                                              #
# ############################################################################ #





# 1. Create a local SQL database -----------------------------------------------
# Download XAMPP
# - https://www.apachefriends.org/
# Install XAMPP
#  - Make sure to tick Apache, MySQL, PHP, and phpMyAdmin
# Start XAMPP Control Panel
#  - Click the "start" buttons next to Apache and MySQL.
# Access phpMyAdmin
#  - Open web browser and navigate to "http://localhost/phpmyadmin"
# Create a new database
#  - In phpMyAdmin, click on the "Databases" tab and enter a name in the
#    "Create database" field.
#  - Then click the "Create" button to create the database.





# 2. Install and load required packages ----------------------------------------
# install.packages("DBI")
# install.packages("RMySQL")
library(DBI)
library(RMySQL)

## 2.1 Set DB filepath -----
# Note: This set of scripts uses a duplicate DB from VISER. You will need to 
#       access and download this DB yourself to use.
fpath <- "INSERT HERE"

## 2.2 Increase maximum allowed packet -----
# We will need to increase the maximum string size of a submitted packet to our
# database. To do this, go back to phpMyAdmin:
#  - Navigate to the main server page
#  - Then click on the "Variables" tab
#  - Then type "packet" into the search bar
#  - Select "edit" next to the "max allowed packet" variable
#  - Add four 0s to the end of the existing variable
#  - Click save

## 2.3 Open connection to database -----
con <- dbConnect(MySQL(), user = "root", password = "", dbname = "INSERT HERE",
                 host = "localhost", port = 3306)





# 3. Load Schemas into DB ------------------------------------------------------
## 3.1 Create table schemas -----
dataString <- paste(
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Metadata-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Reference-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Theme-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev._ReferenceToTheme-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Age-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Area-schema.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data-schema.sql"))),collapse="")
)

## 3.2 Split SQL statements -----
# Need to split statements, as R can only handle submitting one statement at
# a time.
statements <- strsplit(dataString, ";", fixed = TRUE)[[1]]

## 3.3 Remove leading/trailing white space -----
# Just making sure no strange errors occur.
statements <- trimws(statements)

## 3.4 Execute each SQL statement individually -----
for (statement in statements) {
  if (nzchar(statement)) {
    dbExecute(con, statement)
  }
}




# 4. Load Data into DB ---------------------------------------------------------
## 4.1 Create table data -----
dataString <- paste(
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Age.00001.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Area.00001.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data.00001.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data.00002.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data.00003.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data.00004.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Data.00005.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Metadata.00001.sql"))),collapse=""),
  paste(unlist(readLines(paste0(fpath,"anchda-dev.Theme.00001.sql"))),collapse="")
)

## 4.2 Split SQL statements -----
# Need to split statements, as R can only handle submitting one statement at
# a time.
statements <- strsplit(dataString, ";", fixed = TRUE)[[1]]

## 4.3 Remove leading/trailing white space -----
# Just making sure no strange errors occur.
statements <- trimws(statements)

## 4.4 Execute each SQL statement individually -----
for (statement in statements) {
  if (nzchar(statement)) {
    dbExecute(con, statement)
  }
}

# 6. Generate Full Data set -------------------------------------------------------
## 6.1 Create query string -----
queryString <- 
  "SELECT aCode AS Code, aName AS Name, aZone AS ResG, aYear AS Year, mSex AS Sex, startYear AS ageRangeMin, endYear AS ageRangeMax, tName AS Theme, mSource AS Source, tName AS IndicatorName, dCount AS IndicatorValue
  FROM age
  LEFT JOIN (SELECT * 
  FROM (SELECT metadata.id AS mID, metadata.source AS mSource, metadata.Age AS mAge, metadata.sex AS mSex, metadata.Theme as mTheme, theme.id AS tID, theme.name AS tName, theme.Theme AS tTheme
  FROM theme
  LEFT JOIN metadata
  ON metadata.Theme = theme.id) mt
  LEFT JOIN (SELECT data.id AS dID, data.Metadata AS dMetadata, data.Area AS dArea, data.count AS dCount, area.id AS aID, area.code AS aCode, area.zone AS aZone, area.name AS aName, area.year AS aYear
  FROM area
  LEFT JOIN data
  ON area.id = data.Area) da
  ON da.dMetadata = mt.mID) woage
  ON woage.mAge = age.id;"

## 6.2 Send query -----
df <- dbGetQuery(con, queryString)

## 6.3 Save as R object -----
saveRDS(df, file = "viserDB.RDS")

## 6.4 Disconnect from SQL -----
dbDisconnect(con)





# 7. 
#df <- readRDS("viserDB.RDS")







# ############################################################################ #
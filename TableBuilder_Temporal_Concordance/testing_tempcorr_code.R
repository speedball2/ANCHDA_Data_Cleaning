# Required changes to initial interim clean...

# OF: - pivot_wider any additional filter vars from tablebuilder(???)

# OF: Rename columns to GEO_CODE_YEAR format that matches correspondence df column names

# OF: Strip "LGA" leading from LGA_CODE columns




# ---------------------------------------------------------------------------- #
#  ------------------ New and Improved Correspondence Code ------------------- #
# ---------------------------------------------------------------------------- #

# Libraries:
library(tidyverse)

# Read and clean correspondence table:
df.cor <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2016_2021/CG_LGA_2016_LGA_2021.csv")
#df.cor$LGA_CODE_2021 <- df.cor$LGA_CODE_2021 %>% as.numeric()
#df.cor <- df.cor %>% drop_na(LGA_CODE_2021)

# # Read data:
# df <- data.frame(LGA_CODE_2016 = unique(df.cor$LGA_CODE_2016), Year = 2016, SEXP = "Male", val = abs(round(10*rnorm(563),2))) # Test data for males.
# df <- rbind(df, data.frame(LGA_CODE_2016 = unique(df.cor$LGA_CODE_2016), Year = 2016, SEXP = "Female", val = abs(round(10*rnorm(563),2)))) # Test data for females.
# df[sample(c(1:nrow(df)), 40, replace=FALSE), "val"] <- NA # Simulate missing values.
# 

#--------

library(readxl)

SD_2006_SA4_2016 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                               sheet = 4,
                               range = "A6:F161", col_names = TRUE)

SD_2006_SA4_2016 <-  SD_2006_SA4_2016[-1,] #remove empty first row


SD_2006_SA4_2016_quality <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/Data_Cleaning_Github/ANCHDA_Data_Cleaning/TableBuilder_Temporal_Concordance/ASGS_Concordance_Files/2006_2016/CG_SD_2006_SA4_2016.xls",
                                       sheet = 3,
                                       range = "A6:C96", col_names = TRUE)

SD_2006_SA4_2016_quality <-  SD_2006_SA4_2016_quality[-1,] #remove empty first row

#-------

df.cor <- dplyr::left_join(SD_2006_SA4_2016,SD_2006_SA4_2016_quality,by="SA4_CODE_2016") %>% as.data.frame()

names(df.cor)

df <- read.csv("/Users/Current/OneDrive - Queensland University of Technology/General - ACWA_QUT/Data_Collections_INTERIM/Census_Interim_Pre-Temporal-Concordance/census_year12_SD_2006_INTERIM.csv")

names(df)

# OF: IF LGA - strip "LGA" string off front
#df$LGA..UR. <- stringr::str_sub(df$LGA..UR.,4,-1)

names(df) <- c("AGE5P", "SEXP", "HSCP.Highest.Year.of.School.Completed", "SD_CODE_2006", "completed_year12") # Fix names                  


df <- df %>% as.data.frame()
# OF: IF 2021 -- need to take inverse of from:to ratio


# Convert this to a function: ------------------------------------------------ #
# User inputs:
GEO_FROM <- "SD_CODE_2006"                                                     # Original data set concordance year.
GEO_TO <- "SA4_CODE_2016"                                                       # Target year.
GEO_NAME <- "SA4_CODE_2016"                                                        # Desired name for geography code.
VAR_NAME <- "completed_year12"                                                               # Name of the input variable.
FILTER_VARS <- c("AGE5P", "SEXP")                                                # Name of original data set filter variable(s).

# Function code: cor.calc <- function(df, df.cor,  GEO_FROM, GEO_TO, GEO_NAME, VAR_NAME, FILTER_VARS){
  # df: Data set to be transformed.
  # df.cor: Spreadsheet containing concordance columns and uncertainty.
  # GEO_FROM: Original data set concordance year.
  # GEO_TO: Target year.
  # GEO_NAME: Desired name for geography code.
  # VAR_NAME: Name of the input variable.
  # FILTER_VARS:Name of original data set filter variable(s).

  # Didn't want to make a function before quality checks.
# }





# Function contents: --------------------------------------------------------- #

# Identify contribution to correspondence:
df.cor.target <- sort(unique(df.cor[,GEO_TO]))
ls.cor <- list()
for (i in 1:length(df.cor.target)){
  ls.cor <- append(ls.cor, list(which(df.cor[,GEO_TO] %in% df.cor.target[i])))
}

# Create correspondence:

# New data frame:
df.new <- data.frame(GEO = df.cor.target,
                     val = NA,
                     val_uncertainty = recode(df.cor$QI_INDICATOR[match(df.cor.target, df.cor[,GEO_TO])], "Good" = 0, "Acceptable" = 1, "Poor" = 2)) # OF: LABEL VARIES - "Acceptable" or "Fair"

names(df.new) <- c(GEO_NAME, VAR_NAME, paste0(VAR_NAME, "_uncertainty"))

# Prepare final data frame, considering filter variables:
filter_list <- as.data.frame(crossing(df[,FILTER_VARS]))
names(filter_list) <- FILTER_VARS
OUT_df <- data.frame(matrix(nrow=0, ncol=3+ncol(filter_list)))
names(OUT_df) <- c(FILTER_VARS, names(df.new))

# Compute correspondence:
for (i in 1:nrow(filter_list)){
  # Filter:
  df.filt <- df
  for (k in 1:length(names(filter_list))){
    df.filt <- df.filt %>% filter(df[,which(names(df) %in% names(filter_list)[k])] == filter_list[i,k]) 
    #OF: this step seems to be broken - error like "Error in `filter()`:
    #! Problem while computing `..1 = df[, which(names(df) %in% names(filter_list)[k])] == ...`.
    # ✖ Input `..1` must be of size 156 or 1, not size 468.""
  }
  for (j in 1:nrow(df.new)){
    df.new[j,2] <- round(sum(df.filt$val[which(df.filt$LGA_CODE_2016 %in% df.cor$LGA_CODE_2016[ls.cor[[j]]])] * df.cor$RATIO_FROM_TO[ls.cor[[j]]]),2) # OF: needs to be INVERSE of ratio for 2021 to 2016 (1/RATIO_FROM_TO)
  }
  OUT_df <- rbind(OUT_df, cbind(filter_list[i,], df.new))
  names(OUT_df) <- c(FILTER_VARS, names(df.new))
  
}


# issue with above -- needs to account for multiple "from"








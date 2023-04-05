# ---------------------------------------------------------------------------- #
#  ------------------ New and Improved Correspondence Code ------------------- #
# ---------------------------------------------------------------------------- #

# Libraries:
library(tidyverse)

# Read and clean correspondence table:
df.cor <- read.csv("test.csv")
df.cor$LGA_CODE_2021 <- df.cor$LGA_CODE_2021 %>% as.numeric()
df.cor <- df.cor %>% drop_na(LGA_CODE_2021)

# Read data:
df <- data.frame(LGA_CODE_2016 = unique(df.cor$LGA_CODE_2016), Year = 2016, SEXP = "Male", val = abs(round(10*rnorm(563),2))) # Test data for males.
df <- rbind(df, data.frame(LGA_CODE_2016 = unique(df.cor$LGA_CODE_2016), Year = 2016, SEXP = "Female", val = abs(round(10*rnorm(563),2)))) # Test data for females.
df[sample(c(1:nrow(df)), 40, replace=FALSE), "val"] <- NA # Simulate missing values.





# Convert this to a function: ------------------------------------------------ #
# User inputs:
GEO_FROM <- "LGA_CODE_2016"                                                     # Original data set concordance year.
GEO_TO <- "LGA_CODE_2021"                                                       # Target year.
GEO_NAME <- "LGA_CODE21"                                                        # Desired name for geography code.
VAR_NAME <- "val"                                                               # Name of the input variable.
FILTER_VARS <- c("Year", "SEXP")                                                # Name of original data set filter variable(s).

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
                     val_uncertainty = recode(df.cor$OVERALL_QUALITY_INDICATOR[match(df.cor.target, df.cor[,GEO_TO])], "Good" = 0, "Fair" = 1, "Poor" = 2))

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
  }
  for (j in 1:nrow(df.new)){
    df.new[j,2] <- round(sum(df.filt$val[which(df.filt$LGA_CODE_2016 %in% df.cor$LGA_CODE_2016[ls.cor[[j]]])] * df.cor$RATIO_FROM_TO[ls.cor[[j]]]),2)
  }
  OUT_df <- rbind(OUT_df, cbind(filter_list[i,], df.new))
  names(OUT_df) <- c(FILTER_VARS, names(df.new))
  
}











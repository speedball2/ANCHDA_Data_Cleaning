

# ----------------------------------- #
# --- Cleaning Immunisation data  --- #
# ----------------------------------- #

# ----------- #
# --- SWD --- #
# ----------- #

# set working directory 

setwd("~/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_CLEAN/Data_cleaning_code")

# ----------------- #
# --- Libraries --- #
# ----------------- #

library(readxl)
library(tidyr)


# ----------------=---------------------------------------- #
# --- functionalised cleaning code - Immunisation Rates --- #
# --------------------------------------------------------- #


immunisation_SA3 <- read_excel("/Users/Current/OneDrive - Queensland University of Technology/ANCHDA_QUT/Data_Collections_RAW/immunisation_SA3/AIHWMEDICARE_Immunisationratesforchildren_Indicator1.2.1_FullyImmunised_2016-2017_SA4.xlsx",
                               sheet = 4, 
                               range = "B16:I6013")
names(immunisation_SA3) <- c("SA3_CODE16","SA3_NAME16","year","age", "Nchildren_registered","Nfully_immunised","Nfully_not_immunised","Pfully_immunised")


immunisation_SA3$age_char <- immunisation_SA3$age

immunisation_SA3$age <- as.numeric(substr(immunisation_SA3$age,1,1))


#convert vars to numeric

immunisation_SA3[,5:8] <- sapply(immunisation_SA3[,5:8],as.numeric)


# -------------------- #
# --- creating csv --- #
# -------------------- #


write.csv(immunisation_SA3, file = "Immunisation_SA3_2011-2017.csv",
          row.names = F)


view(immunisation_SA3)









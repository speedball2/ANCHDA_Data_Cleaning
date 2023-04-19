
#Harriette's WD
setwd("C:/Users/n9955348/OneDrive - Queensland University of Technology/Shared Documents - ACWA_QUT/General/Data_Collections_RAW/from_custodians/ABS_ERP")


# ----------------- #
# --- libraries --- #
# ----------------- #

library(readxl)
library(tidyr)


# READING IN EXCEL 
df1 <- read_xlsx("Client File-ERP-LS005201.xlsx", 2, "A9:BA36681", T)

# REMOVING SA NAME 
df1 <- df1[,-3]

#RENAME COLUMNS 
names(df1)[1] <- "calendar_year"
names(df1)[2] <- "SA2_CODE16"

#SPLITTING GENDERS
i <- c("SA2_CODE16", "calendar_year")
m <- c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15",
        "m16", "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24"  )

f <- c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8",
        "f9", "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17",
        "f18", "f19", "f20", "f21", "f22", "f23", "f24"  )

fem <- df1[,c(i,f)]
mal <- df1[,c(i,m)]


# CREATING AGE COLUMN/COMBINING (WIDE TO LONG DATA)

#REMOVING LEADING LETTER FOR GENDER 
colnames(mal)<-gsub("m","",colnames(mal))
colnames(fem)<-gsub("f","",colnames(fem))

#COMBINING AGE COLUMNS INTO 1
mal2 <- gather(mal, age_group, estimated_regional_population, gathercol <-  c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                                                                              "9", "10", "11", "12", "13", "14", "15", "16", "17",
                                                                              "18", "19", "20", "21", "22", "23", "24"))
#ADDING sSEX COLUMN MALE
mal2$sex <- "male"

#COMBINING AGE COLUMNS INTO 1
fem2 <- gather(fem, age_group, estimated_regional_population, gathercol <-  c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                                                                              "9", "10", "11", "12", "13", "14", "15", "16", "17",
                                                                              "18", "19", "20", "21", "22", "23", "24"))

#ADDING sSEX COLUMN FEMALE
fem2$sex <- "female"

#MERGE M/F BACK TOGETHER 
full <- rbind(fem2,mal2)

#CHANGE COLORDER

corder <- c("SA2_CODE16","calendar_year", "age_group", "sex", "estimated_regional_population")

full <- full[, corder]

# write csv --------------------------------------------------------------------


write.csv(full, "../../../Data_Collections_INTERIM/ABS_ERP_181_ERP_SA2.csv", row.names = F)



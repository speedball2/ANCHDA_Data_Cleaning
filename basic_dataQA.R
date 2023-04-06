# Read in the data
df <- read.csv("Z:/CDA/Claire WD/indicators_outputs/temp/census_year12_National.csv")

# Check if columns with _code16 or _code2016 is not the first column
if (!grepl("^code_(16|2016)$", tolower(names(df)[1]))) {
  print("the column are not in the right order")
}


# Check if age_group values are in the wrong format
if (!all(grepl("^\\d-\\d", df$age_group))) {
  print("age_group values are in the wrong format")
}


# Check if Total is present in age_group
if (any(grepl("^Total$", df$age_group))) {
  print("Age_group is incorrect, presence of total rows detected")
}

# Check if calendar_year values are in the correct format
if (any(!grepl("^\\d{4}$", df$calendar_year))) {
  print("There is an error in calendar_year")
} else {
  print("calendar_year is correct")
}

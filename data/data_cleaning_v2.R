# ==========================================
# Script 1: Data Cleaning & Extraction
# Recommended Location: Inside the data/ folder of the GitHub repository
# ==========================================

library(dplyr)

# 1. Read the raw data file
# Ensure "raw_sydney_rainfall_1885_latest.csv" is in the current working directory
if (!file.exists("raw_sydney_rainfall_1885_latest.csv")) {
  stop("Cannot find the raw data file 'raw_sydney_rainfall_1885_latest.csv'. Please check the path!")
}

raw_data <- read.csv("raw_sydney_rainfall_1885_latest.csv")

# 2. Clean data according to the team plan
# Extract data from 1968 to 1998, and keep rows where Quality equals 'Y'
cleaned_data <- raw_data %>%
  filter(Year >= 1968 & Year <= 1998) %>%
  filter(Quality == "Y")

# Rename the 5th column containing rainfall to a simple "Rainfall"
colnames(cleaned_data)[5] <- "Rainfall"

# 3. Automatically export the "cleaned" subset for all team members to use
write.csv(cleaned_data, "sydney_rainfall_1968_1998.csv", row.names = FALSE)

cat("[Data Cleaning Successful] Cleaned data file generated: sydney_rainfall_1968_1998.csv\n")

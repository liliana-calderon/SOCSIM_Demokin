#----------------------------------------------------------------------------------------------------
# Functions to download WPP2024 data

# Created on 14-10-2025
# Last modified on 15-10-2025

#----------------------------------------------------------------------------------------------------
# Increase the time limit for downloads
options(timeout=2000)

# Load necessary library
library(data.table)
#----------------------------------------------------------------------------------------------------
# Download the data from the WPP 2024 and save as .csv

download_WPP2024 <- function() {
  
  # Create a sub-folder called "data" to save the WPP data if it does not exist.
  ifelse(!dir.exists("Data"), dir.create("Data"), FALSE)
 
  # URL of the WPP .csv files
  url_fertility <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz"
  url_lifetable_female_hist <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz" 
  url_lifetable_male_hist <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz" 
  url_lifetable_female_proj <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz" 
  url_lifetable_male_proj <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz" 
  
  #  Download Fertility data and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Fertility_by_Age1.csv")) {
    print("Downloading Fertility Data")
    fertility <- fread(url_fertility)
    print("Saving Fertility Data")
    fwrite(fertility, "Data/WPP2024_Fertility_by_Age1.csv", row.names = FALSE)
    } else {warning("Fertility data already saved as .csv")}
  
  #  Download Life tables female (historical) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv")) {
    print("Downloading Lifetable - Female (Historical)")
    lifetable_female_hist <- fread(url_lifetable_female_hist)
    print("Saving Lifetable - Female (Historical)")
    fwrite(lifetable_female_hist, "Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv", row.names = FALSE)
  } else {warning("Lifetable - Female (Historical) already saved as .csv")}
  
  #  Download Life tables female (projections) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv")) {
    print("Downloading Lifetable - Female (Projections)")
    lifetable_female_proj <- fread(url_lifetable_female_proj)
    print("Saving Lifetable - Female (Projections)")
    fwrite(lifetable_female_proj, "Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv", row.names = FALSE)
  } else {warning("Lifetable - Female (Projections) already saved as .csv")}
  
  #  Download Life tables male (historical) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv")) {
    print("Downloading Lifetable - Male (Historical)")
    lifetable_male_hist <- fread(url_lifetable_male_hist)
    print("Saving Lifetable - Male (Historical)")
    fwrite(lifetable_male_hist, "Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv", row.names = FALSE)
  } else {warning("Lifetable - Male (Historical) already saved as .csv")}
  
  #  Download Life tables male (projections) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv")) {
    print("Downloading Lifetable - Male (Projections)")
    lifetable_male_proj <- fread(url_lifetable_male_proj)
    print("Saving Lifetable - Male (Projections)")
    fwrite(lifetable_male_proj, "Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv", row.names = FALSE)
  } else {warning("Lifetable - Male (Projections) already saved as .csv")}
  
  # Return a message indicating the process is complete
  return("World Population Prospects 2024 data downloaded and saved successfully as .csv files.")
}
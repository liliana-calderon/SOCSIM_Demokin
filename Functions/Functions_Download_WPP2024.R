#----------------------------------------------------------------------------------------------------
# Functions to download WPP2024 data

# Created on 14-10-2025
# Last modified on 17-04-2026

#----------------------------------------------------------------------------------------------------
# Increase the time limit for downloads
options(timeout=2000)

# Load necessary library
library(data.table)
require(R.utils)

#----------------------------------------------------------------------------------------------------
# Download the data from the WPP 2024 and save as .csv ----

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


#----------------------------------------------------------------------------------------------------
# Download the population data from the WPP 2024 for Demokin and save as .csv ----

download_WPP2024_pop <- function() {
  
  # Create a sub-folder called "data" to save the WPP data if it does not exist.
  ifelse(!dir.exists("Data"), dir.create("Data"), FALSE)
  
  # URL of the WPP .csv files
  url_population_hist <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz"
  url_population_proj <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv.gz"
  
  #  Download Population on 01 January 1950-2023, by single age (medium) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv")) {
    print("Downloading Population on 01 January 1950-2023")
    population_hist <- fread(url_population_hist)
    print("Saving Population on 01 January 1950-2023")
    fwrite(population_hist, "Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv", row.names = FALSE)
  } else {warning("Population on 01 January 1950-2023 already saved as .csv")}
  
  #  Download Population on 01 January 2024-2100, by single age (medium) and save as .csv file if not already saved
  if (!file.exists("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv")) {
    print("Downloading Population on 01 January 2024-2100")
    population_hist <- fread(url_population_hist)
    print("Saving Population on 01 January 2024-2100")
    fwrite(population_hist, "Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv", row.names = FALSE)
  } else {warning("Population on 01 January 2024-2100 already saved as .csv")}
  
  
  #  Check if Fertility data is already saved as .csv
  if (!file.exists("Data/WPP2024_Fertility_by_Age1.csv")) {
    print("Fertility Data has not been downloaded yet. Please use the download_WPP2024() function")
  } else {}
  
  #  Check if Life tables female (historical) is already saved as .csv
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv")) {
    print("Lifetable - Female (Historical) has not been downloaded yet. Please use the download_WPP2024() function")
  } else {}
  
  #  Check if Life tables female (projections) is already saved as .csv
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv")) {
    print("Lifetable - Female (Projections) has not been downloaded yet. Please use the download_WPP2024() function")
  } else {}
  
  #  Check if Life tables male (historical) is already saved as .csv
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv")) {
    print("Lifetable - Male (Historical)has not been downloaded yet. Please use the download_WPP2024() function" )
  } else {}
  
  #  Check if Life tables male (projections) is already saved as .csv
  if (!file.exists("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv")) {
    print("Lifetable - Male (Projections) has not been downloaded yet. Please use the download_WPP2024() function")
  } else {}
  
  
  # Return a message indicating the process is complete
  return("World Population Prospects 2024 population data downloaded and saved successfully as .csv files.")
}

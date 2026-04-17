#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Run Matrix models using Demokin and WPP data
# U:/SOCSIM/SOCSIM_Demokin/4_Run_Matrix_Models.R

## Run two-sex time-invariant and time-varying matrix models using the 'demokin' package and
## using data from the World Population Prospects 2024. 

# Created on 17-04-2026
# Last modified on 17-04-2026
#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

# Load necessary packages
library(data.table)
library(DemoKin)
library(readr)
library(tidyverse)

install.packages("Demokin")
#----------------------------------------------------------------------------------------------------
# Download population data from the WPP 2024 and save as .csv files ---

# To read gz files directly, fread() requires `R.utils` package
library(R.utils)
# Load functions to download data from the UN World Population Prospects 2024
source("Functions/Functions_Download_WPP2024.R")

# Download WPP 2024 population data (if it is not already saved as .csv file)
download_WPP2024_pop()

# Download WPP 2024 fertility and mortality data (if it is not already saved as .csv file)
download_WPP2024()
#----------------------------------------------------------------------------------------------------
## Load WPP 2024 data, filter and reshape in Demokin format ---- 

## Read the WPP fertility data in csv format
WPP_fert <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)

# Filter WPP fertility data for Sweden and reshape as matrix
SWE_fert <- WPP_fert %>% 
  filter(ISO3_code == "SWE" & Variant == "Medium") %>% 
  mutate(ASFR = ASFR/1000) %>%
  select(age = AgeGrpStart, year = Time, fx = ASFR)  %>% 
  pivot_wider(names_from = year, values_from = fx) %>%
  select(-age) %>%
  as.matrix()


## Read the WPP 2024 historical and projected life tables in csv format
WPP_lifetable_female_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv", show_col_types = FALSE) 
WPP_lifetable_male_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv", show_col_types = FALSE) 
WPP_lifetable_female_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv", show_col_types = FALSE)
WPP_lifetable_male_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv", show_col_types = FALSE) 

# Combine WPP historical and projected female life tables, filter for Sweden and reshape as matrix
SWE_px_female <- bind_rows(WPP_lifetable_female_hist, WPP_lifetable_female_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, px) %>% 
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Combine WPP historical and projected male life tables, filter for Sweden and reshape as matrix
SWE_px_male <- bind_rows(WPP_lifetable_male_hist, WPP_lifetable_male_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, px) %>% 
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Read WPP population data in csv format
population_hist <- read_csv("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv", show_col_types = FALSE)
population_proj <- read_csv("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv", show_col_types = FALSE)

# Combine WPP historical and projected population, filter for Sweden (females), and reshape as matrix
SWE_pop_female <- bind_rows(population_hist, population_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, pop = PopFemale) %>% 
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()

# Combine WPP historical and projected population, filter for Sweden (males), and reshape as matrix
SWE_pop_male <- bind_rows(population_hist, population_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, pop = PopMale) %>% 
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()
#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Estimate Mean and Distributions of Number and Ages of Kin by Age
# U:/SOCSIM/SOCSIM_Demokin/3_Estimate_Mean_Distribution_Kin.R

## Estimate mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation for Sweden (1950-2100)

## To run the following code, it is necessary to have already run the simulations, read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 26-03-2026
# Last modified on 08-04-2026
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of facetted plots
library(ggh4x)  # To facet scales-
#library(metR) # To label the contours
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load theme for the graphs and to convert SOCSIM time to calendar years
# and estimate months corresponding to January and December 
source("Functions/Functions_Graphs.R")

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load function to estimate age x year measures of kin from a SOCSIM microsimulation 
# # It estimates kin alive by age at 1st January of the following year, 
# based on fertility and mortality of a given year
source("Functions/Functions_Estimates_Age_Year.R")
#------------------------------------------------------------------------------------------------------
## Load data and define constants ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Load df with pids of kin, and information from opop, for individuals born 1950-2100
load("kin.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100
#------------------------------------------------------------------------------------------------------
## Prepare the ego_ages_year data with one row per calendar year and age

# Create df with one row per calendar year and age, with months for January and birthday
ego_ages_year <- opop %>%
  # Set 9999 as dod for ego alive at the end of the simulation
  mutate(dod2 = if_else(dod == 0, 9999, dod)) %>%
  select(pid_ego = pid, fem_ego = fem, dob_ego = dob, dod_ego2 = dod2) %>%
  # Calculate birth year and death year for each ego
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year),
         dod_ego_year = asYr(dod_ego2, last_month, final_sim_year)) %>%
  # Expand to one row per calendar year each ego lived through, over 1950-2100
  crossing(year = 1949:2100) %>%
  # Keep only years where ego was alive at least some months
  filter(year >= dob_ego_year & year <= dod_ego_year) %>%
  # Check if ego is alive at 1st January next year
  mutate(year_next = year + 1,
         month_jan_next = jan(year + 1, last_month, final_sim_year),
         # Ego alive at 1st January next year
         ego_alive_next = if_else(month_jan_next <= dod_ego2, 1L, 0L), 
         age_jan_next = floor((month_jan_next - dob_ego) / 12)) %>%
  # Keep only egos alive at 1st January next year
  filter(ego_alive_next == 1L) %>%
  # For each ego and year, calculate month corresponding to 1 January, age at January and the birthday month
  mutate(month_jan = jan(year, last_month, final_sim_year),
         # Age at January of a given year. 
         # Using floor instead of trun allow to get -1 for those born after January
         age_jan = floor((month_jan - dob_ego) / 12)) %>% 
  select(pid_ego, fem_ego, dob_ego, year, year_next,
       month_jan, age_jan, month_jan_next, age_jan_next, dod_ego2)

#----------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by age at 1st January 1950 (31.12.1949) ----

# Get all kin types sorted to be used in the function
kin_types_all <- sort(unique(kin$kin_type))

# Map the function to all calendar years
results <- estimates_age_year(year_start = 1949, year_end = 1949, 
                              ego_ages_year = ego_ages_year)
end_time <- Sys.time()

# Combine and save means for 1950
mean_number_age_stable <- results[["mean_number_age"]]
save(mean_number_age_stable, file = "Measures/mean_number_age_stable.RData")

# Combine and save distributions of numbers for 1950
distribution_number_stable <- results[["dist_number"]]
save(distribution_number_stable, file = "Measures/distribution_number_stable.RData")

# Combine and save distributions of ages for 1950
distribution_age_stable <- results[["dist_age"]]
save(distribution_age_stable, file = "Measures/distribution_age_stable.RData")

# Diagnostic: verify distribution sums to 1 per (year_next, age_jan_next, kin_type)
dist_check_stable <- distribution_number_stable %>%
  group_by(year_next, age_jan_next, kin_type) %>%
  summarise(s = sum(dist_number), .groups = "drop")
if (any(abs(dist_check_stable$s - 1) > 1e-9, na.rm = TRUE)) {
  warning("distribution_number_stable: some groups do not sum to 1. Max deviation: ",
          max(abs(dist_check_stable$s - 1), na.rm = TRUE))
} else {
  cat("distribution_number_stable: all groups sum to 1. OK\n")
}

#----------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by age at every 1st January 1951-2101 ----

# Get all kin types sorted to be used in the function
kin_types_all <- sort(unique(kin$kin_type))

#### Map the function to all calendar years, combine and save the results ----
start_time <- Sys.time()
# Map the function to all calendar years
results <- map(1950:2100, ~ estimates_age_year(year_start = .x, 
                                               year_end = .x, 
                                               ego_ages_year = ego_ages_year), 
               .progress = TRUE)
end_time <- Sys.time()
cat("Processing time:", end_time - start_time)
# Processing time: 1.221339 hours

# Combine and save means for all years
mean_number_age <- results %>% map("mean_number_age") %>% bind_rows() 
save(mean_number_age, file = "Measures/mean_number_age.RData")

# Combine and save distributions of numbers for all years
distribution_number <- results %>% map("dist_number") %>% bind_rows()
save(distribution_number, file = "Measures/distribution_number.RData")

# Combine and save distributions of ages for all years
distribution_age <- results %>% map("dist_age") %>% bind_rows()
save(distribution_age, file = "Measures/distribution_age.RData")

# Diagnostic: verify distribution sums to 1 per (year_next, age_jan_next, kin_type)
dist_check <- distribution_number %>%
  group_by(year_next, age_jan_next, kin_type) %>%
  summarise(s = sum(dist_number), .groups = "drop")
if (any(abs(dist_check$s - 1) > 1e-9, na.rm = TRUE)) {
  warning("distribution_number: some groups do not sum to 1. Max deviation: ",
          max(abs(dist_check$s - 1), na.rm = TRUE))
} else {
  cat("distribution_number: all groups sum to 1. OK\n")
}

#----------------------------------------------------------------------------------------------------
## Quick checks of the results ----

# mean_number_age should have full grid
mean_number_age %>%
  count(year_next) %>%
  summarise(min = min(n), max = max(n))

## Check NAs in mean_number and mean_kin_age
mean_number_age %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))

# Check where mean_number is NA
mean_number_age %>% 
  filter(is.na(mean_number)) %>% 
  distinct(year_next, age_jan_next) %>% 
  print(n=50)
# The mean number is NA when no egos exist at a given age_year (age_jan_next 100)

# Check where mean_kin_age is NA
mean_number_age %>% 
  filter(is.na(mean_kin_age)) %>% 
  filter(mean_number != 0 & !is.na(mean_number))
# The mean age of kin is NA when the mean number o kin is 0 or NA at a given age_year

# dist_number should sum to 1 per (year_next, age_jan_next, kin_type)
distribution_number %>%
  group_by(year_next, age_jan_next, kin_type) %>%
  summarise(s = sum(dist_number), .groups = "drop") %>%
  summarise(min = min(s), max = max(s))

# dist_age should sum to 1 per (year_next, age_jan_next, kin_type)
distribution_age %>%
  group_by(year_next, age_jan_next, kin_type) %>%
  summarise(s = sum(dist_age), .groups = "drop") %>%
  summarise(min = min(s), max = max(s))
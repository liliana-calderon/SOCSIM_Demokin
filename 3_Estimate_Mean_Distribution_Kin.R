#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Estimate Mean and Distributions of Number and Ages of Kin by Age
# U:/SOCSIM/SOCSIM_Demokin/3_Estimate_Mean_Distribution_Kin.R

## Estimate mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation for Sweden (1950-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 26-03-2026
# Last modified on 07-04-2026
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

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load theme for the graphs and to convert SOCSIM time to calendar years
# and estimate months corresponding to January and December 
source("Functions/Functions_Graphs.R")
#------------------------------------------------------------------------------------------------------
## Load data, define constants and check dates ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Load df with pids of kin, and information from opop, for individuals born 1950-2100
load("kin.RData")

# Define some constants 
last_month <- opop %>% 
  pull(dod) %>% 
  max()
final_sim_year <- 2100

# From the output pyramid:
# months 1001 to 3400 correspond to pre-simulation (200 years)
# to get stable population at 31-12-1949, with complete kinship networks
asYr(1001, last_month, final_sim_year)
asYr(3400, last_month, final_sim_year)

# First month of simulation with actual fertility and mortality rates (1950)
# corresponds to the end of Segment 1 in the result.pyr file
jan(1950, last_month, final_sim_year)

#------------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by age at each 1st January ----

# Create df with one row per calendar year and age, with months for January and birthday
ego_ages_year <- opop %>%
  # Set 9999 as dod for ego alive at the end of the simulation
  mutate(dod2 = if_else(dod == 0, 9999, dod)) %>%
  select(pid_ego = pid, fem_ego = fem, dob_ego = dob, dod_ego2 = dod2) %>%
  # Calculate birth year and death year for each ego
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year),
         dod_ego_year = asYr(dod_ego2, last_month, final_sim_year)) %>%
  # Expand to one row per calendar year each ego lived through, over 1950-2100
  crossing(year = 1950:2100) %>%
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

# Get all kin types sorted to be used in the function
kin_types_all <- sort(unique(kin$kin_type))

# Function to estimate the age x year measures to be processed by groups of years ----
# This allow to handle the large number of rows 
# resulting from a data frame with several ego-age-year rows for many kin

estimates_age_year <- function(year_start, year_end) {
  
  cat("Processing years:", year_start, "-", year_end, "\n")
  
  # Filter ego_age_year to the selected years
  ego_ages_year_sel <- ego_ages_year %>%
    filter(year >= year_start & year <= year_end)
  
  # Get egos and kin for the selected years
  pids_year <- ego_ages_year_sel %>% pull(pid_ego) %>% unique()
  kin_year <- kin %>%
    filter(pid_ego %in% pids_year) %>%
    # 9999 as dod for ego alive at the end of the simulation
    mutate(dod_ego2 = if_else(dod_ego == 0, 9999, dod_ego))
  
  # Join ego_ages_year with kin_year and calculate kin age and alive at 1st January
  ego_ages_year_kin <- ego_ages_year_sel %>% 
    left_join(kin_year, by = c("pid_ego", "fem_ego", "dob_ego"),
              relationship = "many-to-many") %>%
    filter(!is.na(kin_type)) %>%  # Remove NA rows from join
    # Calculate months, age and living status at 1st January next year
    mutate(# Set 9999 as dod for kin alive at the end of the simulation
           dod_kin2 = if_else(dod_kin == 0, 9999, dod_kin),
           year_next = year + 1,
           # Month of 1st January next year
           month_jan_next = jan(year + 1, last_month, final_sim_year),
           # Ego age at 1st January next year
           age_jan_next = floor((month_jan_next - dob_ego) / 12),
           # kin age at 1st January next year (Calculate even for dead. Afterwards, we will weight by kin_alive_next 
           kin_age_next = floor((month_jan_next - dob_kin) / 12),
           # Kin alive at 1st January next year
           kin_alive_next = if_else(month_jan_next >= dob_kin & month_jan_next < dod_kin2, 1L, 0L))
  
  # For each ego  mean numbers and ages of kin
  # one row per ego x year_next x age_next x kin_type
  ego_level <- ego_ages_year_kin %>%
    group_by(year_next, age_jan_next, kin_type, pid_ego) %>%
    summarise(n_kin_alive = sum(kin_alive_next, na.rm = TRUE),
              sum_kin_age_alive = sum(kin_age_next * kin_alive_next, na.rm = TRUE),
              .groups = "drop")

  # Mean numbers and ages of kin by egos' age at each 1st January at age +1
  mean_number_age <- ego_level %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    summarise(sum_kin_alive = sum(n_kin_alive, na.rm = TRUE),
              sum_kin_age_alive = sum(sum_kin_age_alive, na.rm = TRUE),
              .groups = "drop" ) %>%
    # Complete with all possible combinations, to check individuals with 0 kin
    complete(year_next = (year_start + 1):(year_end + 1),
             age_jan_next = 0:100,
             kin_type = kin_types_all,
             fill = list(sum_kin_alive = 0, sum_kin_age_alive = 0)) %>%
    # Add number of egos alive on 1st January by age as denominator for means
    left_join(ego_ages_year_sel %>% # already filtered to egos alive at 1st Jan
        group_by(year_next, age_jan_next) %>%
        summarise(n_egos = n_distinct(pid_ego), .groups = "drop"),
      by = c("year_next", "age_jan_next")) %>% 
    mutate(mean_number = if_else(n_egos > 0, sum_kin_alive / n_egos, NA_real_),
           # mean age among living kin (not among egos)
           mean_kin_age = if_else(sum_kin_alive > 0, sum_kin_age_alive / sum_kin_alive, NA_real_) ) %>%
    select(year_next, age_jan_next, kin_type, mean_number, mean_kin_age)
  
  # Distribution of numbers of kin per ego, by year and age
  dist_number <- ego_level %>%
    group_by(year_next, age_jan_next, kin_type, n_kin_alive) %>%
    summarise(n_egos = n(), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(dist_number = n_egos / sum(n_egos)) %>%
    ungroup()
  
  # Distribution of ages of living kin, by year and age
  dist_age <- ego_ages_year_kin %>%
    filter(kin_alive_next == 1L) %>%
    mutate(kin_age_next = pmax(0L, kin_age_next)) %>% 
    group_by(year_next, age_jan_next, kin_type, kin_age_next) %>%
    summarise(n_kin = n(), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(dist_age = n_kin / sum(n_kin)) %>%
    ungroup()

  return(list(mean_number_age = mean_number_age,
              dist_number = dist_number,
              dist_age = dist_age))
}

#----------------------------------------------------------------------------------------------------
#### Map the function to all calendar years, combine and save the results ----

start_time <- Sys.time()
# Map the function to all calendar years
results <- map(1950:2100, ~ estimates_age_year(.x, .x), 
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

# Combine and save distributions of numbers for all years
distribution_age <- results %>% map("dist_age") %>% bind_rows()
save(distribution_age, file = "Measures/distribution_age.RData")

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

#----------------------------------------------------------------------------------------------------
## Plot mean and distributions of numbers and ages of kin by age and year ----

# Load mean number and ages of kin by age of Focal and calendar year
load("Measures/mean_number_age.RData")

# Load distribution of numbers kin by age of Focal and calendar year
load("Measures/distribution_number.RData")

# Load distribution of ages of kin by age of Focal and calendar year
load("Measures/distribution_age.RData")

# Plot the mean number of kin by age
mean_number_age %>%
  rename(Year = year_next, Age = age_jan_next)  %>%  
  recode_kin() %>% 
  ggplot(aes(x = Year, y = Age)) +
  geom_tile(aes(fill = mean_number), linewidth = 0) + 
  facet_wrap(~ kin_type, ncol = 4, dir = "v", axes = "all", scales = "free") +
  scale_fill_viridis_c(option = "F", direction = -1,
                       na.value = "grey90") + # The mean is NA when no egos exist at a given age_year
  scale_x_continuous(breaks = seq(1950, 2100, 50)) +
  labs(fill = "Mean Number of Kin (scaled)") +
  theme_graphs() +
  theme(legend.text = element_text(size = 14),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(2, "cm"))
ggsave("Graphs/Mean_Number_Kin_Age_Scaled.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Age_Scaled.pdf", width = 20, height = 16, dpi = 300, device = "pdf")


## Plot with different scales for each kin type

# Compute max number per kin_type
max_number_kin <- mean_number_age %>%
  recode_kin() %>%
  group_by(kin_type) %>%
  summarise(max_val = max(mean_number, na.rm = TRUE),
            .groups = "drop")

# Split by kin type and plot with purrr
plot_means <- mean_number_age %>% 
  rename(Year = year_next, Age = age_jan_next) %>%
  recode_kin() %>% 
  left_join(max_number_kin, by = "kin_type") %>%
  group_split(kin_type) %>%   
  map(function(.) {
    kin <- unique(.$kin_type)
    max_kin <- unique(.$max_val)
    ggplot(., aes(x = Year, y = Age)) +
      geom_tile(aes(fill = mean_number), linewidth = 0) +
      scale_fill_viridis_c(option = "F", direction = -1, na.value = "grey90",
                            limits = c(0, max_kin), oob = scales::squish) +
      scale_x_continuous(breaks = seq(1950, 2100, 50)) +
      labs(title = kin, fill = "Mean Number") +
      theme_graphs() +
      theme(legend.text = element_text(size = 12),
            legend.key.width = unit(1, "cm")) })

wrap_plots(plot_means, ncol = 4, byrow = T)

ggsave("Graphs/1_Mean_Number_Kin_Age.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/1_Mean_Number_Kin_Age.pdf", width = 20, height = 16, dpi = 300, device = "pdf")
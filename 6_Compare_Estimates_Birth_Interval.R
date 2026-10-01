#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare alternative simulation estimates using a plausible birth interval

# U:/SOCSIM/SOCSIM_Demokin/6_Compare_Estimate_Birth_Interval.R

## Compare estimates of mean number of kin using alternative simulation including a birth interval

# Created on 30-09-2026
# Last modified on 30-09-2026
#------------------------------------------------------------------------------------------------------
## To run this script with a new simulation, all previous scripts (0-5) must have been already run
## The instructions on how to install rsocsim and run a simulation, can be found in 0_Set_Up_Simulation.R
## The input rates and the initial population must have been already created in 0_Set_Up_Simulation.R
#------------------------------------------------------------------------------------------------------
## General settings and functions -----
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

library(tidyverse)
library(cowplot) # For separate legends of facetted plots
library(ggh4x)  # To facet scales-
library(viridis)
library(patchwork) # To combine ggplots

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load function to estimate age x year measures of kin from a SOCSIM microsimulation 
# It estimates kin alive by age at 1st January of the following year, 
# based on fertility and mortality of a given year
source("Functions/Functions_Estimates_Age_Year.R")

## Load function to estimate the distribution of the numbers of kin from Demokin
source("Functions/Functions_Kin_Distribution.R")

#------------------------------------------------------------------------------------------------------
## Run SOCSIM simulations with 'rsocsim' package ----

# Load rsocsim package
library("rsocsim")

# Specify the working directory, where the supfile and ratefiles are.
# If the R session is running through the project file, the following command can be used.
folder <- getwd()
# Otherwise, the working directory must be specified after "folder <- "

# Name of the supervisory file stored in the above folder:
# Supervisory file for rates retrieved from WPP 2024, with marry after childbirth and hetfert 0
# Supplementary analysis with bint 12
supfile <- "Sweden_WPP_bint_12.sup" 

# Load the same seed number used in the main simulation
load("seed.Rda")

## Run the simulation for the random seed. 
start <- Sys.time()

### Run a single SOCSIM-simulation with a given folder and supervisory file
rsocsim::socsim(folder, supfile, seed, process_method = "future")
end <- Sys.time()

print(end-start)

# Time difference of xxx mins for 200000 individuals in the initial population with bint

#----------------------------------------------------------------------------------------------------
## Read the output .opop and .omar files ----

# Load seeds numbers and convert them into numeric
load("seed.Rda")
seed <- as.numeric(seed)

# Read opop
opop_bint <- rsocsim::read_opop(folder = getwd(),
                           supfile = "Sweden_WPP_bint_12.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save opop to use later
save(opop_bint, file = "opop_bint.RData")

# Read omar
omar_bint <- rsocsim::read_omar(folder = getwd(),
                           supfile = "Sweden_WPP_bint_12.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save omar to use later
save(omar_bint, file = "omar_bint.RData")

#----------------------------------------------------------------------------------------------------
##  Retrieve kinship network of individuals alive over 1950-2100 from a SOCSIM microsimulation ----

## This is a simplified version of the code in script 2_Retrieve_Kin_Simulation.R
## Please refer to the above script for more details

## Load opop
load("opop_bint.RData")

# Define some constants 
last_month <- opop_bint %>% pull(dod) %>% max()
final_sim_year <- 2100

# Define period of analysis
year_start <- 1950
year_end   <- 2100

# Months for 1st Jan each year over 1950-2100
# Using year_end+1 to include individuals born over 2100 alive at the end of the simulation (01.01.2101)
jan_months <- jan(year_start:(year_end+1), last_month, final_sim_year) 

# Get pids of individuals (aged 0-100) alive on each 1st January over 1950-2101
egos_bint <- opop_bint %>%
  mutate(dod2 = if_else(dod == 0, 9999, dod)) %>%
  filter(dob <= max(jan_months) & dod2 >= min(jan_months)) %>%
  pull(pid)

# Check length of number of egos selected
length(egos_bint) # 925937

# Retrieve pids of kin of individuals born 1950-2100
start <- Sys.time()
kin_pids_bint <- retrieve_kin_mod(opop = opop_bint, pid = egos_bint, KidsOf = KidsOf) 
save(kin_pids_bint, file = "kin_pids_bint.RData")
end <- Sys.time()
print(end-start)
# Time difference of 13.67515 hours

kin_bint <- kin_pids_bint %>%
  filter(!is.na(pid_kin)) %>%
  left_join(select(opop_bint, c(pid, dob, dod, fem)), 
            by = join_by("pid_ego" == "pid")) %>% 
  rename(dob_ego = dob, dod_ego = dod, fem_ego = fem) %>% # dod = 0 for egos alive at the end
  left_join(select(opop_bint, c(pid, dob, dod, fem)), 
            by = join_by("pid_kin" == "pid")) %>% 
  rename(dob_kin = dob, dod_kin = dod, fem_kin = fem)
save(kin_bint, file = "kin_bint.RData")

#----------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by age at every 1st January 1950-2101 ----

## This is a simplified version of the code in script 3_Estimate_Kin_Simulation.R
## It only includes in the comparison the mean number of kin with birth intervals of 12
## Please refer to the above script for more details on other measures.
#------------------------------------------------------------------------------------------------------
## Prepare the ego_ages_year_bint data with one row per calendar year and age

# Create df with one row per calendar year and age, with months for January and birthday
ego_ages_year_bint <- opop_bint %>%
  filter(fem == 1) %>% #### Only female egos
  # Set 9999 as dod for ego alive at the end of the simulation
  mutate(dod2 = if_else(dod == 0, 9999, dod)) %>%
  select(pid_ego = pid, fem_ego = fem, dob_ego = dob, dod_ego2 = dod2) %>%
  # Calculate birth year and death year for each ego
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year),
         dod_ego_year = asYr(dod_ego2, last_month, final_sim_year)) %>%
  # Expand to one row per calendar year each ego lived through, over 1949-2100
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

# Save the ego_ages_year_bint data for later use
save(ego_ages_year_bint, file = "Measures/ego_ages_year_bint.RData")

#### Map the function to all calendar years, combine and save the results ----
start_time <- Sys.time()
# Map the function to all calendar years
results_bint <- map(1949:2100, ~ estimates_age_year(year_start = .x, 
                                               year_end = .x, 
                                               ego_ages_year = ego_ages_year_bint, 
                                               kin = kin_bint), 
               .progress = TRUE)
end_time <- Sys.time()
print(end_time - start_time)
# Time difference of 1.076137 hours

# Combine and save means for all years
mean_number_age_bint <- results_bint %>% map("mean_number_age") %>% bind_rows() 
save(mean_number_age_bint, file = "Measures/mean_number_age_bint.RData")

# Combine and save distributions of numbers for all years
distribution_number_bint <- results_bint %>% map("dist_number") %>% bind_rows()
save(distribution_number_bint, file = "Measures/distribution_number_bint.RData")

# Combine and save distributions of ages for all years
distribution_age_bint <- results_bint %>% map("dist_age") %>% bind_rows()
save(distribution_age_bint, file = "Measures/distribution_age_bint.RData")

#------------------------------------------------------------------------------------------------------ 
## Load opop, mean numbers of kin by age and year and define constants -----

## Load opop generated in 0_Set_Up_Simulation.R
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Load mean numbers and ages for all years from simulation with bint 0
load("Measures/mean_number_age.RData")

# Load mean numbers and ages for all years from simulation with bint 12
load("Measures/mean_number_age_bint.RData")

# Load estimates from the time-invariant model
load("Measures/demokin_invariant.RData")

# Load estimates from the time-variant model
load("Measures/demokin_variant.RData")

#------------------------------------------------------------------------------------------------------
## Plot mean numbers of kin by age at selected years ----

# Select years to plot
years_plot <- c(1950, 2050) 

# Get mean number of kin at each age of Focal at each selected year from Demokin
demokin_mean_number <- demokin_variant$kin_full %>%
  recode_demokin() %>%
  summarise(mean_number = sum(living), 
            .by = c(year, kin_type, age_focal)) %>%
  mutate(Model = "Analytical (DemoKin)", 
         Birth_Interval = "0")

# Get mean number of kin at each age of Focal at each selected year from Socsim bint 0
socsim_mean_number <- mean_number_age %>%
  filter(year_next %in% years_plot) %>% 
  rename(year = year_next, age_focal = age_jan_next) %>%
  filter(!is.na(mean_number)) %>%
  recode_kin() %>%
  select(year, kin_type, age_focal, mean_number) %>% 
  mutate(Model = "Microsimulation (Socsim)",
         Birth_Interval = "0")

# Get mean number of kin at each age of Focal at each selected year from Socsim bint 12
socsim_mean_number_bint <- mean_number_age_bint %>%
  filter(year_next %in% years_plot) %>%
  rename(year = year_next, age_focal = age_jan_next) %>%
  filter(!is.na(mean_number)) %>%
  recode_kin() %>%
  select(year, kin_type, age_focal, mean_number) %>% 
  mutate(Model = "Microsimulation (Socsim)",
         Birth_Interval = "12")

#------------------------------------------------------------------------------------------------------
# Plot mean number of all kin at each age of Focal at selected years from both models, with different bint

bind_rows(demokin_mean_number, socsim_mean_number, socsim_mean_number_bint) %>% 
  filter(mean_number > 0) %>%
  filter(year %in% years_plot) %>% # Keep only 1950 and 2050 for plotting
  ggplot(aes(x = age_focal, y = mean_number, group = interaction(Model, Birth_Interval)))+
  geom_line(aes(color = Model, linetype = Birth_Interval), linewidth = 1)+
  facet_grid(kin_type ~ year, scales = "free_y") +
  labs(x = "Age of focal", y = "Mean number of living kin", color = "Model") +
  scale_color_manual(values = c( "#B22222", "#007A75", "#007A75"))+
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(x = "Age of focal", y = "Mean number of living kin",
       color = "Model", linetype = "Birth Interval") +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
  theme_graphs()

ggsave("Graphs/App_Mean_Number_Kin_bint.jpg", width = 20, height = 28, dpi = 600)
ggsave("Graphs/App_Mean_Number_Kin_bint.pdf", width = 20, height = 28, dpi = 600, device = "pdf")
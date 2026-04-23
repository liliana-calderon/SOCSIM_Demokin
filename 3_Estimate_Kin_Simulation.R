#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Estimate Mean and Distributions of Number and Ages of Kin by Age
# U:/SOCSIM/SOCSIM_Demokin/3_Estimate_Kin_Simulation.R

## Estimate mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation for Sweden (1950-2100)

## To run the following code, it is necessary to have already run the simulations, read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 26-03-2026
# Last modified on 17-04-2026
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
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load theme for the graphs and to convert SOCSIM time to calendar years
# and estimate months corresponding to January and December 
source("Functions/Functions_Graphs.R")

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load function to estimate age x year measures of kin from a SOCSIM microsimulation 
# It estimates kin alive by age at 1st January of the following year, 
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

#----------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by age at every 1st January 1950-2101 ----
# Results for 01.01.1950 correspond to the stable setting
# Results for 01.01.1951-01.01.2101 correspond to the dynamic setting

#### Map the function to all calendar years, combine and save the results ----
start_time <- Sys.time()
# Map the function to all calendar years
results <- map(1949:2100, ~ estimates_age_year(year_start = .x, 
                                               year_end = .x, 
                                               ego_ages_year = ego_ages_year, 
                                               kin = kin), 
               .progress = TRUE)
end_time <- Sys.time()
cat("Processing time:", end_time - start_time)
# Processing time: 1.678384 hours

# Combine and save means for all years
mean_number_age <- results %>% map("mean_number_age") %>% bind_rows() 
save(mean_number_age, file = "Measures/mean_number_age.RData")

# Combine and save distributions of numbers for all years
distribution_number <- results %>% map("dist_number") %>% bind_rows()
save(distribution_number, file = "Measures/distribution_number.RData")

# Combine and save distributions of ages for all years
distribution_age <- results %>% map("dist_age") %>% bind_rows()
save(distribution_age, file = "Measures/distribution_age.RData")

#----------------------------------------------------------------------------------------------------
## Estimate mean and distributions of numbers and ages of kin by sex and age at every 1st January 1950-2101 ----

#### Map the function to all calendar years, combine and save the results ----
start_time <- Sys.time()
# Map the function to all calendar years
results_sex <- map(1949:2100, ~ estimates_sex_age_year(year_start = .x, 
                                                   year_end = .x, 
                                                   ego_ages_year = ego_ages_year, 
                                                   kin = kin), 
               .progress = TRUE)
end_time <- Sys.time()
cat("Processing time:", end_time - start_time)
# Processing time: 1.678384 hours

# Combine and save means for all years
mean_number_age_sex <- results_sex %>% map("mean_number_age_sex") %>% bind_rows() 
save(mean_number_age_sex, file = "Measures/mean_number_age_sex.RData")

# Combine and save distributions of numbers for all years
distribution_number_sex <- results_sex %>% map("dist_number_sex") %>% bind_rows()
save(distribution_number_sex, file = "Measures/distribution_number_sex.RData")

# Combine and save distributions of ages for all years
distribution_age_sex <- results_sex %>% map("dist_age_sex") %>% bind_rows()
save(distribution_age_sex, file = "Measures/distribution_age_sexRData")

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
  pull(age_jan_next) %>% 
  unique()
# The mean number is NA when no egos exist at a given age_year (age_jan_next 100)

# Check where mean_kin_age is NA
mean_number_age %>% 
  filter(is.na(mean_kin_age)) %>% 
  # The mean age of kin is NA when the mean number of kin is 0 or NA at a given age_year
  # So filtering these cases would confirm if there are other cases with NA mean_kin_age
  filter(mean_number != 0 & !is.na(mean_number))

# dist_number should sum to 1 per (year_next, age_jan_next, kin_type)
distribution_number %>% 
  group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
  summarise(s = sum(dist_number), .groups = "drop") %>%
  summarise(min = min(s), max = max(s))

# dist_age should sum to 1 per (year_next, age_jan_next, kin_type)
distribution_age %>% 
  group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
  summarise(s = sum(dist_age), .groups = "drop") %>%
  summarise(min = min(s), max = max(s))

#----------------------------------------------------------------------------------------------------
## Plot mean and distributions of numbers and ages of kin by age at 1st January 1950 ----

## Mean number of kin at each age of Focal  
mean_number_age %>% 
  filter(year_next == 1950) %>% 
  rename(Age = age_jan_next) %>%
  filter(!is.na(mean_number)) %>% 
  recode_kin() %>%
  ggplot(aes(x = Age, y = mean_number, color = kin_type)) +
  geom_line(linewidth = 1) +
  labs(x = "Age of focal individual", y = "Mean number of living kin", color = "Kin type") +
  scale_color_viridis_d(option = "D", direction =1)+
  theme_graphs()
ggsave("Graphs/Socsim_Mean_Number_Kin_1950.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Socsim_Mean_Number_Kin_1950.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


## Mean age of kin at each age of Focal
mean_number_age %>% 
  filter(year_next == 1950) %>% 
  rename(Age = age_jan_next) %>%
  filter(!is.na(mean_kin_age)) %>% 
  recode_kin() %>%
  ggplot(aes(x = Age, y = mean_kin_age, color = kin_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ kin_type, scales = "free_y", axes = "all") +
  labs(x = "Age of focal individual", y = "Mean age of living kin", color = "Kin type") +
  scale_color_viridis_d(option = "D", direction = 1) +
  theme_graphs()
ggsave("Graphs/Socsim_Mean_Age_Kin_1950.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Socsim_Mean_Age_Kin_1950.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


## Distribution of the number of kin at each age of Focal

plots_dist_number_stable <- 
  distribution_number %>%
  filter(year_next == 1950) %>% 
  rename(Age = age_jan_next) %>%
  recode_kin() %>%
  group_split(kin_type) %>%
  map(function(df) {
    kin <- unique(df$kin_type)
    ggplot(df, aes(x = Age, y = dist_number, fill = as.factor(n_kin_alive))) +
      # Using geom_col instead of geom_area to avoid getting fake values > 1 when stacking
      geom_col(position = "stack", width = 1) + 
      labs(x = "Age of focal individual", y = "Proportion", fill = "Number of kin", title = kin) +
      scale_fill_viridis_d(option = "F", direction = -1) +
      theme_graphs() +
      theme(legend.position = 'none')
  })

cowplot::plot_grid(plotlist = plots_dist_number_stable, ncol = 4)
ggsave("Graphs/Socsim_Distribution_Number_Kin_1950.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Socsim_Distribution_Number_Kin_1950.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


## Proportion of individuals with null or large kin sets

# Helper function to calculate the cut point for large kin sets based on the 75 quantile of the distribution
wtd_quant <- function(x, w, quant = 0.75) {
  df <- tibble(x = x, w = w) %>%
    filter(!is.na(x) & !is.na(w) & w > 0) %>%
    arrange(x)
  if (nrow(df) == 0) return(NA_real_)
  cumw <- cumsum(df$w)
  cutoff <- quant * sum(df$w)
  df$x[which(cumw >= cutoff)[1]]
}

distribution_number %>%
  filter(year_next == 1950) %>% 
  recode_kin() %>% 
  group_by(year_next, age_jan_next, kin_type) %>%
  # Find the 75th percentile of the distribution of kin numbers
  summarise(large_cut = wtd_quant(n_kin_alive, dist_number, 0.75),
            prop_zero  = sum(dist_number[n_kin_alive == 0], na.rm = TRUE),
            prop_large = sum(dist_number[n_kin_alive >= large_cut], na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(c(prop_zero, prop_large),
               names_to = "kin_size",
               values_to = "proportion") %>% 
  ggplot(aes(x = age_jan_next, y = proportion, color = kin_type, linetype = kin_size)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ kin_type, ncol = 2) +
  labs(x = "Age of focal individual", 
       y = "Proportion", 
       color = "Kin type",
       linetype = "Kin size") +
  scale_color_viridis_d(option = "F", direction = 1) +
  scale_linetype_manual(
    values = c("dashed", "solid"), 
    labels = c("Null kin", "Large Kin (≥ 75th per)") ) +
  theme_graphs()
ggsave("Graphs/Socsim_Proportion_Null_Large_1950.jpg", width = 20, height = 16, dpi = 300)
ggsave("Graphs/Socsim_Proportion_Null_Large_1950.pdf", width = 20, height = 16, dpi = 300, device = "pdf")


## Distribution of ages of living kin at each age of Focal
ages_plot <- c(0, 20, 40, 60, 80)

distribution_age %>%
  filter(year_next == 1950) %>% 
  rename(Age = age_jan_next) %>%
  filter(Age %in% ages_plot) %>% 
  recode_kin() %>%
  ggplot(aes(x = kin_age_next, y = dist_age, fill = kin_type)) +
  geom_col(position = "dodge", width = 1) +
  facet_grid(kin_type ~ Age, scales = "free_y") +
  labs(x = "Ages of kin", y = "Proportion of kin", fill = "Kin type") +
  scale_fill_viridis_d(option = "D", direction = 1) +
  theme_graphs()
ggsave("Graphs/Socsim_Distribution_Age_Kin_1950.jpg", width = 20, height = 16, dpi = 300)
ggsave("Graphs/Socsim_Distribution_Age_Kin_1950.pdf", width = 20, height = 16, dpi = 300, device = "pdf")

#------------------------------------------------------------------------------------------------------
## Plot the mean number of kin at each age of Focal for each calendar year -----
# Using different scales for each kin type 

# Compute max number per kin_type
max_number_kin <- mean_number_age %>%
  filter(year_next > 1950) %>% 
  recode_kin() %>%
  group_by(kin_type) %>%
  summarise(max_val = max(mean_number, na.rm = TRUE),
            .groups = "drop")

# Split by kin type and plot with purrr
plot_mean_numbers <- mean_number_age %>% 
  filter(year_next > 1950) %>% 
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

# Wrap the plots for each kin
cowplot::plot_grid(plotlist = plot_mean_numbers, ncol = 4)
ggsave("Graphs/Socsim_Mean_Number_Kin_Age_Time.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Socsim_Mean_Number_Kin_Age_Time.pdf", width = 20, height = 16, dpi = 300, device = "pdf")
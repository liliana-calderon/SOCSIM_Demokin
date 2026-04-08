#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare estimates from SOCSIM microsimulation and Demokin
# U:/SOCSIM/SOCSIM_Demokin/4_Compare_Estimates.R

# Comparison of mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation and the matrix kinship model over 1950-2100

# Created on 08-04-2026
# Last modified on 08-04-2026
#------------------------------------------------------------------------------------------------------
## General settings and functions -----
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

## Load packages 
library(tidyverse)
library(cowplot) # For separate legends of facetted plots
library(ggh4x)  # To facet scales-
library(viridis)
library(patchwork) # To combine ggplots

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load theme for the graphs and to convert SOCSIM time to calendar years
# and estimate months corresponding to January and December 
source("Functions/Functions_Graphs.R")
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
## Load mean and distributions of numbers and ages of kin by age at 1st January 1950 ----

# Get all kin types sorted to be used in the function
kin_types_all <- sort(unique(kin$kin_type))

# Load mean numbers and ages of kin on 01.01.1950
load("Measures/mean_number_age_stable.RData")

# Combine and save distributions of numbers of kin on 01.01.1950
load("Measures/distribution_number_stable.RData")

# Combine and save distributions of ages of kin on 01.01.1950
load("Measures/distribution_age_stable.RData")


#------------------------------------------------------------------------------------------------------
## Plot mean and distributions of numbers and ages of kin by age at 1st January 1950 ----

# Mean number of kin at each age of Focal  
mean_number_age_stable %>%
  rename(Age = age_jan_next) %>%
  filter(!is.na(mean_number)) %>% 
  recode_kin() %>%
  ggplot(aes(x = Age, y = mean_number, color = kin_type)) +
  geom_line(linewidth = 1) +
  labs(x = "Age of focal individual", y = "Mean number of living kin", color = "Kin type") +
  scale_color_viridis_d(option = "D", direction =1)+
  theme_graphs()

# Mean age of kin at each age of Focal
mean_number_age_stable %>%
  rename(Age = age_jan_next) %>%
  filter(!is.na(mean_kin_age)) %>% 
  recode_kin() %>%
  ggplot(aes(x = Age, y = mean_kin_age, color = kin_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ kin_type, scales = "free_y", axes = "all") +
  labs(x = "Age of focal individual", y = "Mean age of living kin", color = "Kin type") +
  scale_color_viridis_d(option = "D", direction = 1) +
  theme_graphs()

# Distribution of the number of kin at each age of Focal

# Split and plot each kin type separately
plots_dist_number <- distribution_number_stable %>%
  rename(Age = age_jan_next) %>%
  recode_kin() %>%
  mutate(n_kin_alive = as.factor(n_kin_alive)) %>%
  group_split(kin_type) %>%
  map(function(df) {
    kin <- unique(df$kin_type)
    ggplot(df, aes(x = Age, y = dist_number, fill = n_kin_alive)) +
      geom_area(color = "#4D4D4D", linewidth = 0.3) +
      labs(x = "Age of focal individual", y = "Proportion", fill = "Number of kin", title = kin) +
      scale_fill_viridis_d(option = "F", direction = -1) +
      theme_graphs() +
      theme(legend.position = 'none')
  })

cowplot::plot_grid(plotlist = plots_dist_number, ncol = 4)
ggsave("Graphs/Distribution_Number_Kin_Age.jpg", width = 20, height = 16,  dpi = 300)

#------------------------------------------------------------------------------------------------------
## Load mean and distributions of numbers and ages of kin by age and year ----

# Load mean number and ages of kin by age of Focal and calendar year
load("Measures/mean_number_age.RData")

# Load distribution of numbers kin by age of Focal and calendar year
load("Measures/distribution_number.RData")

# Load distribution of ages of kin by age of Focal and calendar year
load("Measures/distribution_age.RData")

#------------------------------------------------------------------------------------------------------
## Plot the mean number of kin by age with different scales for each kin type -----

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

# Wrap the plots for each kin
cowplot::plot_grid(plotlist = plots_means, ncol = 4)
ggsave("Graphs/1_Mean_Number_Kin_Age.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/1_Mean_Number_Kin_Age.pdf", width = 20, height = 16, dpi = 300, device = "pdf")
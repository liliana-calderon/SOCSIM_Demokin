#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare estimates from SOCSIM microsimulation and Demokin
# U:/SOCSIM/SOCSIM_Demokin/4_Compare_Estimates.R

# Comparison of mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation and the matrix kinship model over 1950-2100

# Created on 08-04-2026
# Last modified on 10-04-2026
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
## Load opop, means and distributions of kin by age and year and define constants -----
# Results for 01.01.1950 correspond to the stable setting
# Results for 01.01.1951-01.01.2101 correspond to the dynamic setting

## Load opop generated in 0_Set_Up_Simulation.R ##
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Load mean numbers and ages for all years
load("Measures/mean_number_age.RData")

# Load distributions of numbers for all years
load("Measures/distribution_number.RData")

# Load distributions of ages for all years
load("Measures/distribution_age.RData")

#------------------------------------------------------------------------------------------------------
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
ggsave("Graphs/Mean_Number_Kin_Stable.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Stable.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


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
ggsave("Graphs/Mean_Age_Kin_Stable.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Age_Kin_Stable.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


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
ggsave("Graphs/Distribution_Number_Kin_Stable.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Distribution_Number_Kin_Stable.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")


## Proportion of individuals with null or large kin sets

# Helper to calculate a point for large kin sets based on the 75q of the distribution
wtd_quant <- function(x, w, prob = 0.75) {
  # Remove values where weight is NA or zero
  df <- tibble::tibble(x = x, w = w) %>%
    filter(!is.na(x), !is.na(w), w > 0) %>%
    arrange(x)
  if (nrow(df) == 0) return(NA_real_)
  cumw <- cumsum(df$w)
  cutoff <- prob * sum(df$w)
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
ggsave("Graphs/Proportion_Null_Large_Stable.jpg", width = 20, height = 16, dpi = 300)
ggsave("Graphs/Proportion_Null_Large_Stable.pdf", width = 20, height = 16, dpi = 300, device = "pdf")


## Distribution of ages of living kin at each age of Focal
selected_ages <- c(0, 20, 40, 60, 80)

distribution_age %>%
  filter(year_next == 1950) %>% 
  rename(Age = age_jan_next) %>%
  filter(Age %in% selected_ages) %>% 
  recode_kin() %>%
  ggplot(aes(x = kin_age_next, y = dist_age, fill = kin_type)) +
  geom_col(position = "dodge", width = 1) +
  facet_grid(kin_type ~ Age, scales = "free_y") +
  labs(x = "Ages of kin", y = "Proportion of kin", fill = "Kin type") +
  scale_fill_viridis_d(option = "D", direction = 1) +
  theme_graphs()
ggsave("Graphs/Distribution_Age_Kin_Stable.jpg", width = 20, height = 16, dpi = 300)
ggsave("Graphs/Distribution_Age_Kin_Stable.pdf", width = 20, height = 16, dpi = 300, device = "pdf")

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
ggsave("Graphs/Mean_Number_Kin_Age_Time.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Age_Time.pdf", width = 20, height = 16, dpi = 300, device = "pdf")
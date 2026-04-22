#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare estimates from SOCSIM microsimulation and Demokin
# U:/SOCSIM/SOCSIM_Demokin/5_Compare_Estimates.R

# Comparison of mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation and the matrix kinship model over 1950-2100

# Created on 08-04-2026
# Last modified on 22-04-2026
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

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load theme for the graphs and to convert SOCSIM time
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

# Load estimates from the time-invariant model
load("Measures/demokin_invariant.RData")

# Load estimates from the time-variant model
load("Measures/demokin_variant.RData")

#------------------------------------------------------------------------------------------------------
## Plot mean numbers of kin by age at selected years ----

# Select years to plot
years_plot <- c(1950, 2000, 2050, 2100) 

# Get mean number of kin at each age of Focal at each selected year from Demokin
demokin_mean_number <- demokin_variant$kin_summary %>% 
  recode_demokin() %>%
  summarise(mean_number = sum(count_living), 
            .by = c(year, kin_type, age_focal)) %>%
  mutate(Model = "Demokin")

# Get mean number of kin at each age of Focal at each selected year from Socsim
socsim_mean_number <- mean_number_age %>%
  filter(year_next %in% years_plot) %>%
  rename(year = year_next, age_focal = age_jan_next) %>%
  filter(!is.na(mean_number)) %>%
  recode_kin() %>%
  select(year, kin_type, age_focal, mean_number) %>% 
  mutate(Model = "Socsim")

# Plot mean number of kin at each age of Focal at selected years from both models
bind_rows(demokin_mean_number, socsim_mean_number) %>% 
  ggplot(aes(x = age_focal, y = mean_number, color = Model)) +
  geom_line(linewidth = 1) +
  facet_grid(kin_type ~ year, scales = "free_y") +
  labs(x = "Age of focal", y = "Mean number of kin", color = "Model") +
  scale_color_manual(values = c( "#B22222", "#007A75"))+
  theme_graphs()
ggsave("Graphs/Comparison_Mean_Number_Age.jpg", width = 20, height = 30, dpi = 300)
ggsave("Graphs/Comparison_Mean_Number_Age.pdf", width = 20, height = 30, dpi = 300, device = "pdf")

#------------------------------------------------------------------------------------------------------
## Plot mean ages of kin by age at selected years ----

years_plot <- c(1950, 2000, 2050, 2100)

# Get mean age of kin at each age of Focal at each selected year from Demokin
demokin_mean_age <- demokin_variant$kin_summary %>% 
  recode_demokin() %>%
  filter(year %in% years_plot) %>%
  # Weighting by 'count_living' to ensure an age-weighted mean of kin between the sexes.
  summarise(mean_age_kin = weighted.mean(mean_age, count_living, na.rm = TRUE),
            .by = c(year, kin_type, age_focal)) %>%
  mutate(Model = "Demokin")

# Get mean age of kin at each age of Focal at each selected year from Socsim
socsim_mean_age <- mean_number_age %>%
  filter(year_next %in% years_plot) %>%
  rename(year = year_next, age_focal = age_jan_next) %>%
  filter(!is.na(mean_kin_age)) %>%
  recode_kin() %>%
  select(year, kin_type, age_focal, mean_age_kin = mean_kin_age) %>% 
  mutate(Model = "Socsim")

# Plot mean age of kin at each age of Focal at selected years from both models 
bind_rows(demokin_mean_age, socsim_mean_age) %>% 
  filter(!is.na(mean_age_kin)) %>%
  ggplot(aes(x = age_focal, y = mean_age_kin, color = Model)) +
  geom_line(linewidth = 1) +
  facet_grid(kin_type ~ year, scales = "free_y") +
  labs(x = "Age of focal", y = "Mean age of kin", color = "Model") +
  scale_color_manual(values = c("#B22222", "#007A75")) +
  theme_graphs()
ggsave("Graphs/Comparison_Mean_Age_Kin.jpg", width = 20, height = 30, dpi = 300)
ggsave("Graphs/Comparison_Mean_Age_Kin.pdf", width = 20, height = 30, dpi = 300, device = "pdf")


# Mean age of of kin by sex at each age of Focal at selected years for Demokin
demokin_mean_age_sex <- bind_rows(demokin_variant_1950$kin_summary,
                                  demokin_variant$kin_summary) %>%
  recode_demokin() %>%
  filter(year %in% years_plot) %>%
  summarise(mean_age_kin = mean(mean_age, na.rm = TRUE),
            .by = c(year, kin_type, age_focal, sex_kin)) %>%
  mutate(Model = paste0("Demokin_", sex_kin))

# Plot mean age of kin by sex at each age of Focal at selected years for both models
bind_rows(demokin_mean_age_sex, socsim_mean_age) %>%
  ggplot(aes(x = age_focal, y = mean_age_kin, linetype = Model, color = kin_type)) +
  geom_line(linewidth = 1) +
  facet_grid(kin_type ~ year, scales = "free_y") +
  scale_color_viridis_d(option = "D", direction = 1) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  labs(x = "Age of focal",
       y = "Mean age of kin",
       color = "Kin type",
       linetype = "Model/Sex") +
  theme_graphs()
ggsave("Graphs/Comparison_Mean_Age_Kin_Sex.jpg", width = 20, height = 30, dpi = 300)
#------------------------------------------------------------------------------------------------------
## Plot distribution of ages of kin at selected years and ages of Focal ----

ages_plot <- c(0, 20, 40, 60, 80)
years_plot <- c(1950, 2000, 2050, 2100)

# Calculate distribution of ages of kin at selected years and ages of Focal from Demokin
demokin_dist_age <- demokin_variant$kin_full %>%
  recode_demokin() %>%
  filter(year %in% years_plot & age_focal %in% ages_plot & living > 0) %>%
  summarise(n_kin = sum(living), 
            .by = c(year, kin_type, age_focal, age_kin)) %>%
  group_by(year, age_focal, kin_type) %>%
  mutate(dist_age = n_kin / sum(n_kin, na.rm = TRUE), 
         Model = "Demokin") %>% 
  ungroup()

# Wrangle socsim distribution of ages of kin at selected years and ages of Focal from Socsim
socsim_dist_age <- distribution_age %>%
  filter(year_next %in% years_plot & age_jan_next %in% ages_plot) %>%
  rename(year = year_next,
         age_focal = age_jan_next,
         age_kin = kin_age_next) %>%
  recode_kin() %>% 
  mutate(Model = "Socsim")

# Plot distribution of ages of kin at selected years and ages of Focal from models
bind_rows(demokin_dist_age, socsim_dist_age) %>% 
  ggplot(aes(x = age_kin, y = dist_age, fill = Model)) +
  geom_col(position = "identity", width = 1) +
  facet_grid(kin_type ~ age_focal + year, scales = "free_y") +
  labs(x = "Age of kin", 
       y = "Proportion of kin at focal's age", 
       fill = "Model") +
  scale_fill_manual(values = c("#B22222", "#007A75")) + 
  theme_graphs()

# Combine data for plotting
dist_age_data <- bind_rows(demokin_dist_age, socsim_dist_age)

plots_dist_age <- dist_age_data %>%
  group_split(year) %>%
  map(function(df) {
    yr <- unique(df$year)
    ggplot(df, aes(x = age_kin, y = dist_age, fill = Model)) +
      geom_col(position = "identity", width = 1, alpha = 0.6) +
      facet_grid(kin_type ~ age_focal, scales = "free_y", switch = "y") +
      labs(title = as.character(yr),
           x = "Age of kin", y = "Proportion", fill = "Model") +
      scale_fill_manual(values = c("#B22222", "#007A75")) +
      theme_graphs() +
      theme(legend.position = 'bottom',
            strip.text.y.left = element_text(angle = 0, size = 10),
            strip.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 14))
  })

cowplot::plot_grid(plotlist = plots_dist_age, ncol = 1)
ggsave("Graphs/Comparison_Distribution_Age_Kin.jpg", width = 22, height = 32, dpi = 300)
ggsave("Graphs/Comparison_Distribution_Age_Kin.pdf", width = 22, height = 32, dpi = 300, device = "pdf")
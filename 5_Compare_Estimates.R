#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare estimates from SOCSIM microsimulation and Demokin
# U:/SOCSIM/SOCSIM_Demokin/5_Compare_Estimates.R

# Comparison of mean and distributions of numbers and ages of kin by age 
# from a SOCSIM microsimulation and the matrix kinship model over 1950-2100

# Created on 08-04-2026
# Last modified on 21-04-2026
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

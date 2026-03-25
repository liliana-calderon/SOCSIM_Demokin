#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Estimate Mean Number of Kin
# U:/SOCSIM/SOCSIM_Demokin/2_Estimate_Mean_Kin.R

## Estimate mean number of kin from a SOCSIM microsimulation (1950-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 24-03-2026
# Last modified on 25-03-2026
#----------------------------------------------------------------------------------------------------
## General settings and functions -----

rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of faceted plots
#library(ggh4x)  # To facet scales-
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load function to retrieve selected kin
source("Functions/Functions_Kin.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#------------------------------------------------------------------------------------------------------
## Trace kinship network of individuals born between 1950 and 2100 ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Get pids of individuals born between 1950 and 2100
egos <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year)) %>% 
  filter(birth_year >= 1950 & birth_year <= 2000) %>% 
  pull(pid)

# Check length of number of egos selected
length(egos) # 202648

# Retrieve pids of kin of individuals born between 1950 and 2100
kin <- retrieve_kin_mod(opop = opop, pid = egos, KidsOf = KidsOf) 
save(kin, file = "kin.RData")

#------------------------------------------------------------------------------------------------------
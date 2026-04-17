#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin -  Retrieve kinship network of simulated individuals alive over 1950-2100
# U:/SOCSIM/SOCSIM_Demokin/2_Retrieve_Kin_Simulation.R

##  Retrieve kinship network of individuals alive over 1950-2100 from a SOCSIM microsimulation 

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 24-03-2026
# Last modified on 17-04-2026
#----------------------------------------------------------------------------------------------------
## General settings and functions -----

rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of faceted plots

## Load function to retrieve selected kin and recode kin types
source("Functions/Functions_Kin.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")
#------------------------------------------------------------------------------------------------------
# Check months corresponding to presimulation and first year of simulation with actual rates (1950) ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Pre-simulation runs over 200 years, 1750-1949 (months 1001 to 3400)
# to get stable population, with complete kinship networks, at the end of the first segment
asYr(1001, last_month, final_sim_year)
asYr(3400, last_month, final_sim_year)

# To compare the stable setting, we use the population at 1st January 1950
jan(1950, last_month, final_sim_year)

# To compare the dynamic setting, we use the population at 1st January 1951-2101
jan(1951, last_month, final_sim_year)
jan(2101, last_month, final_sim_year)
#------------------------------------------------------------------------------------------------------
## Retrieve kinship network of individuals alive over 1950-2101 ----

# Define period of analysis
year_start <- 1950
year_end   <- 2100

# Months for 1st Jan each year over 1950-2100
# Using year_end+1 to include individuals born over 2100 alive at the end of the simulation (01.01.2101)
jan_months <- jan(year_start:(year_end+1), last_month, final_sim_year) 

# Get pids of individuals (aged 0-100) alive on each 1st January over 1950-2101
egos <- opop %>%
  mutate(dod2 = if_else(dod == 0, 9999, dod)) %>%
  filter(dob <= max(jan_months) & dod2 >= min(jan_months)) %>%
  pull(pid)

# Check length of number of egos selected
length(egos) # 464821

start <- Sys.time()
# Retrieve pids of kin of individuals born 1950-2100
kin_list <- retrieve_kin_mod(opop = opop, pid = egos, KidsOf = KidsOf) 
save(kin_list, file = "kin_list.RData")
end <- Sys.time()
print(end-start)
# Time difference of 5.68921 hours

# Transform nested list into a tibble, using imap (indexed map)
# Get the vector of kin pids for each kin type (.x) 
# and the name (or index) of the kin type (.y)
kin <- kin_list %>%
  imap_dfr(~ tibble(
    index_ego = 1:length(.x),
    kin_type = .y,
    pids_kin = .x)) %>%
  # Match ego_index in the kin list to ego pids in the egos vector
  mutate(pid_ego = egos[index_ego]) %>%
  # Unnest the pids_kin list so that each row corresponds to each kin
  unnest(pids_kin) %>%
  filter(!is.na(pids_kin)) %>%
  select(pid_ego, kin_type, pid_kin = pids_kin) %>% 
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_ego" == "pid")) %>% 
  rename(dob_ego = dob, dod_ego = dod, fem_ego = fem) %>% # dod = 0 for egos alive at the end
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_kin" == "pid")) %>% 
  rename(dob_kin = dob, dod_kin = dod, fem_kin = fem)
save(kin, file = "kin.RData")

#------------------------------------------------------------------------------------------------------
## Estimate mean number of kin alive at birth (as quick check) ----

# Load df with pids of kin and egos, and relevant information from opop (dob, dod, fem)
# for individuals alive on each 1st January over 1950-2101
load("kin.RData")

# Calculate mean number of kin alive at ego's birth 
mean_kin_birth <- kin %>%
  recode_kin_sex() %>%
  # Keep only kin who were born before egos' birth month
  filter(dob_kin < dob_ego) %>% 
  # Keep only kin who died in/after egos' birth month or are still alive
  filter(dob_ego <= dod_kin | dod_kin == 0) %>%
  # Count number of kin types by year
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year)) %>%
  count(dob_ego_year, kin_type, name = "n_kin") %>% 
  # Get number of egos per year as denominator for all kin_types
  left_join(kin %>%
              mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year)) %>%
              distinct(dob_ego_year, pid_ego) %>%
              count(dob_ego_year, name = "n_egos"),
            by = "dob_ego_year") %>%
  mutate(mean_kin = n_kin / n_egos)

# Check kin types in mean_kin_birth (No offspring should appear)
mean_kin_birth %>% pull(kin_type) %>% unique()

# Plot mean number of kin alive at birth, faceted by sex
mean_kin_birth %>% 
  mutate(Sex = ifelse(kin_type %in% c("Greatgrandmothers", "Grandmothers", "Mother", 
                                 "Aunts", "Cousins_Female", "Sisters", "Nieces" ), 
                      "Female", "Male")) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, mean_kin, group = kin_type, colour = kin_type)) +
        geom_line(linewidth = 1) +
        facet_wrap(~Sex)+
        theme_graphs() +
        scale_color_viridis_d(option = "D", direction =1)+
        scale_x_continuous(breaks = c(1850, 1900, 1950, 2000, 2050, 2100))+
        labs(x = "Year", y = "Mean Number Alive at Birth", colour = "Kin")+
        theme(legend.title=element_blank())) %>% 
  cowplot::plot_grid(plotlist = .)

# Plot mean number of kin alive at birth, with both sexes on the same figure
mean_kin_birth %>% 
  mutate(Sex = ifelse(kin_type %in% c("Greatgrandmothers", "Grandmothers", "Mother", 
                                      "Aunts", "Cousins_Female", "Sisters", "Nieces"), 
                      "Female", "Male")) %>% 
  recode_kin_generation() %>% 
  ggplot(aes(x= dob_ego_year, mean_kin, group = kin_type, colour = Generation, linetype = Sex)) +
  geom_line(linewidth = 1) +
  theme_graphs() +
  scale_color_viridis_d(option = "H", direction =1)+
  scale_x_continuous(breaks = c(1850, 1900, 1950, 2000, 2050, 2100))+
  labs(x = "Year", y = "Mean Number Alive at Birth", 
       colour = "Kin Type", linetype = "Kin Sex") +
  guides(colour = guide_legend(order = 1),
         linetype = guide_legend(order = 2))
ggsave(file="Graphs/Mean_Number_Kin_Birth.jpeg", width=18, height=9, dpi=300)
ggsave(file="Graphs/Mean_Number_Kin_Birth.pdf", width=18, height=9, dpi=300, device = "pdf")
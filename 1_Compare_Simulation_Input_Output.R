#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Compare Simulation Input and Output Rates
# U:/SOCSIM/SOCSIM_Demokin/1_Compare_Simulation_Input_Output.R

## Compare input and output age-specific rates from a SOCSIM microsimulation (1950-2100)
# as well as summary measures such as TFR, e0

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 14-10-2025
# Last modified on 19-03-2026

## Based on code prepared for the overlap with ancestors paper
# U:/SOCSIM/SOCSIM_Living_Ancestors/1_Compare_Input_Output.R
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation (useful for the rate calculation)
options(scipen=999999)

## Load packages 
library(tidyverse)
library(ggh4x)  # To facet scales-
library(HMDHFDplus)
library(patchwork) # To combine ggplots
library(readxl)
library(rsocsim) # Functions to estimate rates
library(svglite) # To save svg files
library(viridis)

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

# Load function to calculate life table from asmr 1x1
# Currently, it only works with asmr calculated with rsocsim::estimate_mortality_rates()
source("Functions/Functions_Life_Table.R")

## Load opop and omar generated in 0_Set_Up_Simulation.R 
# from simulation with no heterogeneous fertility
load("opop.RData")
load("omar.RData")

#------------------------------------------------------------------------------------------------------
# Age-Specific Fertility and Mortality rates, 5x5 ----

# Create a sub-folder called "Measures" to save the output measures if it does not exist.
ifelse(!dir.exists("Measures"), dir.create("Measures"), FALSE)

# Estimate age-specific fertility rates
asfr_sim <- rsocsim::estimate_fertility_rates(opop = opop,
                                              final_sim_year = 2100, #[Jan-Dec]
                                              year_min = 1950, # Closed [
                                              year_max = 2100, # Open )
                                              year_group = 5, 
                                              age_min_fert = 15, # Closed [
                                              age_max_fert = 50, # Open )
                                              age_group = 5) #[,)
save(asfr, file = "Measures/asfr.RData")

# Estimate age-specific mortality rates
asmr_sim <- rsocsim::estimate_mortality_rates(opop = opop, 
                                              final_sim_year = 2100, # [Jan-Dec]
                                              year_min = 1950, # Closed
                                              year_max = 2100, # Open )
                                              year_group = 5,
                                              age_max_mort = 100, # Open )
                                              age_group = 5) #[,)
save(asmr, file = "Measures/asmr.RData")

#------------------------------------------------------------------------------------------------------
## Plot Age-specific fertility and mortality rates, 5x5 ----

# Load ASFR and ASMR from the simulations 
load("Measures/asfr.RData")
load("Measures/asmr.RData")

# Create a sub-folder called "Graphs" to save the plots if it does not exist.
ifelse(!dir.exists("Graphs"), dir.create("Graphs"), FALSE)

## ASFR and ASMR from SOCSIM for women
# Age groups for fertility and mortality must be the same to plot the figure below

# Choose years to plot (in intervals).
yrs_plot <- c("[1950,1955)", "[2000,2005)", "[2050,2055)", "[2095,2100)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(asmr_sim$age)

bind_rows(asfr_sim %>%
            mutate(rate = "ASFR",                   
                   sex = "female"),
          asmr_sim %>% 
            mutate(rate = "ASMR") %>% 
            filter(sex == "female")) %>% 
  mutate(age = factor(as.character(age), levels = age_levels)) %>% 
  # Filter rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(socsim !=0 & !is.infinite(socsim) & !is.nan(socsim)) %>% 
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = socsim, group = year, colour = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(. ~ rate, scales = "free") + 
  facetted_pos_scales(y = list(ASFR = scale_y_continuous(),
                               ASMR =  scale_y_continuous(trans = "log10"))) +
  theme_graphs() +
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age", y = "Estimate")
ggsave(file="Graphs/ASFR_ASMR_SOCSIM.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Comparison with WPP data used as input ----

# ASFR ----

# Load the WPP fertility data
WPP_fert <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)

# Filter WPP fertility data for Sweden and medium variant for the projected period
WPP_fert <- WPP_fert %>% 
  filter(ISO3_code == "SWE" & Variant == "Medium" & Time >= 1950 & Time <= 2100) %>% 
  select(Year = Time, Age = AgeGrpStart, ASFR)  %>% 
  mutate(ASFR = ASFR/1000)

# Extract year and age breaks used in the estimate_fertility_rates() to apply the same values to WPP data

# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert <- unique(as.numeric(str_extract_all(asfr_sim$year, "\\d+", simplify = T)))

# Year range to filter WPP data
year_range_fert <- min(year_breaks_fert):max(year_breaks_fert-1)

# Age breaks of fertility rates. Extract all the unique numbers from the intervals 
age_breaks_fert <- unique(as.numeric(str_extract_all(asfr_sim$age, "\\d+", simplify = T)))

# Compute age-specific fertility rates by 5-year age groups
WPP_fert5 <- WPP_fert %>% 
  filter(Year %in% year_range_fert) %>% 
  mutate(year = cut(Year, breaks = year_breaks_fert, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_fert, 
                   include.lowest = F, right = F, ordered_results = T)) %>% 
  filter(!is.na(age)) %>% 
  group_by(year, age) %>%
  summarise(ASFR = mean(ASFR, na.rm = T)) %>%
  ungroup() %>%
  mutate(Source = "WPP", 
         Rate = "ASFR")

# Wrangle SOCSIM data
SocsimF5 <- asfr_sim %>% 
  rename(ASFR = socsim) %>% 
  mutate(Source = "SOCSIM",
         Rate = "ASFR")

## Plot ASFR from WPP vs SOCSIM   

# Same years to plot than above (in intervals). 
yrs_plot <- c("[1950,1955)", "[2000,2005)", "[2050,2055)", "[2095,2100)") 

bind_rows(WPP_fert5, SocsimF5) %>%
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = ASFR, group = interaction(year, Source)))+
  geom_line(aes(colour = year, linetype = Source), linewidth = 1.2)+
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_linetype_manual(values = c("WPP" = "dotted", "SOCSIM" = "solid")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/ASFR_Input_Output.jpeg", width=17, height=9, dpi=300)


# ASMR ----
# Compare the SOCSIM mid-year ASMR with central death rates from WPP life tables.
# Rates will be aggregated as defined in the estimate_mortality_rates()

# Load the WPP 2024 historical and projected life tables in csv format
WPP_lifetable_female_hist <- read_csv("WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz", show_col_types = FALSE) 
WPP_lifetable_male_hist <- read_csv("WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz", show_col_types = FALSE) 
WPP_lifetable_female_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz", show_col_types = FALSE)
WPP_lifetable_male_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz", show_col_types = FALSE) 

# Combine WPP male and female life tables and filter for Sweden
WPP_mort <- bind_rows(WPP_lifetable_female_hist, WPP_lifetable_male_hist,
                      WPP_lifetable_female_proj, WPP_lifetable_male_proj) %>% 
  filter(ISO3_code == "SWE") %>%
  select(Year = Time, Age = AgeGrpStart, mx, ex, Sex)


# Extract year and age breaks used in the estimate_mortality_rates() to apply the same values to WPP data
# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort <- unique(as.numeric(str_extract_all(asmr_sim$year, "\\d+", simplify = T)))

# Year range to filter WPP data
year_range_mort <- min(year_breaks_mort):max(year_breaks_mort-1)

# Age breaks of mortality rates. Extract all the unique numbers from the intervals 
age_breaks_mort <- unique(as.numeric(str_extract_all(asmr_sim$age, "\\d+", simplify = T)))

# Wrangle WPP life tables
WPP_mort5 <- WPP_mort %>% # Check if necessary to delete ex
  filter(Year %in% year_range_mort) %>% 
  mutate(year = cut(Year, breaks = year_breaks_mort, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_mort, 
                   include.lowest = F, right = F, ordered_results = T)) %>%
  filter(!is.na(age)) %>% 
  group_by(year, Sex, age) %>% 
  summarise(mx = mean(mx, na.rm = T)) %>%
  ungroup() %>% 
    mutate(Source = "WPP",
           Rate = "ASMR")

# Wrangle SOCSIM data
SocsimM5 <- asmr_sim %>% 
  rename(mx = socsim) %>% 
  mutate(Sex = ifelse(sex == "male", "Male", "Female"),
         Source = "SOCSIM",
         Rate = "ASMR") %>% 
  select(year, Sex, age,mx, Source, Rate)

## Plot ASMR from WPP vs SOCSIM   

# Same years to plot than above (in intervals). 
yrs_plot <- c("[1950,1955)", "[2000,2005)", "[2050,2055)", "[2095,2100)") 

bind_rows(WPP_mort5, SocsimM5) %>%
  filter(year %in% yrs_plot) %>% 
  filter(mx != 0 & !is.infinite(mx) & !is.nan(mx)) %>% 
  ggplot(aes(x = age, y = mx, group = interaction(year, Source)))+
  facet_wrap(~Sex) +
  geom_line(aes(colour = year, linetype = Source), linewidth = 1.2)+
  scale_y_log10() +
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_linetype_manual(values = c("WPP" = "dotted", "SOCSIM" = "solid")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/ASMR_Input_Output.jpeg", width=17, height=9, dpi=300)


#---------------------------------------------------------------------------------------------------
## Final plot combining ASFR and ASMR ----

# Same years to plot than above (in intervals). 
yrs_plot <- c("[1950,1955)", "[2000,2005)", "[2050,2055)", "[2095,2100)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(SocsimM$age)

## Plotting ASFR and ASMR (for females) from WPP vs SOCSIM 
bind_rows(WPP_fert5 %>% rename(Estimate = ASFR), 
          SocsimF5 %>% rename(Estimate = ASFR)) %>% 
  mutate(Sex = "Female") %>%   
  bind_rows(WPP_mort5 %>% rename(Estimate = mx),
            SocsimM5 %>% rename(Estimate = mx)) %>% 
  # Filter rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(Estimate != 0 & !is.infinite(Estimate) & !is.nan(Estimate)) %>% 
  filter(Sex == "Female") %>% 
  mutate(Year = year,
         age = factor(as.character(age), levels = age_levels),
         Rate = ifelse(Rate == "ASFR", "Age-Specific Fertility Rates", "Age-Specific Mortality Rates")) %>%
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = Estimate, group = interaction(year, Source)))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = year, linetype = Source), linewidth = 1)+
  scale_linetype_manual(values = c("WPP" = "solid","SOCSIM" = "11")) +
  facetted_pos_scales(y = list("Age-Specific Fertility Rates" = scale_y_continuous(),
                               "Age-Specific Mortality Rates" =  scale_y_continuous(trans = "log10")))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age") + 
  theme_graphs()
ggsave(file="Graphs/ASFR_ASMR_Input_Output.jpeg", width=17, height=9, dpi=300)
ggsave(file="Graphs/ASFR_ASMR_Input_Output.pdf", width=17, height=9, dpi=300, device = "pdf")
#----------------------------------------------------------------------------------------------------
# Summary measures: TFR and e0 ----
# Here, we use the socsim rates by 1 year age group and 1 calendar year

# Total Fertility Rate ----

## Retrieve age-specific fertility rates, by 1 year age group and 1 calendar year
asfr_1 <- estimate_fertility_rates(opop = opop,
                                   final_sim_year = 2100, #[Jan-Dec]
                                   year_min = 1950, # Closed [
                                   year_max = 2101, # Open )
                                   year_group = 1, 
                                   age_min_fert = 15, # Closed [
                                   age_max_fert = 50, # Open )
                                   age_group = 1) # [,)
save(asfr_1, file = "Measures/asfr_1.RData")


# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert_1 <- unique(as.numeric(str_extract_all(asfr_1$year, "\\d+", simplify = T)))

# Year range to filter WPP data
year_range_fert_1 <- min(year_breaks_fert_1):max(year_breaks_fert_1-1)

# Age breaks of fertility rates. Extract all the unique numbers from the intervals 
age_breaks_fert_1 <- unique(as.numeric(str_extract_all(asfr_1$age, "\\d+", simplify = T)))

# Retrieve age_group size
age_group_fert_1 <- unique(diff(age_breaks_fert_1)) # Just to check

# Calculate TFR from WPP
TFR_WPP <- WPP_fert %>%  
  filter(Year %in% year_range_fert_1) %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(ASFR, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Source = "WPP")

# Calculate TFR from SOCSIM
TFR <-  asfr_1 %>% 
  mutate(Year = as.numeric(str_extract(year, "\\d+"))) %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(socsim, na.rm = T)) %>%
  ungroup() %>% 
  mutate(Source = "SOCSIM") 

## Plot TFR from WPP vs SOCSIM 
bind_rows(TFR_WPP, TFR) %>%
  mutate(transp = ifelse(Source == "SOCSIM", "0", "1")) %>% 
  ggplot(aes(x = Year, y = TFR, group = Source)) +
  geom_line(aes(colour = Source, alpha = transp), linewidth = 1.3)+
  scale_color_manual(values = c("#007A75", "#5B92E5"))+
  scale_alpha_discrete(guide = "none", range = c(0.2, 1))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/WPP_SOCSIM_TFR.jpeg", width=17, height=9, dpi=300)

# Life Expectancy at birth ----
# Calculate life expectancy at birth 1x1 for the SOCSIM simulation

# Retrieve age-specific mortality rates, by 1 year age group and 1 calendar year
asmr_1 <- estimate_mortality_rates(opop = opop,
                                   final_sim_year = 2100, #[Jan-Dec]
                                   year_min = 1950, # Closed
                                   year_max = 2101, # Open )
                                   year_group = 1,
                                   age_max_mort = 100, # Open )
                                   age_group = 1) # [,)
save(asmr_1, file = "Measures/asmr_1.RData")

# Compute life tables from asmr 1x1 from the SOCSIM simulation
lt_socsim <- lt_socsim(asmr_socsim = asmr_1)
save(lt, file = "Measures/lt.RData")

# Load and wrangle life tables for plotting ----

# Load life tables from asmr 1x1 from the SOCSIM simulation
load("Measures/lt.RData")

# Load asmr 1x1 for the simulations
load("Measures/asmr_1.RData")

# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort_1 <- unique(as.numeric(str_extract_all(asmr_1$year, "\\d+", simplify = T)))

# Year range to filter WPP data
year_range_mort_1 <- min(year_breaks_mort_1):max(year_breaks_mort_1-1)

# Wrangle WPP life tables 
WPP_lt <- WPP_mort
  # bind_rows(WPP_lifetable_female_hist, WPP_lifetable_male_hist,
  #           WPP_lifetable_female_proj, WPP_lifetable_male_proj) %>% 
  # filter(ISO3_code == "SWE") %>%
  # select(Year = Time, Age = AgeGrpStart, mx, ex, Sex)
  mutate(sex = factor(sex, levels = c("male", "female")), 
         Source = "WPP") %>% 
  select(Year, Sex, Age, ex, Source)

# Wrangle SOCSIM life tables
lt_socsim <- lt %>%
  mutate(Year = as.numeric(str_extract(year, "\\d+")),
         Sex = ifelse(sex == "female", "Female", "Male"),
         Source = "SOCSIM") %>% 
  select(Year, Sex, Age, ex, Source)

# Plot life expectancy at birth, Input vs Output
bind_rows(lt_WPP, lt_socsim) %>% 
  filter(Age == 0 & Year %in% year_range_mort_1) %>%
 # mutate(Sex = ifelse(sex == "female", "Female", "Male")) %>% 
  ggplot(aes(x = Year, y = ex, group = Source))+
  geom_line(aes(colour = Source), linewidth = 1.3)+
  scale_color_manual(values = c("#5B92E5", "#007A75"))+
  facet_wrap(~Sex) +
  theme_graphs()+
  labs(y = "e0") 
ggsave(file="Graphs/e0_Input_Output.jpeg", width=17, height=9, dpi=300)
#----------------------------------------------------------------------------------------------------
## Final plot combining TFR and e0 ----

# Define the same y breaks for all plots
y_breaks_TFR <- c(0:5)
y_breaks_e0 <- c(20, 40, 60, 80)

## Plotting TFR and e0 (for females) from WPP/WPP vs SOCSIM 
bind_rows(TFR_WPP %>% rename(Estimate = TFR) %>%  mutate(Rate = "TFR"),
          TFR %>% rename(Estimate = TFR) %>% mutate(Rate = "TFR")) %>% 
  mutate(Sex = "Female") %>%   
  bind_rows(lt_WPP %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0"),
            lt_socsim %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0")) %>% 
  filter(Sex == "Female") %>%
  mutate(Rate = ifelse(Rate == "TFR", "Total Fertility Rate", "Life Expectancy at Birth"), 
         Rate = factor(Rate, levels = c("Total Fertility Rate", "Life Expectancy at Birth"))) %>% 
  ggplot(aes(x = Year, y = Estimate, group = Source))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = Source), linewidth = 1.2) +
  scale_color_manual(values = c("#CA650D", "#0C0B7F", "#007A75")) +
  facetted_pos_scales(y = list("Total Fertility Rate" = scale_y_continuous(breaks = y_breaks_TFR, 
                                                                           limits = c(0, NA)),
                               "Life Expectancy at Birth" =  scale_y_continuous(breaks = y_breaks_e0)))+
  scale_x_continuous(breaks = c(1950, 2000, 2050, 2100))+
  theme_graphs()

ggsave(file="Graphs/TFR_e0_Input_Output.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Run Matrix models using Demokin and WPP data
# U:/SOCSIM/SOCSIM_Demokin/4_Run_Matrix_Models.R

## Run two-sex time-invariant and time-varying matrix models using the 'demokin' package and
## using data from the World Population Prospects 2024. 

# Created on 17-04-2026
# Last modified on 20-04-2026
#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

# Load necessary packages
library(data.table)
library(readr)
library(tidyverse)

# Install Demokin from CRAN (if not already installed) and load the package
# install.packages("Demokin")
library(DemoKin)

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")
#----------------------------------------------------------------------------------------------------
# Download population data from the WPP 2024 and save as .csv files ---

# Load functions to download data from the UN World Population Prospects 2024
source("Functions/Functions_Download_WPP2024.R")

# Download WPP 2024 population data (if it is not already saved as .csv file)
download_WPP2024_pop()

# Download WPP 2024 fertility and mortality data (if it is not already saved as .csv file)
download_WPP2024()
#----------------------------------------------------------------------------------------------------
## Load WPP 2024 data, filter and reshape in Demokin format ---- 

## Read the WPP fertility data in csv format
WPP_fert <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)

# Specify the full age range (of survival matrices) for completion
age_range <- 0:100

# Filter WPP fertility data for Sweden 
# and reshape as matrix with rows representing ages and columns representing years
SWE_fert <- WPP_fert %>% 
  filter(ISO3_code == "SWE" & Variant == "Medium") %>% 
  mutate(ASFR = ASFR/1000) %>%
  select(age = AgeGrpStart, year = Time, fx = ASFR) %>% 
  # Create all age-year combinations for ages 0–100 and fill missing fx with 0
  tidyr::complete(age = age_range, year, fill = list(fx = 0)) %>% 
  pivot_wider(names_from = year, values_from = fx) %>%
  select(-age) %>%
  as.matrix()
row.names(SWE_fert) <- 0:(nrow(SWE_fert) - 1)


## Read the WPP 2024 historical and projected life tables in csv format
WPP_lifetable_female_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv", show_col_types = FALSE) 
WPP_lifetable_male_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv", show_col_types = FALSE) 
WPP_lifetable_female_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv", show_col_types = FALSE)
WPP_lifetable_male_proj <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv", show_col_types = FALSE) 

# Combine WPP historical and projected female life tables, filter for Sweden 
# and reshape as matrix with rows representing ages and columns representing years
SWE_surv_female <- bind_rows(WPP_lifetable_female_hist, WPP_lifetable_female_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, px) %>% 
  pivot_wider(names_from = year, values_from = px) %>% 
  select(-age) %>%
  as.matrix() 
row.names(SWE_surv_female) <- 0:(nrow(SWE_surv_female) - 1)

# Combine WPP historical and projected male life tables, filter for Sweden
# and reshape as matrix with rows representing ages and columns representing years
SWE_surv_male <- bind_rows(WPP_lifetable_male_hist, WPP_lifetable_male_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, px) %>% 
  pivot_wider(names_from = year, values_from = px) %>% 
  select(-age) %>%
  as.matrix()
row.names(SWE_surv_male) <- 0:(nrow(SWE_surv_male) - 1)


# Read WPP population data in csv format
population_hist <- read_csv("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv", show_col_types = FALSE)
population_proj <- read_csv("Data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv", show_col_types = FALSE)

# Combine WPP historical and projected population, filter for Sweden (females), and reshape as matrix
SWE_pop_female <- bind_rows(population_hist, population_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, pop = PopFemale) %>% 
  filter(year != 2101) %>% 
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()
row.names(SWE_pop_female) <- 0:(nrow(SWE_pop_female) - 1)

# Combine WPP historical and projected population, filter for Sweden (males), and reshape as matrix
SWE_pop_male <- bind_rows(population_hist, population_proj) %>% 
  filter(ISO3_code == "SWE") %>% 
  select(age = AgeGrpStart, year = Time, pop = PopMale) %>% 
  filter(year != 2101) %>% 
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()
row.names(SWE_pop_male) <- 0:(nrow(SWE_pop_male) - 1)

#----------------------------------------------------------------------------------------------------
## Run the two-sex time-invariant kinship model ----

## Check what to do with male fertility. 
## Androgynous approximation or shift the rates?
## Also, focal sex? and female birth ratio?

demokin_invariant <- kin2sex(pf = SWE_surv_female[,"1950"],
                             pm = SWE_surv_male[,"1950"],
                             ff = SWE_fert[,"1950"],
                             fm = SWE_fert[,"1950"],
                             time_invariant = TRUE,
                             output_period = 1950,
                             sex_focal = "f",
                             birth_female = .5)
save(demokin_invariant, file = "Measures/demokin_invariant.RData")

# Plot the results
demokin_invariant$kin_summary %>% 
  # Recode kin types 
  recode_demokin() %>% 
  # Group by kin type, age and sex 
  summarise(mean_number = sum(count_living), 
            .by = c(kin_type, age_focal, sex_kin)) %>%
  rename(Age = age_focal) %>% 
  ggplot(aes(x = Age, y = mean_number, color = kin_type, linetype = sex_kin)) +
  geom_line(linewidth = 1) +
  labs(x = "Age of focal individual", y = "Mean number of living kin", color = "Kin type") +
  scale_color_viridis_d(option = "D", direction =1)+
  theme_graphs()
ggsave("Graphs/Mean_Number_Kin_Invariant.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Invariant.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")
#----------------------------------------------------------------------------------------------------
## Run the two-sex time-varying kinship model ----

demokin_variant <- kin2sex(pf = SWE_surv_female,
                           pm = SWE_surv_male,
                           ff = SWE_fert,
                           fm = SWE_fert,
                           nf = SWE_pop_female,
                           nm = SWE_pop_male,
                           time_invariant = FALSE,
                           output_period = c(2000, 2050, 2100),
                           sex_focal = "f",
                           birth_female = .5)
save(demokin_variant, file = "Measures/demokin_variant.RData")

demokin_variant$kin_summary %>%
  # Recode kin types 
  recode_demokin() %>%
  # Group by kin type, age and sex 
  summarise(mean_number = sum(count_living), 
            # Check if needed to distinguish by sex
            .by = c(year, kin_type, age_focal, sex_kin)) %>%
  rename(Age = age_focal) %>%
  ggplot(aes(x = Age, y = mean_number, color = kin_type, linetype = sex_kin)) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  geom_line(linewidth = 1) +
  labs(x = "Age of focal individual", y = "Mean number of living kin", 
       color = "Kin type", linetype = "Sex") +
  scale_color_viridis_d(option = "D", direction =1)+
  theme_graphs()
ggsave("Graphs/Mean_Number_Kin_Variant.jpg", width = 20, height = 36,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Variant.pdf", width = 20, height = 36,  dpi = 300, device = "pdf")


demokin_variant$kin_summary %>%
  recode_demokin() %>% 
  # Group by kin type, age and sex 
  summarise(mean_number = sum(count_living), 
            .by = c(year, kin_type, age_focal, sex_kin)) %>%
  rename(Age = age_focal) %>%
  ggplot(aes(x = Age, y = mean_number,  fill = sex_kin)) +
  geom_area() +
  facet_wrap(kin_type~ year,  ncol = 6, scales = "free_y") +
  labs(y = "Mean number of living kin by sex and Focal's age",
       x = "Age of Focal",
       fill = "Sex of Kin") +
  theme_graphs()
ggsave("Graphs/Mean_Number_Kin_Variant2.jpg", width = 20, height = 16,  dpi = 300)
ggsave("Graphs/Mean_Number_Kin_Variant2.pdf", width = 20, height = 16,  dpi = 300, device = "pdf")

## Compare results for time-invariant and time-variant models
demokin_variant$kin_summary %>%
  mutate(setting = "Variant") %>%
  bind_rows(demokin_invariant$kin_summary %>% mutate(setting = "Invariant")) %>% 
  recode_demokin() %>% 
  summarise(mean_number=sum(count_living), 
            .by = c(setting, kin_type, age_focal, sex_kin)) %>%
  # Create plot
  ggplot(aes(age_focal, mean_number, linetype = type)) +
  geom_line() + 
  facet_grid(cols = vars(kin_type), rows = vars(sex_kin), scales = "free") +
  theme_graphs()
  labs(title = "Time-variant vs. time-invariant kinship models by sex",
       x = "Age of focal individual",
       y = "Number of living relatives",
       linetype = "Model type")
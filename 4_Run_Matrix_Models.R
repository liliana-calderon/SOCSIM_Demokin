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

SWE_1950_invariant <- kin2sex(pf = SWE_surv_female[,"1950"],
                              pm = SWE_surv_male[,"1950"],
                              ff = SWE_fert[,"1950"],
                              fm = SWE_fert[,"1950"],
                              time_invariant = TRUE,
                              output_period = 1950,
                              sex_focal = "f",
                              birth_female = .5)

# Plot the results
SWE_1950_invariant$kin_summary %>% 
  # Combine older and younger aunts/uncles, cousins, siblings and niblings
  mutate(kin = case_when(kin %in% c("ya", "oa") ~ "a",
                         kin %in% c("cya", "coa") ~ "c",
                         kin %in% c("ys", "os") ~ "s",
                         kin %in% c("nys", "nos") ~ "n",
                         TRUE ~ kin)) %>% 
  # Group by kin type, age and sex 
  rename_kin(sex = "2sex") %>% 
  summarise(count=sum(count_living), 
            .by = c(kin_label, age_focal, sex_kin)) %>%
  ggplot() +
  geom_line(aes(age_focal, count))  +
  # facet_wrap(~kin_label, scales = "free_y")
  facet_grid(cols = vars(kin_label), 
             rows = vars(sex_kin), scales = "free") +
  theme_graphs() +
  labs(x = "Age of focal", y= "Number of living kin")


#----------------------------------------------------------------------------------------------------
## Run the two-sex time-varying kinship model ----

SWE_1950_variant <- kin2sex(pf = SWE_surv_female,
                            pm = SWE_surv_male,
                            ff = SWE_fert,
                            fm = SWE_fert,
                            nf = SWE_pop_female,
                            nm = SWE_pop_male,
                            time_invariant = FALSE,
                            output_period = 1950,
                            sex_focal = "f",
                            birth_female = .5)

SWE_1950_variant$kin_summary %>%
  # Combine older and younger aunts/uncles, cousins, siblings and niblings
  mutate(kin = case_when(kin %in% c("ya", "oa") ~ "a",
                         kin %in% c("cya", "coa") ~ "c",
                         kin %in% c("ys", "os") ~ "s",
                         kin %in% c("nys", "nos") ~ "n",
                         TRUE ~ kin)) %>% 
  # Group by kin type, age and sex 
  rename_kin(sex = "2sex") %>% 
  summarise(count=sum(count_living), 
            .by = c(kin_label, age_focal, sex_kin)) %>%
  # Create plot
  ggplot(aes(age_focal, count)) +
  geom_line() + 
  theme_bw() +
  facet_grid(cols = vars(kin_label), 
             rows = vars(sex_kin), scales = "free") +
  labs(x = "Age of focal individual",
       y = "Number of living relatives")


SWE_1950_variant$kin_summary %>%
  # Combine older and younger aunts/uncles, cousins, siblings and niblings
  mutate(kin = case_when(kin %in% c("ya", "oa") ~ "a",
                         kin %in% c("cya", "coa") ~ "c",
                         kin %in% c("ys", "os") ~ "s",
                         kin %in% c("nys", "nos") ~ "n",
                         TRUE ~ kin)) %>% 
  # Group by kin type, age and sex 
  rename_kin(sex = "2sex") %>% 
  summarise(count=sum(count_living), 
            .by = c(kin_label, age_focal, sex_kin)) %>%
  ggplot(aes(age_focal, count, fill=sex_kin)) +
  geom_area() +
  theme_bw() +
  labs(y = "Expected number of living kin by sex and Focal's age",
       x = "Age of Focal",
       fill = "Sex of Kin") +
  facet_wrap(~kin_label, scales = "free_y")

## Compare results for time-invariant and time-variant models
SWE_1950_variant$kin_summary %>%
  mutate(type = "variant") %>%
  bind_rows(SWE_1950_invariant$kin_summary %>% mutate(type = "invariant")) %>% 
  # Combine siblings and aunts/uncles for simplicity
  mutate(kin = case_when(kin %in% c("ys", "os") ~ "s",
                         kin %in% c("ya", "oa") ~ "a",
                         TRUE ~ kin)) %>%
  # Select key relationships
  filter(kin %in% c("d", "m", "gm", "ggm", "s", "a")) %>%
  # Group by relationship type, age, sex, and model type
  rename_kin(sex = "2sex") %>% 
  summarise(count=sum(count_living), 
            .by = c(type, kin_label, age_focal, sex_kin)) %>%
  # Create plot
  ggplot(aes(age_focal, count, linetype = type)) +
  geom_line() + 
  theme_bw() +
  facet_grid(cols = vars(kin_label), rows = vars(sex_kin), scales = "free") +
  labs(title = "Time-variant vs. time-invariant kinship models by sex",
       x = "Age of focal individual",
       y = "Number of living relatives",
       linetype = "Model type")
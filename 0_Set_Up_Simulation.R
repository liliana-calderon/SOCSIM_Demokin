#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM vs Demokin - Set up Simulation using WPP data
# U:/SOCSIM/SOCSIM_Demokin/0_Set_Up_Simulation.R

## Write the input fertility and mortality rates for a SOCSIM micro-simulation 1950-2100
## using data from the World Population Prospects 2024. 
## Create a initial population file and empty marriage file for the simulations
## Run the SOCSIM simulation with the 'rsocsim' package and read the output .opop and .omar files

# Created on 23-10-2024
# Last modified on 30-04-2026
#------------------------------------------------------------------------------------------------------
# SOCSIM simulation: input rates and assumptions ----

# The SOCSIM simulation is implemented using the 'rsocsim' package,
# Available at https://github.com/MPIDR/rsocsim 

# It uses as input historical (1950-2023) and projected (2024-2100) 
# age-specific fertility rates and age-specific probabilities of death
# from the UN World Population Prospects 2024 Revision for 2024-2100. 

# To run the simulation, original WPP rates are converted to monthly rates/probabilities and SOCSIM format

# Female fertility rates are identical for all marital status, but are specified for single and married women
# Other marital status (divorced, widowed, cohabiting) follow the SOCSIM rate default rules. 
# Age-specific female fertility rates are for all birth orders combined over the whole period

# Mortality rates correspond to probabilities of death (qx) of the WPP period life tables. 
# They are identical for all marital status of each sex, and are only specified for single women and men. 
# Other marital status will follow the rate default rules. 

# The first segment of the simulation runs for 100 years to produce a stable age structure, 
# based on 1950 age-specific rates

#----------------------------------------------------------------------------------------------------
# Rate files format (Cf. Socsim oversimplified, p. 26):

# A rate block is a complete set of age speciﬁc rates governing a demographic event 
# for people of a particular sex, group and marital status. 
# In the rate files, the order matters and is always: 
# Event (birth, death, marriage), group (1, ..), sex (F or M), and
# marital status (married, single, divorced, widowed)
# For birth rates, this can be followed by number indicating parity.
# Each subsequent line contains a one month rate or probability
# and the age interval over which it holds (years and months of the upper age bound)
# The interval includes upper age bound in previous line and ends just before upper age bound in current line.
# The first two numbers are years and months of the upper age bound, which are added together.
# e.g. upper age bound of "0 1" refers to the first month of life, thus, [0,1)

#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

# Load necessary packages
library(tidyverse)
library(readr)

# Load functions to write SOCSIM rate files using WPP data ----
source("Functions/Functions_Input_Rates.R")
#----------------------------------------------------------------------------------------------------
# Download fertility and mortality data from the WPP 2024 and save as .csv files ---

# To read gz files directly, fread() requires `R.utils` package
library(R.utils)
# Load function to download data from the UN World Population Prospects 2024
source("Functions/Functions_Download_WPP2024.R")

# Download WPP 2024 data if it is not already downloaded
download_WPP2024()
# Time difference of 7.099603 mins
#----------------------------------------------------------------------------------------------------
## Write fertility rate files for SOCSIM using data from WPP 2024 ----

## NOTES:
# We keep the whole range of the fertility rates by single age of the mother provided by the WPP 2024 [15-49]. 
# Younger [10-14] and older [50-54] age groups are only provided in the estimates by 5-year age groups. 
# Hence, the ages below [0-14] and above [50-100] are set to 0 
# Additionally, SOCSIM requires a final age category with an upper bound corresponding to the maximum length of life. 
# Here, we fixed it to [50-100]

# To convert the annual rates to monthly rates
# we assume that the fertility rates are constant over the year and divide them by 12 (months)
# According to SOCSIM oversimplified, these are rates rather than probabilities. 
# So multiplying a rate by the number of months in the age category gives
# the expected number of births that a woman who lives through the age category will experience.

# The rates are identical for married, single, divorced and widowed females. 
# We only specify the rates for single and married, 
# as other marital status will follow the rate default rules. 

# To write the input fertility rates from WPP 2024, obtained with "download_WPP2024()", 
# type the ISO3_code of the country as listed in the WPP 2024, 
# with the minimum and maximum years of the period to be included in the simulation.
write_socsim_fertility_rates_WPP(Country = "SWE", 
                                 Year_Min = 1950,
                                 Year_Max = 2100) 
#----------------------------------------------------------------------------------------------------
## Write mortality rate files for SOCSIM using data from WPP 2024 ----

## NOTES:

# As input mortality data, SOCSIM requires probabilities of death (qx) from period life tables
# We use the whole age range of the single age life tables provided by the WPP 2024 [0-100]. 
# The final age interval 100 is limited to one year, i.e. [100-101). 

# To convert the annual WPP probabilities to monthly 'SOCSIM' probabilities, 
# we assume that the probability of dying is constant over the year 
# and use the formula $1-(1-_nq_x)^{1/n}$ proposed by Kenneth Watcher. 
# For the final age interval 100, monthly probabilities are equal to 1.

# The probabilities of death are identical for all marital status of each sex, 
# and are only specified for single women and single men in the rate files. 
# Other marital status will follow SOCSIM's rate default rules. 

# The WPP2024 methods used to estimate mortality and life tables vary across countries 
# depending on the type and quality of available empirical data. 
# For further details, see the Methodology of the United Nations population estimates and projections

# The function below filters the WPP 2024 mortality data for the selected country and period
# and convert to SOCSIM format following the procedure explained above. 

# To write the input mortality rates from WPP 2024, obtained with "download_WPP2024()", 
# type the ISO3_code of the country as listed in the WPP 2024, 
# with the minimum and maximum years of the period to be included in the simulation.

# Write input rates using the historical data file
write_socsim_mortality_rates_WPP(Country = "SWE",
                                 Year_Min = 1950,
                                 Year_Max = 2023)

# Write input rates using the projected data file
write_socsim_mortality_rates_WPP(Country = "SWE",
                                 Year_Min = 2024,
                                 Year_Max = 2100)
#------------------------------------------------------------------------------------------------------
# Create initial .opop and empty .omar file for the simulation ----

# Set size of initial population
size_opop <-  200000

# Create data.frame with 14 columns and nrows = size_opop
presim.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim.opop$pid <- 1:size_opop

# Add sex randomly
presim.opop$fem <- sample(0:1, nrow(presim.opop), replace = T)

# Add group 1 for all individuals
presim.opop$group <- 1

# Add random dates of birth # Reduce to 200 to have older population by 1850
presim.opop$dob <- sample(200:1000, nrow(presim.opop), replace = T)

## Check distribution of sex 
table(presim.opop$fem)

# Write initial population for pre-simulation (without fertility multiplier)
write.table(presim.opop, "presim.opop", row.names = F, col.names = F)


## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write empty omar for pre-simulation
write.table(presim.omar, "presim.omar", row.names = F, col.names = F)

#------------------------------------------------------------------------------------------------------
## Install 'rsocsim' package ----
# The instructions to install the rsocsim package and run the simulation 
# can be found on https://github.com/MPIDR/rsocsim/blob/main/readme.md

# To get the updates, uninstall and install again the package
# remove.packages("rsocsim")

# Option A: Install rsocsim from source
# To get the latest version from GitHun, RTools and remotes must be installed.
# install_github was deprecated in devtools 2.5.0
# install.packages("remotes")
# remotes::install_github("MPIDR/rsocsim@v1.5.9")

# Install rsocsim from a binary package (1.5.9) downloaded from the GitHub releases page: 
# install.packages(file.choose(),repos=NULL,type="binary") 

#------------------------------------------------------------------------------------------------------
## Run SOCSIM simulations with 'rsocsim' package ----
# Load rsocsim package
library("rsocsim")

# Specify the working directory, where the supfile and ratefiles are.
# If the R session is running through the project file, the following command can be used.
folder <- getwd()
# Otherwise, the working directory must be specified after "folder <- "

# Name of the supervisory file stored in the above folder:
# Supervisory file for rates retrieved from WPP 2024, with marry after childbirth and hetfert 0
supfile <- "Sweden_WPP.sup" 

# Random number generator seed:
seed <- as.character(sample(1:99999, 1, replace = F))

# Save the seed number to use them later to read the data
save(seed, file = "seed.Rda")
load("seed.Rda")

## Run the simulation for the random seed. 
start <- Sys.time()

### Run a single SOCSIM-simulation with a given folder and supervisory file
rsocsim::socsim(folder, supfile, seed, process_method = "future")
end <- Sys.time()

print(end-start)

# Time difference of 5.144692 hours for 200000 individuals in the initial population

#----------------------------------------------------------------------------------------------------
## Read the output .opop and .omar files ----

# Load seeds numbers and convert them into numeric
load("seed.Rda")
seed <- as.numeric(seed)

# Read opop
opop <- rsocsim::read_opop(folder = getwd(),
                           supfile = "Sweden_WPP.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save opop to use later
save(opop, file = "opop.RData")

# Read omar
omar <- rsocsim::read_omar(folder = getwd(),
                           supfile = "Sweden_WPP.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save omar to use later
save(omar, file = "omar.RData")
#----------------------------------------------------------------------------------------------------
# Functions to Write SOCSIM fertility and mortality input rate files using WPP2024 data

# Initial version created on 22-10-2024
# Last modified on 18-03-2026
#----------------------------------------------------------------------------------------------------
# Create a sub-folder called "rates" to save the rate files if it does not exist.
ifelse(!dir.exists("rates"), dir.create("rates"), FALSE)
# If the sub-folder name changes, 
# it must be also changed in the functions when opening the output file connection
#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input fertility rates from WPP 2024 data ----

write_socsim_fertility_rates_WPP <- function(Country, Year_Min, Year_Max) {
  
  # Load the WPP data in csv format
  fertility <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)
  
  # Wrangle data and compute monthly fertility rates
  ASFR <- fertility %>%
    filter(ISO3_code == Country & Time >= Year_Min & Time <= Year_Max & Variant == "Medium") %>%
    select(Year = Time, Age = AgeGrpStart, ASFR)  %>%
    mutate(ASFR = ASFR/1000,
           Age_up = Age + 1, # SOCSIM uses the upper Age bound
           Month = 0,
           ASFR_mo = ASFR/12) %>%
    select(-ASFR)
  
  # Tibble with expected ages
  Tibble_ages <- tibble(Age = c(0, 12, 13, 14, seq(15,55), 56))
  
  # Add rows with rates = 0 for ages [0-14] and [51-101]
  ASFR <- ASFR %>%
    # create every Year × Age combination
    complete(Year, Age = Tibble_ages$Age) %>%
    # Fill blanks and compute Age_up and Month consistently
    mutate(ASFR_mo = coalesce(ASFR_mo, 0),
           Age_up  = case_when(Age == 0 ~ 12L,
                               Age == 56 ~ 101L,
                               TRUE ~ Age + 1L),
           Month   = 0) %>%
    arrange(Year, Age_up) %>%
    select(Year, Age_up, Month, ASFR_mo)
  
  # Extract the years in the dataset
  years <- ASFR %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASFR
  rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the fertility rate files for each year
  
  for(Year in years) {
    
    # Find the index of each year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*46 + rows_ageF
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"fert",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include Country and year of the corresponding rates
    cat(c("** Period (Monthly) Age-Specific Fertility Rates for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz", "\n"),
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"),
        file = outfilename)
    cat(c("** NB: The original WPP annual rates have been converted into monthly rates", "\n"),
        file = outfilename)
    cat(c("** The open age intervals (14- and 49+) are limited to one year [14-15) and [49-50). Ages below and above are set to 0", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print birth rates (single females)
    cat("birth", "1", "F", "single", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print birth rates (married females)
    cat("birth", "1", "F", "married", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1],"\n"), file = outfilename) }
    
    close(outfilename)
    
  }
  
}

#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input mortality rates from WPP 2024 data ----

write_socsim_mortality_rates_WPP <- function(Country, Year_Min, Year_Max) {
  
  # Load the WPP data in csv format
  lifetable_female <- read_csv(paste0("Data/WPP2024_Life_Table_Complete_Medium_Female_",Year_Min,"-",Year_Max,".csv"), show_col_types = FALSE)
  lifetable_male <- read_csv(paste0("Data/WPP2024_Life_Table_Complete_Medium_Male_",Year_Min,"-",Year_Max,".csv"), show_col_types = FALSE)
  
  # Wrangle data and compute monthly mortality probabilities
  ASMP <- bind_rows(lifetable_female, lifetable_male) %>%
    filter(ISO3_code == Country) %>%
    select(Year = Time, Sex, Age = AgeGrpStart, qx) %>%
    mutate(qx_mo = ifelse(Age == 100, 1, 1-(1-qx)^(1/12)),
           Age_up = Age + 1, # SOCSIM uses the upper age bound
           Month = 0) %>%
    select(c(Year, Age_up, Month, Sex, qx_mo)) %>%
    pivot_wider(names_from = Sex, values_from = qx_mo)
  
  # Extract the years available in WPP
  years <- ASMP %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASMP
  rows_ageM <- ASMP %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the mortality rate files for each year
  
  for(Year in years) {
    
    # Find the index of each Year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*101 + rows_ageM
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"mort",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include Country and Year of the corresponding probabilities
    cat(c("** Period (Monthly) Age-Specific Probabilities of Death for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024. Single age life tables up to age 100 Medium variant", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz", "and", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz", "and", "\n"),
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"),
        file = outfilename)
    cat(c("** NB: The original WPP annual probabilities have been converted into monthly probabilities", "\n"),
        file = outfilename)
    cat(c("** The final age interval is limited to one year [100-101)", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single females)
    cat("death", "1", "F", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,5)], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single males)
    cat("death", "1", "M", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,4)], "\n"), file = outfilename) }
    
    close(outfilename)
    
  }
}
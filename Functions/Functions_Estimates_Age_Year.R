#----------------------------------------------------------------------------------------------------
# Function to estimate age x year mean and distributions of kin from a SOCSIM microsimulation 
# to be processed by groups of years to handle the large number of rows resulting from a data 
# with several ego-age-year rows for many kin types

# It estimates kin alive by age at 1st January of the following year, 
# based on fertility and mortality of a given year

# Created on 27-03-2026
# Last modified on 29-04-2026
#----------------------------------------------------------------------------------------------------
library(dtplyr)

estimates_age_year <- function(year_start, year_end, ego_ages_year, kin) {
  
  cat("Processing years:", year_start, "-", year_end, "\n")
  
  # Filter ego_age_year to the selected years
  ego_ages_year_sel <- ego_ages_year %>%
    filter(year >= year_start & year <= year_end)
  
  # Get egos for the selected years
  pids_year <- ego_ages_year_sel %>% pull(pid_ego) %>% unique()
  
  # Get kin for the selected years
  kin_year <- kin %>%
    filter(pid_ego %in% pids_year) %>%
    # 9999 as dod for ego alive at the end of the simulation
    mutate(dod_ego2 = if_else(dod_ego == 0, 9999, dod_ego))
  
  # Get all kin types sorted for completion
  kin_types_all <- sort(unique(kin$kin_type))
  
  # Join ego_ages_year with kin_year and calculate kin age and alive at 1st January
  ego_ages_year_kin <- ego_ages_year_sel %>% 
    left_join(kin_year, by = c("pid_ego", "fem_ego", "dob_ego", "dod_ego2"),
              relationship = "many-to-many") %>%
    filter(!is.na(kin_type)) %>%  # Remove NA rows from join
    # Calculate months, age and living status at 1st January next year
    mutate(# Set 9999 as dod for kin alive at the end of the simulation
      dod_kin2 = if_else(dod_kin == 0, 9999, dod_kin),
      year_next = year + 1,
      # Month of 1st January next year
      month_jan_next = jan(year + 1, last_month, final_sim_year),
      # Ego age at 1st January next year
      age_jan_next = floor((month_jan_next - dob_ego) / 12),
      # kin age at 1st January next year (Calculate even for dead. Afterwards, we will weight by kin_alive_next 
      kin_age_next = floor((month_jan_next - dob_kin) / 12),
      # Kin alive at 1st January next year
      kin_alive_next = if_else(month_jan_next >= dob_kin & month_jan_next < dod_kin2, 1L, 0L))
  
  # For each ego_age_year, sum kin alive in January by year_age_kin type
  # Egos with no kin record of a given type are handled in the completion below.
  ego_level_from_kin <- ego_ages_year_kin %>%
    lazy_dt() %>%
    group_by(year_next, age_jan_next, kin_type, pid_ego) %>%
    summarise(n_kin_alive = sum(kin_alive_next, na.rm = TRUE),
              sum_kin_age_alive = sum(kin_age_next * kin_alive_next, na.rm = TRUE),
              .groups = "drop") %>% 
    as.data.frame()
  
  
  # Create complete ego_level df with one row per ego-year-age-kin type
  # including 0 values for egos with no kin of a given type, to be used as denominator for distributions
  ego_level <- ego_ages_year_sel %>%
    distinct(pid_ego, year_next, age_jan_next) %>% 
    # Complete with all possible combinations of year_age_kin, to check individuals with 0 kin
    crossing(kin_type = kin_types_all) %>%
    lazy_dt() %>%
    # Add number of kin alive and sum of ages of kin alive by ego-year-age-kin type
    left_join(ego_level_from_kin,
              by = c("pid_ego", "year_next", "age_jan_next", "kin_type")) %>%
    # Egos with no kin of a given kin_type must be completes with n_kin_alive = 0.
    mutate(n_kin_alive = replace_na(n_kin_alive, 0L),
           sum_kin_age_alive = replace_na(sum_kin_age_alive, 0)) %>% 
    as.data.frame()
  
  
  # Mean numbers and ages of kin by egos' age at each 1st January at age +1
  mean_number_age <- ego_level %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    summarise(sum_kin_alive = sum(n_kin_alive, na.rm = TRUE),
              sum_kin_age_alive = sum(sum_kin_age_alive, na.rm = TRUE),
              .groups = "drop" ) %>%
    # Complete with all possible combinations to check individuals with 0 kin
    complete(year_next = (year_start + 1):(year_end + 1),
             age_jan_next = 0:100,
             kin_type = kin_types_all,
             fill = list(sum_kin_alive = 0, sum_kin_age_alive = 0)) %>%
    # Add number of egos alive on 1st January by age as denominator for means
    left_join(ego_ages_year_sel %>% # already filtered to egos alive at 1st Jan
                group_by(year_next, age_jan_next) %>%
                summarise(n_egos = n_distinct(pid_ego), .groups = "drop"),
              by = c("year_next", "age_jan_next")) %>% 
    mutate(mean_number = if_else(n_egos > 0, sum_kin_alive / n_egos, NA_real_),
           # mean age among living kin (not among egos)
           mean_kin_age = if_else(sum_kin_alive > 0, sum_kin_age_alive / sum_kin_alive, NA_real_) ) %>%
    select(year_next, age_jan_next, kin_type, mean_number, mean_kin_age)
  
  
  # Distribution of numbers of kin per ego, by year and age
  dist_number <- ego_level %>%
    # For each group, calculate maximimum observed n_kin_alive value to complete with 0 values up to that maximum
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(max_kin = if (all(is.na(n_kin_alive))) 0 else max(n_kin_alive, na.rm = TRUE)) %>%
    group_by(year_next, age_jan_next, kin_type, n_kin_alive) %>%
    summarise(n_egos_n_kin = n(), max_kin = first(max_kin), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    complete(n_kin_alive = 0:max(max_kin, na.rm = TRUE), fill = list(n_egos_n_kin = 0)) %>%
    # Add total number of egos alive on 1st January by age as denominator
    left_join(ego_level %>% 
                group_by(year_next, age_jan_next) %>%
                summarise(n_egos = n_distinct(pid_ego), .groups = "drop"),
              by = c("year_next", "age_jan_next")) %>% 
    mutate(dist_number = if_else(n_egos > 0, n_egos_n_kin / n_egos, NA_real_)) %>%
    ungroup() %>%
    select(-max_kin)
  

  # Distribution of ages of living kin, by year and age
  dist_age <- ego_ages_year_kin %>%
    filter(kin_alive_next == 1L) %>%
    mutate(kin_age_next = pmax(0L, kin_age_next)) %>% 
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(max_age = if (all(is.na(kin_age_next))) 0L else max(kin_age_next, na.rm = TRUE)) %>%
    group_by(year_next, age_jan_next, kin_type, kin_age_next) %>%
    summarise(n_kin = n(), max_age = first(max_age), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    complete(kin_age_next = 0:max(max_age, na.rm = TRUE),
             fill = list(n_kin = 0)) %>%
    mutate(dist_age = n_kin / sum(n_kin, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-max_age)
  
  return(list(mean_number_age = mean_number_age,
              dist_number = dist_number,
              dist_age = dist_age))
}

#----------------------------------------------------------------------------------------------------
estimates_sex_age_year <- function(year_start, year_end, ego_ages_year, kin) {
  
  cat("Processing years:", year_start, "-", year_end, "\n")
  
  # Filter ego_age_year to the selected years
  ego_ages_year_sel <- ego_ages_year %>%
    filter(year >= year_start & year <= year_end)
  
  # Get egos for the selected years
  pids_year <- ego_ages_year_sel %>% pull(pid_ego) %>% unique()
  
  # Get kin for the selected years
  kin_year <- kin %>%
    filter(pid_ego %in% pids_year) %>%
    # 9999 as dod for ego alive at the end of the simulation
    mutate(dod_ego2 = if_else(dod_ego == 0, 9999, dod_ego))
  
  # Get all kin types sorted for completion
  kin_types_all <- sort(unique(kin$kin_type))
  
  # Join ego_ages_year with kin_year and calculate kin age and alive at 1st January
  ego_ages_year_kin <- ego_ages_year_sel %>% 
    left_join(kin_year, by = c("pid_ego", "fem_ego", "dob_ego", "dod_ego2"),
              relationship = "many-to-many") %>%
    filter(!is.na(kin_type)) %>%  # Remove NA rows from join
    # Calculate months, age and living status at 1st January next year
    mutate(# Set 9999 as dod for kin alive at the end of the simulation
      dod_kin2 = if_else(dod_kin == 0, 9999, dod_kin),
      year_next = year + 1,
      # Month of 1st January next year
      month_jan_next = jan(year + 1, last_month, final_sim_year),
      # Ego age at 1st January next year
      age_jan_next = floor((month_jan_next - dob_ego) / 12),
      # kin age at 1st January next year (Calculate even for dead. Afterwards, we will weight by kin_alive_next 
      kin_age_next = floor((month_jan_next - dob_kin) / 12),
      # Kin alive at 1st January next year
      kin_alive_next = if_else(month_jan_next >= dob_kin & month_jan_next < dod_kin2, 1L, 0L))
  
  # For each ego_age_year, sum kin alive in January by year_age_kin type
  # Egos with no kin record of a given type are handled in the completion below.
  ego_level_from_kin <- ego_ages_year_kin %>%
    lazy_dt() %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin, pid_ego) %>%
    summarise(n_kin_alive = sum(kin_alive_next, na.rm = TRUE),
              sum_kin_age_alive = sum(kin_age_next * kin_alive_next, na.rm = TRUE),
              .groups = "drop") %>% 
    as.data.frame()
  
  
  # Create complete ego_level df with one row per ego-year-age-kin type
  # including 0 values for egos with no kin of a given type, to be used as denominator for distributions
  ego_level <- ego_ages_year_sel %>%
    distinct(pid_ego, year_next, age_jan_next) %>% 
    # Complete with all possible combinations of year_age_kin, to check individuals with 0 kin
    crossing(kin_type = kin_types_all, 
             fem_kin = 0:1) %>%
    lazy_dt() %>%
    # Add number of kin alive and sum of ages of kin alive by ego-year-age-kin type
    left_join(ego_level_from_kin,
              by = c("pid_ego", "year_next", "age_jan_next", "kin_type", "fem_kin")) %>%
    # Egos with no kin of a given kin_type must be completes with n_kin_alive = 0.
    mutate(n_kin_alive = replace_na(n_kin_alive, 0L),
           sum_kin_age_alive = replace_na(sum_kin_age_alive, 0)) %>% 
    as.data.frame()
  
  
  # Mean numbers and ages of kin by egos' age at each 1st January at age +1
  mean_number_age <- ego_level %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
    summarise(sum_kin_alive = sum(n_kin_alive, na.rm = TRUE),
              sum_kin_age_alive = sum(sum_kin_age_alive, na.rm = TRUE),
              .groups = "drop" ) %>%
    # Complete with all possible combinations to check individuals with 0 kin
    complete(year_next = (year_start + 1):(year_end + 1),
             age_jan_next = 0:100,
             kin_type = kin_types_all,
             fem_kin = 0:1, 
             fill = list(sum_kin_alive = 0, sum_kin_age_alive = 0)) %>%
    # Add number of egos alive on 1st January by age as denominator for means
    left_join(ego_ages_year_sel %>% # already filtered to egos alive at 1st Jan
                group_by(year_next, age_jan_next) %>%
                summarise(n_egos = n_distinct(pid_ego), .groups = "drop"),
              by = c("year_next", "age_jan_next")) %>% 
    mutate(mean_number = if_else(n_egos > 0, sum_kin_alive / n_egos, NA_real_),
           # mean age among living kin (not among egos)
           mean_kin_age = if_else(sum_kin_alive > 0, sum_kin_age_alive / sum_kin_alive, NA_real_) ) %>%
    select(year_next, age_jan_next, kin_type, fem_kin, mean_number, mean_kin_age)
  
  
  # Distribution of numbers of kin per ego, by year and age
  dist_number <- ego_level %>%
    # For each group, calculate maximimum observed n_kin_alive value to complete with 0 values up to that maximum
    group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
    mutate(max_kin = if (all(is.na(n_kin_alive))) 0 else max(n_kin_alive, na.rm = TRUE)) %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin, n_kin_alive) %>%
    summarise(n_egos_n_kin = n(), max_kin = first(max_kin), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
    complete(n_kin_alive = 0:max(max_kin, na.rm = TRUE), fill = list(n_egos_n_kin = 0)) %>%
    # Add total number of egos alive on 1st January by age as denominator
    left_join(ego_level %>% 
                group_by(year_next, age_jan_next) %>%
                summarise(n_egos = n_distinct(pid_ego), .groups = "drop"),
              by = c("year_next", "age_jan_next")) %>% 
    mutate(dist_number = if_else(n_egos > 0, n_egos_n_kin / n_egos, NA_real_)) %>%
    ungroup() %>%
    select(-max_kin)
  
  
  # Distribution of ages of living kin, by year and age
  dist_age <- ego_ages_year_kin %>%
    filter(kin_alive_next == 1L) %>%
    mutate(kin_age_next = pmax(0L, kin_age_next)) %>% 
    group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
    mutate(max_age = if (all(is.na(kin_age_next))) 0L else max(kin_age_next, na.rm = TRUE)) %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin, kin_age_next) %>%
    summarise(n_kin = n(), max_age = first(max_age), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type, fem_kin) %>%
    complete(kin_age_next = 0:max(max_age, na.rm = TRUE),
             fill = list(n_kin = 0)) %>%
    mutate(dist_age = n_kin / sum(n_kin, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-max_age)
  
  return(list(mean_number_age = mean_number_age,
              dist_number = dist_number,
              dist_age = dist_age))
}
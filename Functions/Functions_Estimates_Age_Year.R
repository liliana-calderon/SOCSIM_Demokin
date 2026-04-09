#----------------------------------------------------------------------------------------------------
# Function to estimate age x year mean and distributions of kin from a SOCSIM microsimulation 
# to be processed by groups of years to handle the large number of rows resulting from a data 
# with several ego-age-year rows for many kin types

# It estimates kin alive by age at 1st January of the following year, 
# based on fertility and mortality of a given year

# Created on 27-03-2026
# Last modified on 08-04-2026
#----------------------------------------------------------------------------------------------------
estimates_age_year <- function(year_start, year_end, ego_ages_year) {
  
  cat("Processing years:", year_start, "-", year_end, "\n")
  
  # Filter ego_age_year to the selected years
  ego_ages_year_sel <- ego_ages_year %>%
    filter(year >= year_start & year <= year_end)
  
  # Get egos and kin for the selected years
  pids_year <- ego_ages_year_sel %>% pull(pid_ego) %>% unique()
  kin_year <- kin %>%
    filter(pid_ego %in% pids_year) %>%
    # 9999 as dod for ego alive at the end of the simulation
    mutate(dod_ego2 = if_else(dod_ego == 0, 9999, dod_ego))
  
  # Join ego_ages_year with kin_year and calculate kin age and alive at 1st January
  ego_ages_year_kin <- ego_ages_year_sel %>% 
    left_join(kin_year, by = c("pid_ego", "fem_ego", "dob_ego"),
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
  
  # For each ego, sum kin counts per (year_next, age_jan_next, kin_type)
  # Only egos that appear in kin_year contribute here; egos with no kin records
  # for a given type are handled in the completion step below.
  ego_level_from_kin <- ego_ages_year_kin %>%
    group_by(year_next, age_jan_next, kin_type, pid_ego) %>%
    summarise(n_kin_alive = sum(kin_alive_next, na.rm = TRUE),
              sum_kin_age_alive = sum(kin_age_next * kin_alive_next, na.rm = TRUE),
              .groups = "drop")

  # All egos alive at each (year_next, age_jan_next) — the correct universe for the
  # distribution denominator.  Egos who have no kin records for a given kin_type
  # truly have 0 living kin of that type and must be counted as n_kin_alive = 0.
  ego_alive_per_age <- ego_ages_year_sel %>%
    distinct(pid_ego, year_next, age_jan_next)

  # Complete ego_level: every alive ego × every kin type.
  # Missing combinations (no kin records) receive n_kin_alive = 0.
  ego_level <- ego_alive_per_age %>%
    crossing(kin_type = kin_types_all) %>%
    left_join(ego_level_from_kin,
              by = c("pid_ego", "year_next", "age_jan_next", "kin_type")) %>%
    mutate(n_kin_alive       = replace_na(n_kin_alive, 0L),
           sum_kin_age_alive = replace_na(sum_kin_age_alive, 0))
  
  # Mean numbers and ages of kin by egos' age at each 1st January at age +1
  mean_number_age <- ego_level %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    summarise(sum_kin_alive = sum(n_kin_alive, na.rm = TRUE),
              sum_kin_age_alive = sum(sum_kin_age_alive, na.rm = TRUE),
              .groups = "drop" ) %>%
    # Complete with all possible combinations, to check individuals with 0 kin
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
    group_by(year_next, age_jan_next, kin_type, n_kin_alive) %>%
    summarise(n_egos = n(), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(dist_number = n_egos / sum(n_egos)) %>%
    ungroup()
  
  # Distribution of ages of living kin, by year and age
  dist_age <- ego_ages_year_kin %>%
    filter(kin_alive_next == 1L) %>%
    mutate(kin_age_next = pmax(0L, kin_age_next)) %>% 
    group_by(year_next, age_jan_next, kin_type, kin_age_next) %>%
    summarise(n_kin = n(), .groups = "drop") %>%
    group_by(year_next, age_jan_next, kin_type) %>%
    mutate(dist_age = n_kin / sum(n_kin)) %>%
    ungroup()
  
  return(list(mean_number_age = mean_number_age,
              dist_number = dist_number,
              dist_age = dist_age))
}
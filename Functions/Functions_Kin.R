#----------------------------------------------------------------------------------------------------
# Function to retrieve members of a kinship network of a given ego(s) from SOCSIM microsimulation outputs
# and recode the kin names

# To run the functions, .opop file must be set in the GlobalEnv

# Created on 24-03-2026
# Last modified on 25-03-2026
#----------------------------------------------------------------------------------------------------
# Function to retrieve members of a kinship network ----
# The retrieve_kin_mod() is a simplified version of the function created by Mallika Snyder, 
# included in the rsocsim package. 
# The name of the vectors for grandparents and great-grand parents is slightly different, 
# to keep consistency with the name in other papers 
# (e.g., mf the mother of the father, fm the father of the mother).

retrieve_kin_mod <- function (opop = opop, pid = pid, KidsOf = KidsOf) {
  
  # Helper function to negate
  "%ni%" <- Negate("%in%")
  
  # NA values
  zna <- function(x) { return(ifelse(x == 0, NA, x)) }
  
  opop <- opop %>% 
    select(pid, mom, pop) %>% 
    # Change 0s (nothing) to NA for the pids of the initial population
    mutate(mom = ifelse(mom == 0, NA, mom), 
           pop = ifelse(pop == 0, NA, pop))
  
  ### Grandmothers
  opop$mm <- opop$mom[match(opop$mom, opop$pid)]
  opop$mf <- opop$mom[match(opop$pop, opop$pid)]
  ### Grandfathers
  opop$fm <- opop$pop[match(opop$mom, opop$pid)]  
  opop$ff <- opop$pop[match(opop$pop, opop$pid)]
  
  ### Great-grandmothers
  opop$mmm <- opop$mm[match(opop$mom, opop$pid)]
  opop$mfm <- opop$mf[match(opop$mom, opop$pid)]
  opop$mmf <- opop$mm[match(opop$pop, opop$pid)]
  opop$mff <- opop$mf[match(opop$pop, opop$pid)]
  ### Great-grandfathers
  opop$fmm <- opop$fm[match(opop$mom, opop$pid)]
  opop$ffm <- opop$ff[match(opop$mom, opop$pid)] 
  opop$fmf <- opop$fm[match(opop$pop, opop$pid)]
  opop$fff <- opop$ff[match(opop$pop, opop$pid)]
  
  
  # Function to find children. 
  
  # Find all instances where a given PID is a mother or father and package them  in a list
  kidsOf <- with(opop, { c(tapply(pid, mom, c), tapply(pid, pop, c)) })
  kidsOf["0"] <- NULL; kidsOf["0"] <- NULL # Twice as there are two 0s
  
  # Initialise a list
  KidsOf <- list()
  KidsOf[as.numeric(names(kidsOf))] <- kidsOf
  
  # Helper function to subset kids
  ko <- function(KidsOf = KidsOf, p) {
    lapply(p, function(x) {
      unique(as.vector(unlist(KidsOf[x])))
    })
  }
  
  # Initialise a list to save each type of kin kin 
  res <- list()
  
  # Parents
  res$parents <- lapply(pid, function(pid) {
    as.vector(unlist(opop[opop$pid == pid, c("mom", "pop")])) })
  
  # Grandparents
  res$gparents <- lapply(pid, function(pid) {
    as.vector(unlist(opop[opop$pid == pid, c("mm", "mf", # Grandmothers
                                             "fm", "ff")])) }) # Grandfathers
  
  # Great-grandparents
  res$ggparents <- lapply(pid, function(pid) {
    as.vector(unlist(opop[opop$pid == pid, c("mmm", "mfm", "mmf", "mff", ### Great-grandmothers
                                             "fmm", "ffm", "fmf", "fff")])) }) ### Great-grandfathers
  
  
  # Children
  res$children <- ko(KidsOf = KidsOf, p = pid)
  
  # Grandchildren
  res$gchildren <- ko(KidsOf = KidsOf, p = res$children)
  
  # Great-grandchildren
  res$ggchildren <- ko(KidsOf = KidsOf, p = res$gchildren)
  
  
  # Siblings
  res$siblings <- ko(KidsOf = KidsOf, p = res$parents)
  res$siblings <- lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])   # Remove ego
  
  # Niblings
  res$niblings <- ko(KidsOf = KidsOf, p = res$siblings)
  
  
  # Aunts and uncles
  res$unclesaunts <- ko(KidsOf = KidsOf, p = res$gparents)
  res$unclesaunts <- lapply(seq_along(res$unclesaunts), function(i) res$unclesaunts[[i]][res$unclesaunts[[i]] %ni% res$parents[[i]]])   # Remove ego's parents
  
  # Cousins
  res$cousins <- ko(KidsOf = KidsOf, p = res$unclesaunts)  
  
  
  # Replace NULLs and integer(0) with NA
  
  for (i in 1:length(names(res))) {
    res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA 
  }
  
  return(res)
  
}

#----------------------------------------------------------------------------------------------------
## Function to recode kin types into more descriptive names ----

## Re-code the kin_type variable to have more readable labels
recode_kin <- function(opop_kin) {
  opop_kin <- opop_kin %>%
    mutate(kin_type = case_when(kin_type %in% c("parents") ~ "Parents", 
                                kin_type %in% c("gparents") ~ "Grandparents", 
                                kin_type %in% c("ggparents") ~ "Greatgrandparents",
                                kin_type %in% c("children") ~ "Children", 
                                kin_type %in% c("gchildren") ~ "Grandchildren", 
                                kin_type %in% c("ggchildren") ~ "Greatgrandchildren",
                                kin_type %in% c("siblings") ~ "Siblings", 
                                kin_type %in% c("niblings") ~ "Niblings", 
                                kin_type %in% c("unclesaunts") ~ "Aunts_Uncles", 
                                kin_type %in% c("cousins") ~ "Cousins", 
                                TRUE ~ NA_character_),
           kin_type = factor(kin_type, 
                             levels = c("Parents", "Grandparents", "Greatgrandparents",
                                        "Children", "Grandchildren", "Greatgrandchildren",
                                        "Siblings", "Niblings", 
                                        "Aunts_Uncles", "Cousins")))
  return(opop_kin)
}

## Re-code the kin_type variable distinguishing between male and female kin
recode_kin_sex <- function(opop_kin) {
  opop_kin <- opop_kin %>% 
    mutate(kin_type = case_when(kin_type %in% c("parents") & fem_kin == 1 ~ "Mother", 
                                kin_type %in% c("parents") & fem_kin == 0 ~ "Father", 
                                kin_type %in% c("gparents") & fem_kin == 1 ~ "Grandmothers", 
                                kin_type %in% c("gparents") & fem_kin == 0 ~ "Grandfathers", 
                                kin_type %in% c("ggparents") & fem_kin == 1 ~ "Greatgrandmothers",
                                kin_type %in% c("ggparents") & fem_kin == 0 ~ "Greatgrandfathers",
                                kin_type %in% c("children") & fem_kin == 1 ~ "Daughters", 
                                kin_type %in% c("children") & fem_kin == 0 ~ "Sons", 
                                kin_type %in% c("gchildren") & fem_kin == 1 ~ "Granddaughters", 
                                kin_type %in% c("gchildren") & fem_kin == 0 ~ "Grandsons", 
                                kin_type %in% c("ggchildren") & fem_kin == 1 ~ "Greatgranddaughters",
                                kin_type %in% c("ggchildren") & fem_kin == 0 ~ "Greatgrandsons", 
                                kin_type %in% c("siblings") & fem_kin == 1 ~ "Sisters", 
                                kin_type %in% c("siblings") & fem_kin == 0 ~ "Brothers",
                                kin_type %in% c("niblings") & fem_kin == 1 ~ "Nieces", 
                                kin_type %in% c("niblings") & fem_kin == 0 ~ "Nephews",  
                                kin_type %in% c("unclesaunts") & fem_kin == 1 ~ "Aunts", 
                                kin_type %in% c("unclesaunts") & fem_kin == 0 ~ "Uncles",  
                                kin_type %in% c("cousins") & fem_kin == 1 ~ "Cousins_Female", 
                                kin_type %in% c("cousins") & fem_kin == 0 ~ "Cousins_Male",  
                                TRUE ~ NA_character_),
           kin_type = factor(kin_type, 
                             levels = c("Mother", "Father", 
                                        "Grandmothers", "Grandfathers", 
                                        "Greatgrandmothers","Greatgrandfathers",
                                        "Daughters", "Sons", 
                                        "Granddaughters", "Grandsons", 
                                        "Greatgranddaughters","Greatgrandsons", 
                                        "Sisters", "Brothers",
                                        "Nieces", "Nephews",  
                                        "Aunts", "Uncles",  
                                        "Cousins_Female", "Cousins_Male")))
  return(opop_kin)
}


## Re-code the kin_type variable distinguishing between male and female kin, from previous recode
recode_kin_sex2 <- function(opop_kin) {
  opop_kin <- opop_kin %>% 
    mutate(kin_type = case_when(kin_type %in% c("Parents") & fem == 1 ~ "Mother", 
                                kin_type %in% c("Parents") & fem == 0 ~ "Father", 
                                kin_type %in% c("Grandparents") & fem == 1 ~ "Grandmothers", 
                                kin_type %in% c("Grandparents") & fem == 0 ~ "Grandfathers", 
                                kin_type %in% c("Greatgrandparents") & fem == 1 ~ "Greatgrandmothers",
                                kin_type %in% c("Greatgrandparents") & fem == 0 ~ "Greatgrandfathers",
                                kin_type %in% c("Children") & fem == 1 ~ "Daughters", 
                                kin_type %in% c("Children") & fem == 0 ~ "Sons", 
                                kin_type %in% c("Grandchildren") & fem == 1 ~ "Granddaughters", 
                                kin_type %in% c("Grandchildren") & fem == 0 ~ "Grandsons", 
                                kin_type %in% c("Greatgrandchildren") & fem == 1 ~ "Greatgranddaughters",
                                kin_type %in% c("Greatgrandchildren") & fem == 0 ~ "Greatgrandsons", 
                                kin_type %in% c("Siblings") & fem == 1 ~ "Sisters", 
                                kin_type %in% c("Siblings") & fem == 0 ~ "Brothers",
                                kin_type %in% c("Niblings") & fem == 1 ~ "Nieces", 
                                kin_type %in% c("Niblings") & fem == 0 ~ "Nephews",  
                                kin_type %in% c("Aunts-Uncles") & fem == 1 ~ "Aunts", 
                                kin_type %in% c("Aunts-Uncles") & fem == 0 ~ "Uncles",  
                                kin_type %in% c("Cousins") & fem == 1 ~ "Cousins_Female", 
                                kin_type %in% c("Cousins") & fem == 0 ~ "Cousins_Male",  
                                TRUE ~ NA_character_),
           kin_type = factor(kin_type, 
                             levels = c("Mother", "Father", 
                                        "Grandmothers", "Grandfathers", 
                                        "Greatgrandmothers","Greatgrandfathers",
                                        "Daughters", "Sons", 
                                        "Granddaughters", "Grandsons", 
                                        "Greatgranddaughters","Greatgrandsons", 
                                        "Sisters", "Brothers",
                                        "Nieces", "Nephews",  
                                        "Aunts", "Uncles",  
                                        "Cousins_Female", "Cousins_Male")))
  return(opop_kin)
}
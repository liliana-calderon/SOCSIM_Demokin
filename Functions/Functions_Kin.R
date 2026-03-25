#----------------------------------------------------------------------------------------------------
# Function to retrieve members of a kinship network of a given ego(s) from SOCSIM microsimulation outputs

# To run the functions, .opop and omar files must be set in the GlobalEnv

# This is a simplified version of the retrieve_kin function created by Mallika Snyder, 
# included in the rsocsim package. 
# The name of the vectors for grandparents and great-grand parents is slightly different, 
# to keep consistency with the name in other papers 
# (e.g., mf the mother of the father, fm the father of the mother).

# Created on 24-03-2026
# Last modified on 25-03-2026
#----------------------------------------------------------------------------------------------------

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
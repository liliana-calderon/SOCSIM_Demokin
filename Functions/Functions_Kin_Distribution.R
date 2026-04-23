#----------------------------------------------------------------------------------------------------
# Functions to estimate the distribution of the number of kin
# given a kin type and the expected number of kin (k)

# Function written by Sha Jiang
#----------------------------------------------------------------------------------------------------
# Following the convention in matrix kinship models (e.g. Caswell):
#   - Poisson(k) is used for most kin types (children, grandchildren,
#     siblings, nieces/nephews, cousins, aunts/uncles, ...).
#   - Binomial(N, k/N) is used for ancestor kin types whose maximum number is bounded a priori 
#     (parents N = 2, grandparents N = 4, great-grandparents N = 8; 
#     or N = 1, 2, 4 in the matrilineal version;
#     Please adjust it according to the kinship model used).

# P(at least one kin) is then:
#   - Poisson:  1 - exp(-k)
#   - Binomial: 1 - (1 - k/N)^N

# Important caveats (please read before using):

# 1. The Poisson PMF table is TRUNCATED at `max_n` (default qpois(0.9999, k), floor of 10).
#    The last row's `cdf` will therefore be very close to but not exactly equal to 1 
#    (the missing tail mass is at most ~1e-4 with the  default). 
#    If you need the CDF / P(K >= n) accurate at higher quantiles, pass a larger `max_n`.
 
# @param kin_type character. Either a descriptive name
#   ("children", "grandchildren", "siblings", "parents", "grandparents",
#    "great-grandparents", "mother", "grandmother", "great-grandmother", ...)
#   or a DemoKin-style code ("d", "gd", "ggd", "m", "gm", "ggm",
#   "os", "ys", "nos", "nys", "oa", "ya", "coa", "cya").

# @param k numeric. Expected number of kin (mean of the distribution).

# @param N integer, optional. 
#   Maximum possible number of ancestors. Only used for binomial kin types. 
#   If NULL, a sensible default is chosen from `kin_type` 
#   (parents = 2, grandparents = 4, great-grandparents = 8;
#   matrilineal: m = 1, gm = 2, ggm = 4).

# @param max_n integer, optional. 
# Largest value of n to tabulate for Poisson kin. 
# Defaults to qpois(0.9999, k), with a floor of 10.

# @param distribution character, optional.
#  Force "poisson" or "binomial"  instead of inferring from `kin_type`.

# @return A data.frame with columns:
#   - kin_type:    the input kin type
#   - n:           number of kin (0, 1, 2, ...)
#   - pmf:         P(K = n)
#   - cdf:         P(K <= n)
#   - p_at_least:  P(K >= n)
# Attributes: `distribution`, `expected_k`, `P_at_least_one`.

# @examples
# # Expected 1.8 grandchildren -> Poisson distribution
# kin_distribution("grandchildren", k = 1.8)
#
# # Parents: bounded above by 2 -> Binomial(2, 0.9)
# kin_distribution("parents", k = 1.8)
#
# # DemoKin code for granddaughters
# kin_distribution("gd", k = 0.7)

kin_distribution <- function(kin_type,
                             k,
                             N = NULL,
                             max_n = NULL,
                             distribution = NULL) {

  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 0) {
    stop("`k` must be a single non-negative number.")
  }

  # ---- 1. Decide which distribution to use --------------------------------
  binomial_defaults <- c(
    # Two-sex / generic
    "parents"            = 2,
    "grandparents"       = 4,
    "greatgrandparents"  = 8, ## Removed the hyphen for matching code
    # Matrilineal (DemoKin-style codes and names)
    "m"                  = 1,
    "gm"                 = 2,
    "ggm"                = 4,
    "mother"             = 1,
    "grandmother"        = 2,
    "greatgrandmother"   = 4
  )

  kt_key <- tolower(kin_type)

  if (is.null(distribution)) {
    distribution <- if (kt_key %in% names(binomial_defaults)) "binomial" else "poisson"
  }
  distribution <- match.arg(tolower(distribution), c("poisson", "binomial"))

  # ---- 2. Build the PMF ---------------------------------------------------
  if (distribution == "binomial") {
    if (is.null(N)) {
      if (!(kt_key %in% names(binomial_defaults))) {
        stop("Binomial distribution requested but no default N for kin type '",
             kin_type, "'. Please supply N explicitly.")
      }
      N <- binomial_defaults[[kt_key]]
    }
    if (k > N) {
      stop(sprintf("Expected k = %g exceeds the maximum N = %d for kin type '%s'.",
                   k, N, kin_type))
    }
    p     <- k / N
    n_vec <- 0:N
    pmf   <- dbinom(n_vec, size = N, prob = p)
    dist_label <- sprintf("Binomial(N = %d, p = %.4f)", N, p)

  } else { # poisson
    if (is.null(max_n)) {
      max_n <- max(10L, qpois(0.9999, lambda = k))
    }
    n_vec <- 0:max_n
    pmf   <- dpois(n_vec, lambda = k)
    dist_label <- sprintf("Poisson(lambda = %.4f)", k)
  }

  # ---- 3. CDF and survival -----------------------------------------------
  cdf        <- cumsum(pmf)
  p_at_least <- 1 - c(0, head(cdf, -1))   # P(K >= n)

  out <- data.frame(
    kin_type   = kin_type,
    n          = n_vec,
    pmf        = pmf,
    cdf        = cdf,
    p_at_least = p_at_least,
    stringsAsFactors = FALSE
  )

  attr(out, "distribution")    <- dist_label
  attr(out, "expected_k")      <- k
  attr(out, "P_at_least_one")  <- unname(1 - pmf[1])
  class(out) <- c("kin_dist", "data.frame")
  out
}

# Pretty print: show the distribution and a small head of the table.
print.kin_dist <- function(x, n_show = 10, ...) {
  cat("Kin type     :", unique(x$kin_type), "\n")
  cat("Distribution :", attr(x, "distribution"), "\n")
  cat("E[K]         :", attr(x, "expected_k"), "\n")
  cat("P(K >= 1)    :", round(attr(x, "P_at_least_one"), 4), "\n\n")
  NextMethod("print", head(x, n_show))
}


library(dplyr)
library(logitr)

fpath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- read.csv(paste0(fpath, "./genetic_data_dce_final.csv")) |>
  mutate(across(everything(), ~if_else(. == -999, 0, .))) |>
  as_tibble()

df_raw$obsID <- rep(1:(nrow(df_raw)/3), each = 3)
df_raw$asc <- if_else(df_raw$alternative == 3, 1, 0)
df_raw$cost_con <- df_raw$cost_con * 100

saveRDS(df_raw, "./Data/df_prepared.rds")

# Core concepts of MMNL:
# Random utility maximisation: individuals choose options that max utility
#   This contains an observable and unobservable component
# Preference distribution: parameters are distributed according to pre-specified family
# Accounts for correlation between individuals' responses
# Individuals are independent of one another

# Categorical cost -- just to validate directions and linearity assumptions
mmnl_costcat <- logitr(
  data = df_raw,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
    # Costs as discrete, ref = cost0
    "cost5", "cost15", "cost30", "cost150",
    # When to screen
    "when_2", "when_3",
    # How to screen
    "how_2", "how_3",
    # Types of conditions to screen
    "type_2", "type_3", "type_4",
    # How to receive education on test
    "edu_2", "edu_3",
    # Which clinician should deliver screening
    "clin_2", "clin_3",
    # Wait times
    "wait_2", "wait_3",
    # Alternative-specific constant
    "asc"
  ),
  randPars = c(
    cost5 = "n", cost15 = "n", cost30 = "n", cost150 = "n",
    when_2 = "n", when_3 = "n",
    how_2 = "n", how_3 = "n",
    type_2 = "n", type_3 = "n", type_4 = "n",
    edu_2 = "n", edu_3 = "n",
    clin_2 = "n", clin_3 = "n",
    wait_2 = "n", wait_3 = "n"
  ),
  panelID = "record",
  numDraws = 800, # Convergence reached -- no change between 500 to 800 draws
  drawType = "sobol",
  options = list(
    ftol_rel = 1e-8,
    ftol_abs = 1e-8,
    maxeval = 10000
  ),
  numCores = parallelly::availableCores()
)

summary(mmnl_costcat)
# Model has converged, gives stable estimates and reflects nlogit code

saveRDS(mmnl_costcat, "./Models/mmnl_categorical.rds")
# However, continuous cost estimates provide better precision
# Additionally, it is likely that unobserved variation in preference for screening vs optout exists
# A random effect for the ASC should improve results' applicability to SG
# Finally, switch the effects to their lowest level for coherence

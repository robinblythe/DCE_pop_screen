library(dplyr)
library(logitr)

fpath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- read.csv(paste0(fpath, "./genetic_data_dce_final.csv")) |>
  mutate(across(everything(), ~if_else(. == -999, 0, .))) |>
  as_tibble()

df_raw$obsID <- rep(1:(nrow(df_raw)/3), each = 3)
df_raw$asc <- if_else(df_raw$alternative == 3, 1, 0)

# Core concepts of MMNL:
# Random utility maximisation: individuals choose options that max utility
#   This contains an observable and unobservable component
# Preference distribution: parameters are distributed according to pre-specified family
# Accounts for correlation between individuals' responses
# Individuals are independent of one another

mmnl_fit <- logitr(
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
  numDraws = 500,
  drawType = "sobol",
  numCores = parallelly::availableCores()
)

summary(mmnl_fit)

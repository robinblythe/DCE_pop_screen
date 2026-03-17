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
    wait_2 = "n", wait_3 = "n",
    asc = "n"
  ),
  panelID = "record",
  numDraws = 500, # try increasing draws until results have stable SEs
  drawType = "sobol",
  numCores = parallelly::availableCores()
)

# Model has converged, gives stable estimates and reflects nlogit code
summary(mmnl_costcat)

saveRDS(mmnl_costcat, "./Models/mmnl_categorical.rds")


# Cost as a constant
mmnl_costcon <- logitr(
  data = df_raw,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
    # Costs as discrete, ref = cost0
    "cost_con",
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
    cost_con = "n",
    when_2 = "n", when_3 = "n",
    how_2 = "n", how_3 = "n",
    type_2 = "n", type_3 = "n", type_4 = "n",
    edu_2 = "n", edu_3 = "n",
    clin_2 = "n", clin_3 = "n",
    wait_2 = "n", wait_3 = "n",
    asc = "n"
    ), 
  #numMultiStarts = 10, # try this for asc*relMus or asc*raceMal
  panelID = "record",
  numDraws = 500,
  drawType = "sobol",
  numCores = parallelly::availableCores()
)

summary(mmnl_costcon)
# Notes: the large SDs in the random coefficients suggest heterogeneity
# These could be the parameters that we allow additional payment to handle
# E.g., extra cost if testing outside marriage, or to speed up wait times to below 16 weeks

saveRDS(mmnl_costcon, "./Models/mmnl_continuous.rds")


# Sensitivity analysis: drop the 12 people who opted out
optins <- df_raw |> 
  group_by(record) |> 
  filter(asc == 1) |> 
  summarise(optouts = sum(choice)) |> 
  filter(optouts < 10) |> 
  pull(record)



# Sensitivity analysis: run for religion and race, including as fixed effects relative to reference groups (two models)
# in paper - talk about the religion influence
# 1st choice - VIH first, maybe 

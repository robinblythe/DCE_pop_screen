# We are interested in knowing whether program uptake is potentially a function of racial groupings (Malay or Indian)
# Accordingly, it could be useful to determine how much preference heterogeneity is being absorbed by race

library(logitr)
library(dplyr)

df_prepared <- readRDS("./Data/df_prepared.RDS") |>
  mutate(cost_mal = raceMal * cost_con, # Create interactions
         cost_ind = raceInd * cost_con,
         asc_mal = raceMal * asc,
         asc_ind = raceInd * asc)

# Cost as a continuous variable
mmnl_subgroup <- logitr(
  data = df_prepared,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
    # Costs as continuous
    "cost_con",
    # Potential race-specific interactions
    "cost_mal", "cost_ind",
    # When to screen: Ref = 3
    "when_1", "when_2",
    # How to screen, Ref = 1
    "how_2", "how_3",
    # Types of conditions to screen, Ref = 1
    "type_2", "type_3", "type_4",
    # How to receive education on test, Ref = 3
    "edu_1", "edu_2",
    # Which clinician should deliver screening, Ref = 3
    "clin_1", "clin_2",
    # Wait times, Ref = 3
    "wait_1", "wait_2",
    # Alternative-specific constant
    "asc",
    # Potential race-specific opt-out
    "asc_mal", "asc_ind"
    # Note - race variables without interaction are excluded; model fails with them in, maybe due to perfect collinearity
  ),
  randPars = c(
    cost_con = "n",
    when_1 = "n", when_2 = "n",
    how_2 = "n", how_3 = "n",
    type_2 = "n", type_3 = "n", type_4 = "n",
    edu_1 = "n", edu_2 = "n",
    clin_1 = "n", clin_2 = "n",
    wait_1 = "n", wait_2 = "n",
    asc = "n" # ASC included as random effect
  ), 
  panelID = "record",
  numDraws = 800,
  drawType = "sobol",
  options = list(
    ftol_rel = 1e-8,
    ftol_abs = 1e-8,
    maxeval = 10000
  ),
  numCores = parallelly::availableCores(),
  numMultiStarts = 10
)

summary(mmnl_subgroup)
# Some evidence that Malays are slightly more cost-sensitive
# Some evidence that Malays are more likely to opt out and Indians less

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")
summary(mmnl_costcon)

# No material difference in AIC
AIC(mmnl_costcon)
AIC(mmnl_subgroup)

# Notes: the large SDs in the random coefficients suggest substantial heterogeneity
# Large negative coefficient on the ASC suggests people prefer not to opt out if possible
saveRDS(mmnl_subgroup, "./Models/mmnl_subgroup.rds")
write.csv(summary(mmnl_subgroup)$coefTable, "./Tables/mmnl_subgroup_results.csv")

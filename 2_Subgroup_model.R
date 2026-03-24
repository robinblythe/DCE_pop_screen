library(logitr)

df_prepared <- readRDS("./Data/df_prepared.RDS")

# Sensitivity analysis: run for race, including as fixed effects relative to reference groups (two models)
df_prepared$asc_raceMal <- with(df_prepared, asc * raceMal)
df_prepared$asc_raceInd <- with(df_prepared, asc * raceInd)

mmnl_costcon_race <- logitr(
  data = df_prepared,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
    # Costs as continuous
    "cost_con",
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
    "asc", "asc_raceMal", "asc_raceInd"
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

summary(mmnl_costcon_race)
# Log-likelihood shows virtually no change. Model can't really detect whether differences exist.

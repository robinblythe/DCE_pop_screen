library(logitr)
df_prepared <- readRDS("./Data/df_prepared.RDS")

# Re-added code for initial categorical model
mmnl_costcat <- logitr(
  data = df_prepared,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
    # Costs as categorical; cost0 as reference
    "cost5", "cost15", "cost30", "cost150",
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
    "asc"
  ),
  panelID = "record",
  numDraws = 800,
  drawType = "sobol",
  options = list(
    ftol_rel = 1e-8,
    ftol_abs = 1e-8,
    maxeval = 10000
  )
)

summary(mmnl_costcat)

# Cost as a continuous variable - Fixed effects only first
mmnl_costcon_FE <- logitr(
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
    "asc"
  ),
  panelID = "record",
  numDraws = 800,
  drawType = "sobol",
  options = list(
    ftol_rel = 1e-8,
    ftol_abs = 1e-8,
    maxeval = 10000
  )
)

summary(mmnl_costcon_FE)


mmnl_costcon <- logitr(
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
    "asc"
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

summary(mmnl_costcon)

# Model comparisons: huge reduction in log likelihood, large SDs in random coefficients
# Results suggest substantial heterogeneity and random effects should be used
# Large negative coefficient on the ASC suggests people prefer not to opt out if possible
saveRDS(mmnl_costcon, "./Models/mmnl_continuous.rds")
write.csv(summary(mmnl_costcon)$coefTable, "./Tables/mmnl_results.csv")

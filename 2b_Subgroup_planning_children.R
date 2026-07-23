# We are interested in knowing whether preferences appear materially different among respondents planning to have children
library(logitr)
library(dplyr)

df_prepared <- readRDS("./Data/df_prepared.RDS") |>
  filter(ch_fN != 0) # filter out those not planning to have children in future

# Model for those planning children or unsure
mmnl_plan_ch <- logitr(
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

summary(mmnl_plan_ch)
saveRDS(mmnl_plan_ch, "./Models/mmnl_plan_children.rds")

# Comparisons with base model
mmnl_full <- readRDS("./Models/mmnl_continuous.rds")

coef_full <- summary(mmnl_full)$coefTable[,1:2] |>
  mutate(`Full model estimate (SE)` = paste0(round(Estimate, 3), " (", round(`Std. Error`, 3), ")")) |>
  rename(Estimate_full = Estimate, SE_full = `Std. Error`)

coef_plan_ch <- summary(mmnl_plan_ch)$coefTable[,1:2] |>
  mutate(`Subgroup model estimate (SE)` = paste0(round(Estimate, 3), " (", round(`Std. Error`, 3), ")")) |>
  rename(Estimate_subgroup = Estimate, SE_subgroup = `Std. Error`)

coefs <- bind_cols(
  coef_costcon,
  coef_plan_ch
) |>
  mutate(
    z_score = (Estimate_subgroup - Estimate_full)/sqrt(SE_subgroup^2 + SE_full^2),
    p_value = round(2 * pnorm(-abs(z_score)), 3)
  ) |>
  select("Full model estimate (SE)", "Subgroup model estimate (SE)", "z_score", "p_value")

write.csv(coefs, "./Tables/Model_comp_planning_children.csv")

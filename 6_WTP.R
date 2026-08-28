library(logitr)
library(tidyverse)
df_prepared <- readRDS("./Data/df_prepared.RDS")

# Estimate WTP space directly
mmnl_wtp <- logitr(
  data = df_prepared,
  outcome = "choice", # Binary flag for option chosen
  obsID = "obsID",
  pars = c(
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
    when_1 = "n", when_2 = "n",
    how_2 = "n", how_3 = "n",
    type_2 = "n", type_3 = "n", type_4 = "n",
    edu_1 = "n", edu_2 = "n",
    clin_1 = "n", clin_2 = "n",
    wait_1 = "n", wait_2 = "n",
    asc = "n" # ASC included as random effect
  ),
  scalePar = "cost_con", # Ensures model is in WTP space
  #randScale = "ln", # Include if intending to include price sensitivity in addition to attribute heterogeneity
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

summary(mmnl_wtp)
saveRDS(mmnl_wtp, "./Models/mmnl_wtp.rds")
mmnl_wtp <- readRDS("./Models/mmnl_wtp.rds")

# Compare the two - shows different wtp results
mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")
wtpCompare(mmnl_costcon, mmnl_wtp, "cost_con")

# WTP
varnames <- c(
  "Screening available at any time", "Married couples only",
  "Stepwise screening", "Individual screening",
  "Extremely severe & severe conditions", "Extremely severe, severe & moderate conditions", "All conditions regardless of severity",
  "In-person appointment pre and post-test", "In-person appointment only for positive tests",
  "OB/GYN", "GP/polyclinic",
  "Up to 4 weeks wait", "Up to 8 weeks wait"
  )

p_wtp <- summary(mmnl_wtp)$coefTable[2:14,] |>
  mutate(
    attribute = varnames,
    wtp_sgd = Estimate,
    ci_lower = (Estimate - 1.96 * `Std. Error`),
    ci_upper = (Estimate + 1.96 * `Std. Error`)
    ) |>
  ggplot(aes(x = reorder(attribute, wtp_sgd), y = wtp_sgd))

p_wtp +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(-150, 750), breaks = seq(-250, 750, 100)) +
  coord_flip() +
  labs(x = "Attribute level", y = "Incremental willingness-to-pay (SGD)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Save wtp results as table
saveRDS(p_wtp$data, "./Tables/wtp_results.rds")
write.csv(p_wtp$data, "./Tables/wtp_results.csv")
# As figure
ggsave("./Figures/WTP.png", height = 5, width = 8)

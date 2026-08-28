library(dplyr)
library(logitr)

fpath <- paste0("C:/Users/ ", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- read.csv(paste0(fpath, "./genetic_data_dce_final.csv")) |>
  mutate(
    across(everything(), ~ if_else(. == -999, 0, .)),
    cost_con = case_when(
      # Colnames are incorrect; true costs were 0, 100, 300, 600, 1200
      cost_con == 0.5 ~ 100,
      cost_con == 1.5 ~ 300,
      cost_con == 3 ~ 600,
      cost_con == 15 ~ 1200,
      .default = 0
    )
  ) |>
  as_tibble()

df_raw$obsID <- rep(1:(nrow(df_raw) / 3), each = 3)
df_raw$asc <- if_else(df_raw$alternative == 3, 1, 0)

saveRDS(df_raw, "./Data/df_prepared.rds")

# Core concepts of MMNL:
# Random utility maximisation: individuals choose options that max utility
#   This contains an observable and unobservable component
# Preference distribution: parameters are distributed according to pre-specified family
# Accounts for correlation between individuals' responses
# Individuals are independent of one another

# However, continuous cost estimates provide better precision
# Additionally, it is likely that unobserved variation in preference for screening vs optout exists
# Finally, switch the effects to their lowest level (lowest utility service) for coherence

options(scipen = 100, digits = 2)
library(logitr)
library(tidyverse)

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

### Uptake predictions - select groups of interest:

# Option 1: the base-case (cheapest to provide)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_1 == 1 & edu_3 == 1 & clin_3 == 1 & wait_3 == 1
# Reference category

# Option 2: current practice (closest to existing pilot format)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_2 == 1 & edu_2 == 1 & clin_3 == 1 & wait_2 == 1

# Option 3: practical based on minimising overall cost + copayment
# cost_con == 0 & when_1 == 1 & how_2 == 1 & type_2 == 1 & edu_3 == 1 & clin_3 == 1 & wait_1 == 1

# Want to know uptake by copayment:
# Get the choice sets below, but repeat for every combination of costs from 0:C, where C = some reasonable copayment
choice_sets <- data.frame(
  Policy = 1:3,
  obsID = 1:3,
  cost_con = 0,
  when_1 = 0,
  when_2 = 0,
  when_3 = 1,
  how_1 = c(1, 1, 0),
  how_2 = c(0, 0, 1),
  how_3 = 0,
  type_1 = c(1, 0, 0),
  type_2 = c(0, 1, 0),
  type_3 = c(0, 0, 1),
  type_4 = 0,
  edu_1 = 0,
  edu_2 = c(0, 1, 1),
  edu_3 = c(1, 0, 0),
  clin_1 = 0,
  clin_2 = 0,
  clin_3 = 1,
  wait_1 = c(0, 0, 1),
  wait_2 = c(0, 1, 0),
  wait_3 = c(1, 0, 0),
  asc = c(0, 0, 0)
)

# Vector of copayment amounts
# Full cost of test is only around $430, so doesn't make much sense to test further
cost_values <- seq(0, 500, 10)

# Number of alternatives per original choice set
n_alts <- nrow(choice_sets)

# Repeat the original dataset for each copay
expanded_choice_sets <- choice_sets[rep(1:n_alts, times = length(cost_values)), ]
expanded_choice_sets$cost_con <- rep(cost_values, each = n_alts)
expanded_choice_sets$obsID <- 1:nrow(expanded_choice_sets) # set new obsID per decision

# Create the optout choice
single_choice <- rbind(expanded_choice_sets, expanded_choice_sets) # Replicate and bind the dataset
single_choice[(nrow(expanded_choice_sets) + 1):nrow(single_choice), 3:22] <- 0 # Replace the bottom half with null choices
single_choice[(nrow(expanded_choice_sets) + 1):nrow(single_choice), 23] <- 1 # Replace the asc with 1
single_choice <- single_choice |> arrange(obsID, Policy)

# Predict uptake for individual program against optout, assuming copayments apply
predicted_uptake <- predict(
  mmnl_costcon,
  newdata = single_choice,
  obsID = "obsID",
  type = "prob",
  returnData = TRUE,
  interval = "confidence"
) |>
  filter(asc == 0) |>
  mutate(
    Policy = Policy,
    predicted_uptake = predicted_prob,
    predicted_uptake_lower = predicted_prob_lower,
    predicted_uptake_upper = predicted_prob_upper,
    WTP = cost_con,
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    .keep = "none"
  )

print(predicted_uptake, digits = 3, row.names = FALSE)
write.csv(predicted_uptake, file = "./Tables/uptake_vs_optout.csv")

# Now from the full menu of alternatives
df_alternatives <- expanded_choice_sets |>
  mutate(obsID = rep(1:(nrow(expanded_choice_sets) / 3), each = 3, length.out = nrow(expanded_choice_sets)))

optout <- data.frame(
  Policy = 4,
  obsID = 1:length(unique(df_alternatives$obsID)),
  cost_con = 0,
  when_1 = 0,
  when_2 = 0,
  when_3 = 0,
  how_1 = 0,
  how_2 = 0,
  how_3 = 0,
  type_1 = 0,
  type_2 = 0,
  type_3 = 0,
  type_4 = 0,
  edu_1 = 0,
  edu_2 = 0,
  edu_3 = 0,
  clin_1 = 0,
  clin_2 = 0,
  clin_3 = 0,
  wait_1 = 0,
  wait_2 = 0,
  wait_3 = 0,
  asc = 1
)

df_alts <- bind_rows(df_alternatives, optout) |>
  arrange(obsID, Policy)

cost_test <- 430

uptake_alts <- predict(
  mmnl_costcon,
  newdata = df_alts,
  obsID = "obsID",
  type = "prob",
  interval = "confidence",
  returnData = TRUE
) |>
  mutate(
    Policy = Policy,
    predicted_uptake = predicted_prob,
    predicted_uptake_lower = predicted_prob_lower,
    predicted_uptake_upper = predicted_prob_upper,
    WTP = cost_con,
    Estimated_eligible_population = NA_real_,
    Budget_impact = cost_test * Estimated_eligible_population * predicted_uptake,
    Budget_impact_low = cost_test * Estimated_eligible_population * predicted_uptake_lower,
    Budget_impact_high = cost_test * Estimated_eligible_population * predicted_uptake_upper,
    .keep = "none"
  )

print(uptake_alts, digits = 3)
write.csv(uptake_alts, file = "./Tables/uptake_alts.csv")

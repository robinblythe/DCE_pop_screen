options(scipen = 100, digits = 2)
library(logitr)
library(tidyverse)

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

# WTP
wtp_costcon <- wtp(mmnl_costcon, "cost_con")
rownames(wtp_costcon)
rownames(wtp_costcon)[2:14] <- c(
  "Screening available at any time", "Married couples only",
  "Stepwise screening", "Individual screening",
  "Extremely severe & severe conditions", "Extremely severe, severe & moderate conditions", "All conditions regardless of severity",
  "In-person appointment pre and post-test", "In-person appointment only for positive tests",
  "OB/GYN", "GP/polyclinic",
  "Up to 4 weeks wait", "Up to 8 weeks wait"
)

p_wtp <- wtp_costcon[2:14,] |>
  mutate(
    attribute = rownames(wtp_costcon)[2:14],
    wtp_sgd = Estimate,  # Convert to actual SGD
    ci_lower = (Estimate - 1.96 * `Std. Error`),
    ci_upper = (Estimate + 1.96 * `Std. Error`)
  ) |>
  ggplot(aes(x = reorder(attribute, wtp_sgd), y = wtp_sgd))

p_wtp +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(-200, 700), breaks = seq(-200, 700, 100)) +
  coord_flip() +
  labs(x = NULL, y = "WTP (SGD)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

print(p_wtp$data)

# Save wtp results as table
write.csv(p_wtp$data[,c("attribute", "wtp_sgd", "ci_lower", "ci_upper")], "./Tables/wtp_results.csv", row.names = FALSE)
# As figure
ggsave("./Figures/WTP.png", height = 8, width = 12)


### Uptake predictions - select groups of interest:

# Option 1: the base-case (cheapest to provide)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_1 == 1 & edu_3 == 1 & clin_3 == 1 & wait_3 == 1
# Reference category

# Option 2: current practice (closest to existing pilot format)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_2 == 1 & edu_2 == 1 & clin_3 == 1 & wait_2 == 1

# Option 3: practical based on minimising overall cost + copayment
# cost_con == 0 & when_1 == 1 & how_2 == 1 & type_2 == 1 & edu_3 == 1 & clin_3 == 1 & wait_1 == 1

# Option 4: the "full service" -- all options at maximum utility
# cost_con == 1500 & when_1 == 1 & how_2 == 1 & type_4 == 1 & edu_1 == 1 & clin_2 == 1 & wait_1 == 1

# Option 4 should be discarded due to the high testing cost.

# We now have a few options for how to predict uptake
# The most ethical thing would be simply to limit the copayment to the additional charges individuals might incur.
# For policy options 2 & 4, this is the cost of an additional consult ($37.50 for a citizen):
choice_sets <- data.frame(
  Policy = 1:3,
  obsID = 1:3,
  cost_con = c(0, 37.5, 37.5),
  when_1 = 0,
  when_2 = 0,
  when_3 = 1,
  how_1 = c(1, 1, 0),
  how_2 = c(0, 0, 1),
  how_3 = 0,
  type_1 = c(1, 0, 0),
  type_2 = c(0, 1, 1),
  type_3 = 0,
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

optout_set <- choice_sets
optout_set[,3:22] <- 0
optout_set$asc <- 1

# Each option is a trade-off between optin and optout
single_choice <- bind_rows(choice_sets, optout_set) |> 
  arrange(obsID)

# Predict uptake for individual program against optout, assuming copayments apply
predict_uptake <- predict(
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

# Repeat the analysis with no copayments
single_choice_free <- single_choice |> mutate(cost_con = 0) |> filter(Policy != 1)

predict_uptake_free <- predict(
  mmnl_costcon,
  newdata = single_choice_free,
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

uptake <- bind_rows(predict_uptake, predict_uptake_free) |>
  select(Policy, predicted_uptake, predicted_uptake_lower, predicted_uptake_upper, WTP) |>
  rename(
    "Predicted uptake" = predicted_uptake,
    "Predicted uptake (lower)" = predicted_uptake_lower,
    "Predicted uptake (upper)" = predicted_uptake_upper,
    "WTP (SGD)" = WTP) |>
  arrange(Policy)
  

print(uptake, digits = 3, row.names = FALSE)
write.csv(uptake, file = "./Tables/uptake_vs_optout.csv")

# Now from the full menu of alternatives
df_alternatives <- bind_rows(choice_sets, optout_set[1,]) |> 
  mutate(obsID = 1,
         Policy = 1:4)

cost_test <- 430

uptake_alternat <- predict(
  mmnl_costcon, 
  newdata = df_alternatives, 
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
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    Estimated_eligible_population = NA_real_,
    Budget_impact = cost_test * Estimated_eligible_population * predicted_uptake,
    Budget_impact_low = cost_test * Estimated_eligible_population * predicted_uptake_lower,
    Budget_impact_high = cost_test * Estimated_eligible_population * predicted_uptake_upper,
    .keep = "none"
  ) |>
  select(Policy:WTP, Estimated_eligible_population:Budget_impact_high)
  
print(uptake_alternat, digits = 3)
write.csv(uptake_alternat, file = "./Tables/uptake_alternatives.csv")
  
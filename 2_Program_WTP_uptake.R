library(logitr)
library(tidyverse)

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

# WTP
wtp_costcon <- wtp(mmnl_costcon, "cost_con")
rownames(wtp_costcon)
rownames(wtp_costcon)[2:14] <- c(
  "Married couples only", "Married couples before conception only",
  "Stepwise screening", "Individual screening",
  "Extremely severe & severe conditions", "Extremely severe, severe & moderate conditions", "All conditions regardless of severity",
  "In-person appointments only if test positive", "Online/written materials only",
  "GP/polyclinic", "Genetics counsellor",
  "Up to 8 weeks wait", "Up to 16 weeks wait"
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
  scale_y_continuous(limits = c(-300, 700), breaks = seq(-300, 700, 100)) +
  coord_flip() +
  labs(x = NULL, y = "WTP (SGD)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Save wtp results as table
write.csv(p_wtp$data[,c("attribute", "wtp_sgd", "ci_lower", "ci_upper")], "./Tables/wtp_results.csv", row.names = FALSE)
# As figure
ggsave("./Figures/WTP.png", height = 8, width = 12)


### Uptake predictions - select groups of interest:

# Option 1: the base-case (cheapest to provide)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_1 == 1 & edu_3 == 1 & clin_3 == 1 & wait_3 == 1
# Current WTP for this service is -$364, so free is appropriate

# Option 2: current practice (closest to existing pilot format)
# cost_con == 0 & when_3 == 1 & how_1 == 1 & type_2 == 1 & edu_2 == 1 & clin_3 == 1 & wait_2 == 1
# Current WTP for this service is $10, so a very small copayment

# Option 3: the "full service" -- all options at maximum utility
# cost_con == 1500 & when_1 == 1 & how_2 == 1 & type_4 == 1 & edu_1 == 1 & clin_2 == 1 & wait_1 == 1
# Current WTP for this service is $579, so a large copayment, but still far less than service cost

# Option 4: practical based on minimising overall cost + copayment
# cost_con == 0 & when_1 == 1 & how_2 == 1 & type_2 == 1 & edu_3 == 1 & clin_3 == 1 & wait_1 == 1
# Current WTP for this service is $157

# Next step: calculate overall cost of service provision - copayment for each
# Option 1: 

choice_sets <- data.frame(
  Policy = 1:5,
  obsID = 1:5,
  cost_con = c(0, 10, 579, 157, 0),
  when_1 = c(0, 0, 1, 1, 0),
  when_2 = 0,
  when_3 = c(1, 1, 0, 0, 0),
  how_1 = c(1, 1, 0, 0, 0),
  how_2 = c(0, 0, 1, 1, 0),
  how_3 = 0,
  type_1 = c(1, 0, 0, 0, 0),
  type_2 = c(0, 1, 0, 1, 0),
  type_3 = 0,
  type_4 = c(0, 0, 1, 0, 0),
  edu_1 = c(0, 0, 1, 0, 0),
  edu_2 = c(0, 1, 0, 0, 0),
  edu_3 = c(1, 0, 0, 1, 0),
  clin_1 = 0,
  clin_2 = c(0, 0, 1, 0, 0),
  clin_3 = c(1, 1, 0, 1, 0),
  wait_1 = c(0, 0, 1, 1, 0),
  wait_2 = c(0, 1, 0, 0, 0),
  wait_3 = c(1, 0, 0, 0, 0),
  asc = c(0, 0, 0, 0, 1)
)

optout_set <- choice_sets[1:4,]
optout_set[,3:22] <- 0
optout_set$asc <- 1

# Each option is a trade-off between optin and optout
single_choice <- bind_rows(choice_sets, optout_set) |> 
  arrange(obsID) 

# Predict uptake for individual program against optout
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
    cost = cost_con,
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    .keep = "none"
  )

print(predict_uptake, digits = 3)
write.csv(predict_uptake, file = "./Tables/uptake_vs_optout.csv")

# Now from the full menu of alternatives
df_alternatives <- choice_sets |>
  mutate(obsID = 1,
         Policy = case_when(
           cost_con == 1500 ~ 3,
           when_1 == 1 ~ 4,
           wait_2 == 1 ~ 2,
           wait_3 == 1 ~ 1,
           when_1 == 0 ~ 5
         ))

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
    cost = cost_con,
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    .keep = "none"
  )
  
print(uptake_alternat, digits = 3)
write.csv(uptake_alternat, file = "./Tables/uptake_alternatives.csv")
  
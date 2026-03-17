library(logitr)
library(tidyverse)

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

# WTP
wtp_costcon <- wtp(mmnl_costcon, "cost_con")
rownames(wtp_costcon)[2:14] <- c( # Fix number of attribute levels
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
    wtp_sgd = Estimate * 100,  # Convert to actual SGD
    ci_lower = (Estimate - 1.96 * `Std. Error`) * 100,
    ci_upper = (Estimate + 1.96 * `Std. Error`) * 100
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

write.csv(p_wtp$data[,c("attribute", "wtp_sgd", "ci_lower", "ci_upper")], "./Tables/wtp_results.csv", row.names = FALSE)

ggsave("./Figures/WTP.png", height = 8, width = 12)


# Uptake predictions
choice_sets <- expand.grid(
  cost_con = c(0, 5, 15, 30, 150),
  when = c(1, 2, 3),
  how = c(1, 2, 3),
  type = c(1, 2, 3, 4),
  edu = c(1, 2, 3),
  clin = c(1, 2, 3),
  wait = c(1, 2, 3)
)
choice_sets$obsID <- 1:nrow(choice_sets)

choices <- fastDummies::dummy_cols(choice_sets, c("when", "how", "type", "edu", "clin", "wait")) |>
  select(obsID, cost_con, when_1:wait_3)
choices$asc <- 0

optout <- choices |> select(obsID, cost_con, when_1:wait_3)
optout[,2:21] <- 0
optout$asc <- 1

single_choice <- bind_rows(choices, optout) |> 
  arrange(obsID) 

predict_uptake <- predict(
  mmnl_costcon,
  newdata = single_choice,
  obsID = "obsID", 
  returnData = TRUE
) |>
  filter(asc == 0) |>
  mutate(
    obsID = obsID,
    predicted_uptake = predicted_prob,
    cost = cost_con * 10,
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    .keep = "none"
  )
remove(choice_sets, choices, optout, single_choice)


p_uptake <- bind_rows(
  predict_uptake |> filter(how == 1, type == 1, edu == 1, clin == 1, wait == 1) |>
    select(cost, level = when, predicted_uptake) |> mutate(attribute = "When to Screen"),
  
  predict_uptake |> filter(when == 1, type == 1, edu == 1, clin == 1, wait == 1) |>
    select(cost, level = how, predicted_uptake) |> mutate(attribute = "How to Screen"),
  
  predict_uptake |> filter(when == 1, how == 1, edu == 1, clin == 1, wait == 1) |>
    select(cost, level = type, predicted_uptake) |> mutate(attribute = "Condition Type"),
  
  predict_uptake |> filter(when == 1, how == 1, type == 1, clin == 1, wait == 1) |>
    select(cost, level = edu, predicted_uptake) |> mutate(attribute = "Education Method"),
  
  predict_uptake |> filter(when == 1, how == 1, type == 1, edu == 1, wait == 1) |>
    select(cost, level = clin, predicted_uptake) |> mutate(attribute = "Clinician Type"),
  
  predict_uptake |> filter(when == 1, how == 1, type == 1, edu == 1, clin == 1) |>
    select(cost, level = wait, predicted_uptake) |> mutate(attribute = "Wait Time")
) |>
  ggplot(aes(x = factor(level), y = predicted_uptake, color = attribute, group = attribute))

p_uptake +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~cost, 
             labeller = labeller(cost = function(x) paste0("Cost: $", x)), 
             ncol = 1,
             scales = "free") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = scales::breaks_pretty(n = 3)
    ) +
  scale_colour_viridis_d() +
  labs(title = "Predicted Uptake by Attribute Level Across Different Costs",
       x = "Attribute Level",
       y = "Predicted Uptake Probability",
       color = "Attribute") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("./Figures/uptake_by_attribute.png", height = 10, width = 8)

# maybe try a scatter with cost on one axis and combo of alternatives on other

# Add analysis of 4 alternatives presented together: total uptake including opt-out
library(logitr)
library(tidyverse)

mmnl_subgroup <- readRDS("./Models/mmnl_subgroup.rds")


# Marginal uptake by attribute level
choice_sets_mal <- expand.grid(
  cost_con = c(0, 100, 300, 600, 1200),
  cost_ind = 0,
  when = c(1, 2, 3),
  how = c(1, 2, 3),
  type = c(1, 2, 3, 4),
  edu = c(1, 2, 3),
  clin = c(1, 2, 3),
  wait = c(1, 2, 3),
  asc_ind = 0
)
choice_sets_mal$cost_mal <- choice_sets_mal$cost_con

choice_sets_mal$obsID <- 1:nrow(choice_sets_mal)

choices <- fastDummies::dummy_cols(choice_sets_mal, c("when", "how", "type", "edu", "clin", "wait")) |>
  select(obsID, cost_con, cost_mal, cost_ind, when_1:wait_3, asc_ind)
choices$asc <- 0
choices$asc_mal <- 0

optout <- choices |> select(obsID, cost_con, cost_mal, cost_ind, when_1:wait_3, asc_ind)
optout[,2:23] <- 0
optout$asc <- 1
optout$asc_mal <- 1

single_choice_mal <- bind_rows(choices, optout) |>
  arrange(obsID)

predict_uptake <- predict(
  mmnl_subgroup,
  newdata = single_choice_mal,
  type = "prob",
  obsID = "obsID", 
  returnData = TRUE,
) |>
  filter(asc == 0) |>
  mutate(
    obsID = obsID,
    predicted_uptake = predicted_prob,
    cost = cost_con,
    when = when_1 + when_2 * 2 + when_3 * 3,
    how = how_1 + how_2 * 2 + how_3 * 3,
    type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4,
    edu = edu_1 + edu_2 * 2 + edu_3 * 3,
    clin = clin_1 + clin_2 * 2 + clin_3 * 3,
    wait = wait_1 + wait_2 * 2 + wait_3 * 3,
    .keep = "none"
  )

p_uptake_mal <- bind_rows(
  predict_uptake |> filter(how == 1, type == 1, edu == 3, clin == 3, wait == 3) |>
    select(cost, level = when, predicted_uptake) |> mutate(attribute = "When to Screen"),
  
  predict_uptake |> filter(when == 3, type == 1, edu == 3, clin == 3, wait == 3) |>
    select(cost, level = how, predicted_uptake) |> mutate(attribute = "How to Screen"),
  
  predict_uptake |> filter(when == 3, how == 1, edu == 3, clin == 3, wait == 3) |>
    select(cost, level = type, predicted_uptake) |> mutate(attribute = "Condition Type"),
  
  predict_uptake |> filter(when == 3, how == 1, type == 1, clin == 3, wait == 3) |>
    select(cost, level = edu, predicted_uptake) |> mutate(attribute = "Education Method"),
  
  predict_uptake |> filter(when == 3, how == 1, type == 1, edu == 3, wait == 3) |>
    select(cost, level = clin, predicted_uptake) |> mutate(attribute = "Clinician Type"),
  
  predict_uptake |> filter(when == 3, how == 1, type == 1, edu == 3, clin == 3) |>
    select(cost, level = wait, predicted_uptake) |> mutate(attribute = "Wait Time")
) |>
  ggplot(aes(x = factor(level), y = predicted_uptake, color = attribute, group = attribute))

p_uptake_mal +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~cost, 
             labeller = labeller(cost = function(x) paste0("Cost: $", x)), 
             ncol = 1,
             scales = "free") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = scales::breaks_extended(n = 4),
  ) +
  scale_colour_viridis_d() +
  labs(title = "Predicted uptake by attribute across copayment levels",
       x = "Attribute Level",
       y = "Predicted Uptake Probability",
       color = "Attribute") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("./Figures/uptake_by_attribute.png", height = 10, width = 8)
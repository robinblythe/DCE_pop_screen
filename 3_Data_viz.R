library(tidyverse)
library(logitr)

p_uptake <- read.csv("./Tables/uptake_vs_optout.csv") |>
  select(-X) |>
  mutate(Policy = factor(Policy)) |>
  ggplot()

p_uptake +
  geom_ribbon(aes(x = WTP, ymin = predicted_uptake_lower, ymax = predicted_uptake_upper, fill = Policy), alpha = 0.7) +
  geom_line(aes(x = WTP, y = predicted_uptake), linewidth = 1.3) +
  facet_wrap(~Policy, labeller = labeller(Policy = function(x) paste0("Policy ", x))) + 
  scale_y_continuous(limits = c(0.7, 0.9), breaks = seq(0.7, 0.9, 0.05)) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 125)) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#0072B2")) +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Willingness-to-pay for screening (SGD)",
       y = "Predicted uptake")
ggsave("./Figures/predicted_uptake_optout.png", height = 6, width = 8)


mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

choice_sets <- expand.grid(
  cost_con = c(0, 50, 150, 300, 1500),
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
  labs(title = "Predicted uptake by attribute across copayment levels",
       x = "Attribute Level",
       y = "Predicted Uptake Probability",
       color = "Attribute") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("./Figures/uptake_by_attribute.png", height = 10, width = 8)


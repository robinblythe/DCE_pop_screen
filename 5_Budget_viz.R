library(tidyverse)
library(logitr)
library(patchwork)

source("./99_utils.R")

# Policy estimation parameters
# Stepwise assumes if positive, do second test, but this is based on our existing ~80 gene panel
# New panel means more genes screened, which means more positives and also more counselling sessions
# Can't estimate this with any precision. Need to make some simplifying assumptions that:
# Tests are roughly the same cost regardless of whether baseline or more comprehensive
# Rough rate of counselling will stay the same
# These are limitations to bring up in discussion section
set.seed(090426)

cost_single_test <- 430
cost_single_visit <- 154.03
cost_obgyn <- 240

pos_rate <- rbeta(10000, 1, 114) # Based on pilot results -- not likely to get a better estimate for less/more comprehensive panels
pos_rate_stepwise <- rbeta(10000, 60, 40) # Based on rough estimates that around 60% of individuals carry at least 1 variant on the panel
cost_type1 <- rnorm(10000, cost_single_test/2, 25) # Add uncertainty based on some plausible ranges
cost_type3 <- rlnorm(10000, log(cost_single_test), log(1.11)) # Add uncertainty based on some plausible ranges

cost_baseline <- median(cost_type1) * 2
cost_baseline_low <- quantile(cost_type1, 0.025) * 2
cost_baseline_high <- quantile(cost_type1, 0.975) * 2

cost_pilot <- cost_single_test * 2 + cost_single_visit * median(pos_rate)
cost_pilot_low <- cost_single_test * 2 + cost_single_visit * quantile(pos_rate, 0.025)
cost_pilot_high <- cost_single_test * 2 + cost_single_visit * quantile(pos_rate, 0.975)

vector_utilitymax <- (cost_type3 + cost_type3 * pos_rate_stepwise) + cost_single_visit * 2
cost_utilitymax <- median(vector_utilitymax)
cost_utilitymax_low <- quantile(vector_utilitymax, 0.025)
cost_utilitymax_high <- quantile(vector_utilitymax, 0.975)

Estimated_eligible_population <- 25000

# Policy 1 is baseline, policy 2 is pilot, policy 3 is utility maximising
budget_optout <- readRDS("./Tables/uptake_vs_optout.rds") |>
  mutate(
    cost_test = case_when(
      Policy == 1 ~ cost_baseline,
      Policy == 2 ~ cost_pilot,
      Policy == 3 ~ cost_utilitymax
    ),
    cost_test_low = case_when(
      Policy == 1 ~ cost_baseline_low,
      Policy == 2 ~ cost_pilot_low,
      Policy == 3 ~ cost_utilitymax_low
    ),
    cost_test_high = case_when(
      Policy == 1 ~ cost_baseline_high,
      Policy == 2 ~ cost_pilot_high,
      Policy == 3 ~ cost_utilitymax_high
    ),
    Budget_impact = pmax(cost_test - Copayment, 0) * Estimated_eligible_population * predicted_uptake,
    Budget_impact_low = pmax(cost_test_low - Copayment, 0) * Estimated_eligible_population * predicted_uptake_lower,
    Budget_impact_high = pmax(cost_test_high - Copayment, 0) * Estimated_eligible_population * predicted_uptake_upper,
    Budget_impact_couple = pmax(cost_test - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple,
    Budget_impact_couple_low = pmax(cost_test_low - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple_lower,
    Budget_impact_couple_high = pmax(cost_test_high - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple_upper
    )


budget_alts <- readRDS("./Tables/uptake_alts.rds") |>
  mutate(
    cost_test = case_when(
      Policy == 1 ~ cost_baseline,
      Policy == 2 ~ cost_pilot,
      Policy == 3 ~ cost_utilitymax
    ),
    cost_test_low = case_when(
      Policy == 1 ~ cost_baseline_low,
      Policy == 2 ~ cost_pilot_low,
      Policy == 3 ~ cost_utilitymax_low
    ),
    cost_test_high = case_when(
      Policy == 1 ~ cost_baseline_high,
      Policy == 2 ~ cost_pilot_high,
      Policy == 3 ~ cost_utilitymax_high
    ),
    Budget_impact = pmax(cost_test - Copayment, 0) * Estimated_eligible_population * predicted_uptake,
    Budget_impact_low = pmax(cost_test_low - Copayment, 0) * Estimated_eligible_population * predicted_uptake_lower,
    Budget_impact_high = pmax(cost_test_high - Copayment, 0) * Estimated_eligible_population * predicted_uptake_upper,
    Budget_impact_couple = pmax(cost_test - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple,
    Budget_impact_couple_low = pmax(cost_test_low - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple_lower,
    Budget_impact_couple_high = pmax(cost_test_high - Copayment, 0) * Estimated_eligible_population * predicted_uptake_couple_upper
  )


# Budgetary impact
colours <- c("#012169", "#EF7C00")

p_budget_optout <- budget_optout |>
  mutate(Policy = factor(Policy, labels = c("Basic screening", "Pilot continuation", "Utility-maximising"))) |>
  select(Policy, Copayment,
         individual = Budget_impact,
         individual_low = Budget_impact_low,
         individual_high = Budget_impact_high,
         couple = Budget_impact_couple,
         couple_low = Budget_impact_couple_low,
         couple_high = Budget_impact_couple_high) |>
  pivot_longer(
    cols = c(individual, couple),
    names_to = "Scenario",
    values_to = "Budget_impact"
  ) |>
  mutate(
    ci_low = if_else(Scenario == "individual", individual_low,  couple_low),
    ci_high = if_else(Scenario == "individual", individual_high, couple_high),
    Scenario = factor(Scenario,
                      levels = c("individual", "couple"),
                      labels = c("Individual decision", "Shared decision (p²)"))
  ) |>
  ggplot(aes(x = Copayment, y = Budget_impact, colour = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.5) +
  geom_line(aes(y = Budget_impact)) +
  facet_wrap(~Policy, nrow = 1) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 3e7, 2e6)) +
  scale_x_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 300)) +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(x = "Copayment per couple (SGD)",
       y = "Annual budget impact (SGD)"
  )
p_budget_optout


p_budget_alts <- budget_alts |>
  mutate(Policy = factor(Policy, labels = c("Basic screening", "Pilot continuation", "Utility-maximising", "Opt-out"))) |>
  select(Policy, Copayment,
         individual = Budget_impact,
         individual_low = Budget_impact_low,
         individual_high = Budget_impact_high,
         couple = Budget_impact_couple,
         couple_low = Budget_impact_couple_low,
         couple_high = Budget_impact_couple_high) |>
  pivot_longer(
    cols = c(individual, couple),
    names_to = "Scenario",
    values_to = "Budget_impact"
  ) |>
  mutate(
    ci_low  = if_else(Scenario == "individual", individual_low,  couple_low),
    ci_high = if_else(Scenario == "individual", individual_high, couple_high),
    Scenario = factor(Scenario,
                      levels = c("individual", "couple"),
                      labels = c("Individual uptake", "Couple uptake (p²)"))
  ) |>
  filter(Policy != "Opt-out",
         Scenario == "Individual uptake") |>
  group_by(Copayment) |>
  summarise(Budget_impact_alt = sum(Budget_impact, na.rm = T),
            ci_low_alt = sum(ci_low, na.rm = T),
            ci_high_alt = sum(ci_high, na.rm = T)) |>
  ggplot(aes(x = Copayment, y = Budget_impact_alt)) +
  geom_ribbon(aes(ymin = ci_low_alt, ymax = ci_high_alt), alpha = 0.5, fill = colours[1]) +
  geom_line(aes(y = Budget_impact_alt), colour = colours[1]) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 3e7, 2e6)) +
  scale_x_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 100)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Copayment per couple (SGD)",
       y = "Annual budget impact (SGD)"
  )
p_budget_alts

(p_budget_optout) / (p_budget_alts) +
  plot_annotation(tag_levels = "A")

ggsave("./Figures/budget_plot.png", height = 8, width = 8)

write.csv(p_budget_optout$data, "./Tables/Budget_impact_optout.csv")
write.csv(p_budget_alts$data, "./Tables/Budget_impact_alts.csv")

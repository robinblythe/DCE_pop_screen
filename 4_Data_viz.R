library(tidyverse)
library(logitr)
library(patchwork)
library(flextable)

source("./99_utils.R")

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

p_wtp <- wtp_costcon[2:14, ] |>
  mutate(
    attribute = rownames(wtp_costcon)[2:14],
    wtp_sgd = Estimate, # Convert to actual SGD
    ci_lower = (Estimate - 1.96 * `Std. Error`),
    ci_upper = (Estimate + 1.96 * `Std. Error`)
  ) |>
  ggplot(aes(x = reorder(attribute, wtp_sgd), y = wtp_sgd))

p_wtp +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(-100, 600), breaks = seq(-100, 600, 100)) +
  coord_flip() +
  labs(x = "Attribute level", y = "Incremental willingness-to-pay (SGD)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Save wtp results as table
write.csv(p_wtp$data, "./Tables/wtp_results.csv")
# As figure
ggsave("./Figures/WTP.png", height = 8, width = 12)

# Overall results table
flext <- tabulate_service_costs(p_wtp$data) |>
  flextable() |>
  colformat_double(digits = 2) |>
  fit_to_width(max_width = 6.5) |>
  fontsize(size = 10) |>
  theme_booktabs() |>
  add_footer_lines("Note: Reference categories set to 0. All values in 2025 Singapore dollars (SGD)") |>
  suppressWarnings()

save_as_docx(flext, path = "./Tables/service_cost_table.docx")

# Uptake
p_uptake <- read.csv("./Tables/uptake_vs_optout.csv") |>
  select(-X) |>
  mutate(Policy = factor(Policy)) |>
  ggplot()

p_uptake +
  geom_ribbon(aes(x = WTP, ymin = predicted_uptake_lower, ymax = predicted_uptake_upper, fill = Policy), alpha = 0.7) +
  geom_line(aes(x = WTP, y = predicted_uptake), linewidth = 1.3) +
  facet_wrap(~Policy, labeller = labeller(Policy = function(x) paste0("Policy ", x))) +
  scale_y_continuous(limits = c(0.7, 1), breaks = seq(0.7, 1, 0.05)) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 125)) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#0072B2")) +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Willingness-to-pay for screening (SGD)",
    y = "Predicted uptake (compared to opt-out)"
  )

ggsave("./Figures/predicted_uptake_optout.png", height = 6, width = 8)

# Uptake as a function of possible alternatives
dat <- read.csv("./Tables/uptake_alts.csv")
dat$WTP[dat$Policy == 4] <- unique(dat$WTP)

p_alts <- dat |>
  select(Policy, predicted_uptake:WTP) |>
  mutate(Policy = factor(Policy, labels = c("Basic screening", "Pilot continuation", "Utility-maximising", "Opt-out"))) |>
  ggplot()

p_alts +
  geom_area(aes(x = WTP, y = predicted_uptake, fill = Policy), position = "stack") +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1)) +
  scale_fill_manual(values = c(
    "Basic screening" = "#E69F00",
    "Pilot continuation" = "#56B4E9",
    "Utility-maximising" = "#0072B2",
    "Opt-out" = "#999999"
  )) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Willingness-to-pay for screening (SGD)",
    y = "Predicted uptake by program"
  )

ggsave("./Figures/predicted_uptake_alternatives.png", height = 5, width = 7)


# Combined uptake plot
(p_uptake +
  geom_ribbon(aes(x = WTP, ymin = predicted_uptake_lower, ymax = predicted_uptake_upper, fill = Policy), alpha = 0.7) +
  geom_line(aes(x = WTP, y = predicted_uptake), linewidth = 1.3) +
  facet_wrap(~Policy, labeller = labeller(Policy = function(x) paste0("Policy ", x))) +
  scale_y_continuous(limits = c(0.7, 1), breaks = seq(0.7, 1, 0.05)) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 125)) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#0072B2")) +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Willingness-to-pay for screening (SGD)",
    y = "Predicted uptake (compared to opt-out)"
  )) /
  (p_alts +
    geom_area(aes(x = WTP, y = predicted_uptake, fill = Policy), position = "stack") +
    scale_fill_manual(values = c(
      "Basic screening" = "#E69F00",
      "Pilot continuation" = "#56B4E9",
      "Utility-maximising" = "#0072B2",
      "Opt-out" = "#999999"
    )) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(
      x = "Willingness-to-pay for screening (SGD)",
      y = "Predicted uptake (compared to other programs)"
    )) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

ggsave("./Figures/predicted_uptake_combined.png", height = 9, width = 9)

# Marginal uptake by attribute level
choice_sets <- expand.grid(
  cost_con = c(0, 100, 300, 600, 1200),
  when = c(1, 2, 3),
  how = c(1, 2, 3),
  type = c(1, 2, 3, 4),
  edu = c(1, 2, 3),
  clin = c(1, 2, 3),
  wait = c(1, 2, 3)
)

p_uptake <- make_choice_table(choice_sets = choice_sets) |>
  ggplot(aes(x = factor(level), y = predicted_uptake, color = attribute, group = attribute))

p_uptake +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~cost,
    labeller = labeller(cost = function(x) paste0("Cost: $", x)),
    space = "free_x"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = scales::breaks_extended(n = 6),
  ) +
  scale_colour_viridis_d() +
  labs(
    x = "Attribute Level",
    y = "Predicted Uptake Probability vs Opt-out",
    color = "Attribute"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("./Figures/uptake_by_attribute.png", height = 8, width = 10)

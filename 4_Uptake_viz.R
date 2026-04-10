library(tidyverse)
library(logitr)
library(patchwork)

source("./99_utils.R")

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

# WTP
wtp_costcon <- wtp(mmnl_costcon, "cost_con")
rownames(wtp_costcon)
rownames(wtp_costcon)[2:15] <- c(
  "Screening available at any time", "Married couples only",
  "Stepwise screening", "Individual screening",
  "Extremely severe & severe conditions", "Extremely severe, severe & moderate conditions", "All conditions regardless of severity",
  "In-person appointment pre and post-test", "In-person appointment only for positive tests",
  "OB/GYN", "GP/polyclinic",
  "Up to 4 weeks wait", "Up to 8 weeks wait",
  "Opt out"
)

wtp <- wtp_costcon[2:15, ] |>
  mutate(
    attribute = rownames(wtp_costcon)[2:15],
    wtp_sgd = Estimate, 
    ci_lower = (Estimate - 1.96 * `Std. Error`),
    ci_upper = (Estimate + 1.96 * `Std. Error`)
  )

p_wtp <- wtp |>
  filter(attribute != "Opt out") |>
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
saveRDS(p_wtp$data, "./Tables/wtp_results.rds")
write.csv(p_wtp$data, "./Tables/wtp_results.csv")
# As figure
ggsave("./Figures/WTP.png", height = 5, width = 8)

# Uptake
p_uptake <- readRDS("./Tables/uptake_vs_optout.rds") |>
  mutate(Policy = factor(Policy, labels = c("Basic screening", "Pilot continuation", "Utility-maximising"))) |>
  ggplot() +
  geom_ribbon(aes(x = Copayment, ymin = predicted_uptake_lower, ymax = predicted_uptake_upper, fill = Policy), alpha = 0.7) +
  geom_line(aes(x = Copayment, y = predicted_uptake), linewidth = 1.3) +
  facet_wrap(~Policy) +
  scale_y_continuous(limits = c(0.5, 0.95), breaks = seq(0.5, 1, 0.05)) +
  scale_x_continuous(limits = c(0, 1201), breaks = seq(0, 1200, 300)) +
  scale_fill_manual(values = c("Basic screening" = "#E69F00", "Pilot continuation" = "#56B4E9", "Utility-maximising" = "#0072B2")) +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Copayment per couple (SGD)",
    y = "Predicted uptake (compared to opt-out)"
  )
p_uptake

# Uptake as a function of possible alternatives
dat <- readRDS("./Tables/uptake_alts.rds")
dat$WTP[dat$Policy == 4] <- unique(dat$WTP)

p_alts <- dat |>
  mutate(Policy = factor(Policy, labels = c("Basic screening", "Pilot continuation", "Utility-maximising", "Opt-out"))) |>
  ggplot() +
  geom_area(aes(x = Copayment, y = predicted_uptake, fill = Policy), position = "stack") +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 100)) +
  scale_fill_manual(values = c(
    "Basic screening" = "#E69F00",
    "Pilot continuation" = "#56B4E9",
    "Utility-maximising" = "#0072B2",
    "Opt-out" = "#999999"
  )) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Copayment per couple (SGD)",
    y = "Predicted uptake by program"
  )
p_alts 

# Combined uptake plot
(p_uptake) / (p_alts) +
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

p_uptake_level <- make_choice_table(choice_sets = choice_sets) |>
  ggplot(aes(x = factor(level), y = predicted_uptake, color = attribute, group = attribute))

p_uptake_level +
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


# Helper function for WTP table
# Create the complete table
tabulate_service_costs <- function(dat) {
  cost_single_test <- 430
  cost_single_visit <- 154.03
  USD_to_SGD <- 1.28 # As of 18 March 2026
  cost_obgyn <- 240
  cost_GP <- cost_single_visit
  cost_counsellor <- cost_single_visit

  service_costs <- tibble::tibble(
    Attribute = c(
      "When screening should be subsidised",
      "",
      "",
      "",
      "How results are received",
      "",
      "",
      "",
      "Type of conditions screened",
      "",
      "",
      "",
      "",
      "Education and counselling",
      "",
      "",
      "",
      "Clinician providing the screening service",
      "",
      "",
      "",
      "Wait times",
      "",
      ""
    ),
    Levels = c(
      "For married couples before conception only",
      "For married couples only",
      "Any time, including before couples are married",
      NA,
      "Our risk as a couple",
      "One of us tested first, and if we are a carrier, the other is tested for those genes",
      "Our risk both as individuals and as a couple",
      NA,
      "Extremely severe (conditions with shortened lifespan in infancy/childhood or intellectual disability)",
      "Extremely severe and severe (conditions with shortened lifespan in early adulthood, impaired mobility, or disabling organ impairment)",
      "Extremely severe, severe, and moderate (conditions causing visual or hearing impairments and immune deficiency)",
      "All conditions regardless of severity, including conditions with onset later in life such as genetic risks of heart disease or cancer",
      NA,
      "Only written or online informational materials are available",
      "In-person appointment alone or with my partner are available only if we are both carriers of a disease, with online/written informational materials available before our test",
      "In-person appointments alone or with my partner are available both before and after testing",
      NA,
      "Genetic counselling service",
      "General practitioner/polyclinic",
      "Obstetrician/gynaecologist",
      NA,
      "Up to 16 weeks",
      "Up to 8 weeks",
      "Up to 4 weeks"
    ),
    `Willingness-to-pay (SGD)` = c(
      "Reference", round(dat["Married couples only", "wtp_sgd"], 2), round(dat["Screening available at any time", "wtp_sgd"], 2), NA,
      "Reference", round(dat["Stepwise screening", "wtp_sgd"], 2), round(dat["Individual screening", "wtp_sgd"], 2), NA,
      "Reference", round(dat["Extremely severe & severe conditions", "wtp_sgd"], 2), round(dat["Extremely severe, severe & moderate conditions", "wtp_sgd"], 2), round(dat["All conditions regardless of severity", "wtp_sgd"], 2), NA,
      "Reference", round(dat["In-person appointment only for positive tests", "wtp_sgd"], 2), round(dat["In-person appointment pre and post-test", "wtp_sgd"], 2), NA,
      "Reference", round(dat["GP/polyclinic", "wtp_sgd"], 2), round(dat["OB/GYN", "wtp_sgd"], 2), NA,
      "Reference", round(dat["Up to 8 weeks wait", "wtp_sgd"], 2), round(dat["Up to 4 weeks wait", "wtp_sgd"], 2)
    ),
    Lower = c(
      NA, dat["Married couples only", "ci_lower"], dat["Screening available at any time", "ci_lower"], NA,
      NA, dat["Stepwise screening", "ci_lower"], dat["Individual screening", "ci_lower"], NA,
      NA, dat["Extremely severe & severe conditions", "ci_lower"], dat["Extremely severe, severe & moderate conditions", "ci_lower"], dat["All conditions regardless of severity", "ci_lower"], NA,
      NA, dat["In-person appointment only for positive tests", "ci_lower"], dat["In-person appointment pre and post-test", "ci_lower"], NA,
      NA, dat["GP/polyclinic", "ci_lower"], dat["OB/GYN", "ci_lower"], NA,
      NA, dat["Up to 8 weeks wait", "ci_lower"], dat["Up to 4 weeks wait", "ci_lower"]
    ),
    Upper = c(
      NA, dat["Married couples only", "ci_upper"], dat["Screening available at any time", "ci_upper"], NA,
      NA, dat["Stepwise screening", "ci_upper"], dat["Individual screening", "ci_upper"], NA,
      NA, dat["Extremely severe & severe conditions", "ci_upper"], dat["Extremely severe, severe & moderate conditions", "ci_upper"], dat["All conditions regardless of severity", "ci_upper"], NA,
      NA, dat["In-person appointment only for positive tests", "ci_upper"], dat["In-person appointment pre and post-test", "ci_upper"], NA,
      NA, dat["GP/polyclinic", "ci_upper"], dat["OB/GYN", "ci_upper"], NA,
      NA, dat["Up to 8 weeks wait", "ci_upper"], dat["Up to 4 weeks wait", "ci_upper"]
    ),
    `Incremental service cost (SGD)` = c(
      0, 0, 0, NA,
      0, -(1 - 0.6) * cost_single_test, 0, NA,
      -0.5 * cost_single_test, 0, (325 * USD_to_SGD - cost_single_test), (1989 * USD_to_SGD - cost_single_test), NA,
      -cost_single_visit, 0, cost_single_visit, NA,
      cost_counsellor, cost_GP, cost_obgyn, NA,
      0, 0, 0
    )
  )
  service_costs$`Incremental program cost (SGD)` <- service_costs$`Incremental service cost (SGD)` - as.numeric(service_costs$`Willingness-to-pay (SGD)`)
  return(service_costs)
}


# Helper function for marginal uptake plot
make_choice_table <- function(choice_sets) {
  choice_sets$obsID <- 1:nrow(choice_sets)

  choices <- fastDummies::dummy_cols(choice_sets, c("when", "how", "type", "edu", "clin", "wait")) |>
    select(obsID, cost_con, when_1:wait_3)
  choices$asc <- 0

  optout <- choices |> select(obsID, cost_con, when_1:wait_3)
  optout[, 2:21] <- 0
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
  )
  return(p_uptake)
}

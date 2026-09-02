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
      when = when_1 * 3 + when_2 * 2 + when_3, # ref = 3
      how = how_1 + how_2 * 2 + how_3 * 3, # Ref = 1
      type = type_1 + type_2 * 2 + type_3 * 3 + type_4 * 4, # Ref = 1
      edu = edu_1 * 3 + edu_2 * 2 + edu_3, # Ref = 3
      clin = clin_1 * 3 + clin_2 * 2 + clin_3, # Ref = 3
      wait = wait_1 * 3 + wait_2 * 2 + wait_3, # Ref = 3
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

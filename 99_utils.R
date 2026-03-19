dat <- p_wtp$data

# Define cost parameters
cost_single_test <- 430
cost_single_visit <- 154.03
USD_to_SGD <- 1.28 # As of 18 March 2026
cost_obgyn <- 240
cost_GP <- 37.50
cost_counsellor <- 37.50

# Create the complete table
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
    "Any time, including before couples are married",
    "For married couples only",
    "For married couples before conception only",
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
    "In-person appointments alone or with my partner are available both before and after testing",
    "In-person appointment alone or with my partner are available only if we are both carriers of a disease, with online/written informational materials available before our test",
    "Only written or online informational materials are available",
    NA,
    "Obstetrician/gynaecologist",
    "General practitioner/polyclinic",
    "Genetic counselling service",
    NA,
    "Up to 4 weeks",
    "Up to 8 weeks",
    "Up to 16 weeks"
  ),
  
  `Willingness-to-pay (SGD)` = c(
    "Reference", round(dat["Married couples only", "wtp_sgd"], 2), round(dat["Married couples before conception only", "wtp_sgd"], 2), NA,
    "Reference", round(dat["Stepwise screening", "wtp_sgd"], 2), round(dat["Individual", "wtp_sgd"], 2), NA,
    "Reference", round(dat["Extremely severe & severe conditions", "wtp_sgd"], 2), round(dat["Extremely severe, severe & moderate conditions", "wtp_sgd"], 2), round(dat["All conditions regardless of severity", "wtp_sgd"], 2), NA,
    "Reference", round(dat["In-person appointments only if test positive", "wtp_sgd"], 2), round(dat["Online/written materials only", "wtp_sgd"], 2), NA,
    "Reference", round(dat["GP/polyclinic", "wtp_sgd"], 2), round(dat["Genetics counsellor", "wtp_sgd"], 2), NA,
    "Reference", round(dat["Up to 8 weeks wait", "wtp_sgd"], 2), round(dat["Up to 16 weeks wait", "wtp_sgd"], 2)
  ),
  
  Lower = c(
    NA, dat["Married couples only", "ci_lower"], dat["Married couples before conception only", "ci_lower"], NA,
    NA, dat["Stepwise screening", "ci_lower"], dat["Individual", "ci_lower"], NA,
    NA, dat["Extremely severe & severe conditions", "ci_lower"], dat["Extremely severe, severe & moderate conditions", "ci_lower"], dat["All conditions regardless of severity", "ci_lower"], NA,
    NA, dat["In-person appointments only if test positive", "ci_lower"], dat["Online/written materials only", "ci_lower"], NA,
    NA, dat["GP/polyclinic", "ci_lower"], dat["Genetics counsellor", "ci_lower"], NA,
    NA, dat["Up to 8 weeks wait", "ci_lower"], dat["Up to 16 weeks wait", "ci_lower"]
  ),
  
  Upper = c(
    NA, dat["Married couples only", "ci_upper"], dat["Married couples before conception only", "ci_upper"], NA,
    NA, dat["Stepwise screening", "ci_upper"], dat["Individual", "ci_upper"], NA,
    NA, dat["Extremely severe & severe conditions", "ci_upper"], dat["Extremely severe, severe & moderate conditions", "ci_upper"], dat["All conditions regardless of severity", "ci_upper"], NA,
    NA, dat["In-person appointments only if test positive", "ci_upper"], dat["Online/written materials only", "ci_upper"], NA,
    NA, dat["GP/polyclinic", "ci_upper"], dat["Genetics counsellor", "ci_upper"], NA,
    NA, dat["Up to 8 weeks wait", "ci_upper"], dat["Up to 16 weeks wait", "ci_upper"]
  ),
  
  `Incremental service cost (SGD)` = c(
    0, 0, 0, NA,
    0, -(1 - 0.6) * cost_single_test, 0, NA,
    -0.5 * cost_single_test, 0, (325 * USD_to_SGD - cost_single_test), (1989 * USD_to_SGD - cost_single_test), NA,
    cost_single_visit, 0, -cost_single_visit, NA,
    cost_obgyn, cost_GP, cost_counsellor, NA,
    0, 0, 0
  )
)
service_costs$`Incremental program cost (SGD)` <- service_costs$`Incremental service cost (SGD)` - as.numeric(service_costs$`Willingness-to-pay (SGD)`)



library(gmnl)
library(tidyverse)

# ============================================
# STEP 1: Define Your 4 Service Configurations
# ============================================

# Create a data frame with your 4 proposed programs
# Use the SAME coding as in your original data

programs <- data.frame(
  program_id = 1:4,
  program_name = c(
    "Basic Screening",
    "Standard Screening", 
    "Comprehensive Screening",
    "Premium Screening"
  ),
  
  # Define attribute levels for each program
  # Use effects coding as in your model
  
  # Cost (in dollars - we'll use continuous)
  cost = c(5, 15, 30, 50),
  
  # Timing: 1=before pregnancy, 2=early pregnancy, 3=anytime
  when = c(1, 2, 1, 2),
  
  # How: 1=individual, 2=couple required, 3=couple optional
  how = c(1, 2, 3, 3),
  
  # Type: 1=common only, 2=common+moderate, 3=common+moderate+rare, 4=comprehensive
  type = c(1, 2, 3, 4),
  
  # Education: 1=basic info, 2=standard counseling, 3=comprehensive counseling
  education = c(1, 2, 3, 3),
  
  # Clinician: 1=GP, 2=specialist, 3=choice
  clinician = c(1, 2, 3, 3),
  
  # Wait time: 1=<1 week, 2=1-2 weeks, 3=>2 weeks
  wait = c(1, 2, 2, 3)
)

print(programs)

# ============================================
# STEP 2: Convert to Effects-Coded Variables
# ============================================

# Create the same effects coding as in your estimation
programs_coded <- programs %>%
  mutate(
    # Cost continuous
    COST_CON = cost,
    
    # When (reference = level 3)
    WHEN_2 = case_when(when == 1 ~ 1, when == 3 ~ -1, TRUE ~ 0),
    WHEN_3 = case_when(when == 2 ~ 1, when == 3 ~ -1, TRUE ~ 0),
    
    # How (reference = level 3)
    HOW_2 = case_when(how == 1 ~ 1, how == 3 ~ -1, TRUE ~ 0),
    HOW_3 = case_when(how == 2 ~ 1, how == 3 ~ -1, TRUE ~ 0),
    
    # Type (reference = level 4)
    TYPE_2 = case_when(type == 1 ~ 1, type == 4 ~ -1, TRUE ~ 0),
    TYPE_3 = case_when(type == 2 ~ 1, type == 4 ~ -1, TRUE ~ 0),
    TYPE_4 = case_when(type == 3 ~ 1, type == 4 ~ -1, TRUE ~ 0),
    
    # Education (reference = level 3)
    EDU_2 = case_when(education == 1 ~ 1, education == 3 ~ -1, TRUE ~ 0),
    EDU_3 = case_when(education == 2 ~ 1, education == 3 ~ -1, TRUE ~ 0),
    
    # Clinician (reference = level 3)
    CLIN_2 = case_when(clinician == 1 ~ 1, clinician == 3 ~ -1, TRUE ~ 0),
    CLIN_3 = case_when(clinician == 2 ~ 1, clinician == 3 ~ -1, TRUE ~ 0),
    
    # Wait (reference = level 3)
    WAIT_2 = case_when(wait == 1 ~ 1, wait == 3 ~ -1, TRUE ~ 0),
    WAIT_3 = case_when(wait == 2 ~ 1, wait == 3 ~ -1, TRUE ~ 0)
  )

# ============================================
# STEP 3: Calculate Population-Level Utility for Each Program
# ============================================

# Extract coefficients from your estimated model
coefs <- coef(mmnl_race_het)

# Function to calculate mean utility for a race group
calculate_utility <- function(program_row, coefs, race = "Chinese") {
  
  # Get race-specific coefficients
  if (race == "Chinese") {
    beta_cost <- -exp(coefs["COST_CON"])
    beta_when2 <- coefs["WHEN_2"]
    beta_when3 <- coefs["WHEN_3"]
    beta_how2 <- coefs["HOW_2"]
    beta_how3 <- coefs["HOW_3"]
    beta_type2 <- coefs["TYPE_2"]
    beta_type3 <- coefs["TYPE_3"]
    beta_type4 <- coefs["TYPE_4"]
    beta_edu2 <- coefs["EDU_2"]
    beta_edu3 <- coefs["EDU_3"]
    beta_clin2 <- coefs["CLIN_2"]
    beta_clin3 <- coefs["CLIN_3"]
    beta_wait2 <- coefs["WAIT_2"]
    beta_wait3 <- coefs["WAIT_3"]
  } else if (race == "Malay") {
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEMAL"])
    beta_when2 <- coefs["WHEN_2"] + coefs["WHEN_2:RACEMAL"]
    beta_when3 <- coefs["WHEN_3"] + coefs["WHEN_3:RACEMAL"]
    beta_how2 <- coefs["HOW_2"] + coefs["HOW_2:RACEMAL"]
    beta_how3 <- coefs["HOW_3"] + coefs["HOW_3:RACEMAL"]
    beta_type2 <- coefs["TYPE_2"] + coefs["TYPE_2:RACEMAL"]
    beta_type3 <- coefs["TYPE_3"] + coefs["TYPE_3:RACEMAL"]
    beta_type4 <- coefs["TYPE_4"] + coefs["TYPE_4:RACEMAL"]
    beta_edu2 <- coefs["EDU_2"] + coefs["EDU_2:RACEMAL"]
    beta_edu3 <- coefs["EDU_3"] + coefs["EDU_3:RACEMAL"]
    beta_clin2 <- coefs["CLIN_2"] + coefs["CLIN_2:RACEMAL"]
    beta_clin3 <- coefs["CLIN_3"] + coefs["CLIN_3:RACEMAL"]
    beta_wait2 <- coefs["WAIT_2"] + coefs["WAIT_2:RACEMAL"]
    beta_wait3 <- coefs["WAIT_3"] + coefs["WAIT_3:RACEMAL"]
  } else if (race == "Indian") {
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEIND"])
    beta_when2 <- coefs["WHEN_2"] + coefs["WHEN_2:RACEIND"]
    beta_when3 <- coefs["WHEN_3"] + coefs["WHEN_3:RACEIND"]
    beta_how2 <- coefs["HOW_2"] + coefs["HOW_2:RACEIND"]
    beta_how3 <- coefs["HOW_3"] + coefs["HOW_3:RACEIND"]
    beta_type2 <- coefs["TYPE_2"] + coefs["TYPE_2:RACEIND"]
    beta_type3 <- coefs["TYPE_3"] + coefs["TYPE_3:RACEIND"]
    beta_type4 <- coefs["TYPE_4"] + coefs["TYPE_4:RACEIND"]
    beta_edu2 <- coefs["EDU_2"] + coefs["EDU_2:RACEIND"]
    beta_edu3 <- coefs["EDU_3"] + coefs["EDU_3:RACEIND"]
    beta_clin2 <- coefs["CLIN_2"] + coefs["CLIN_2:RACEIND"]
    beta_clin3 <- coefs["CLIN_3"] + coefs["CLIN_3:RACEIND"]
    beta_wait2 <- coefs["WAIT_2"] + coefs["WAIT_2:RACEIND"]
    beta_wait3 <- coefs["WAIT_3"] + coefs["WAIT_3:RACEIND"]
  } else {  # Other
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEOTH"])
    beta_when2 <- coefs["WHEN_2"] + coefs["WHEN_2:RACEOTH"]
    beta_when3 <- coefs["WHEN_3"] + coefs["WHEN_3:RACEOTH"]
    beta_how2 <- coefs["HOW_2"] + coefs["HOW_2:RACEOTH"]
    beta_how3 <- coefs["HOW_3"] + coefs["HOW_3:RACEOTH"]
    beta_type2 <- coefs["TYPE_2"] + coefs["TYPE_2:RACEOTH"]
    beta_type3 <- coefs["TYPE_3"] + coefs["TYPE_3:RACEOTH"]
    beta_type4 <- coefs["TYPE_4"] + coefs["TYPE_4:RACEOTH"]
    beta_edu2 <- coefs["EDU_2"] + coefs["EDU_2:RACEOTH"]
    beta_edu3 <- coefs["EDU_3"] + coefs["EDU_3:RACEOTH"]
    beta_clin2 <- coefs["CLIN_2"] + coefs["CLIN_2:RACEOTH"]
    beta_clin3 <- coefs["CLIN_3"] + coefs["CLIN_3:RACEOTH"]
    beta_wait2 <- coefs["WAIT_2"] + coefs["WAIT_2:RACEOTH"]
    beta_wait3 <- coefs["WAIT_3"] + coefs["WAIT_3:RACEOTH"]
  }
  
  # Calculate utility
  utility <- beta_cost * program_row$COST_CON +
    beta_when2 * program_row$WHEN_2 +
    beta_when3 * program_row$WHEN_3 +
    beta_how2 * program_row$HOW_2 +
    beta_how3 * program_row$HOW_3 +
    beta_type2 * program_row$TYPE_2 +
    beta_type3 * program_row$TYPE_3 +
    beta_type4 * program_row$TYPE_4 +
    beta_edu2 * program_row$EDU_2 +
    beta_edu3 * program_row$EDU_3 +
    beta_clin2 * program_row$CLIN_2 +
    beta_clin3 * program_row$CLIN_3 +
    beta_wait2 * program_row$WAIT_2 +
    beta_wait3 * program_row$WAIT_3
  
  return(utility)
}

# Calculate utility for each program and race
races <- c("Chinese", "Malay", "Indian", "Other")

utility_results <- expand.grid(
  program_id = programs_coded$program_id,
  race = races,
  stringsAsFactors = FALSE
) %>%
  left_join(programs_coded, by = "program_id") %>%
  rowwise() %>%
  mutate(
    utility = calculate_utility(cur_data(), coefs, race)
  ) %>%
  ungroup()

print(utility_results %>% select(program_name, race, utility, cost))

# ============================================
# STEP 4: Calculate Choice Probabilities
# ============================================

# Choice probability relative to opt-out (utility = 0)
# P(choose program j) = exp(V_j) / (exp(V_j) + exp(0))
#                     = exp(V_j) / (1 + exp(V_j))

utility_results <- utility_results %>%
  mutate(
    prob_vs_optout = exp(utility) / (1 + exp(utility))
  )

# Choice probabilities when all 4 programs are offered simultaneously
# P(choose program j) = exp(V_j) / sum(exp(V_k) for all k + exp(0))

choice_probs <- utility_results %>%
  group_by(race) %>%
  mutate(
    sum_exp_utility = sum(exp(utility)) + 1,  # +1 for opt-out
    prob_among_all = exp(utility) / sum_exp_utility,
    prob_optout = 1 / sum_exp_utility
  ) %>%
  ungroup()

print(choice_probs %>% 
        select(program_name, race, utility, prob_vs_optout, prob_among_all))

# ============================================
# STEP 5: Calculate WTP for Each Program
# ============================================

# WTP = utility without cost / cost coefficient
# This tells you the maximum someone would pay for this program

calculate_wtp_program <- function(program_row, coefs, race) {
  
  # Get race-specific cost coefficient
  if (race == "Chinese") {
    beta_cost <- -exp(coefs["COST_CON"])
  } else if (race == "Malay") {
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEMAL"])
  } else if (race == "Indian") {
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEIND"])
  } else {
    beta_cost <- -exp(coefs["COST_CON"] + coefs["COST_CON:RACEOTH"])
  }
  
  # Calculate utility WITHOUT cost component
  utility_no_cost <- calculate_utility(program_row, coefs, race) - 
    beta_cost * program_row$COST_CON
  
  # WTP = utility / |cost coefficient|
  wtp <- -utility_no_cost / beta_cost
  
  # Net benefit = WTP - actual cost
  net_benefit <- wtp - program_row$cost
  
  return(list(wtp = wtp, net_benefit = net_benefit))
}

wtp_results <- utility_results %>%
  rowwise() %>%
  mutate(
    w
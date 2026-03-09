library(tidyverse)

fpath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- read.csv(paste0(fpath, "./genetic_data_dce_final.csv")) |>
  mutate(across(everything(), ~if_else(. == -999, 0, .))) |>
  as_tibble()

df_raw$obsID <- rep(1:(nrow(df_raw)/3), each = 3)
df_raw$asc <- if_else(df_raw$alternative == 3, 1, 0)

mmnl_costcon <- readRDS("./Models/mmnl_continuous.rds")

coefs <- coef(mmnl_costcon)

# Malays
# Muslims


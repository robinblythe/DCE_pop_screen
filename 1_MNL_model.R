library(mlogit)
library(gmnl)
library(tidyverse)

fpath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- read.csv(paste0(fpath, "./genetic_data_dce_final.csv")) |>
  mutate(across(everything(), ~ifelse(. == -999, 0, .)),
         chid = as.integer(paste0(record, task)))

#df_mlogit <- dfidx(df_raw, idx = c("chid", "alternative"), choice = "choice", shape = "long")

df_mlogit_data <- mlogit.data(
  df_raw, 
  choice = "choice", 
  shape = "long", 
  alt.var = "alternative", 
  chid.var = "chid", 
  id.var = "record"
  )


model1 <- gmnl(
  choice ~ cost5 + cost15 + cost30 + cost150 + 
    when_2 + when_3 + 
    how_2 + how_3 + 
    type_2 + type_3 + type_4 + 
    edu_2 + edu_3 + 
    clin_2 + clin_3 + 
    wait_2 + wait_3,
  data = df_mlogit_data,
  model = "mixl",
  ranp = c(
    cost5 = "n", cost15 = "n", cost30 = "n", cost150 = "n",
    when_2 = "n", when_3 = "n",
    how_2 = "n", how_3 = "n",
    type_2 = "n", type_3 = "n", type_4 = "n",
    edu_2 = "n", edu_3 = "n",
    clin_2 = "n", clin_3 = "n",
    wait_2 = "n", wait_3 = "n"
  ),
  reflevel = "3",
  R = 50,
  halton = NA,
  panel = TRUE,
  correlation = TRUE,
  method = "bfgs",
  iterlim = 200
)

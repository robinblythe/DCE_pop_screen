fpath <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/NUS Dropbox/Robin Daniel Blythe/Carrier screening program/Preference studies/DCEs/ECS preferences/Results")
df_raw <- readxl::read_xlsx(paste0(fpath, "./145127211 NUS_SG DCE - FL CE Data N=500 20260115.xlsx"))

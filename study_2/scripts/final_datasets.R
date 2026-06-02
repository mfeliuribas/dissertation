# ----- Create final datasets --------------------------------------------------
#
# This script contains the code used to create the final dataset for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
experiment_bls <- read.csv(here("study_2", "tidy_data", 
                                "experiment_bls_tidy.csv"))

questionnaire_bls <- read.csv(here("study_2", "tidy_data", 
                                   "questionnaire_bls_tidy.csv"))

lextale_sp_bls <- read.csv(here("study_2", "tidy_data", 
                                "lextale_sp_bls_tidy.csv"))
  
lextale_en_bls <- read.csv(here("study_2", "tidy_data", 
                                "lextale_en_bls_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 3. Join datasets -------------------------------------------------------
data_bls <- experiment_bls |>
  left_join(questionnaire_bls |> select(-c(session:list)), 
            by = "participant")|>
  left_join(lextale_sp_bls |> select(-c(session:list)), 
            by = "participant") |>
  left_join(lextale_en_bls |> select(participant, lextale_trad_en, lextale_avg_en), 
            by = "participant")

# Fix LexTale issue 
data_bls_final <- data_bls |>
  mutate(
    lextale_trad = case_when(
      language == "sp" ~ lextale_trad_sp,
      language == "en" ~ lextale_trad_en),
    lextale_avg = case_when(
      language == "sp" ~ lextale_avg_sp,
      language == "en" ~ lextale_avg_en))|>
  select(-c(global_sp, global_en, lextale_trad_sp, lextale_avg_sp, 
            lextale_trad_en, lextale_avg_en))

# Should I z-score here?


# Write final dataset
data_bls_final |>
  write_csv(here("study_2", "final_data", "bls_final.csv"))

# ------------------------------------------------------------------------------


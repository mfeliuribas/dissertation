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

# Standardze BLP and LexTale scores
  # Z-score LexTale by language (lextale-esp separate from lextale-eng)
data_bls_final <- data_bls_final |>
  mutate(
    blp_z = (blp_score - mean(blp_score)) / sd(blp_score)) |>
  #mutate(
    #blp_z = round(blp_z, 2)) |>
  relocate(blp_z, .after = blp_score)

data_bls_final |>
  summarize(
    mean_blp_z = mean(blp_z),
    sd_blp_z = sd(blp_z))

data_bls_final <- data_bls_final |>
  group_by(language) |>
  mutate(
    lextale_trad_z = (lextale_trad - mean(lextale_trad)) / sd(lextale_trad),
    lextale_avg_z = (lextale_avg - mean(lextale_avg)) / sd(lextale_avg)) |>
  ungroup() |>
  #mutate(
    #lextale_trad_z = round(lextale_trad_z, 2),
    #lextale_avg_z = round(lextale_avg_z, 2)) |>
  relocate(lextale_trad_z, .after = lextale_trad) |>
  relocate(lextale_avg_z, .after = lextale_avg)
  
data_bls_final |>
  group_by(language) |>
  summarize(
    mean_lextale_trad_z = mean(lextale_trad_z),
    sd_lextale_trad_z = sd(lextale_trad_z),
    mean_lextale_avg_z = mean(lextale_avg_z),
    sd_lextale_avg_z = sd(lextale_avg_z))

# Write final dataset
data_bls_final |>
  write_csv(here("study_2", "final_data", "bls_final.csv"))

# ------------------------------------------------------------------------------


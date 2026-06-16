# ----- Create final datasets --------------------------------------------------
#
# This script contains the code used to create the final datasets for Study 2:
#   1. Bilingual data
#   2. Spanish data (bilinguals + monolinguals)
#   3. English data (bilinguals + monolinguals)
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

spanish_mls <- read.csv(here("study_2", "tidy_data", "spanish_mls_tidy.csv"))

english_mls <- read.csv(here("study_2", "tidy_data", "english_mls_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 3. Join bilingual datasets ---------------------------------------------
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



# ----- 4. Join Spanish datasets -----------------------------------------------
# Remove English data from BLs
experiment_sp_bls <- experiment_bls |>
  filter(language == "sp")

count(experiment_bls, language)

# Merge BLs datasets
spanish_bls <- experiment_sp_bls |>
  left_join(questionnaire_bls, by = "participant")

spanish_bls <- spanish_bls |>
  select(-c(session.y, language.y, list.y)) |>
  rename(session = session.x,
         language = language.x,
         list = list.x)

# Check if merging worked (BL data)
sample_participants_sp <- c("HS_002", "HS_052", "HS_101")

questionnaire_bls |>
  filter(participant %in% sample_participants_sp) |>
  arrange(participant) |>
  view()

experiment_sp_bls |>
  filter(participant %in% sample_participants_sp) |>
  arrange(participant) |>
  view()

spanish_bls |>
  filter(participant %in% sample_participants_sp) |>
  arrange(participant) |>
  view()

# Drop columns not necessary for Spanish analysis (BL data)
spanish_bls <- spanish_bls |>
  select(-c(session, blp_score, global_sp, global_en))

# Prepare ML dataset for merging (i.e., delete BLP/LexTale columns, transform 
# gender options to lowercase)
data.frame(colnames(spanish_mls))

spanish_mls <- spanish_mls |>
  select(-c(residence_city, residence_count, age_acq_sp:lextale_clavel)) |>
  mutate(gender = tolower(gender))

# Check if BL and ML datasets have the same columns and in the same order
setdiff(names(spanish_bls), names(spanish_mls))
setdiff(names(spanish_mls), names(spanish_bls))

identical(names(spanish_bls), names(spanish_mls))

# Merge BL and ML datasets
spanish_dataset <- bind_rows(spanish_bls, spanish_mls)

# Delete 'language' column and create new variable 'group' (Spanish dataset)
spanish_dataset <- spanish_dataset |>
  select(-language)

spanish_dataset <- spanish_dataset |>
  separate(
    col = participant,
    into = c("ling_profile", "participant"),
    sep = "_") |>
  relocate(ling_profile, .before = participant) |>
  mutate(ling_profile = case_when(
    ling_profile == "HS" ~ "bilingual",
    ling_profile == "ML" ~ "monolingual"))

count(spanish_dataset, ling_profile)

# Write final Spanish dataset
spanish_dataset |>
  write_csv(here("study_2", "final_data", "spanish_final.csv"))
# ------------------------------------------------------------------------------



# ----- 5. Join English datasets -----------------------------------------------
# Remove Spanish data from BLs
experiment_en_bls <- experiment_bls |>
  filter(language == "en")

count(experiment_bls, language)

# Merge BLs datasets
english_bls <- experiment_en_bls |>
  left_join(questionnaire_bls, by = "participant")
  
english_bls <- english_bls |>
  select(-c(session.y, language.y, list.y)) |>
  rename(session = session.x,
         language = language.x,
         list = list.x)

# Check if merging worked (BL data)
sample_participants_en <- c("HS_004", "HS_042", "HS_092")

questionnaire_bls |>
  filter(participant %in% sample_participants_en) |>
  arrange(participant) |>
  view()

experiment_en_bls |>
  filter(participant %in% sample_participants_en) |>
  arrange(participant) |>
  view()

english_bls |>
  filter(participant %in% sample_participants_en) |>
  arrange(participant) |>
  view()

# Drop columns not necessary for English analysis (BL data)
english_bls <- english_bls |>
  select(-c(session, blp_score, global_sp, global_en))

# Prepare ML dataset for merging (i.e., delete BLP/LexTale columns)
data.frame(colnames(english_mls))

english_mls <- english_mls |>
  select(-c(residence_city, residence_count, age_acq_sp:lextale_wrought))

# Check if BL and ML datasets have the same columns and in the same order
setdiff(names(english_bls), names(english_mls))
setdiff(names(english_mls), names(english_bls))

identical(names(english_bls), names(english_mls))

# Merge BL and ML datasets
english_dataset <- bind_rows(english_bls, english_mls)

# Delete 'language' column and create new variable 'group' (Spanish dataset)
english_dataset <- english_dataset |>
  select(-language)

english_dataset <- english_dataset |>
  separate(
    col = participant,
    into = c("ling_profile", "participant"),
    sep = "_") |>
  relocate(ling_profile, .before = participant) |>
  mutate(ling_profile = case_when(
    ling_profile == "HS" ~ "bilingual",
    ling_profile == "ML" ~ "monolingual"))

count(english_dataset, ling_profile)

# Write final English dataset
english_dataset |>
  write_csv(here("study_2", "final_data", "english_final.csv"))
# ------------------------------------------------------------------------------

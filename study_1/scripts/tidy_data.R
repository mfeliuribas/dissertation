# ----- Tidy data --------------------------------------------------------------
#
# This script contains the code used to tidy the corpora data and create the 
# final datasets for Study 1 (master, futurity, modality).
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("readxl")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load raw data -------------------------------------------------------
data_corpus_raw <- read_excel(here("study_1", "data", "corpus_bls_raw.xlsx"))
# ------------------------------------------------------------------------------



# ----- 3. Check and tidy data: Master dataset ---------------------------------
# Visualize data
glimpse(data_corpus_raw)
head(data_corpus_raw)

colnames(data_corpus_raw)

# Remove unnecessary columns
data_corpus_tidy <- data_corpus_raw |>
  select(-c(4, 20))

colnames(data_corpus_tidy)

# Create master dataset
data_corpus_tidy |>
  write_csv(here("study_1", "data", "corpus_bls_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 4. Check and tidy data: Futurity dataset -------------------------------
# Select futurity reading and create subset
future_readings <- c("future", "future_hyp")

data_futurity_raw <- data_corpus_tidy |> 
  filter(reading %in% future_readings)

# Collapse lexical futures together and conditional futures together ("recode")
data_futurity_raw |>
  count(form) 

data_futurity_tidy <- data_futurity_raw |>
  mutate(form = fct_recode(form,
                           "lex_futures" = "querer_inf",
                           "lex_futures" = "tener_inf",
                           "lex_futures" = "pensar_inf",
                           "lex_futures" = "planear_inf",
                           "lex_futures" = "poder_inf",
                           "lex_futures" = "querer",
                           "lex_futures" = "estar_para",
                           "lex_futures" = "necesitar_inf",
                           "cond"        = "cond_inf"))

# Collapse grammatical person/number together
data_futurity_tidy <- data_futurity_tidy |>
  mutate(gram_pers = fct_recode(gram_pers,
                           "1pers" = "1_sg",
                           "2pers" = "2_sg",
                           "3pers" = "3_sg",
                           "1pers" = "1_pl",
                           "3pers" = "3_pl"))

# Collapse semantic verb classes together
data_futurity_tidy |>
  count(verb_class) 

data_futurity_tidy <- data_futurity_tidy |>
  mutate(verb_class = fct_recode(verb_class,
                                "dynamic" = "dynamic",
                                "stative" = "modal",
                                "motion"  = "motion",
                                "dynamic" = "other",
                                "stative" = "perception",
                                "stative" = "psychological",
                                "stative" = "stative"))

# Convert characters into factors and set reference level
data_futurity_tidy <- data_futurity_tidy |>
  mutate(
    temp_dist = factor(
      temp_dist,
      levels = c("immediate", "near", "distant", "nonspecific"))) |>
  mutate(
    gram_pers = factor(
      gram_pers,
      levels = c("1pers", "2pers", "3pers", "inf"))) |>
  mutate(
    verb_class = factor(
      verb_class,
      levels = c("dynamic", "motion", "stative")))

# Create futurity dataset
data_futurity_tidy |>
  write_csv(here("study_1", "data", "corpus_futurity_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 5. Check and tidy data: Modality dataset -------------------------------
# Select modality reading and create subset
modal_readings <- c("inf_cert", "epistemic", "exhortative", "concessive",
                    "epistemic_dm", "attenuation", "lex_form")

data_modality_raw <- data_corpus_tidy |> 
  filter(reading %in% modal_readings)

# Collapse semantic verb classes together
data_modality_tidy <- data_modality_raw |>
  mutate(verb_class = fct_recode(verb_class,
                                 "dynamic" = "dynamic",
                                 "modal" = "modal",
                                 "motion" = "motion",
                                 "dynamic" = "other",
                                 "stative" = "perception",
                                 "stative" = "psychological",
                                 "stative" = "stative"))

# Fix problem with temp_dist column
data_modality_tidy <- data_modality_tidy |>
  mutate(temp_dist = NA)

# Create modality (sub)dataset
data_modality_tidy |>
  write_csv(here("study_1", "data", "corpus_modality_tidy.csv"))
# ------------------------------------------------------------------------------

# ----- Analysis: Modality -----------------------------------------------------
#
# This script contains the code used to analyze the modality data for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
#library("brms")
# library("marginaleffects")
# library("ggdist")
# library("scales")
# library("glue")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260930)
# ------------------------------------------------------------------------------


# ----- 3. Load data -----------------------------------------------------------
data_corpus <- read_csv(here("study_1", "data", "corpus_bls_tidy.csv"))

data_corpus_modality <- read_csv(here("study_1", "data", 
                                      "corpus_modality_tidy.csv"))

# Create subset with the PF and the MF
data_corpus_subset <- data_corpus |>
  filter(form %in% c("MF", "PF"))

future_readings <- c("future", "future_hyp")
modal_readings <- c("inf_cert", "epistemic", "exhortative", "concessive",
                    "epistemic_dm", "attenuation", "lex_form")

data_corpus_subset <- data_corpus_subset |>
  filter(reading %in% c(future_readings, modal_readings))
# ------------------------------------------------------------------------------


# ----- 4. Check overall distribution: PF vs MF --------------------------------
# Create 'domain' category (futurity vs modality)
data_corpus_subset <- data_corpus_subset |>
  mutate(domain = case_when(
    reading %in% future_readings ~ "futurity",
    reading %in% modal_readings  ~ "modality")) |>
  relocate(domain, .before = reading)

# Distribution within the domains (pcts calculated across forms)
data_corpus_subset |>
  count(form, domain) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Distribution within the domains (pcts calculated within forms)
# Temporal and hypothetical futures are collapsed
data_corpus_subset |>
  count(form, domain) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n)) |>
  ungroup() |>
  write_csv(here("study_1", "tables", "table_distribution_pfmf_v1.csv"))

# Distribution within domains, distinguishing futures
data_corpus_subset |>
  mutate(
    domain_reading = case_when(
      domain == "modality" ~ "modality",
      reading == "future" ~ "futurity_temp" ,
      reading == "future_hyp" ~ "futurity_mod")) |>
  relocate(domain_reading, .after = domain) |>
  count(form, domain_reading) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(form, desc(n)) |>
  ungroup() |>
  write_csv(
    here("study_1", "tables", "table_distribution_pfmf_v2.csv"))

# Distribution within the domains (pcts calculated within forms)
# Hypothetical futures EXCLUDED
data_corpus_subset |>
  filter(reading == "future" | domain == "modality") |>
  count(form, domain) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(form, desc(n)) |>
  ungroup() |>
  write_csv(
    here("study_1", "tables", "table_distribution_pfmf_v3.csv"))
# ------------------------------------------------------------------------------

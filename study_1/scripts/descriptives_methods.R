# ----- Descriptive stats: Methods ---------------------------------------------
#
# This script contains the code used to do descriptive stats for the methods 
# section for Study 1.
#
# ------------------------------------------------------------------------------

# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("readxl")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("scales")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
data_raw <- read_excel(here("study_1", "data", "corpus_bls_raw.xlsx"))
# ------------------------------------------------------------------------------



# ----- 3. Check and tidy data  ------------------------------------------------
# Visualize data
glimpse(data_raw)
head(data_raw)

colnames(data_raw)

# Remove unnecessary columns
data_tidy <- data_raw |>
  select(-c(4, 20))

colnames(data_tidy)
# ------------------------------------------------------------------------------



# ----- 4. Check data distribution: Corpus and Demographics --------------------
# Tokens by corpus
data_tidy |>
  count(corpus)

  # Obs: 
  # Total of 1,821 observations.
  # 1,347 tokens from cesa.
  # 474 tokens from stc.

# Participants by corpus
data_tidy |>
  distinct(corpus, participant) |>
  count(corpus)

# Obs: 
  # Total of 90 participants.
  # 48 from cesa.
  # 42 from stc.

# Age
data_tidy |>
  group_by(corpus, participant) |>
  summarize(age = first(age)) |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

  # Obs:
    #   corpus    mean_age    sd_age    min_age    max_age
    #   cesa       24.7        8.18        18        55
    #   stc        29.4        13.3        18        59

# Gender
data_tidy |>
  group_by(participant, corpus) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(corpus, gender)

  # Obs:
    # cesa = 30 fem, 18 male
    # stc =  24 fem, 18 male

# Bilingualism
data_tidy |>
  group_by(corpus, participant) |>
  summarize(type_bil = first(type_bil), .groups = "drop") |>
  count(corpus, type_bil)

  # Obs:
    # cesa   seq         35
    # cesa   sim         13
    # stc    seq         34
    # stc    sim          8
# ------------------------------------------------------------------------------



# ----- 5. Check data distribution: Semantic reading ---------------------------
# Overall distribution by semantic reading
data_tidy |>
  count(reading, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))


# Plot: Overall distribution by semantic reading
data_tidy |>
  count(reading) |>
  ggplot() +
  aes(x = n, y = reorder(reading, n)) +
  geom_col(fill = "gray") +
  labs(title = "Distribution of semantic readings",
       x = "Number of tokens",
       y = NULL) +
  theme_minimal()

  # Obs/Notes:
    # Almost 69% of the data has a future reading (either purely temporal or 
    #   hypothetical).
    # About 19% of the data has modal readings, including categories within 
    #   epistemic modalities and other (e.g., deontic).
    # About 3% of the data must be excluded (EXC + ambiguous).
    # The rest (~ 9%) is habitual, lexicalized formulae/fixed expressions, and 
    #   PF within the protesis of conditional and temporal sentences.
# ------------------------------------------------------------------------------






# ----- 7. Check data distribution: Modal reading ------------------------------
# Select modal readings and create subset
modal_readings <- c("inf_cert", "epistemic", "exhortative", "concessive",
                    "epistemic_dm", "attenuation", "lex_form")

data_modality_raw <- data_tidy |> 
  filter(reading %in% modal_readings)

nrow(data_modality_raw) 

# See distribution of forms in modality reading
data_modality_raw |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# See distribution within modality
data_modality_raw |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

data_modality_raw |>
  count(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(reading)

data_modality_raw |>
  filter(form == "MF") |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1))

data_modality_raw |>
  filter(form == "PF") |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# Obs:
  # 386 modal tokens: 296 PF (76.7%), 80 MF (20.7%), and 10 PI (2.6%).
  # MF has most modal tokens in the epistemic (53.8%) category, whereas the PF
  #   in the inferred certainty (70.5) category. The PI only is represented in 
  #   the lexicalized formulae category.

# Table: Use of PF and MF to express epistemic modality
epistemic_readings <- c("inf_cert", "epistemic", "epistemic_dm", "attenuation")

data_epistemic_raw <- data_tidy |> 
  filter(reading %in% epistemic_readings)

nrow(data_epistemic_raw)

data_epistemic_raw |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1))

data_epistemic_raw |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

data_epistemic_raw |>
  count(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(reading)

data_epistemic_raw |>
  count(form, reading) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, reading, pct) |>
  pivot_wider(names_from = reading, values_from = pct, values_fill = 0)

data_epistemic_raw |>
  count(form, reading) |>
  group_by(reading) |> 
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, reading, pct) |>
  pivot_wider(names_from = reading, values_from = pct, values_fill = 0)

# Obs:
  # 290 epistemic tokens: 223 PF (76.9%), and 57 MF (23.1%).
  # Most of the epistemic tokens are inferred certainty (73.4%) followed by 
  #   present probability (15.9%).
  # MF has most modal tokens in the epistemic (64.2% out of the MF total) 
  #   category, whereas the PF in the inferred certainty (93.1 out of the PF 
  #   total) category. 
  # To express attentuation (66.7%), present probability (93.5%), and other 
  #   sort of epistemic modality via discourse markers (63.3%), the preferred 
  #   form seems to be the MF. Conversely, the PF is almost the only one used 
  #   for inferred certainty (98.2%).

# Use of PF and MF by verb class (epistemic modality only)
data_epistemic_raw |>
  count(form, verb_class) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, verb_class, pct) |>
  pivot_wider(names_from = verb_class, values_from = pct, values_fill = 0)

# Obs:
  # The epistemic MF is mainly used with states (67.2%), while the PF with 
  #   dynamic verbs (49.1%), followed by states (42,8%).

# Use of PF and MF by bilingualism (epistemic modality only)
data_epistemic_raw |>
  count(form, type_bil) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, type_bil, pct) |>
  pivot_wider(names_from = type_bil, values_from = pct, values_fill = 0)

# Use of PF and MF by bilingualism and reading (epistemic modality only)
data_epistemic_raw |>
  count(form, reading, type_bil) |>
  group_by(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, reading, type_bil, pct) |> 
  pivot_wider(names_from = type_bil, values_from = pct, values_fill = 0)

# Obs:
  # Most MF and PF epistemic tokens are produced by sequential bilinguals. No 
  #   surprise here, since most participants are sequential bilinguals. 

# Use of PF and MF by adverb (epistemic modality only)
data_epistemic_raw |>
  count(form, adv_spec) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  select(form, adv_spec, pct) |>
  pivot_wider(names_from = adv_spec, values_from = pct, values_fill = 0)

# Obs: Most tokens have no adverb. 
# ------------------------------------------------------------------------------



# ----- 8. Check data distribution: tables for futurity and epistemicity -------
# Define the analysis set for temporal futures (excluding future_hyp)
futurity_set <- data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"), reading == "future") |>
  droplevels()

nrow(futurity_set)

# Table: distributions of tokens per speaker + summary of distribution
futurity_set |>
  count(participant, name = "n_tokens") |>
  arrange(n_tokens) |>
  print(n = 86)

futurity_set |>
  count(participant, name = "num_tokens") |>
  summarise(
    num_speakers   = n(),
    min = min(num_tokens), median = median(num_tokens),
    mean = mean(num_tokens), max = max(num_tokens))

# Table: form by temporal distance (futurity)
futurity_set |>
  count(form, temp_dist) |>
  pivot_wider(names_from = temp_dist, values_from = n, values_fill = 0)

# Table: form by grammatical person (futurity)
futurity_set |>
  count(form, gram_pers) |>
  pivot_wider(names_from = gram_pers, values_from = n, values_fill = 0)

# Table: form by verb class (futurity)
futurity_set |>
  count(form, verb_class) |>
  pivot_wider(names_from = verb_class, values_from = n, values_fill = 0)

# Table: form by grammatical person and temporal distance (futurity)
futurity_set |>
  mutate(temp_dist = factor(temp_dist,
                            levels = c("immediate", "near", "distant", "nonspecific"))) |>
  count(form, gram_pers, temp_dist) |>
  pivot_wider(names_from = temp_dist, values_from = n, values_fill = 0,
              names_sort = TRUE) |>
  arrange(form, gram_pers)


# Define the analysis set for epistemic modality 
epistemic_set <- c("inf_cert", "epistemic", "epistemic_dm", "attenuation")

epistemic_set <- data_tidy |>
  filter(reading %in% c("inf_cert", "epistemic", "epistemic_dm", "attenuation")) |>
  droplevels()

nrow(epistemic_set)

# Table: distributions of tokens per speaker + summary of distribution
epistemic_set |>
  count(participant, name = "n_tokens") |>
  arrange(n_tokens) |>
  print(n = 71)

epistemic_set |>
  count(participant, name = "num_tokens") |>
  summarise(
    num_speakers   = n(),
    min = min(num_tokens), median = median(num_tokens),
    mean = mean(num_tokens), max = max(num_tokens))

# Table: form by reading (epistemicity)
epistemic_set |>
  count(form, reading) |>
  pivot_wider(names_from = reading, values_from = n, values_fill = 0)

# Table: form by verb class (epistemicity)
epistemic_set |>
  count(form, verb_class) |>
  pivot_wider(names_from = verb_class, values_from = n, values_fill = 0)

# Table: form by bilingualism (epistemicity)
epistemic_set |>
  count(form, type_bil) |>
  pivot_wider(names_from = type_bil, values_from = n, values_fill = 0)

# Table: form by verb class and reading (epistemicity)
epistemic_set |>
  count(form, type_bil, reading) |>
  pivot_wider(names_from = reading, values_from = n, values_fill = 0,
              names_sort = TRUE) |>
  arrange(form, type_bil)
# ------------------------------------------------------------------------------

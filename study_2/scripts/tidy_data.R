# ----- Tidy data --------------------------------------------------------------
#
# This script contains the code used to tidy the data and create the final
# dataset for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load raw data -------------------------------------------------------
# Bilinguals
experiment_bls_raw <- read.csv(here("study_2", "raw_data", 
                                "experiment_bls_raw.csv"))

questionnaire_bls_raw <- read.csv(here("study_2", "raw_data", 
                                       "questionnaire_bls_raw.csv"))

lextale_bls_raw <- read.csv(here("study_2", "raw_data", "lextale_bls_raw.csv"))
# ------------------------------------------------------------------------------


# ----- 3. Tidy experiment data ------------------------------------------------
# ----- 3.1. Bilinguals --------------------------------------------------------

# Visualize data
head(experiment_bls_raw)
glimpse(experiment_bls_raw)

# Check column index numbers
data.frame(colnames(experiment_bls_raw))

# Remove training items and distractors
experiment_bls_tidy <- experiment_bls_raw |>
  select(-c(6:7, 44:79, 116:151, 188:223, 224:225, 334:369))

glimpse(experiment_bls_tidy)
data.frame(colnames(experiment_bls_tidy))
setdiff(colnames(experiment_bls_raw), colnames(experiment_bls_tidy))

# Remove participants who did not finish Session 1 (e.g. no BLP data), who 
# completed the samesession twice by mistake, or who showed inconsistent data 
# (e.g., between the responses in the interest form and the BLP)

experiment_bls_tidy <- experiment_bls_tidy |>
  filter(!participant %in% c("HS_006", "HS_010","HS_019", "HS_023", "HS_024", 
                             "HS_025", "HS_026", "HS_027", "HS_028","HS_045",
                             "HS_048", "HS_055")) |>
  filter(!is.na(participant))

experiment_bls_tidy <- experiment_bls_tidy |>
  filter(!(participant == "HS_002" & session == 2 & Trial == "Span" 
           & Lista == 2)) |>
  filter(!(participant == "HS_004" & session == 2 & Trial == "Span" 
           & Lista == 2)) |>
  filter(!(participant == "HS_005" & session == 2 & Trial == "Span" 
           & Lista == 3)) |>
  filter(!(participant == "HS_007" & session == 2 & Trial == "Span" 
           & Lista == 2)) |>
  filter(!(participant == "HS_013" & session == 1 & Trial == "Span"  
           & Lista  == 1)) |>
  filter(!(participant == "HS_014" & session == 1 & Trial == "Eng"  
           & List  == 3)) |>
  filter(!(participant == "HS_029" & session == 1 & Trial == "Eng"  
           & List  == 1)) |>
  filter(!(participant == "HS_036" & session == 1 & Trial == "Span" 
           & is.na(Lista))) |>
  filter(!(participant == "HS_043" & session == 1 & Trial == "Span" 
           & is.na(Lista))) |>
  filter(!(participant == "HS_050" & session == 1 & Trial == "Span" 
           & Lista == 2))

experiment_bls_tidy |> 
  count(participant)

# Merge 'Lista' and 'List' values by creating a new variable, 'list_assigned'
experiment_bls_tidy <- experiment_bls_tidy |>
  mutate(list_assigned = case_when(
    !is.na(Lista) ~ Lista,
    !is.na(List) ~ List)) |>
  relocate(list_assigned, .after = "List") |>
  select(-Lista, -List)

# Pivot data and create variables for condition, context, verb form, rating, and
# stimuli language
experiment_bls_final <- experiment_bls_tidy |>
  pivot_longer(
    cols = c(starts_with("list") & !matches("list_assigned")),
    names_to = "item",
    values_to = "rating") |>
  separate(
    col = item,
    into = c("list", "condition", "context_num", "stim_lang"),
    sep = "_") |>
  separate(
    col = context_num,
    into = c("context_num", "form"),
    sep = 1) |>
  mutate(form = case_when(
    form == "a" ~ "PF", 
    form == "b" ~ "PI",
    form == "c" ~ "PP")) |>
  relocate(stim_lang, .after = "list")

# Convert list (character) into integers 
#class(experiment_bls_final$list)
experiment_bls_final <- experiment_bls_final |>
  mutate(
    list = str_remove(list, "list")) |>
  mutate(list = as.integer(list))

# Remove sessions/lists not assigned/completed
experiment_bls_final <- experiment_bls_final |>
  filter(
    Trial == "Span" & stim_lang == "sp" | Trial == "Eng" & stim_lang == "en") |>
  filter(list == list_assigned)

experiment_bls_final |>
  count(participant, Trial, stim_lang, list, list_assigned) |>
  arrange(n) |>
  print(n = 168)

# Create new (i.e., manipulated) variables: temporal distance and plan certainty
experiment_bls_final <- experiment_bls_final |>
  mutate(
    temp_dist = NA,
    plan_cert = NA) |>
  relocate(temp_dist, plan_cert, .after = condition) |>
  mutate(temp_dist = if_else(
      condition %in% c("cond1", "cond4"), "immediate",
      if_else(condition %in% c("cond2", "cond5"), "near", "distant"))) |>
  mutate(plan_cert = if_else(
      condition %in% c("cond1", "cond2", "cond3"), "certain", "uncertain"))

experiment_bls_final |>
  count(condition, temp_dist, plan_cert)

# Create item column
# Convert 'context' number (character) into integer
#class(experiment_bls_final$context_num)
experiment_bls_final <- experiment_bls_final |>
  mutate(
    context_num = as.integer(context_num))

experiment_bls_final <- experiment_bls_final |>
  mutate(
    item = case_when(
      condition == "cond1" ~ 0,
      condition == "cond2" ~ 6,
      condition == "cond3" ~ 12,
      condition == "cond4" ~ 18,
      condition == "cond5" ~ 24,
      condition == "cond6" ~ 30) + context_num,
    item = paste("Q", item, sep = "")) |>
  relocate(item, .before = "condition")

experiment_bls_final |>
  count(condition, temp_dist, plan_cert, item) |>
  arrange(condition, item) |>
  print(n = 36)
  
# Delete redundant columns ('Trial' vs 'stim_lang'; 'list_assigned' vs 'list';
# 'condition', 'context')
experiment_bls_final <- experiment_bls_final |>
  select(-c("Trial", "list_assigned", "condition", "context_num")) |>
  rename(language = stim_lang) |>
  relocate(language, .after = "session")

colnames(experiment_bls_final)

# Scale ratings for beta regression (0-1)
# Come back to this after going over Heiss' tutorial to see what type of 
# additional transformations are needed: 
# "One problem with beta regression, though, is that the beta distribution does 
# not include 0 and 1. As we saw earlier, if your data has has 0s or 1s, the 
# model breaks."

experiment_bls_final <- experiment_bls_final |>
  mutate(rating_scaled = rating / 100) |>
  rename(rating_pct = rating) |>
  rename(rating = rating_scaled) |>
  select(-rating_pct)

# Write csv for tidy experiment data
write_csv(experiment_bls_final,
          here("study_2", "tidy_data", "experiment_bls_tidy.csv"))
# ------------------------------------------------------------------------------

# ----- 3.2. Monolinguals-Sp ---------------------------------------------------
# ------------------------------------------------------------------------------

# ----- 3.3. Monolinguals-En ---------------------------------------------------
# ------------------------------------------------------------------------------



# ----- 4. Tidy BLP data -------------------------------------------------------
# ----- 4.1. Bilinguals --------------------------------------------------------
# ------------------------------------------------------------------------------

# ----- 4.2. Monolinguals-Sp ---------------------------------------------------
# ------------------------------------------------------------------------------

# ----- 4.3. Monolinguals-En ---------------------------------------------------
# ------------------------------------------------------------------------------




# ----- 5. Tidy LexTale data ---------------------------------------------------
# ----- 5.1. Bilinguals --------------------------------------------------------
# ------------------------------------------------------------------------------

# ----- 5.2. Monolinguals-Sp ---------------------------------------------------
# ------------------------------------------------------------------------------

# ----- 5.3. Monolinguals-En ---------------------------------------------------
# ------------------------------------------------------------------------------


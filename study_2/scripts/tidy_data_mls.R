# ----- Tidy data: Monolinguals ------------------------------------------------
#
# This script contains the code used to tidy the monolingual data (Spanish and 
# English groups) for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load raw data -------------------------------------------------------
spanish_mls_raw <- read.csv(here("study_2", "raw_data", "spanish_mls_raw.csv"))

english_mls_raw <- read.csv(here("study_2", "raw_data", "english_mls_raw.csv"))
# ------------------------------------------------------------------------------


# ----- 3. Spanish -------------------------------------------------------------
# Visualize data
head(spanish_mls_raw)
glimpse(spanish_mls_raw)

# Check column index numbers
data.frame(colnames(spanish_mls_raw))

# Remove training items and distractors
spanish_mls_tidy <- spanish_mls_raw |>
  select(-c(3:4, 41:76, 113:148, 185:220))

data.frame(colnames(spanish_mls_tidy))
setdiff(colnames(spanish_mls_raw), colnames(spanish_mls_tidy))

# Remove participants who did not finish the whole experiment.
spanish_mls_tidy <- spanish_mls_tidy |>
  filter(!is.na(Q3))

# Pivot data and create variables for condition, context, verb form, rating, and
# stimuli language
spanish_mls_final <- spanish_mls_tidy |>
  pivot_longer(
    cols = c(starts_with("list") & !matches("Lista")),
    names_to = "item",
    values_to = "rating") |>
  separate(
    col = item,
    into = c("list", "condition", "context_num", "language"),
    sep = "_") |>
  separate(
    col = context_num,
    into = c("context_num", "form"),
    sep = 1) |>
  mutate(form = case_when(
    form == "a" ~ "PF", 
    form == "b" ~ "PI",
    form == "c" ~ "PP"))

spanish_mls_final <- spanish_mls_final |>
  relocate(language, list, .after = "participant") |>
  relocate(condition, context_num, form, rating, .after = "Lista")

# Convert list (character) into integers 
class(spanish_mls_final$list)
spanish_mls_final <- spanish_mls_final |>
  mutate(
    list = str_remove(list, "list")) |>
  mutate(list = as.integer(list))

# Remove lists not assigned/completed
spanish_mls_final <- spanish_mls_final |>
  filter(list == Lista)

spanish_mls_final |>
  group_by(participant, list, Lista) |>
  summarise(obs_count = n(), .groups = "drop") |>
  print(n = 35)

# Create new (i.e., manipulated) variables: temporal distance and plan certainty
spanish_mls_final <- spanish_mls_final |>
  mutate(
    temp_dist = NA,
    plan_cert = NA) |>
  relocate(temp_dist, plan_cert, .after = condition) |>
  mutate(temp_dist = if_else(
    condition %in% c("cond1", "cond4"), "immediate",
    if_else(condition %in% c("cond2", "cond5"), "near", "distant"))) |>
  mutate(plan_cert = if_else(
    condition %in% c("cond1", "cond2", "cond3"), "certain", "uncertain"))

spanish_mls_final |>
  count(condition, temp_dist, plan_cert)

# Create item column
# Convert 'context' number (character) into integer
class(spanish_mls_final$context_num)

spanish_mls_final <- spanish_mls_final |>
  mutate(
    context_num = as.integer(context_num))

spanish_mls_final <- spanish_mls_final |>
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

spanish_mls_final |>
  count(condition, temp_dist, plan_cert, item) |>
  arrange(condition, item) |>
  print(n = 36)

# Delete redundant columns ('Lista' vs 'list'; 'condition', 'context')
spanish_mls_final <- spanish_mls_final |>
  select(-c("Lista", "condition", "context_num")) 

# Scale ratings for beta regression (0-1)
spanish_mls_final <- spanish_mls_final |>
  mutate(rating_scaled = rating / 100) |>
  rename(rating_pct = rating) |>
  rename(rating = rating_scaled) |>
  select(-rating_pct) |>
  relocate(rating, .after = form)

# Adapt 0s and 1s for beta regression
spanish_mls_final <- spanish_mls_final |>
  mutate(
    rating_beta = if_else(rating == 1, 1 - 0.001,
                          if_else(rating == 0, 0 + 0.001, rating))) |>
  relocate(rating_beta, .after = rating)

any(spanish_mls_final$rating_beta == 0, na.rm = TRUE)
any(spanish_mls_final$rating_beta == 1, na.rm = TRUE)

spanish_mls_final |>
  filter(rating != rating_beta) |>
  count(rating, rating_beta)

# Rename BLP columns
spanish_mls_final <- spanish_mls_final |>
  rename(
    # Biographical Information
    age                = Q3,
    gender             = Q4,
    gender_other       = Q4_3_TEXT,
    residence_city     = Q5,
    residence_count    = Q6,
    country_origin     = Q7,
    origin_mom         = Q8,
    origin_dad         = Q9,
    education          = Q10,
    # Module 1: Language History
    age_acq_sp         = Q11_1,
    age_acq_en         = Q11_2,
    age_comfort_sp     = Q12_1,
    age_comfort_en     = Q12_2,
    years_school_sp    = Q13_1,
    years_school_en    = Q13_2,
    years_country_sp   = Q14_1,
    years_country_en   = Q14_2,
    years_family_sp    = Q15_1,
    years_family_en    = Q15_2,
    years_work_sp      = Q16_1,
    years_work_en      = Q16_2,
    # Module 2: Language Use
    use_family_sp      = Q17_1,
    use_family_en      = Q17_2,
    use_family_other   = Q17_3,
    use_friends_sp     = Q18_1,
    use_friends_en     = Q18_2,
    use_friends_other  = Q18_3,
    use_work_sp        = Q19_1,
    use_work_en        = Q19_2,
    use_work_other     = Q19_3,
    use_self_sp        = Q20_1,
    use_self_en        = Q20_2,
    use_self_other     = Q20_3,
    use_count_sp       = Q21_1,
    use_count_en       = Q21_2,
    use_count_other    = Q21_3,
    # Module 3: Language Proficiency
    prof_speak_sp      = Q22_1,
    prof_speak_en      = Q22_2,
    prof_understand_sp = Q23_1,
    prof_understand_en = Q23_2,
    prof_read_sp       = Q24_1,
    prof_read_en       = Q24_2,
    prof_write_sp      = Q25_1,
    prof_write_en      = Q25_2,
    # Module 4: Language Attitudes
    att_self_sp        = Q26_1,
    att_self_en        = Q26_2,
    att_culture_sp     = Q27_1,
    att_culture_en     = Q27_2,
    att_native_sp      = Q28_1,
    att_native_en      = Q28_2,
    att_perceived_sp   = Q29_1,
    att_perceived_en   = Q29_2)

data.frame(colnames(spanish_mls_final))

# Drop gender_other column - All participants self-identified with female/male.
spanish_mls_final |>
  count(gender_other)

spanish_mls_final <- spanish_mls_final |>
  select(-gender_other)

# Write csv for tidy Spanish monolingual data
write_csv(spanish_mls_final,
          here("study_2", "tidy_data", "spanish_mls_tidy.csv"))
# ------------------------------------------------------------------------------


# ----- 4. English -------------------------------------------------------------
# Visualize data
head(english_mls_raw)
glimpse(english_mls_raw)

# Check column index numbers
data.frame(colnames(english_mls_raw))

# Remove training items and distractors
english_mls_tidy <- english_mls_raw |>
  select(-c(3:4, 113:148))

data.frame(colnames(english_mls_tidy))
setdiff(colnames(english_mls_raw), colnames(english_mls_tidy))

# Rename some of the BLP columns to match the other datasets
english_mls_tidy <- english_mls_tidy |>
  rename(
    # Biographical Information
    gender_other       = gender_3_TEXT,
    residence_count    = residence_country,
    country_origin     = origin_self,
    education          = level_education,
    # Module 1: Language History
    age_acq_sp         = age_learning_1,
    age_acq_en         = age_learning_2,
    age_comfort_sp     = age_comfy_1,
    age_comfort_en     = age_comfy_2,
    years_school_sp    = years_classes_1,
    years_school_en    = years_classes_2,
    years_country_sp   = years_region_1,
    years_country_en   = years_region_2,
    years_family_sp    = years_family_1,
    years_family_en    = years_family_2,
    years_work_sp      = years_work_1,
    years_work_en      = years_work_2,
    # Module 2: Language Use
    use_family_sp      = lang_family_1,
    use_family_en      = lang_family_2,
    use_family_other   = lang_family_3,
    use_friends_sp     = lang_friends_1,
    use_friends_en     = lang_friends_2,
    use_friends_other  = lang_friends_3,
    use_work_sp        = lang_school_1,
    use_work_en        = lang_school_2,
    use_work_other     = lang_school_3,
    use_self_sp        = lang_self_1,
    use_self_en        = lang_self_2,
    use_self_other     = lang_self_3,
    use_count_sp       = lang_count_1,
    use_count_en       = lang_count_2,
    use_count_other    = lang_count_3,
    # Module 3: Language Proficiency
    prof_speak_sp      = speaking_1,
    prof_speak_en      = speaking_2,
    prof_understand_sp = listening_1,
    prof_understand_en = listening_2,
    prof_read_sp       = reading_1,
    prof_read_en       = reading_2,
    prof_write_sp      = writing_1,
    prof_write_en      = writing_2,
    # Module 4: Language Attitudes
    att_self_sp        = self_lang_ord_1,
    att_self_en        = self_lang_ord_2,
    att_culture_sp     = identify_culture_1,
    att_culture_en     = identify_culture_2,
    att_native_sp      = nativelikeness_imp_1,
    att_native_en      = nativelikeness_imp_2,
    att_perceived_sp   = nativelikeness_want_1,
    att_perceived_en   = nativelikeness_want_2)

data.frame(colnames(english_mls_tidy))

# Check 'gender_other' variable and collapse it with 'gender'
english_mls_tidy |>
  filter(!is.na(gender_other)) |>
  select(participant, gender, gender_other)

english_mls_tidy <- english_mls_tidy |>
  mutate(gender = case_when(
    participant == "ML_354" ~ "None",
    TRUE ~ gender)) |>
  select(-gender_other) |>
  mutate(gender = case_when(
    gender == "Female" ~ "female",
    gender == "Male" ~ "male",
    gender == "None" ~ "none",
    TRUE ~ gender))

# Fix participant ID problem: ML_258 must be ML_358
# The _2XX tag is used for Spanish monolinguals
english_mls_tidy <- english_mls_tidy %>%
  mutate(participant = if_else(participant == "ML_258", "ML_358", participant))

# Remove participants who did not finish the whole experiment (n=3) or who were 
# exposed to a language other than English during their early childhood or did 
# not grow up in the U.S. (n=42).
english_mls_tidy <- english_mls_tidy |>
  filter(!is.na(participant)) |>
  filter(!is.na(age))

english_mls_us <- english_mls_tidy |>
  filter(
    country_origin == "United States of America",
    age_acq_sp != "Since birth",
    age_acq_en == "Since birth",
    age_comfort_en == "As early as I can remember",
    years_family_en == "My whole life",
    use_family_other == "0") |>
  select(1, 111:162)

english_mls_tidy <- english_mls_tidy %>%
  filter(participant %in% english_mls_us$participant)

# Pivot data and create variables for condition, context, verb form, rating, and
# stimuli language
english_mls_final <- english_mls_tidy |>
  rename(list_assigned = List) |>
  pivot_longer(
    cols = c(starts_with("list") & !matches("list_assigned")),
    names_to = "item",
    values_to = "rating") |>
  separate(
    col = item,
    into = c("list", "condition", "context_num", "language"),
    sep = "_") |>
  separate(
    col = context_num,
    into = c("context_num", "form"),
    sep = 1) |>
  mutate(form = case_when(
    form == "a" ~ "PF", 
    form == "b" ~ "PI",
    form == "c" ~ "PP"))

english_mls_final <- english_mls_final |>
  relocate(language, list, .after = "participant") |>
  relocate(condition, context_num, form, rating, .after = "list_assigned")

# Convert list (character) into integers 
class(english_mls_final$list)
english_mls_final <- english_mls_final |>
  mutate(
    list = str_remove(list, "list")) |>
  mutate(list = as.integer(list))

# Remove lists not assigned/completed
english_mls_final <- english_mls_final |>
  filter(list == list_assigned)

english_mls_final |>
  group_by(participant, list, list_assigned) |>
  summarise(obs_count = n(), .groups = "drop") |>
  print(n = 36)

# Create new (i.e., manipulated) variables: temporal distance and plan certainty
english_mls_final <- english_mls_final |>
  mutate(
    temp_dist = NA,
    plan_cert = NA) |>
  relocate(temp_dist, plan_cert, .after = condition) |>
  mutate(temp_dist = if_else(
    condition %in% c("cond1", "cond4"), "immediate",
    if_else(condition %in% c("cond2", "cond5"), "near", "distant"))) |>
  mutate(plan_cert = if_else(
    condition %in% c("cond1", "cond2", "cond3"), "certain", "uncertain"))

english_mls_final |>
  count(condition, temp_dist, plan_cert)

# Create item column
# Convert 'context' number (character) into integer
class(english_mls_final$context_num)

english_mls_final <- english_mls_final |>
  mutate(
    context_num = as.integer(context_num))

english_mls_final <- english_mls_final |>
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

english_mls_final |>
  count(condition, temp_dist, plan_cert, item) |>
  arrange(condition, item) |>
  print(n = 36)

# Delete redundant columns ('list_assigned' vs 'list'; 'condition', 'context')
english_mls_final <- english_mls_final |>
  select(-c("list_assigned", "condition", "context_num"))

# Scale ratings for beta regression (0-1)
english_mls_final <- english_mls_final |>
  mutate(rating_scaled = rating / 100) |>
  rename(rating_pct = rating) |>
  rename(rating = rating_scaled) |>
  select(-rating_pct) |>
  relocate(rating, .after = form)

# Adapt 0s and 1s for beta regression
english_mls_final <- english_mls_final |>
  mutate(
    rating_beta = if_else(rating == 1, 1 - 0.001,
                          if_else(rating == 0, 0 + 0.001, rating))) |>
  relocate(rating_beta, .after = rating)

any(english_mls_final$rating_beta == 0, na.rm = TRUE)
any(english_mls_final$rating_beta == 1, na.rm = TRUE)

english_mls_final |>
  filter(rating != rating_beta) |>
  count(rating, rating_beta)

# Fix LexTale labels/trainings
english_mls_final <- english_mls_final |>
  rename(lextale_ablaze = lextale_abalze,
       lextale_alberation = lextale_albertion) |>
  select(-lextale_platery)

# Write csv for tidy English monolingual data
write_csv(english_mls_final,
          here("study_2", "tidy_data", "english_mls_tidy.csv"))
# ------------------------------------------------------------------------------

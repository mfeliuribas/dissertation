# ----- Tidy data: Monolinguals ------------------------------------------------
#
# This script contains the code used to tidy the monolingual data and create the 
# final datasets for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load raw data -------------------------------------------------------
spanish_mls_raw <- read.csv(here("study_2", "raw_data", "spanish_mls_raw.csv"))
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

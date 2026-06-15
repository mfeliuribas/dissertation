# ----- Tidy data: Bilinguals --------------------------------------------------
#
# This script contains the code used to tidy the bilingual data and create the 
# final dataset for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load raw data -------------------------------------------------------
experiment_bls_raw <- read.csv(here("study_2", "raw_data", 
                                "experiment_bls_raw.csv"))

questionnaire_bls_raw <- read.csv(here("study_2", "raw_data", 
                                       "questionnaire_bls_raw.csv"))

lextale_bls_raw <- read.csv(here("study_2", "raw_data", "lextale_bls_raw.csv"))
# ------------------------------------------------------------------------------


# ----- 3. Tidy data -----------------------------------------------------------
# ----- 3.1. Experiment --------------------------------------------------------

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
# completed the same session twice by mistake, or who showed inconsistent data 
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

# ----- 3.2. BLP ----------------------------------------------------------------
# Visualize data
head(questionnaire_bls_raw)
glimpse(questionnaire_bls_raw)

# Check column index numbers
data.frame(colnames(questionnaire_bls_raw))

#Q3-Q10: Biographical Info.
#Q11-Q16: Module 1 (Lang. History)
#Q17-Q21: Module 2 (Lang. Use)
#Q22-Q25: Module 3 (Lang. Proficiency)
#Q26-Q29: Module 4 (Lang. Attitudes)

# Remove participants who did not finish Session 1 (e.g. no BLP data), who 
# completed the samesession twice by mistake, or who showed inconsistent data 
# (e.g., between the responses in the interest form and the BLP)

questionnaire_bls_tidy <- questionnaire_bls_raw |>
  filter(!participant %in% c("HS_006", "HS_010","HS_019", "HS_023", "HS_024", 
                             "HS_025", "HS_026", "HS_027", "HS_028","HS_045",
                             "HS_048", "HS_055")) |>
  filter(!is.na(participant))

questionnaire_bls_tidy <- questionnaire_bls_tidy |>
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

questionnaire_bls_tidy |> 
  count(participant) |>
  filter( n > 1)

# Merge 'Lista' and 'List' values by creating a new variable, 'list_assigned'
questionnaire_bls_tidy <- questionnaire_bls_tidy |>
  mutate(list_assigned = case_when(
    !is.na(Lista) ~ Lista,
    !is.na(List) ~ List)) |>
  relocate(list_assigned, .after = "List") |>
  select(-Lista, -List)

# Rename variables/values to match experiment dataset
questionnaire_bls_tidy <- questionnaire_bls_tidy |>
  rename(list = list_assigned) |>
  rename(language = Trial) |>
  mutate(language = case_when(
    language == "Eng" ~ "en",
    language == "Span" ~ "sp"))

questionnaire_bls_tidy <- questionnaire_bls_tidy |>
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

data.frame(colnames(questionnaire_bls_tidy))

# Tidy responses in Module 1: Language History
questionnaire_bls_final <- questionnaire_bls_tidy |>
  mutate(
    age_acq_sp = case_when(
      age_acq_sp == "Since birth" ~ "0",
      TRUE ~ age_acq_sp),
    age_acq_en = case_when(
      age_acq_en == "Since birth" ~ "0",
      TRUE ~ age_acq_en),
    age_comfort_sp = case_when(
      age_comfort_sp == "As early as I can remember" ~ "0",
      age_comfort_sp == "Not yet" ~ "20",
      TRUE ~ age_comfort_sp),
    age_comfort_en = case_when(
      age_comfort_en == "As early as I can remember" ~ "0",
      age_comfort_en == "Not yet" ~ "20",
      TRUE ~ age_comfort_en),
    years_country_sp = case_when(
      years_country_sp == "My whole life" ~ "20",
      TRUE ~ years_country_sp),
    years_country_en = case_when(
      years_country_en == "My whole life" ~ "20",
      TRUE ~ years_country_en),
    years_family_sp = case_when(
      years_family_sp == "My whole life" ~ "20",
      TRUE ~ years_family_sp),
    years_family_en = case_when(
      years_family_en == "My whole life" ~ "20",
      TRUE ~ years_family_en)) |>
  mutate(across(age_acq_sp:years_work_en, as.numeric))

# Score responses in Module 1: Language History
  # 6 questions: each worth between 0 and 20
  # Each item is worth the numerical value given in the response, with three 
  # exceptions:
    # (1)The first two items (AoA and AoC) are scored in the reverse: A “20” 
    # response is worth 0, a “19” is worth 1, and so on
    # (2) Phrasal responses “Since birth” and “For as long as I can remember” 
    # are worth 20 points
    # (3) “Not yet” is worth 0 points
questionnaire_bls_final <- questionnaire_bls_final |>
  mutate(
    age_acq_sp_score     = 20 - age_acq_sp,
    age_acq_en_score     = 20 - age_acq_en,
    age_comfort_sp_score = 20 - age_comfort_sp,
    age_comfort_en_score = 20 - age_comfort_en,
    years_school_sp_score  = years_school_sp,
    years_school_en_score  = years_school_en,
    years_country_sp_score = years_country_sp,
    years_country_en_score = years_country_en,
    years_family_sp_score  = years_family_sp,
    years_family_en_score  = years_family_en,
    years_work_sp_score    = years_work_sp,
    years_work_en_score    = years_work_en) 

questionnaire_bls_final <- questionnaire_bls_final |>
  mutate(
    history_sp = rowSums(across(c(age_acq_sp_score, age_comfort_sp_score, 
                                  years_school_sp,years_country_sp, 
                                  years_family_sp, years_work_sp)), 
                         na.rm = TRUE),
    history_en = rowSums(across(c(age_acq_en_score, age_comfort_en_score, 
                                  years_school_en,years_country_en, 
                                  years_family_en, years_work_en)), 
                         na.rm = TRUE))

# Score responses in Module 2: Language Use
  # 5 questions: each worth between 0 and 10
  # Each item is worth the numerical value given in the response
questionnaire_bls_final <- questionnaire_bls_final |>
  mutate(
    use_sp = (use_family_sp + use_friends_sp + use_work_sp + 
                use_self_sp + use_count_sp) / 10,
    use_en = (use_family_en + use_friends_en + use_work_en + 
                use_self_en + use_count_en) / 10)

# Score responses in Module 3: Language Proficiency
  # 4 questions: each worth between 0 and 6
  # Each item is worth the numerical value given in the response
  # Module 3: Language Proficiency
  # 4 questions, each worth 0-6, max = 24
questionnaire_bls_final <- questionnaire_bls_final |>
  mutate(
    proficiency_sp = rowSums(across(c(prof_speak_sp, prof_understand_sp, 
                                      prof_read_sp, prof_write_sp)), 
                             na.rm = TRUE),
    proficiency_en = rowSums(across(c(prof_speak_en, prof_understand_en, 
                                      prof_read_en, prof_write_en)), 
                             na.rm = TRUE))

# Score responses in Module 4: Language Attitudes
  # 4 questions: each worth between 0 and 6
  # Each item is worth the numerical value given in the response
questionnaire_bls_final <- questionnaire_bls_final |>
  mutate(
    attitudes_sp = rowSums(across(c(att_self_sp, att_culture_sp, 
                                    att_native_sp, att_perceived_sp)), 
                           na.rm = TRUE),
    attitudes_en = rowSums(across(c(att_self_en, att_culture_en, 
                                    att_native_en, att_perceived_en)), 
                           na.rm = TRUE))

# Apply adjustment factors and calculate global score
  # To ensure that each module receives equal weighting in the global language 
  # index, you need to multiply the score for each module (each language 
  # calculated separately) by the following adjustment factors:
  # Module	Adjustment factor
  # Language History	0.454
  # Language Use	1.09
  # Language Proficiency	2.27
  # Language Attitudes	2.27
  # Add the adjusted module totals together to yield a global score for each 
  # language. Total points possible is 218.
blp_scores_bls <- questionnaire_bls_final |>
  relocate(history_sp, history_en, use_sp, use_en, proficiency_sp, 
           proficiency_en, attitudes_sp, attitudes_en, .after = education) 

blp_scores_bls <- blp_scores_bls |>
  mutate(
    global_sp = (history_sp * 0.454) + (use_sp * 1.09) + (proficiency_sp * 2.27) 
    + (attitudes_sp * 2.27),
    global_en = (history_en * 0.454) + (use_en * 1.09) + (proficiency_en * 2.27) 
    + (attitudes_en * 2.27)) |>
  relocate(global_sp, global_en, .after = education)

# Calculate dominance index
  # To obtain the language dominance index, subtract one language total from the 
  # other to render a dominance score that ranges from -218 to +218. 
  # Scores approaching zero indicate balanced bilingualism. More positive or 
  # more negative scores reflect increased language dominance.
  # More positive scores = Spanish dominant
  # More negative scores = English dominant
blp_scores_bls <- blp_scores_bls |>
  mutate(blp_score = global_sp - global_en) |>
  relocate(blp_score, .after = education)

# Prepare and write final dataset
blp_scores_bls |>
  filter(!is.na(gender_other)) |>
  select(participant, gender, gender_other)

blp_scores_bls <- blp_scores_bls |>
  mutate(gender = case_when(
    participant == "HS_047" ~ "non-binary",
    TRUE ~ gender)) |>
  select(-gender_other) |>
  mutate(gender = case_when(
    gender == "Female" ~ "female",
    gender == "Male" ~ "male",
    TRUE ~ gender))

blp_final_bls <- blp_scores_bls |>
  select(-c(residence_city, residence_count, history_sp:years_work_en_score)) |>
  write_csv(here("study_2", "tidy_data", "questionnaire_bls_tidy.csv"))
# ------------------------------------------------------------------------------

# ----- 3.3. LexTale -----------------------------------------------------------
# Visualize data
head(lextale_bls_raw)
glimpse(lextale_bls_raw)

# Check column index numbers
data.frame(colnames(lextale_bls_raw))

# Remove participants who did not finish Session 1 (e.g. no BLP data), who 
# completed the samesession twice by mistake, or who showed inconsistent data 
# (e.g., between the responses in the interest form and the BLP)

lextale_bls_tidy <- lextale_bls_raw |>
  filter(!participant %in% c("HS_006", "HS_010","HS_019", "HS_023", "HS_024", 
                             "HS_025", "HS_026", "HS_027", "HS_028","HS_045",
                             "HS_048", "HS_055")) |>
  filter(!is.na(participant))

lextale_bls_tidy <- lextale_bls_tidy |>
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

lextale_bls_tidy |> 
  count(participant)

# Merge 'Lista' and 'List' values by creating a new variable, 'list_assigned'
lextale_bls_tidy <- lextale_bls_tidy |>
  mutate(list_assigned = case_when(
    !is.na(Lista) ~ Lista,
    !is.na(List) ~ List)) |>
  relocate(list_assigned, .after = "List") |>
  select(-Lista, -List)

# Rename variables/values to match experiment dataset
lextale_bls_tidy <- lextale_bls_tidy |>
  rename(list = list_assigned) |>
  rename(language = Trial) |>
  mutate(language = case_when(
    language == "Eng" ~ "en",
    language == "Span" ~ "sp"))

# Create Spanish subset
lextale_bls_sp <- lextale_bls_tidy |>
  filter(language == "sp") |>
  select(-c(lextale_mensible:lextale_wrought)) |>
  rename_with(~ str_remove(., "^lextale_"), starts_with("lextale_"))

data.frame(colnames(lextale_bls_sp))

# Tidy Spanish subset
lextale_bls_sp <- lextale_bls_sp |>
  pivot_longer(
    cols = -c(participant, session, language, list),
    names_to = "item",
    values_to = "response") |>
  mutate(response = case_when(
    response == "word" ~ 1, 
    response == "nonword" ~ 0))

# Correct responses (LexTale-Esp)
# Answer key: 0 = nonword; 1 = word
correct_responses <- c(
  "terzo" = 0, "pellizcar" = 1, "pulmones" = 1, "batillon" = 0, "zapato" = 1, 
  "tergiversar" = 1, "pesimo" = 1, "cadena" = 0, "hacha" = 1, "antar" = 0, 
  "cenefa" = 1, "asesinato" = 1, "helar" = 1, "yunque" = 1, "regar" = 1, 
  "abracer" = 0, "floroso" = 0, "arsa" = 0, "brecedad" = 0, "avido" = 1, 
  "capillo" = 0, "lacayo" = 1, "lampera" = 0, "latigo" = 1, "bisagra" = 1, 
  "secuestro" = 1, "acutacion" = 0, "merodear" = 1, "decar" = 0, "alardio" = 0, 
  "pandilla" = 1, "fatacidad" = 0, "pauca" = 0, "aviso" = 1, "rompido" = 0, 
  "loro" = 1, "granuja" = 1, "estornudar" = 1, "torpe" = 1, "alfombra" = 1, 
  "rebuscar" = 1, "cadallo" = 0, "canela" = 1, "cuchara" = 1, "jilguero" = 1, 
  "martillo" = 1, "cartinar" = 0, "ladron" = 1, "ganar" = 1, "flamida" = 0, 
  "candado" = 1, "camisa" = 1, "vegada" = 0, "fomentar" = 1, "nevar" = 1, 
  "musgo" = 1, "tacano" = 1, "plaudir" = 0, "besar" = 1, "matar" = 1, 
  "seda" = 1, "flaco" = 1, "esposante" = 0, "orgulloso" = 1, "bizcocho" = 1, 
  "hacido" = 0, "cabello" = 1, "alegre" = 1, "engatusar" = 1, "temblo" = 0, 
  "polvoriento" = 1, "pemicion" = 0, "hervidor" = 1, "cintro" = 0, "yacer" = 1, 
  "atar" = 1, "tiburon" = 1, "frondoso" = 1, "tropaje" = 0, "hormiga" = 1, 
  "pozo" = 1, "empirador" = 0, "guante" = 1, "escuto" = 0, "laud" = 1, 
  "barato" = 1, "grodo" = 0, "acantilado" = 1, "prisa" = 1, "clavel" = 1)

lextale_answer_key_sp <- tibble(
  item = names(correct_responses),
  is_real = correct_responses)

# Merge participant responses and correct responses (LexTale-Esp)
lextale_bl_sp_final <- left_join (
  lextale_bls_sp,
  lextale_answer_key_sp,
  by = "item")

# Check if participant was correct or not (LexTale-Esp)
# 1 = correct (match between participant and answer key)
# 0 = incorrect (mismatch between participant and answer key)
lextale_bl_sp_final <- lextale_bl_sp_final |>
  mutate(is_correct = case_when(
    is_real == 1 & response == 1 ~ 1,
    is_real == 0 & response == 0 ~ 1,
    TRUE ~ 0))

# Summarize responses per participant (LexTale-Esp)
# word_correct = word exists and participant said it exists (match)
# word_incorrect = word exists but participant said it doesn't exist (mismatch)
# nonword_correct = word doesn't exist and participant said it doesn't exist (match)
# nonword_incorrect = word doesn't exist but participant said it exists (mismatch)
lextale_bl_sp_scores <- lextale_bl_sp_final |>
  group_by(participant, session, language, list) |>
  summarize(
    total_items = n(),
    word_correct = sum(is_real == 1 & is_correct == 1, na.rm = TRUE),
    word_incorrect = sum(is_real == 1 & is_correct == 0, na.rm = TRUE),
    nonword_correct = sum(is_real == 0 & is_correct == 1, na.rm = TRUE),
    nonword_incorrect = sum(is_real == 0 & is_correct == 0, na.rm = TRUE),
    .groups = "drop") |>
  relocate(total_items, .after = nonword_incorrect)

# Score LexTale-Esp - Method 1 ("traditional method")
# Formula: score = (word_correct) − (2 * nonword_incorrect)
lextale_bl_sp_scores <- lextale_bl_sp_scores |>
  mutate(lextale_traditional = word_correct - 2 * nonword_incorrect)

# Score LexTale-Esp - Method 2 ("averaged % correct")
# Formula:
# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2
# The percentage of correct responses, corrected for the unequal proportion of 
# words and nonwords in the test by averaging the percentages correct for these 
# two item types. 
lextale_bl_sp_scores <- lextale_bl_sp_scores |>
  mutate(
    total_word = word_correct + word_incorrect,
    total_nonword = nonword_correct + nonword_incorrect,
    pct_word = (word_correct / total_word) * 100,
    pct_nonword = (nonword_correct / total_nonword) * 100,
    lextale_avg = (pct_word + pct_nonword) / 2)

# Tidy final scores (LexTale-Esp)
lextale_bl_sp_scores_final <- lextale_bl_sp_scores |>
  relocate(lextale_traditional, lextale_avg, .after = list) |>
  mutate(
    lextale_avg = round(lextale_avg, 2)) |>
  rename(lextale_trad_sp = lextale_traditional,
         lextale_avg_sp = lextale_avg) |>
  select(participant:lextale_avg_sp) |>
  write_csv(here("study_2", "tidy_data", "lextale_sp_bls_tidy.csv"))

# Create English subset
lextale_bls_en <- lextale_bls_tidy |>
  filter(language == "en") |>
  select(-c(lextale_terzo:lextale_clavel)) |>
  rename_with(~ str_remove(., "^lextale_"), starts_with("lextale_")) |>
  rename(ablaze = abalze,
         alberation = albertion) |>
  select(-platery)

data.frame(colnames(lextale_bls_en))

# Tidy English subset
lextale_bls_en <- lextale_bls_en |>
  pivot_longer(
    cols = -c(participant, session, language, list),
    names_to = "item",
    values_to = "response") |>
    mutate(response = case_when(
    response == "word" ~ 1, 
    response == "nonword" ~ 0))

# Correct responses (LexTale-English)
# Answer key: 0 = nonword; 1 = word
# For some reason 'breeding' was not recorded in any participant (bls/mls). 
# Check what the issue might have been and acknowledge it in the methods section.
# Also, adjust the scores formula accordingly. 
correct_responses_en <- c(
  "mensible" = 0, "scornful" = 1, "stoutly" = 1, "ablaze" = 1, "kermshaw" = 0, 
  "moonlit" = 1, "lofty" = 1, "hurricane" = 1, "flaw" = 1, "alberation" = 0, 
  "unkempt" = 1, "festivity" = 1, "screech" = 1, "savory" = 1, "plaudate" = 0, 
  "shin" = 1, "fluid" = 1, "spaunch" = 0, "allied" = 1, "slain" = 1, 
  "recipient" = 1, "exprate" = 0, "eloquence" = 1, "cleanliness" = 1,
  "dispatch" = 1, "rebondicate" = 0, "ingenious" = 1,"bewitch" = 1, "skave" = 0, 
  "plaintively" = 1, "kilp" = 0, "interfate" = 0, "hasty" = 1, "lengthy" = 1, 
  "fray" = 1, "crumper" = 0, "upkeep" = 1, "majestic" = 1, "magrity" = 0, 
  "nourishment" = 1, "abergy" = 0, "proom" = 0, "turmoil" = 1, 
  "carbohydrate" = 1, "scholar" = 1, "turtle" = 1, "fellick" = 0, 
  "destription" = 0, "cylinder" = 1, "censorship" = 1, "celestial" = 1, 
  "rascal" = 1, "purrage" = 0, "pulsh" = 0, "muddy" = 1, "quirty" = 0, 
  "pudour" = 0, "listless" = 1, "wrought" = 1)

lextale_answer_key_en <- tibble(
  item = names(correct_responses_en),
  is_real = correct_responses_en)

# Merge participant responses and correct responses (LexTale-English)
lextale_bl_en_final <- left_join (
  lextale_bls_en,
  lextale_answer_key_en,
  by = "item")

# Check if participant was correct or not (LexTale-English)
# 1 = correct (match between participant and answer key)
# 0 = incorrect (mismatch between participant and answer key)
lextale_bl_en_final <- lextale_bl_en_final |>
  mutate(is_correct = case_when(
    is_real == 1 & response == 1 ~ 1,
    is_real == 0 & response == 0 ~ 1,
    TRUE ~ 0))

# Summarize responses per participant (LexTale-English)
# word_correct = word exists and participant said it exists (match)
# word_incorrect = word exists but participant said it doesn't exist (mismatch)
# nonword_correct = word doesn't exist and participant said it doesn't exist (match)
# nonword_incorrect = word doesn't exist but participant said it exists (mismatch)
lextale_bl_en_scores <- lextale_bl_en_final |>
  group_by(participant, session, language, list) |>
  summarize(
    total_items = n(),
    word_correct = sum(is_real == 1 & is_correct == 1, na.rm = TRUE),
    word_incorrect = sum(is_real == 1 & is_correct == 0, na.rm = TRUE),
    nonword_correct = sum(is_real == 0 & is_correct == 1, na.rm = TRUE),
    nonword_incorrect = sum(is_real == 0 & is_correct == 0, na.rm = TRUE),
    .groups = "drop") |>
  relocate(total_items, .after = nonword_incorrect)

# Score LexTale-English - Method 1 ("traditional method")
# Formula: score = (word_correct) − (2 * nonword_incorrect)
lextale_bl_en_scores <- lextale_bl_en_scores |>
  mutate(lextale_traditional = word_correct - 2 * nonword_incorrect)

# Score LexTale-English - Method 2 ("averaged % correct")
# Formula:
# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2
# The percentage of correct responses, corrected for the unequal proportion of 
# words and nonwords in the test by averaging the percentages correct for these 
# two item types. 
lextale_bl_en_scores <- lextale_bl_en_scores |>
  mutate(
    total_word = word_correct + word_incorrect,
    total_nonword = nonword_correct + nonword_incorrect,
    pct_word = (word_correct / total_word) * 100,
    pct_nonword = (nonword_correct / total_nonword) * 100,
    lextale_avg = (pct_word + pct_nonword) / 2)

# Tidy final scores (LexTale-English)
lextale_bl_en_scores_final <- lextale_bl_en_scores |>
  relocate(lextale_traditional, lextale_avg, .after = list) |>
  mutate(
    lextale_avg = round(lextale_avg, 2)) |>
  rename(lextale_trad_en = lextale_traditional,
         lextale_avg_en = lextale_avg) |>
  select(participant:lextale_avg_en) |>
  write_csv(here("study_2", "tidy_data", "lextale_en_bls_tidy.csv"))

# Create final dataset for bilingual scores (LexTale-Esp + English)
lextale_sp_bls <- read.csv(here("study_2", "tidy_data", 
                                "lextale_sp_bls_tidy.csv"))
lextale_en_bls <- read.csv(here("study_2", "tidy_data", 
                                "lextale_en_bls_tidy.csv"))

lextale_bls_tidy <- bind_rows(
  lextale_sp_bls,
  lextale_en_bls) |>
  arrange(participant, session)

lextale_bls_final <- lextale_bls_tidy |>
  mutate(
    lextale_trad = coalesce(lextale_trad_sp, lextale_trad_en),
    lextale_avg = coalesce(lextale_avg_sp, lextale_avg_en)) |>
  select(-lextale_trad_sp, -lextale_trad_en, -lextale_avg_sp, -lextale_avg_en) |>
  write_csv(here("study_2", "tidy_data", "lextale_bls_tidy.csv"))
# ------------------------------------------------------------------------------

# ----- Do descriptive stats ---------------------------------------------------
#
# This script contains the code used to do descriptive stats for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
library("ggplot2")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
data_bls <- read.csv(here("study_2", "final_data", "bls_final.csv"))
data_blp_raw <- read.csv(here("study_2", "raw_data", "questionnaire_bls_raw.csv"))
data_mls_sp <- read.csv(here("study_2", "tidy_data", "spanish_mls_tidy.csv"))
data_mls_en <- read.csv(here("study_2", "tidy_data", "english_mls_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 3. Descriptive stats for BLs -------------------------------------------
# Total number of participants
data_bls |>
  summarize(total_participants = n_distinct(participant))

data_bls |>
  group_by(participant) %>%
  filter(n_distinct(language) == 1) %>%
  summarise(language = first(language))

# Obs:
  # A total of 89 bilinguals were included in the final dataset. However, of 
  # them, five only completed the English version and six only completed the 
  # Spanish version.

# Family origin
data_bls |>
  group_by(participant) |>
  summarize(
    origin_mom = first(origin_mom),
    origin_dad = first(origin_dad))

data_bls |>
  group_by(participant) |>
  summarize(
    origin_mom = first(origin_mom)) |>
  count(origin_mom, sort = TRUE) 

data_bls |>
  group_by(participant) |>
  summarize(
    origin_dad = first(origin_dad)) |>
  count(origin_dad, sort = TRUE) 

data_bls |>
  group_by(participant) |>
  summarize(origin_mom = first(origin_mom),
            origin_dad = first(origin_dad),
            .groups = "drop") |>
  pivot_longer(c(origin_mom, origin_dad), values_to = "country") |>
  filter(!is.na(country)) |>
  distinct(participant, country) |>
  count(country, sort = TRUE) |>
  mutate(pct = round(100 * n / n_distinct(data_bls$participant), 1))

# Origin self
data_bls |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

data_bls |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  mutate(
    born = if_else(
      country_origin == "United States of America", "USA", "Abroad")) |>
  count(born)

# Obs:
  # 68 participants were born in the U.S.
  # 21 were born abroad.

# Self-reported gender
data_bls |>
  group_by(participant) |>
  summarize(gender = first(gender)) |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

# Obs:
  # Female = 66 (74.2%)
  # Male = 22 (24.7%)
  # Non-binary = 1 (1.1%)

# Age
data_bls |>
  group_by(participant) |>
  summarize(age = first(age)) |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

# Obs:
  # Range: 18-47
  # Mean: 20.9
  # SD: 4.40

# Language dominance
data_bls |>
  group_by(participant) |>
  summarize(blp_score = first(blp_score)) |>
  summarize(
    mean_blp_score = mean(blp_score),
    sd_blp_score   = sd(blp_score),
    min_blp_score  = min(blp_score),
    max_blp_score  = max(blp_score))

# Obs:
  # Range: -155 to 53.9
  # Mean: -53.4
  # SD: 41.6

# AoA
data_blp <- data_blp_raw |>
  filter(!participant %in% c("HS_006", "HS_010","HS_019", "HS_023", "HS_024", 
                             "HS_025", "HS_026", "HS_027", "HS_028","HS_045",
                             "HS_048", "HS_055")) |>
  filter(!is.na(participant))

data_blp <- data_blp |>
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

data_blp <- data_blp |>
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

data.frame(colnames(data_blp))

data_blp |>
  group_by(participant) |>
  summarize(age_acq_sp = first(age_acq_sp), .groups = "drop") |>
  count(age_acq_sp, sort = TRUE)

data_blp |>
  group_by(participant) |>
  summarize(age_acq_en = first(age_acq_en), .groups = "drop") |>
  count(age_acq_en, sort = TRUE)

data_blp |>
  group_by(participant) |>
  summarize(
    age_acq_sp = first(age_acq_sp),
    age_acq_en = first(age_acq_en),
    .groups = "drop") |>
  mutate(
    age_acq_sp = if_else(age_acq_sp == "Since birth", "0", age_acq_sp),
    age_acq_en = if_else(age_acq_en == "Since birth", "0", age_acq_en),
    age_acq_sp = as.numeric(age_acq_sp),
    age_acq_en = as.numeric(age_acq_en)) |>
  filter(age_acq_sp <= 8 & age_acq_en <= 8)

aoa_bls <- data_blp |>
  group_by(participant) |>
  summarize(
    age_acq_sp = first(age_acq_sp),
    age_acq_en = first(age_acq_en),
    .groups = "drop") |>
  mutate(
    age_acq_sp = if_else(age_acq_sp == "Since birth", "0", age_acq_sp),
    age_acq_en = if_else(age_acq_en == "Since birth", "0", age_acq_en),
    age_acq_sp = as.numeric(age_acq_sp),
    age_acq_en = as.numeric(age_acq_en)) |>
  mutate(both_before_9 = age_acq_sp <= 9 & age_acq_en <= 9)

aoa_bls |> 
  filter(!both_before_9 | is.na(both_before_9))

# Obs:
  # The final dataset have to be re-done. 
  # HS_017, HS_030, and HS_099 report learning English after the age of 11. 
  # HS_039, HS_042, HS_071, HS_073, HS_087, HS_100 report learning Spanish after 
    # age 11. However, I feel that they interpreted this question as taking 
    # classes, because they report spending their whole lives in a family where 
    # Spanish is spoken.
# ------------------------------------------------------------------------------



# ----- 4. Descriptive stats for MLs -------------------------------------------
# Total number of participants
data_mls_sp |>
  summarize(total_participants = n_distinct(participant))

data_mls_en |>
  summarize(total_participants = n_distinct(participant))

# Obs:
  # A total of 35 Spanish monolinguals participated. 
  # A total of 36 Spanish monolinguals participated. 

# Origin self
data_mls_sp |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

data_mls_en |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

# Self-reported gender
data_mls_sp |>
  group_by(participant) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

data_mls_en |>
  group_by(participant) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

# Obs:
  # Female = 27 (77.1%) -- SP; 25 (69.4%) -- EN
  # Male = 8 (22.9%) -- SP; 10 (27.8%) -- EN
  # Other = 1 (2.8%) -- EN

# Age
data_mls_sp |>
  group_by(participant) |>
  summarize(age = first(age), .groups = "drop") |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

data_mls_en |>
  group_by(participant) |>
  summarize(age = first(age), .groups = "drop") |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

# Obs:
  # Range: 18-32 -- SP; 18-41 -- EN
  # Mean: 24.1 -- SP; 20.4 -- EN
  # SD: 3.71 -- SP; 4.32 -- EN

# Spanish monolinguals: Language
data_mls_sp |>
  group_by(participant) |>
  summarize(
    age_acq_sp = first(age_acq_sp),
    age_comfort_sp = first(age_comfort_sp), 
    .groups = "drop") |>
  count(age_acq_sp, age_comfort_sp, sort = TRUE)

# Obs: 
  # All Spanish monolinguals reported speaking Spanish and feeling comfortable 
  # with this language since birth.

data_mls_sp |>
  group_by(participant) |>
  summarize(age_acq_en = first(age_acq_en)) |>
  summarize(
    mean_age_acq_en = mean(age_acq_en),
    sd_age_acq_en   = sd(age_acq_en),
    min_age_acq_en  = min(age_acq_en),
    max_age_acq_en  = max(age_acq_en))

# Obs: 
  # All Spanish monolinguals started learning English when they started school. 
  # Range: 3-20 
  # Mean: 8.97 
  # SD: 4.29 

data_mls_sp |>
  group_by(participant) |>
  summarize(
    prof_speak_en      = first(prof_speak_en),
    prof_understand_en = first(prof_understand_en),
    prof_read_en       = first(prof_read_en),
    prof_write_en      = first(prof_write_en),
    .groups = "drop") |>
  summarize(
    mean_speak  = mean(prof_speak_en, na.rm = TRUE),
    sd_speak    = sd(prof_speak_en, na.rm = TRUE),
    mean_listen = mean(prof_understand_en, na.rm = TRUE),
    sd_listen   = sd(prof_understand_en, na.rm = TRUE),
    mean_read   = mean(prof_read_en, na.rm = TRUE),
    sd_read     = sd(prof_read_en, na.rm = TRUE),
    mean_write  = mean(prof_write_en, na.rm = TRUE),
    sd_write    = sd(prof_write_en, na.rm = TRUE))

# Obs:
  # 4.03 for speaking (SD = 1.04)
  # 4.57 for listening (SD = 0.655)
  # 4.63 for reading (SD = 0.877)
  # 4.2 for writing (SD = 1.05)

data_mls_sp |>
  group_by(participant) |>
  summarize(years_country_en = first(years_country_en)) |>
  summarize(
    mean_years_country_en = mean(years_country_en),
    sd_years_country_en   = sd(years_country_en),
    min_years_country_en  = min(years_country_en),
    max_years_country_en  = max(years_country_en))

data_mls_sp |>
  group_by(participant) |>
  summarize(years_country_en = first(years_country_en), .groups = "drop") |>
  mutate(
    abroad = if_else(years_country_en > 0, "Some time abroad", "Never")) |>
  count(abroad)

# Obs: 
  # 13 reported having lived some time abroad, whereas 22 have never.
  # Range: 0-5 
  # Mean: 1.03 
  # SD: 1.60 

# BLP Scores (code adapted from `tidy_data_bls.R`)
data.frame(colnames(data_mls_sp))

blp_mls_sp <- data_mls_sp |>
  select(1, 9:59) 

blp_mls_sp <- blp_mls_sp |>
  mutate(
    age_acq_sp = case_when(
      age_acq_sp == "Since birth" ~ 0,
      TRUE ~ as.numeric(age_acq_sp)),
    age_acq_en = case_when(
      age_acq_en == "Since birth" ~ 0,
      TRUE ~ as.numeric(age_acq_en)),
    age_comfort_sp = case_when(
      age_comfort_sp == "As early as I can remember" ~ 0,
      age_comfort_sp == "Not yet" ~ 20,
      TRUE ~ as.numeric(age_comfort_sp)),
    age_comfort_en = case_when(
      age_comfort_en == "As early as I can remember" ~ 0,
      age_comfort_en == "Not yet" ~ 20,
      TRUE ~ as.numeric(age_comfort_en)),
    years_country_sp = case_when(
      years_country_sp == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_country_sp)),
    years_country_en = case_when(
      years_country_en == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_country_en)),
    years_family_sp = case_when(
      years_family_sp == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_family_sp)),
    years_family_en = case_when(
      years_family_en == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_family_en)))

blp_mls_sp <- blp_mls_sp|>
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

blp_mls_sp <- blp_mls_sp |>
  mutate(
    history_sp = rowSums(across(c(age_acq_sp_score, age_comfort_sp_score, 
                                  years_school_sp,years_country_sp, 
                                  years_family_sp, years_work_sp)), 
                         na.rm = TRUE),
    history_en = rowSums(across(c(age_acq_en_score, age_comfort_en_score, 
                                  years_school_en,years_country_en, 
                                  years_family_en, years_work_en)), 
                         na.rm = TRUE))

blp_mls_sp <- blp_mls_sp |>
  mutate(
    use_sp = (use_family_sp + use_friends_sp + use_work_sp + 
                use_self_sp + use_count_sp) / 10,
    use_en = (use_family_en + use_friends_en + use_work_en + 
                use_self_en + use_count_en) / 10)

blp_mls_sp <- blp_mls_sp |>
  mutate(
    proficiency_sp = rowSums(across(c(prof_speak_sp, prof_understand_sp, 
                                      prof_read_sp, prof_write_sp)), 
                             na.rm = TRUE),
    proficiency_en = rowSums(across(c(prof_speak_en, prof_understand_en, 
                                      prof_read_en, prof_write_en)), 
                             na.rm = TRUE))

blp_mls_sp <- blp_mls_sp |>
  mutate(
    attitudes_sp = rowSums(across(c(att_self_sp, att_culture_sp, 
                                    att_native_sp, att_perceived_sp)), 
                           na.rm = TRUE),
    attitudes_en = rowSums(across(c(att_self_en, att_culture_en, 
                                    att_native_en, att_perceived_en)), 
                           na.rm = TRUE))

blp_scores_mls_sp <- blp_mls_sp |>
  relocate(history_sp, history_en, use_sp, use_en, proficiency_sp, 
           proficiency_en, attitudes_sp, attitudes_en, .after = education) 

blp_scores_mls_sp <- blp_scores_mls_sp |>
  mutate(
    global_sp = (history_sp * 0.454) + (use_sp * 1.09) + (proficiency_sp * 2.27) 
    + (attitudes_sp * 2.27),
    global_en = (history_en * 0.454) + (use_en * 1.09) + (proficiency_en * 2.27) 
    + (attitudes_en * 2.27)) |>
  relocate(global_sp, global_en, .after = education)

blp_scores_mls_sp <- blp_scores_mls_sp |>
  mutate(blp_score = global_sp - global_en) |>
  relocate(blp_score, .after = education)

blp_scores_mls_sp |>
  group_by(participant) |>
  summarize(blp_score = first(blp_score)) |>
  summarize(
    mean_blp_score = mean(blp_score),
    sd_blp_score   = sd(blp_score),
    min_blp_score  = min(blp_score),
    max_blp_score  = max(blp_score))

# Obs:
  # Range: 85.6 to 179
  # Mean: 188
  # SD: 22.1

# LexTale Scores







# English monolinguals: Language
data_mls_en |>
  group_by(participant) |>
  summarize(
    age_acq_en = first(age_acq_en),
    age_comfort_en = first(age_comfort_en), 
    .groups = "drop") |>
  count(age_acq_en, age_comfort_en, sort = TRUE)

# Obs: 
# All English monolinguals reported speaking English and feeling comfortable 
# with this language since birth.

data_mls_en |>
  group_by(participant) |>
  summarize(age_acq_sp = first(age_acq_sp)) |>
  summarize(
    mean_age_acq_sp = mean(age_acq_sp),
    sd_age_acq_sp   = sd(age_acq_sp),
    min_age_acq_sp  = min(age_acq_sp),
    max_age_acq_sp  = max(age_acq_sp))

# Obs: 
  # All English monolinguals started learning Spanish around high school. 
  # Range: 11-20 
  # Mean: 16.1 
  # SD: 2.44 

data_mls_en |>
  group_by(participant) |>
  summarize(
    prof_speak_sp      = first(prof_speak_sp),
    prof_understand_sp = first(prof_understand_sp),
    prof_read_sp       = first(prof_read_sp),
    prof_write_sp      = first(prof_write_sp),
    .groups = "drop") |>
  summarize(
    mean_speak  = mean(prof_speak_sp, na.rm = TRUE),
    sd_speak    = sd(prof_speak_sp, na.rm = TRUE),
    mean_listen = mean(prof_understand_sp, na.rm = TRUE),
    sd_listen   = sd(prof_understand_sp, na.rm = TRUE),
    mean_read   = mean(prof_read_sp, na.rm = TRUE),
    sd_read     = sd(prof_read_sp, na.rm = TRUE),
    mean_write  = mean(prof_write_sp, na.rm = TRUE),
    sd_write    = sd(prof_write_sp, na.rm = TRUE))

# Obs:
# 0.861 for speaking (SD = 0.351)
# 1.14 for listening (SD = 0.351)
# 0.944 for reading (SD = 0.333)
# 0.889 for writing (SD = 0.319)

# BLP Scores (code adapted from `tidy_data_bls.R`)
data.frame(colnames(data_mls_en))

blp_mls_en <- data_mls_en |>
  select(1, 9:59) 

blp_mls_en <- blp_mls_en |>
  mutate(
    age_acq_sp = case_when(
      age_acq_sp == "Since birth" ~ 0,
      TRUE ~ as.numeric(age_acq_sp)),
    age_acq_en = case_when(
      age_acq_en == "Since birth" ~ 0,
      TRUE ~ as.numeric(age_acq_en)),
    age_comfort_sp = case_when(
      age_comfort_sp == "As early as I can remember" ~ 0,
      age_comfort_sp == "Not yet" ~ 20,
      TRUE ~ as.numeric(age_comfort_sp)),
    age_comfort_en = case_when(
      age_comfort_en == "As early as I can remember" ~ 0,
      age_comfort_en == "Not yet" ~ 20,
      TRUE ~ as.numeric(age_comfort_en)),
    years_country_sp = case_when(
      years_country_sp == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_country_sp)),
    years_country_en = case_when(
      years_country_en == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_country_en)),
    years_family_sp = case_when(
      years_family_sp == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_family_sp)),
    years_family_en = case_when(
      years_family_en == "My whole life" ~ 20,
      TRUE ~ as.numeric(years_family_en)))

blp_mls_en <- blp_mls_en|>
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

blp_mls_en <- blp_mls_en |>
  mutate(
    history_sp = rowSums(across(c(age_acq_sp_score, age_comfort_sp_score, 
                                  years_school_sp,years_country_sp, 
                                  years_family_sp, years_work_sp)), 
                         na.rm = TRUE),
    history_en = rowSums(across(c(age_acq_en_score, age_comfort_en_score, 
                                  years_school_en,years_country_en, 
                                  years_family_en, years_work_en)), 
                         na.rm = TRUE))

blp_mls_en <- blp_mls_en |>
  mutate(
    use_sp = (use_family_sp + use_friends_sp + use_work_sp + 
                use_self_sp + use_count_sp) / 10,
    use_en = (use_family_en + use_friends_en + use_work_en + 
                use_self_en + use_count_en) / 10)

blp_mls_en <- blp_mls_en |>
  mutate(
    proficiency_sp = rowSums(across(c(prof_speak_sp, prof_understand_sp, 
                                      prof_read_sp, prof_write_sp)), 
                             na.rm = TRUE),
    proficiency_en = rowSums(across(c(prof_speak_en, prof_understand_en, 
                                      prof_read_en, prof_write_en)), 
                             na.rm = TRUE))

blp_mls_en <- blp_mls_en |>
  mutate(
    attitudes_sp = rowSums(across(c(att_self_sp, att_culture_sp, 
                                    att_native_sp, att_perceived_sp)), 
                           na.rm = TRUE),
    attitudes_en = rowSums(across(c(att_self_en, att_culture_en, 
                                    att_native_en, att_perceived_en)), 
                           na.rm = TRUE))

blp_scores_mls_en <- blp_mls_en |>
  relocate(history_sp, history_en, use_sp, use_en, proficiency_sp, 
           proficiency_en, attitudes_sp, attitudes_en, .after = education) 

blp_scores_mls_en <- blp_scores_mls_en |>
  mutate(
    global_sp = (history_sp * 0.454) + (use_sp * 1.09) + (proficiency_sp * 2.27) 
    + (attitudes_sp * 2.27),
    global_en = (history_en * 0.454) + (use_en * 1.09) + (proficiency_en * 2.27) 
    + (attitudes_en * 2.27)) |>
  relocate(global_sp, global_en, .after = education)

blp_scores_mls_en <- blp_scores_mls_en |>
  mutate(blp_score = global_sp - global_en) |>
  relocate(blp_score, .after = education)

blp_scores_mls_en |>
  group_by(participant) |>
  summarize(blp_score = first(blp_score)) |>
  summarize(
    mean_blp_score = mean(blp_score),
    sd_blp_score   = sd(blp_score),
    min_blp_score  = min(blp_score),
    max_blp_score  = max(blp_score))

# Obs:
  # Range: -198 to -147
  # Mean: -172
  # SD: 9.02

# LexTale


# ------------------------------------------------------------------------------



# ----- 5. Check BL data (Austin) ----------------------------------------------
# Check participant behavior
data_bls |>
  filter(is.na(rating)) |>
  count(participant)

data_bls |>
  group_by(participant) |>
  summarize(
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    min_rating = min(rating, na.rm = TRUE),
    max_rating = max(rating, na.rm = TRUE)) |>
  arrange(sd_rating) |>
  print(n=89)

# Data distribution
df <- subset(data_bls, form == "PP")

# Pre-compute stats per language
# Subset to rows where form == "PP"
pp <- df[df$form == "PP", ]

# Compute summary stats
m  <- mean(pp$rating, na.rm = TRUE)
md <- median(pp$rating, na.rm = TRUE)

# Histogram
hist(pp$rating,
     breaks = seq(0, 1, by = 0.1),   # rating ranges 0 to 1
     col    = "lightblue",
     border = "white",
     main   = "Histogram of Ratings (form = PP)",
     xlab   = "Rating",
     xlim   = c(0, 1))

# Vertical lines at mean and median
abline(v = m,  col = "red",  lwd = 2, lty = 2)
abline(v = md, col = "darkgreen", lwd = 2, lty = 3)

# Legend
legend("topright",
       legend = c(paste0("Mean = ",   round(m, 3)),
                  paste0("Median = ", round(md, 3))),
       col    = c("red", "darkgreen"),
       lwd    = 2,
       lty    = c(2, 3),
       bty    = "n")


library(purrr)
frm = "PF"
pp  <- subset(data_bls, form == frm)
bw  <- 0.1
set.seed(42)

# Pre-compute proportions within each language
pp_props <- pp %>%
  mutate(rating_bin = floor(rating / bw) * bw) %>%
  group_by(language, rating_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(language) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Observed + null stats per language
stats <- pp %>%
  group_by(language) %>%
  summarise(
    n            = n(),
    Obs_Mean     = mean(rating, na.rm = TRUE),
    Obs_Median   = median(rating, na.rm = TRUE),
    # simulate null: n random draws from Uniform(0,1)
    Null_Mean    = map_dbl(n, ~ mean(runif(.x, 0, 1))),
    Null_Median  = map_dbl(n, ~ median(runif(.x, 0, 1))),
    .groups      = "drop"
  ) %>%
  mutate(label = paste0(
    "Obs Mean = ",    round(Obs_Mean,   3),
    "\nObs Median = ", round(Obs_Median, 3),
    "\nNull Mean = ",  round(Null_Mean,  3),
    "\nNull Median = ", round(Null_Median, 3)
  ))

ggplot(pp_props, aes(x = rating_bin, y = prop)) +
  geom_col(fill = "lightblue", color = "white", width = bw * 0.9) +
  # Observed lines
  geom_vline(data = stats, aes(xintercept = Obs_Mean,    color = "Obs Mean"),
             linetype = "dashed",  linewidth = 1) +
  geom_vline(data = stats, aes(xintercept = Obs_Median,  color = "Obs Median"),
             linetype = "dotted",  linewidth = 1) +
  # Null lines
  geom_vline(data = stats, aes(xintercept = Null_Mean,   color = "Null Mean"),
             linetype = "dashed",  linewidth = 1) +
  geom_vline(data = stats, aes(xintercept = Null_Median, color = "Null Median"),
             linetype = "dotted",  linewidth = 1) +
  geom_text(data = stats, aes(x = Inf, y = Inf, label = label),
            hjust = 1.05, vjust = 1.2, size = 3, lineheight = 0.95,
            inherit.aes = FALSE) +
  scale_color_manual(
    name   = "Statistic",
    values = c("Obs Mean"    = "red",
               "Obs Median"  = "darkgreen",
               "Null Mean"   = "orange",
               "Null Median" = "purple")
  ) +
  facet_grid(language ~ .) +
  labs(title = paste("Observed vs Null Rating Proportions by Language (form =",frm, ")"),
       x = "Rating", y = "Proportion") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal()
# ------------------------------------------------------------------------------


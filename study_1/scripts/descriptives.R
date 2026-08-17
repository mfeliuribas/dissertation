# ----- Do descriptive stats ---------------------------------------------------
#
# This script contains the code used to do descriptive stats for Study 1.
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



# ----- 6. Check data distribution: Futurity reading ---------------------------
# Select futurity reading and create subset
future_readings <- c("future", "future_hyp")

data_futurity_raw <- data_tidy |> 
  filter(reading %in% future_readings)

nrow(data_futurity_raw) 

# See distribution of forms in futurity reading (all coded forms)
data_futurity_raw |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Collapse lexical futures together and conditional futures together ("recode")
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

# Table: Futurity by form (with recoded values)
data_futurity_tidy |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Plot: Futurity by form (with recoded values)
data_futurity_tidy |>
  count(form) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = pct, y = reorder(form, pct))) +
  geom_col(fill = "gray") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.2, size = 3) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  labs(title = "Futurity by form",
       x = "Percentage of tokens", y = NULL) +
  theme_minimal()

# Obs:
  # There are 1,254 tokens labeled with either 'future' or 'future_hyp'.
  # Of these, the majority (76%) are PF, followed by lexical futures (9.1%), 
  #   the PI (6.2%) and the MF (3.4%).
  # Around 5-6% of the tokens are other forms (cond., subj., inf.)

# See distribution within futurity: pure futures vs hypothetical futures
data_futurity_tidy |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

data_futurity_tidy |>
  count(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Obs:
  # Of the 1,254 tokens labeled with either 'future' or 'future_hyp', most of 
  #   them (~ 91%) are considered temporal futures, whereas 9% are 
  #   hypothetical futures, usually embedded in conditional clauses.
  # While a variety of forms is used to express future-time reference, in this
  #   dataset, only the PF and the MF are used to talk about hypothetical  
  #   futures. Within this specific subtype of future, the most used structure 
  #   is the PF (107 tokens; MF = 7 tokens). Interestingly, no lexical 
  #   futures are used to talk about hypothetical future situations. In the case 
  #   of the PI, that is probably not surprising and generally expected. 


# Table: Use of PF, MF, and PI to express futurity
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"), reading == "future") |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Obs:
  # When considering only the PF, MF, and PI, there is a total of 1,074 tokens.
  #   for future-time reference (future and future_hyp). Of these, 953 (88.7%) 
  #   are PF, 78 (or 7.3%) are PI, and 43 (or 4%) are MF. 
  # 180 tokens belong to structures other than the targets of the study.
  # 960 are  temporal futures: 846 PF, 78 PI, 36 MF). 


# Use of PF, MF, and PI by temporal distance and reading
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  count(temp_dist, form, reading) |>
  group_by(temp_dist, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() #|>
  #arrange(form, desc(n))

# Obs:
  # All forms have at least 1 token in each specific temporal category..
  # The PF is the form most used to express futurity, regardless of the temporal 
  #   specification.
  # The MF seems to be favored in nonspecific futures (28), followed by near (6),
  #   distant (1) and immediate (1).
  # The PI appears to be favored in near and immediate futures. It is also used 
  #   to a lesser extent, for distant and nonspecific futures. In the case of 
  #   the distant futures, they are most likely scheduled events (graduations, 
  #   birthdays, etc.).
  # Hypothetical futures are always nonspecific and only occur with the PF (105)
  #   and the MF (7).

# Use of PF, MF, and PI by temporal distance (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(temp_dist) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

# Obs:
  # Almost 40% of the futures are nonspecific. This means that the temporal 
  #   reference was not explicit. While it might have been for the interview
  #   participants, it was not for the researchers.
  # Nonspecific futures are followed by near (31.8%) and immediate (15%), and 
  #   finally distant (13.5%) futures.
  # Importantly, some of the tokens coded as nonspecific might be distant, but 
  #   with the information provided in the context, and in the absence of clear 
  #   temporal markers (advs, temporal subordinates, etc.), it is impossible to 
  #   tell.


# Table: PF, MF, and PI across temporal distance (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(temp_dist, form) |>
  group_by(temp_dist) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() #|>
  #arrange(form, desc(n))

# Obs:
  # All forms are used for all temporal distance values.
  # Overall, the PF is the preferred form for any temporal distance value. 
  # For all temporally marked events, the PI is used over the MF:
  #   immediate: 13.9 vs 0.7%
  #   near: 15.4 vs 2%
  #   distant: 6.2 vs 0.8% 
  # Conversely, for nonspecific events the MF is employed over the PI (7.3% vs 
  #   0.8%).
  # Again, however, the PF is the preferred form in any case, especially for 
  #   distant (93.1%) and nonspecific events (91.9%).

# Plot: PF, MF, and PI across temporal distance (future reading only)
plot_temp_dist_data <- data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future",
         temp_dist %in% c("immediate", "near", "distant", "nonspecific")) |>
  count(temp_dist, form) |>
  group_by(temp_dist) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(temp_dist = factor(temp_dist,
                            levels = c("immediate", "near", "distant", 
                                       "nonspecific")))

plot_temp_dist_data |>
  ggplot() +
  aes(x = temp_dist, y = pct, fill = form) +
  geom_col(width = 0.75) +
  geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 2) +
  scale_fill_viridis_d(end = 0.75) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title = "Forms across temporal distance",
       subtitle = "Future readings only",
       x = NULL, y = NULL, fill = "Form") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8))

# Table: PF, MF, and PI within grammatical person (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(gram_pers, form) |>
  group_by(gram_pers) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  arrange(form, desc(n))

plot_gram_pers_data <- data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future") |>
  count(gram_pers, form) |>
  group_by(gram_pers) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(gram_pers = factor(gram_pers,
                            levels = c("1_sg", "2_sg", "3_sg", "1_pl", "3_pl")))

plot_gram_pers_data |>
  ggplot() +
  aes(x = gram_pers, y = pct, fill = form) +
  geom_col(width = 0.75) +
  geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 2) +
  scale_fill_viridis_d(end = 0.75) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title = "Forms across grammatical person",
       subtitle = "Future readings only",
       x = NULL, y = NULL, fill = "Form") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8))

# Obs:
  # The percentages are calculated within each grammatical person because of the 
  #   group_by(). This means that, for each person, the MF, PF, PI are added 
  #   together, and each count is expressed as a share of that grammatical 
  #   person's total.
  # All forms are used for all grammatical person values. There are very small 
  #   counts for some combinations of form and grammtical person.
  # The only exception is the 2_pl -- there are no examples as expected.
  # Most of the tokens are in 1_sg, which makes sense given the nature of the 
  #   sociolinguistic interviews.
  # Within each structure, the 1_sg has the most tokens and the 2_sg the least.
  # The PF is by far the most frequent form across all grammatical person values
  #   and accounts approximately for 81-96% of tokens in each case.
  # The PI seem to compete with PF a bit more in the 1_sg.
  # The use of the MF is very limited and never exceeds the 5% for any person. 
  # Importantly, while the 3_sg is essentially PF-dominant, the MF makes its 
  #   strongest showing. Where the first person tends to pull its non-PF share 
  #   toward PI, the 3_sg instead tips a bit more toward MF. This gives the 3_sg 
  #   a somewhat more even split between the PI and the MF. I wonder if this 
  #   observation might be related to the fact that the epistemic MF is typically 
  #   used in the third person, and especially the 3_sg (at least according to 
  #   traditional grammars).


# Table: Grammatical person within form (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(gram_pers, form) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  arrange(form, desc(n))

# Obs:
  # All forms follow a similar pattern, yet not identical:
  #   PF: 1_sg > 3_sg > 3_pl > 1_pl > 2_sg
  #   MF: 1_sg > 3_sg > 1_pl > 3_pl > 2_sg
  #   MF: 1_sg > 3_sg > 1_pl > 3_pl > 2_sg
  # For the PI, approx. 75% of the data fails within the first person. If the 
  #   grammatical person is considered a marker of certainty, it makes a lot of 
  #   sense. 
  # Interestingly, approx. 55% of the MF tokens are also in the first person. If 
  #   the MF is considered a marker of uncertainty, then this result makes less 
  #   sense. However, it might be that grammatical person/certainty is 
  #   interacting with temporal distance. 

# Temporal distance x Grammatical person
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),reading == "future") |>
  count(temp_dist, gram_pers, form) |>
  group_by(temp_dist, gram_pers) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  print(n=44)

data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future")|> 
  mutate(temp_dist = factor(temp_dist,
                            levels = c("immediate", "near", "distant", 
                                       "nonspecific"))) |>
  count(temp_dist, gram_pers, form) |>
  ggplot() +
  aes(x = gram_pers, y = n, fill = form) +
  geom_col(position = "fill") +
  facet_wrap(~ temp_dist) +
  scale_fill_viridis_d(end = 0.75) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Form by person and temporal distance",
       x = "Grammatical person", y = "Proportion", fill = "Form") +
  theme_minimal()

# Obs:
  # The PI seems to be favored in immediate and near contexts, especially in the
  #   first person. The PI is used to some extent to talk about distant and 
  #   nonspecific events, basically in 1_sg (perhaps adding some certainty to 
  #   it?)
  # The pattern for the MF is almost the opposite. It is near absent in the 
  #   three specific temporal distances, and present when the event is not 
  #   temporally anchored. 
  # The PF is the dominant form throughout.

# Table: verb class (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(verb_class) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Obs:
  # Half of the futurity tokens refer to dynamic (non-motion) verbs.
  # 20% of the tokens refer to motion verbs.
  # Almost 30% refer to verbs denoting state: stative (20%), modal (3.5%), 
  #   psychological (3.2%) and perceptual (2.8%).
  # 0.2% refer to other, which are presumably dynamic verbs.


# Table: PF, MF, and PI across verb class (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(verb_class, form) |>
  group_by(verb_class) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() #|>
#arrange(form, desc(n))

# Table: Verb class within form (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(verb_class, form) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  arrange(form, desc(n))

# Obs:
  # Within each specific form, the MF is favored in verbs denoting states, that 
  #   is, stative (41.7%), modal (8.3%), perception (5.6%), and psychological 
  #   (2.8) verbs. They account for 58.4% of the MF tokens.
  # 25% of the tokens are for dynamic (non-motion) verbs and 16.7 for motion 
  #   verbs.
  # In the case of the PF, almost 70% of the tokens refer to non-states:
  #   dynamic (52%) and motion (17.7%) verbs.
  # As for the PI, almost 90% of the verbs are non-states, especially motion 
  #   (47.4%). Only 11.6% of the PI tokens are states.


# Forms across speakers
total_speakers <- n_distinct(data_futurity_tidy$participant)

data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"), reading == "future") |>
  group_by(form) |>
  summarize(
    num_tokens    = n(),
    num_speakers  = n_distinct(participant),
    pct_speakers = round(100 * num_speakers / total_speakers, 1),
    tokens_per_speaker = round(num_tokens / num_speakers, 1)) |>
  ungroup()

# Obs:
  # MF: 36 tokens produced by 21 speakers (23.9% of speakers in the dataset).
  # PI: 78 tokens produced by 36 speakers (40.9% of speakers in the dataset).
  # PF: 846 tokens produced by 86 speakers (97.7% of speakers in the dataset).
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

# ----- Descriptive stats: Futurity --------------------------------------------
#
# This script contains the code used to do descriptive stats for the futurity 
# data for Study 1.
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



# ----- 4. Check data distribution: Futurity reading ---------------------------
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
  # There are 1,253 tokens labeled with either 'future' or 'future_hyp'.
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
  # Of the 1,253 tokens labeled with either 'future' or 'future_hyp', most of 
  #   them (~ 91%) are considered temporal futures, whereas 9% are 
  #   hypothetical futures, usually embedded in conditional clauses.
  # While a variety of forms is used to express future-time reference, in this
  #   dataset, only the PF and the MF are used to talk about hypothetical  
  #   futures. Within this specific subtype of future, the most used structure 
  #   is the PF (107 tokens; MF = 7 tokens). Interestingly, no lexical 
  #   futures are used to talk about hypothetical future situations. In the case 
  #   of the PI, that is probably not surprising and generally expected. 


# See distribution within futurity: pure futures
data_futurity_tidy |>
  filter(reading == "future") |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n)) |>
  write_csv(here("study_1", "tables", "table_futurity_descriptives.csv"))

# See distribution within futurity: hypothetical futures
data_futurity_tidy |>
  filter(reading == "future_hyp") |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n)) |>
  write_csv(here("study_1", "tables", "table_hyp-futurity_descriptives.csv"))

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



# ----- 5. Probability of choice: PF, MF, PI -----------------------------------
# Load libs
library("brms")
library("marginaleffects")
library("ggdist")
library("glue")

# Set seed
set.seed(20260911)

# Load data
data_corpus_futurity <- read_csv(here("study_1", "data", 
                                      "corpus_futurity_tidy.csv"))

# Filter form
subset_corpus_futurity <- data_corpus_futurity |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future")

# Specify model formula
form_multi <- bf(form ~ 0 + Intercept)

# Figure out priors
get_prior(formula = form_multi, family = categorical(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors_multi <- c(
  prior(normal(0, 2), class = "b", dpar = "muPF"),
  prior(normal(0, 2), class = "b", dpar = "muPI"))

# Fit simple model
mod0_multi <- brm(
  formula = form_multi, 
  prior = priors_multi, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = categorical(link = "logit"))#, 
  #file = here("study_1", "models", "mod0_multi"))

# Model output
summary(mod0_multi)

# Get posterior and store in vector
mod0_multi_post <- predictions(model = mod0_multi) |>
  posterior_draws() # |>
# write_csv(here("mod0_post.csv"))

# Table - posterior summary of intercept-only model
mod0_multi_post |>
  group_by(group) |>
  median_qi(x = draw) |>
  transmute(
    Response = group, 
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Plot - posterior summary of intercept-only model
prob_choice_multi <- mod0_multi_post |>
  mutate(group = factor(group, levels = c("PI", "MF", "PF"))) |>
  ggplot() +
  aes(x = draw, y = group, fill = group) +
  scale_fill_viridis_d(
    name = "Probability of choice",
    end = 0.9,
    breaks = c("PI", "MF", "PF"),
    labels = c("0.081 [0.065, 0.099]",
               "0.037 [0.026, 0.050]",
               "0.882 [0.860, 0.901]")) +
  stat_slab() +
  stat_pointinterval(show.legend = FALSE, pch = 21, 
                     position = position_nudge(y = -0.05)) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)) +
  labs(y = NULL, x = NULL, fill = "Probability of choice") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

  ggsave(
    here("study_1", "plots", "plot_multi_choice_futurity.png"),
    plot = prob_choice_multi,
    width = 8.5,
    height = 5,
    units = "in",
    dpi = 300)


# Response    Estimate             
# MF          0.037 [0.026, 0.05]  
# PF          0.882 [0.86, 0.901]  
# PI          0.081 [0.065, 0.099] 
# ------------------------------------------------------------------------------

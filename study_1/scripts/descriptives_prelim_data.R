# ----- Do descriptive stats ---------------------------------------------------
#
# This script contains the code used to do descriptive stats for Study 1.
# The data used in this script is the preliminary dataset, not the final one 
# (August 13).
#
# ------------------------------------------------------------------------------

# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("scales")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
# data_raw <- read.csv2(here("study_1", "data", "prelim_data_corpus.csv"))


# There are some issues with the accent marks (of course) - must check it for 
# the final dataset. Meanwhile, use the following code to fix the error:
data_raw <- read_csv2(here("study_1", "data", "prelim_data_corpus.csv"),
                      locale = locale(encoding = "Latin1"))
# ------------------------------------------------------------------------------



# ----- 3. Check and tidy data  ------------------------------------------------
# Visualize data
glimpse(data_raw)
head(data_raw)

colnames(data_raw)

# Remove unnecessary columns
data_tidy <- data_raw |>
  select(-c(4, 20:23))

colnames(data_tidy)
# ------------------------------------------------------------------------------



# ----- 4. Check data distribution: Corpus -------------------------------------
# Tokens by corpus
data_tidy |>
  count(corpus)

  # Obs:
    # With the preliminary dataset, 1,664 tokens are from the CESA and 204 from 
    #   the STC.
# ------------------------------------------------------------------------------



# ----- 5. Check data distribution: Semantic reading ---------------------------
# Overall distribution by semantic reading
data_tidy |>
  count(reading) |>
  print(n = 50)

# Overall distribution by target semantic categories
semantic_categories <- c("EXCLUSION", "ambiguous", "concessive", "ep_hedging",
                         "epistemic", "epistemic_dm", "exhortative", "future", 
                         "future_hyp", "habitual", "inf_cert", "lex_form", 
                         "prot_cond", "prot_temp")

data_tidy |>
  filter(reading %in% semantic_categories) |>
  count(reading, sort = TRUE)

# Distribution of tokens to be reviewed
data_tidy |>
  filter(!reading %in% semantic_categories) |> 
  count(reading, sort = TRUE) |>
  print(n = Inf)

# Overall distribution by semantic reading + tokens to be reviewed
data_tidy |>
  mutate(category = if_else(reading %in% semantic_categories, reading, 
                            "NEEDS REVIEW")) |>
  count(category, sort = TRUE) |>
  print(n = Inf) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# Plot: Overall distribution by semantic reading (without NEEDS REVIEW)
data_tidy |>
  filter(reading %in% semantic_categories) |>
  count(reading) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot() +
  aes(x = pct, y = reorder(reading, pct)) +
  geom_col(fill = "gray") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = 1, size = 2) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Distribution of semantic readings",
       x = "Percentage of tokens",
       y = NULL) +
  theme_minimal()

  # Obs/Notes:
    # There are some misspelled categories --- they must be checked and corrected 
    #   in the original xlsx file (e.g., exhorative, inf_certainty).
    # There are many readings right now. However, a considerable amount of these 
    #   are for not-so-straightforward tokens, that is, tokens that must be 
    #   reviewed.
    # As it stands right now (August 4), around 65% of the data has a future 
    #   reading (future + hypothetical futures), about 10% needs review, and 
    #   1.5% must be excluded. The rest is a mix of inferred certainty, 
    #   habitual, epistemic, etc.
# ------------------------------------------------------------------------------



# ----- 6. Check data distribution: Futurity reading ---------------------------
# Select futurity reading and create subset
future_readings <- c("future", "future_hyp")

data_futurity <- data_tidy |> 
  filter(reading %in% future_readings)

nrow(data_futurity) 

# See distribution of forms in futurity reading (all coded forms, as in the 
# original dataset)
data_futurity |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Collapse lexical futures together and conditional futures together ("recode")
data_futurity_tidy <- data_futurity |>
  mutate(form = fct_recode(form,
                                     "lex_futures" = "querer_inf",
                                     "lex_futures" = "tener_inf",
                                     "lex_futures" = "pensar_inf",
                                     "lex_futures" = "planear_inf",
                                     "lex_futures" = "estar_apunto",
                                     "lex_futures" = "estar_para",
                                     "lex_futures" = "necesitar_inf",
                                     "lex_futures" = "poder_inf",
                                     "lex_futures" = "querer",
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
  # There are 1,217 tokens labeled with either 'future' or 'future_hyp'.
  # Of these, the majority (75.3%) are PF, followed by lexical futures (8.5%), 
  #   the PI (6.8%) and the MF (4.3%).
  # Around 5% of the tokens are other forms, such as conditionals, subjunctive, 
  #   infinitives, and PP. 
  # The PP must be checked, though.

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
  # Of the 1,217 tokens labeled with either 'future' or 'future_hyp', most of 
  #   them, almost a 92% are considered temporal futures, whereas 8.3% are 
  #   hypothetical futures, usually embedded in conditional clauses.
  # While a variety of forms is used to express future-time reference, in this
  #   dataset, only the PF and the MF are used to talk about hypothetical  
  #   futures. Within this specific subtype of futures, the most used structure, 
  #   though, is the PF (95 tokens; MF = 6 tokens). Interestingly, no lexical 
  #   futures are used to talk about hypothetical future situations. In the case 
  #   of the PI, that is probably not surprising and generally expected. 


# Table: Use of PF, MF, and PI to express futurity
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI", "PI")) |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Obs:
  # When considering only the PF, MF, and PI, there is a total of 1,052 tokens.
  # Of these, 917 (or 87.2%) are PF, 83 (or 7.9%) are PI, and 52 (or 4.9%) are
  #   MF. 
  # 165 tokens belong to structures other than the targets of the study.


# Use of PF, MF, and PI by temporal distance and reading
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI", "PI")) |>
  count(temp_dist, form, reading) |>
  group_by(temp_dist, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

# Obs:
  # Hypothetical futures are always nonspecific. 
  # There are some hyp_futures with no temp_dist value --- MUST CHECK!
  # As for the pure futures:

# Use of PF, MF, and PI by temporal distance (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI", "PI")) |>
  filter(reading == "future") |>
  count(temp_dist) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

# Obs:
  # Almost 40% of the futures are nonspecific. This means that the temporal 
  #   reference was not explicit. While it might have been for the interview
  #   participants, it was not for the researchers.
  # Nonspecific futures are followed by near (31.1%) and immediate (18.8%) 
  #   futures.
  # Only 10.6% of the tokens refer to events happening in the distance future. 
  #   Importantly, some of the tokens coded as nonspecific might be distant, but 
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
  ungroup()

# Obs:
  # All forms are used for all temporal distance values.
  # Overall, the PF is the preferred form for any temporal distance value. 
  # For immediate and near events, the PI is used over the MF (14.5% and 15.2% 
  #   vs 1.7% and 3.7%). It is also the the case for distant events (6.9% vs 3%). 
  #   Presumably, these are cases when the event is scheduled and most likely 
  #   to happen (e.g., graduation, birthday, etc).
  # Conversely, for nonspecific events the MF is employed over the PI (7.8% vs 
  #   1.3%).
  # Again, however, the PF is the preferred form in any case. 

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

# Table: PF, MF, and PI across grammatical person (future reading only)
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI")) |>
  filter(reading == "future") |>
  count(gram_pers, form) |>
  group_by(gram_pers) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

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
  # Most tokens of the PF are in the 1 pers.sg.form (366), followed by the
  #   3 pers. sg. form (190) and the 3 pers. pl. form (107). 
  # In the case of the MF, it is the 3 pers. sg. and the 1 pers. sg. forms, both 
  #   having 16 tokens each. 
  # As for the PI, it is the 1 pers. sg. (49) and the 3 pers. sg. (18).


# Temporal distance x Grammatical person
data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI",reading == "future")) |>
  count(temp_dist, gram_pers, form) |>
  group_by(temp_dist, gram_pers) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future",
         !temp_dist %in% c("nonspecific/distant"),
         !is.na(temp_dist)) |> 
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



# Check verb class (once the dataset is completed)

# ------------------------------------------------------------------------------



# ----- 7. Check data distribution: Epistemic reading --------------------------
# Select epistemic reading and create subset
modal_readings <- c("concessive", "ep_hedging", "epistemic", "epistemic_dm", 
                    "inf_cert")
epistemic_readings <- c("ep_hedging", "epistemic", "epistemic_dm", "inf_cert")


data_modality <- data_tidy |> 
  filter(reading %in% modal_readings)

data_epistemic <- data_tidy |> 
  filter(reading %in% epistemic_readings)

nrow(data_modality) # 251 modal tokens, of which 226 are somehow epistemic
nrow(data_epistemic)


# See distribution of forms in modal-epistemic reading
data_epistemic |>
  filter(form %in% c("PF", "MF")) |>
  count(form) |>
  mutate(pct = round(n / sum(n) * 100, 1))

data_epistemic |>
  filter(form %in% c("PF", "MF")) |>
  count(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 1))

data_epistemic |>
  filter(form == "PF") |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1))

data_epistemic |>
  filter(form == "MF") |>
  count(reading) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# Obs:
  # Both the PF and the MF are used to express some sort of epistemic modality. 
  # Of the total of tokens, 78.5% are PFs and 21.5% are MFs.
  # Within modal reading, most of the PF tokens refer to inferred certainty 
  #   situations. Importantly, the rest 1.7% is used in an epistemic sense, that 
  #   is, to express conjecture or doubt in the present tense. This use appears 
  #   to be innovative and 'incorrect' according to traditional grammars. 
  # As for the MF, it is mainly used to express probability in the present (66% 
  #   of the cases). It is also used as a discourse marker (25.5%) and as a means 
  #   of epistemic hedging (8.5%).

# Uses of the PF and the MF
data_tidy |>
  filter(form %in% c("MF", "PF")) |>
  mutate(reading_group = case_when(
    reading %in% future_readings    ~ "future",
    reading %in% epistemic_readings ~ "epistemic",
    TRUE                            ~ "other")) |>
  count(form, reading_group) |>
  group_by(form) |> 
  mutate(pct = round(n / sum(n) * 100, 1)) |> 
  ungroup() |>
  mutate(cell = paste0(pct, "% (", n, ")")) |> 
  select(form, reading_group, cell) |>
  pivot_wider(names_from = reading_group, values_from = cell)

# Obs:
    # Of all the instances of the MF, 42.5% of the time is used to express 
    #   futurity, while 38.5% to express epistemic modality. In 23 cases (18.9%) 
    #   is this form used with a different purpose.
    # As for the PF, 65.5% of the time is used to express futurity, while 12.3% 
    #   to express some sort of epistemic modality. The other 22.3% is used with 
    #   a purpose other than future-time reference and epistemic modality 
    #   (e.g., habitual). 
# ------------------------------------------------------------------------------



# ----- 8. Most suitable analysis? [Austin] ---------------- -------------------
# the exact data your RQ2 model will use
data_rq2 <- data_futurity_tidy |>
  filter(form %in% c("PF", "MF", "PI"),
         reading == "future",
         !temp_dist %in% c("nonspecific/distant"),
         !is.na(temp_dist))

# missingness across the variables you'll model
data_rq2 |>
  select(form, temp_dist, gram_pers, verb_class) |>   # swap in your real predictor names
  summarise(across(everything(), ~ sum(is.na(.x))))

# smallest cells in the full three-way cross-tab
data_rq2 |>
  count(temp_dist, gram_pers, form) |>
  arrange(n)

data_rq2 |> count(temp_dist, gram_pers, form) |> arrange(n) |> print(n = Inf)

data_rq2 |>
  count(temp_dist, gram_pers, form) |>
  summarise(cells_under_5 = sum(n < 5),
            total_cells   = n())

data_rq2 <- data_rq2 |>
  mutate(person_grp = case_when(
    gram_pers %in% c("1_sg", "1_pl") ~ "1st",
    gram_pers %in% c("2_sg")         ~ "2nd",
    gram_pers %in% c("3_sg", "3_pl") ~ "3rd",
    TRUE ~ gram_pers))

# re-check with the collapsed version
data_rq2 |>
  count(temp_dist, person_grp, form) |>
  summarise(under5 = sum(n < 5), total = n())

data_rq2 |>
  mutate(temp_grp = case_when(
    temp_dist %in% c("immediate", "near") ~ "proximate",
    temp_dist %in% c("distant", "nonspecific") ~ "distal",
    TRUE ~ temp_dist)) |>
  count(temp_grp, person_grp, form) |>
  summarise(under5 = sum(n < 5), total = n())

# form × person alone
data_rq2 |> count(person_grp, form) |> summarise(under5 = sum(n < 5), total = n())

# form × temporal distance alone
data_rq2 |> count(temp_dist, form) |> summarise(under5 = sum(n < 5), total = n())
# ------------------------------------------------------------------------------

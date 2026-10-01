# ----- Analysis: Modality -----------------------------------------------------
#
# This script contains the code used to analyze the modality data for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("brms")
library("marginaleffects")
library("ggdist")
library("scales")
library("glue")
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


# ----- 4. Total instances of PF and MF ----------------------------------------
data_corpus |>
  count(form)

data_corpus |>
  filter(form %in% c("PF", "PF/PI")) |>
  mutate(domain = case_when(
    reading %in% future_readings ~ "future",
    reading %in% modal_readings  ~ "modal",
    TRUE                         ~ "other")) |>
  count(domain) |>
  mutate(pct = round(n / sum(n) * 100, 2))

data_corpus |>
  filter(form == "MF") |>
  count(reading, form)
# ------------------------------------------------------------------------------


# ----- 5. Check overall distribution: PF vs MF --------------------------------
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
  ungroup() # |>
  # write_csv(
  #   here("study_1", "tables", "table_distribution_pfmf_v2.csv"))

# Distribution within the domains (pcts calculated within forms)
# Hypothetical futures EXCLUDED
data_corpus_subset |>
  filter(reading == "future" | domain == "modality") |>
  count(form, domain) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(form, desc(n)) |>
  ungroup() #|>
  # write_csv(
  #   here("study_1", "tables", "table_distribution_pfmf_v3.csv"))
# ------------------------------------------------------------------------------


# ----- 4. Modal uses of the PF and the MF -------------------------------------
epistemic_readings <- c("inf_cert", "epistemic", "epistemic_dm", "attenuation")

data_corpus_subset |>
  mutate(reading = case_when(
    reading %in% epistemic_readings ~ "epistemic",
    TRUE ~ reading)) |>
  filter(domain == "modality") |>
  group_by(reading) |>
  count(reading)

data_corpus_subset |>
  mutate(reading = case_when(
    reading %in% epistemic_readings ~ "epistemic",
                               TRUE ~ reading)) |>
  filter(domain == "modality") |>
  group_by(form) |>
  count(form, reading) |>
  mutate(pct = round(n / sum(n) * 100, 2)) |>
  ungroup() |>
  write_csv(
    here("study_1", "tables", "table_modality.csv"))

data_corpus_subset |>
  filter(form == "MF",
         reading == "lex_form") |>
  group_by(token) |>
  count(token)

data_corpus_subset |>
  filter(form == "PF",
         reading == "lex_form") |>
  group_by(token) |>
  count(token)

# Convert characters into factors and set reference levels
data_corpus_subset |>
  mutate(reading = case_when(
    reading %in% epistemic_readings ~ "epistemic",
    TRUE ~ reading)) |>
  filter(domain == "modality") |>
  group_by(reading) |>
  count(reading)

data_corpus_modality <- data_corpus_modality |>
  mutate(
    modality = fct_recode(reading,
                               "epistemic"   = "attenuation",
                               "concessive"  = "concessive",
                               "epistemic"   = "epistemic",
                               "epistemic"   = "epistemic_dm",
                               "exhortative" = "exhortative",
                               "epistemic"   = "inf_cert",
                               "lex_form"    = "lex_form"),
    stative_class = factor(modality,
                           levels = c("epistemic", "concessive", "lex_form",
                                      "exhortative"))) |>
  relocate(modality, .after = reading)

# Filter and recode form
data_corpus_modality <- data_corpus_modality |>
  filter(form %in% c("PF", "MF")) |>
  mutate(form_rc = fct_recode(form,
                           "0" = "PF",
                           "1" = "MF"),
         form_rc = as.numeric(as.character(form_rc))) |>
  relocate(form_rc, .after = form)

# Model with modal readings
# Specify model formula
form_modality <- bf(form_rc ~ 0 + Intercept + modality + (1 | participant))

# Figure out the priors
get_prior(formula = form_modality, 
          family = bernoulli(link = "logit"), 
          data = data_corpus_modality)

# Set priors
priors_modality <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("student_t(3, 0, 1.5)", class = "sd"))

# Fit simple model
mod_modality <- brm(
  formula = form_modality, 
  prior = priors_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_corpus_modality, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod_modality"))

# Model output
summary(mod_modality)

# Get posteriors
mod_modality_preds <- avg_predictions(
  model = mod_modality,
  variables = "modality",
  re_formula = NULL)

mod_modality_post <- avg_predictions(
  model = mod_modality, 
  variables = "modality", 
  re_formula = NULL) |>
  posterior_draws()

# Table - posterior summary of mod (predicted prob of using MF)
mod_modality_post |>
  group_by(modality) |>
  median_qi(x = draw) |>
  transmute(
    Modality = modality,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod1 (predicted prob of using MF and PF)
mod_modality_table <- mod_modality_post |>
  mutate(
    draw_PF = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PF),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PF = "PF")) |>
  group_by(modality, form) |>
  median_qi(x = draw) |>
  transmute(
    Modality = modality,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Modality, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

# # A tibble: 4 × 3
# Modality        MF                    PF                  
# 1 epistemic     0.213 [0.176, 0.255]  0.787 [0.745, 0.824]
# 2 concessive    0.11 [0.039, 0.224]   0.89 [0.776, 0.961] 
# 3 exhortative   0.083 [0.024, 0.19]   0.917 [0.81, 0.976] 
# 4 lex_form      0.445 [0.303, 0.59]   0.555 [0.41, 0.697] 

# Within each modal notion, the PF is the most likely choice.
# ------------------------------------------------------------------------------






# ----- 5. Epistemic uses of the PF and the MF ---------------------------------
# Convert characters into factors and set reference levels
data_corpus_modality_subset <- data_corpus_modality |>
  filter(reading %in% c("epistemic", "inf_cert", "epistemic_dm", 
                        "attenuation")) |>
  mutate(
    reading = factor(
      reading,
      levels = c("epistemic", "inf_cert", "epistemic_dm", "attenuation")))

# Raw counts and percentatges
data_corpus_modality_subset |>
  count(form, reading) |>
  group_by(form) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(form, desc(n)) |>
  ungroup() |>
  write_csv(
   here("study_1", "tables", "table_distribution_epistemic.csv"))

# Model with modal readings
# Specify model formula
form_epistemic <- bf(form_rc ~ 0 + reading + (1 | participant))

# Figure out the priors
get_prior(formula = form_epistemic, 
          family = bernoulli(link = "logit"), 
          data = data_corpus_modality_subset)

# Set priors
priors_epistemic <- c(
  # prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("student_t(3, 0, 1.5)", class = "sd"))

# Fit simple model
mod_epistemic <- brm(
  formula = form_epistemic, 
  prior = priors_epistemic, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_corpus_modality_subset, 
  family = bernoulli(link = "logit"),
  control = list(adapt_delta = 0.95), 
  file = here("study_1", "models", "mod_epistemic"))

# Model output
summary(mod_epistemic)

# Get posteriors
mod_epistemic_preds <- avg_predictions(
  model = mod_epistemic,
  variables = "reading",
  re_formula = NULL)

mod_epistemic_post <- avg_predictions(
  model = mod_epistemic, 
  variables = "reading", 
  re_formula = NULL) |>
  posterior_draws()

# Table - posterior summary of mod (predicted prob of using MF)
mod_epistemic_post |>
  group_by(reading) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod1 (predicted prob of using MF and PF)
mod_epistemic_table <- mod_epistemic_post |>
  mutate(
    draw_PF = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PF),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PF = "PF")) |>
  group_by(reading, form) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Reading, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate) |>
  write_csv(here("study_1", "tables", "table_epistemic.csv"))

# Plot
mod_epistemic_plot_data <- mod_epistemic_post |>
  mutate(
    p_MF = draw,
    p_PF = 1 - draw,
    reading = fct_recode(reading,
                         "Possibility"        = "epistemic",
                         "Inferred Certainty" = "inf_cert",
                         "Discourse Marker"   = "epistemic_dm",
                         "Attenuation"        = "attenuation")) |>
  pivot_longer(
    cols = c(p_MF, p_PF),
    names_to = "form",
    values_to = "prob") |>
  mutate(
    form = recode(form, p_MF = "MF", p_PF = "PF"),
    form = fct_relevel(form, "PF", "MF"),
    reading = fct_rev(reading))

plot_epistemic_post <- mod_epistemic_plot_data |>
  ggplot() +
  aes(x = prob, y = reading, colour = form) +
  stat_pointinterval(position = position_dodge(width = 0.5)) +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = "Posterior probability",
       y = NULL,
       colour = "Form") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "right",
    axis.title.x = element_text(margin = margin(t = 10)))

ggsave(
  here("study_1", "plots", "plot_epistemic_post.png"),
  plot = plot_epistemic_post,
  width = 6.5,
  height = 3.5,
  units = "in",
  dpi = 300)

# Comparisons
mod_epistemic_post <- avg_comparisons(
  mod_epistemic,
  variables = list(
    reading = "pairwise"),
  re_formula = NULL) |>
  posterior_draws()

mod_epistemic_post |>
  group_by(contrast) |>
  median_qi(draw) |>
  transmute(
    Comparison = case_when(
      contrast == "mean(attenuation) - mean(epistemic_dm)" ~ "Attenuation − Discourse Marker",
      contrast == "mean(attenuation) - mean(epistemic)" ~ "Attenuation − Possibility",
      contrast == "mean(attenuation) - mean(inf_cert)" ~ "Attenuation − Inferred Certainty",
      contrast == "mean(epistemic_dm) - mean(epistemic)" ~ "Discourse Marker − Possibility",
      contrast == "mean(epistemic_dm) - mean(inf_cert)" ~ "Discourse Marker − Inferred Certainty",
      contrast == "mean(inf_cert) - mean(epistemic)" ~ "Inferred Certainty − Possibility"),
    Estimate = glue("{round(draw, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  write_csv(here("study_1", "tables", "table_epistemic_comp.csv"))

plot_epistemic_comp <- mod_epistemic_post |>
  mutate(
    Comparison = case_when(
      contrast == "mean(attenuation) - mean(epistemic_dm)" ~ "Attenuation − Discourse Marker",
      contrast == "mean(attenuation) - mean(epistemic)" ~ "Attenuation − Possibility",
      contrast == "mean(attenuation) - mean(inf_cert)" ~ "Attenuation − Inferred Certainty",
      contrast == "mean(epistemic_dm) - mean(epistemic)" ~ "Discourse Marker − Possibility",
      contrast == "mean(epistemic_dm) - mean(inf_cert)" ~ "Discourse Marker − Inferred Certainty",
      contrast == "mean(inf_cert) - mean(epistemic)" ~ "Inferred Certainty − Possibility")) |>
  ggplot() +
  aes(x = draw, y = Comparison) +
  stat_halfeye(position = position_dodge(0.5)) +
  labs(y = NULL, x = "Posterior difference MF (epistemic modality)")

ggsave(
  here("study_1", "plots", "plot_epistemic_comp.png"),
  plot = plot_epistemic_comp,
  width = 8.5,
  height = 5,
  units = "in",
  dpi = 300)


# ------------------------------------------------------------------------------

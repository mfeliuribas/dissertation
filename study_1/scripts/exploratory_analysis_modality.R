# ----- Exploratory Analysis: Modality -----------------------------------------
#
# This script contains the code used to make an exploratory analysis of the 
# modality data for Study 1. After fitting several models, I concluded that the 
# data were too sparse and limited to support reliable modeling, so the final 
# analysis of modality is purely descriptive (see ´analysis_modality´).
#
# This code is kept here for reference. It is NOT part of the data analysis.
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
data_corpus_modality <- read_csv(here("study_1", "data", 
                                      "corpus_modality_tidy.csv"))

# Convert characters into factors and set reference levels
# Collapse verb class into two categories: statives vs non-statives
data_corpus_modality <- data_corpus_modality |>
  mutate(
    reading = factor(
      reading,
      levels = c("epistemic", "inf_cert"))) |>
  mutate(
    verb_class = factor(verb_class,
    levels = c("dynamic", "modal", "motion", "stative"))) |>
  mutate(
    stative_class = fct_recode(verb_class,
                            "non-stative" = "dynamic",
                            "non-stative" = "modal",
                            "non-stative" = "motion",
                            "stative"     = "stative"),
    stative_class = factor(stative_class,
      levels = c("stative", "non-stative"))) |>
  mutate(
    type_bil = factor(
      type_bil,
      levels = c("seq", "sim"))) |>
  relocate(stative_class, .after = verb_class)

# Filter and recode form
subset_corpus_modality <- data_corpus_modality |>
  filter(reading %in% c("inf_cert", "epistemic")) |>
  mutate(form = fct_recode(form,
                                "0" = "PF",
                                "1" = "MF"),
         form = as.numeric(as.character(form)))
# ------------------------------------------------------------------------------



# ----- 4. Fit models ----------------------------------------------------------
# ----- 4.1. Model 0 -----------------------------------------------------------
# Specify model formula
form0_corpus_modality <- bf(form ~ 0 + Intercept)

# Figure out the priors
get_prior(formula = form0_corpus_modality, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_modality)

# Set priors
priors0_modality <- prior(normal(0, 1.5), class = b, coef = "Intercept")

# Fit simple model
mod0_modality <- brm(
  formula = form0_corpus_modality, 
  prior = priors0_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_modality, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod0_modality"))

# Model output
summary(mod0_modality)

# Get posteriors
mod0_modality_post <- as_draws_df(mod0_modality)

# Convert the intercept from log-odds to probability
mod0_modality_post <- mod0_modality_post |>
  mutate(p_MF = plogis(b_Intercept))

# Summarize probability
mod0_modality_post |>
  median_qi(p_MF)

mod0_modality_post |>
  median_qi(p_MF) |>
  transmute(
    Estimate = glue(
      "{round(p_MF, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Create posterior probability for both the MF and the PF
mod0_modality_post <- as_draws_df(mod0_modality) |>
  mutate(
    p_MF = plogis(b_Intercept),
    p_PF = 1 - p_MF)

mod0_modality_post |>
  pivot_longer(
    cols = c(p_MF, p_PF),
    names_to = "form",
    values_to = "prob") |>
  mutate(
    form = recode(
      form,
      p_MF = "MF",
      p_PF = "PF")) |>
  group_by(form) |>
  median_qi(prob) |>
  transmute(
    Response = form,
    Estimate = glue(
      "{round(prob, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Plot of posterior distribution
mod0_modality_plot <- mod0_modality_post |>
  pivot_longer(
    cols = c(p_MF, p_PF),
    names_to = "form",
    values_to = "prob") |>
  mutate(
    form = recode(
      form,
      p_MF = "MF",
      p_PF = "PF"),
    form = fct_relevel(form, "MF", "PF"))

mod0_modality_plot %>%
  ggplot() +
  aes(x = prob, y = form, fill = form) +
  scale_fill_viridis_d(name = NULL, end = 0.5) +
  stat_slab(show.legend = FALSE) +
  stat_pointinterval(
    show.legend = FALSE,
    pch = 21,
    position = position_nudge(y = -0.05)) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    labels = label_percent()) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    y = NULL,
    x = "Posterior probability")

# Response     Estimate           
#   1 MF       0.183 [0.14, 0.233]
#   2 PF       0.817 [0.767, 0.86]


# ----- 4.2. Model 1 (reading) -------------------------------------------------
# Specify model formula
form1_corpus_modality <- bf(form ~ 0 + Intercept + reading + 
  (1 | participant))

# Figure out the priors
get_prior(formula = form1_corpus_modality, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_modality)

# Set priors
priors1_modality <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("student_t(3, 0, 1.5)", class = "sd"))

# Fit simple model
mod1_modality <- brm(
  formula = form1_corpus_modality, 
  prior = priors1_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_modality, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod1_modality"))

# Model output
summary(mod1_modality)

# Get posteriors
mod1_modality_preds <- avg_predictions(
  model = mod1_modality,
  variables = "reading",
  re_formula = NULL)

mod1_modality_post <- avg_predictions(
  model = mod1_modality, 
  variables = "reading", 
  re_formula = NULL) |>
  posterior_draws()

# Table - posterior summary of mod1 (predicted prob of using MF)
mod1_modality_post |>
  group_by(reading) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod1 (predicted prob of using MF and PF)
mod1_modality_table <- mod1_modality_post |>
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
    values_from = Estimate)

mod1_modality_table


# ----- 4.3. Model 2 (reading + verb_class) ------------------------------------
# Specify model formula
form2_corpus_modality <- bf(form ~ 0 + Intercept + reading + verb_class +
                     (1 | participant))

# Figure out the priors
get_prior(formula = form2_corpus_modality, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_modality)

# Set priors
priors2_modality <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("student_t(3, 0, 1.5)", class = "sd"))


# Fit simple model
mod2_modality <- brm(
  formula = form2_corpus_modality, 
  prior = priors2_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_modality, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod2_modality"))

# Model output
summary(mod2_modality)

# Get posteriors
mod2_modality_preds <- avg_predictions(
  model = mod2_modality,
  variables = c("reading", "verb_class"),
  re_formula = NULL)

mod2_modality_post <- mod2_modality_preds |>
  posterior_draws()

mod2_modality_vc_preds <- avg_predictions(
  model = mod2_modality,
  variables = "verb_class",
  re_formula = NULL)

mod2_modality_vc_post <- mod2_modality_vc_preds |>
  posterior_draws()

# Table - posterior summary of mod2 (predicted prob of using MF)
mod2_modality_post |>
  group_by(reading, verb_class) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Verb = verb_class,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod2 (predicted prob of using MF and PI)
mod2_modality_table <- mod2_modality_post |>
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
  group_by(reading, verb_class, form) |>
  median_qi(x = draw) |>
  mutate(
    Reading = reading,
    Verb = verb_class,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Reading, Verb, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

mod2_modality_table

# Table - posterior summary of mod2, verb_class ONLY (predicted prob of using MF 
# and PF)
mod2_modality_vc_table <- mod2_modality_vc_post |>
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
  group_by(verb_class, form) |>
  median_qi(x = draw) |>
  mutate(
    Verb = verb_class,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Verb, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

# The model estimates probabilities for factors with no MF data, probably due to 
# the priors? This is important because they hardly reflect the reality. For 
# example, saying that the probability of having an epistemic MF with motion 
# verbs is 0.724 is theoretically impossible (i.e., superinternal implication) 
# and not actually found in the dataset. 
# Think about this issue!
# I decided to create a new variable called 'stative_class' to collapse all non-
# statives together and see if this makes a difference.


# ----- 4.4. Model 3 (reading + stative_class + type_bil) ----------------------
# Specify model formula
form3_corpus_modality <- bf(form ~ 0 + Intercept + reading + stative_class + 
                              type_bil +
                              (1 | participant))

# Figure out the priors
get_prior(formula = form3_corpus_modality, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_modality)

# Set priors
priors3_modality <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("exponential(1)", class = "sd"))

# Fit simple model
mod3_modality <- brm(
  formula = form3_corpus_modality, 
  prior = priors3_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_modality, 
  family = bernoulli(link = "logit"),
  control = list(adapt_delta = 0.95), 
  file = here("study_1", "models", "mod3_modality"))

# Model output
summary(mod3_modality)

# Get posteriors
mod3_modality_preds <- avg_predictions(
  model = mod3_modality,
  variables = c("reading", "stative_class", "type_bil"),
  re_formula = NULL)

mod3_modality_post <- mod3_modality_preds |>
  posterior_draws()

mod3_modality_read_preds <- avg_predictions(
  model = mod3_modality,
  variables = "reading",
  re_formula = NULL)

mod3_modality_read_post <- mod3_modality_read_preds |>
  posterior_draws()

mod3_modality_stative_preds <- avg_predictions(
  model = mod3_modality,
  variables = "stative_class",
  re_formula = NULL)

mod3_modality_stative_post <- mod3_modality_stative_preds |>
  posterior_draws()

mod3_modality_bil_preds <- avg_predictions(
  model = mod3_modality,
  variables = "type_bil",
  re_formula = NULL)

mod3_modality_bil_post <- mod3_modality_bil_preds |>
  posterior_draws()

# Table - posterior summary of mod3 (predicted prob of using MF)
mod3_modality_post |>
  group_by(reading, stative_class, type_bil) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Verb = stative_class,
    Bilingualism = type_bil,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod3 (predicted prob of using MF and PF)
mod3_modality_table <- mod3_modality_post |>
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
  group_by(reading, stative_class, type_bil, form) |>
  median_qi(x = draw) |>
  mutate(
    Reading = reading,
    Verb = stative_class,
    Bilingualism = type_bil,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Reading, Verb, Bilingualism, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

# Table - posterior summary of mod3, type_bil ONLY (predicted prob of using MF 
# and PF)
mod3_modality_bil_table <- mod3_modality_bil_post |>
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
  group_by(type_bil, form) |>
  median_qi(x = draw) |>
  mutate(
    Bilingualism = type_bil,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Bilingualism, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

# This table is not very informative. All predictors should probably interact, 
# but I do not think that the model can handle this (sparse data, small dataset).


# Observations:
#   - The model might not be very useful for the modality data.
#   - After building it, I realized that some factors are contingent upon reading, 
#     so probably an interaction would be useful. However, given the sparse and 
#     and limited amount of data I hardly doubt that the model could handle the 
#     interactions It would probably give very wide CrIs, which may not be very 
#     informative.
#   - Based on this, it would be probably better to do descriptive stats for the 
#     modality data and perhaps support the observations with very simple models,
#     such as mod1.


# ----- 4.4. Model 4 (reading * stative_class) ------------------------------------
# Specify model formula
form4_corpus_modality <- bf(form ~ 0 + Intercept + reading * stative_class +
                              (1 | participant))

# Figure out the priors
get_prior(formula = form4_corpus_modality, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_modality)

# Set priors
priors4_modality <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("student_t(3, 0, 1.5)", class = "sd"))


# Fit simple model
mod4_modality <- brm(
  formula = form4_corpus_modality, 
  prior = priors4_modality, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_modality, 
  family = bernoulli(link = "logit"),
  control = list(adapt_delta = 0.95), 
  file = here("study_1", "models", "mod4_modality"))

# Model output
summary(mod4_modality)

# Get posteriors
mod4_modality_preds <- avg_predictions(
  model = mod4_modality,
  variables = c("reading", "stative_class"),
  re_formula = NULL)

mod4_modality_post <- mod4_modality_preds |>
  posterior_draws()


# Table - posterior summary of mod4 (predicted prob of using MF)
mod4_modality_post |>
  group_by(reading, stative_class) |>
  median_qi(x = draw) |>
  transmute(
    Reading = reading,
    Verb = stative_class,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod2 (predicted prob of using MF and PI)
mod4_modality_table <- mod4_modality_post |>
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
  group_by(reading, stative_class, form) |>
  median_qi(x = draw) |>
  mutate(
    Reading = reading,
    Verb = stative_class,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Reading, Verb, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

mod4_modality_table
# ------------------------------------------------------------------------------
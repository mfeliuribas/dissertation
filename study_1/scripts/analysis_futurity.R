# ----- Analysis: Futurity -----------------------------------------------------
#
# This script contains the code used to analyze the futurity data for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("brms")
#library("dplyr")
# library("tibble")
# library("knitr")
library("marginaleffects")
library("ggdist")
library("scales")
library("glue")
# library("cmdstanr")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260911)
# ------------------------------------------------------------------------------


# ----- 3. Load data -----------------------------------------------------------
data_corpus_futurity <- read_csv(here("study_1", "data", 
                                      "corpus_futurity_tidy.csv"))

# Filter and recode form; convert characters into factors
subset_corpus_futurity <- data_corpus_futurity |>
  filter(form %in% c("MF", "PI"),
         reading == "future") |>
  mutate(form = fct_recode(form,
                                "0" = "PI",
                                "1" = "MF"),
         form = as.numeric(as.character(form)))

subset_corpus_futurity <- subset_corpus_futurity |>
  mutate(
    temp_dist = factor(
      temp_dist,
      levels = c("immediate", "near", "distant", "nonspecific")))

subset_corpus_futurity <- subset_corpus_futurity |>
  mutate(
    gram_pers = factor(
      gram_pers,
      levels = c("1pers", "2pers", "3pers")))

subset_corpus_futurity <- subset_corpus_futurity |>
  mutate(verb_class = fct_recode(verb_class,
                                 "stative" = "modal")) |>
  mutate(
    verb_class = factor(
      verb_class,
      levels = c("dynamic", "motion", "stative")))
# ------------------------------------------------------------------------------



# ----- 4. Fit models ----------------------------------------------------------
# ----- 4.1. Model 0 -----------------------------------------------------------
# Specify model formula
form0_corpus <- bf(form ~ 0 + Intercept)

# Figure out the priors
get_prior(formula = form0_corpus, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors0 <- prior(normal(0, 1.5), class = b)

# Fit simple model
mod0_future <- brm(
  formula = form0_corpus, 
  prior = priors0, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod0_future"))

# Model output
summary(mod0_future)

# Get posteriors
mod0_future_post <- as_draws_df(mod0_future)

# Convert the intercept from log-odds to probability
mod0_future_post <- mod0_future_post |>
  mutate(p_MF = plogis(b_Intercept))

# Summarize probability
mod0_future_post |>
  median_qi(p_MF)

mod0_future_post |>
  median_qi(p_MF) |>
  transmute(
    Estimate = glue(
      "{round(p_MF, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Create posterior probability for both the MF and the PI
mod0_future_post <- as_draws_df(mod0_future) |>
  mutate(
    p_MF = plogis(b_Intercept),
    p_PI = 1 - p_MF)

mod0_future_post |>
  pivot_longer(
    cols = c(p_MF, p_PI),
    names_to = "form",
    values_to = "prob") |>
  mutate(
    form = recode(
      form,
      p_MF = "MF",
      p_PI = "PI")) |>
  group_by(form) |>
  median_qi(prob) |>
  transmute(
    Response = form,
    Estimate = glue(
      "{round(prob, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Plot of posterior distribution
mod0_future_plot <- mod0_future_post |>
  pivot_longer(
    cols = c(p_MF, p_PI),
    names_to = "form",
    values_to = "prob") |>
  mutate(
    form = recode(
      form,
      p_MF = "MF",
      p_PI = "PI"),
    form = fct_relevel(form, "MF", "PI"))

mod0_future_plot %>%
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


# ----- 4.2. Model 1 (temp_dist) -----------------------------------------------
# Specify model formula
form1_corpus <- bf(form ~ 0 + Intercept + temp_dist + 
  (1 | participant))

# Figure out the priors
get_prior(formula = form1_corpus, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors1 <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("exponential(1)", class = "sd"))

# Fit simple model
mod1_future <- brm(
  formula = form1_corpus, 
  prior = priors1, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod1_future"))

# Model output
summary(mod1_future)

# Get posteriors
mod1_future_preds <- avg_predictions(
  model = mod1_future,
  variables = "temp_dist",
  re_formula = NULL)

mod1_future_post <- avg_predictions(
  model = mod1_future, 
  variables = "temp_dist", 
  re_formula = NULL) |>
  posterior_draws()

# Table - posterior summary of mod1 (predicted prob of using MF)
mod1_future_post |>
  group_by(temp_dist) |>
  median_qi(x = draw) |>
  transmute(
    Time = temp_dist,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod1 (predicted prob of using MF and PI)
mod1_future_table <- mod1_future_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
  group_by(temp_dist, form) |>
  median_qi(x = draw) |>
  transmute(
    Time = temp_dist,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Time, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

mod1_future_table


# ----- 4.3. Model 2 (temp_dist + gram_pers) -----------------------------------
# Specify model formula
form2_corpus <- bf(form ~ 0 + Intercept + temp_dist + gram_pers +
                     (1 | participant))

# Figure out the priors
get_prior(formula = form2_corpus, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors2 <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("exponential(1)", class = "sd"))

# Fit simple model
mod2_future <- brm(
  formula = form2_corpus, 
  prior = priors2, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod2_future"))

# Model output
summary(mod2_future)

# Get posteriors
mod2_future_preds <- avg_predictions(
  model = mod2_future,
  variables = c("temp_dist", "gram_pers"),
  re_formula = NULL)

mod2_future_post <- mod2_future_preds |>
  posterior_draws()

mod2_future_gp_preds <- avg_predictions(
  model = mod2_future,
  variables = "gram_pers",
  re_formula = NULL)

mod2_future_gp_post <- mod2_future_gp_preds |>
  posterior_draws()

# Table - posterior summary of mod2 (predicted prob of using MF)
mod2_future_post |>
  group_by(temp_dist, gram_pers) |>
  median_qi(x = draw) |>
  transmute(
    Time = temp_dist,
    Person = gram_pers,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod2 (predicted prob of using MF and PI)
mod2_future_table <- mod2_future_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
  group_by(temp_dist, gram_pers, form) |>
  median_qi(x = draw) |>
  mutate(
    Time = temp_dist,
    Person = gram_pers,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Time, Person, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

mod2_future_table

# Table - posterior summary of mod2, gram_pers ONLY (predicted prob of using MF 
# and PI)
mod2_future_gp_table <- mod2_future_gp_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
  group_by(gram_pers, form) |>
  median_qi(x = draw) |>
  mutate(
    Person = gram_pers,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Person, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)







# ----- 4.4. Model 3 (temp_dist + gram_pers + verb_class) ----------------------
# Specify model formula
form3_corpus <- bf(form ~ 0 + Intercept + temp_dist + gram_pers + verb_class + 
                     (1 | participant))

# Figure out the priors
get_prior(formula = form3_corpus, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors3 <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("exponential(1)", class = "sd"))

# Fit simple model
mod3_future <- brm(
  formula = form3_corpus, 
  prior = priors3, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = bernoulli(link = "logit"), 
  file = here("study_1", "models", "mod3_future"))

# Model output
summary(mod3_future)

# Get posteriors
mod3_future_preds <- avg_predictions(
  model = mod3_future,
  variables = c("temp_dist", "gram_pers", "verb_class"),
  re_formula = NULL)

mod3_future_post <- mod3_future_preds |>
  posterior_draws()

mod3_future_verb_preds <- avg_predictions(
  model = mod3_future,
  variables = "verb_class",
  re_formula = NULL)

mod3_future_verb_post <- mod3_future_verb_preds |>
  posterior_draws()

# Table - posterior summary of mod3 (predicted prob of using MF)
mod3_future_post |>
  group_by(temp_dist, gram_pers, verb_class) |>
  median_qi(x = draw) |>
  transmute(
    Time = temp_dist,
    Person = gram_pers,
    Verb = verb_class,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# Table - posterior summary of mod3 (predicted prob of using MF and PI)
mod3_future_table <- mod3_future_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
  group_by(temp_dist, gram_pers, verb_class, form) |>
  median_qi(x = draw) |>
  mutate(
    Time = temp_dist,
    Person = gram_pers,
    Verb = verb_class,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Time, Person, Verb, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)

mod3_future_table

# Table - posterior summary of mod3, verb_class ONLY (predicted prob of using MF 
# and PI)
mod3_future_verb_table <- mod3_future_verb_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
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


# ----- 4.5. Model 4 (interaction term -- EXPLORATORY) -------------------------
# Specify model formula
form4_corpus <- bf(form ~ 0 + Intercept + temp_dist * gram_pers + verb_class + 
                     (1 | participant))

# Figure out the priors
get_prior(formula = form4_corpus, 
          family = bernoulli(link = "logit"), 
          data = subset_corpus_futurity)

# Set priors
priors4 <- c(
  prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  prior("normal(0, 1)",   class = "b"),
  prior("exponential(1)", class = "sd"))

# Fit simple model
mod4_future <- brm(
  formula = form4_corpus, 
  prior = priors4, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = subset_corpus_futurity, 
  family = bernoulli(link = "logit"))#, 
  #file = here("study_1", "models", "mod4_future"))

# Model output
summary(mod4_future)

# Get posteriors
mod4_future_preds <- avg_predictions(
  model = mod4_future,
  variables = c("temp_dist", "gram_pers", "verb_class"),
  re_formula = NULL)

mod4_future_post <- mod4_future_preds |>
  posterior_draws()

mod4_future_interaction_preds <- avg_predictions(
  model = mod4_future,
  variables = c("temp_dist", "gram_pers"),
  re_formula = NULL)

mod4_future_interaction_post <- mod4_future_interaction_preds |>
  posterior_draws()

# Table for temp_dist and gram_pers interaction
mod4_future_interaction_table <- mod4_future_interaction_post |>
  mutate(
    draw_PI = 1 - draw) |>
  pivot_longer(
    cols = c(draw, draw_PI),
    names_to = "form",
    values_to = "draw") |>
  mutate(
    form = recode(
      form,
      draw = "MF",
      draw_PI = "PI")) |>
  group_by(temp_dist, gram_pers, form) |>
  median_qi(x = draw) |>
  mutate(
    Time = temp_dist,
    Person = gram_pers,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Time, Person, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate)


# ----- 4.6. Models 3 and 4: comparison ----------------------------------------
loo_mod3 <- loo(mod3_future)
loo_mod4 <- loo(mod4_future)

loo_compare(loo_mod3, loo_mod4)

# Comments:
  # Adding the interaction does not make a difference: 
  # elpd_diff is 0.0 (mod4) vs -0.1 (mod3)
  # se_diff 0.0 (mod4) vs 0.5 (mod3).

  # Rule of thumb: if 'elpd_diff' is less than 4, models are "very similar".
  # "If the raw ELPD difference is small (less than 4), it doesn't make much 
  # sense to rely on its standardized value: it is not very useful to conclude 
  # that a model is much better than another if both models make very similar 
  # predictions."

  # Sources: 
  #   https://easystats.github.io/report/reference/report.compare.loo.html
  #   https://stats.stackexchange.com/questions/608881/how-to-interpret-elpd-diff-of-bayesian-loo-estimate-in-bayesian-logistic-regress
  #   https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html
# ------------------------------------------------------------------------------

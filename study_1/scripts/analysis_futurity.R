# ----- Analysis: Futurity -----------------------------------------------------
#
# This script contains the code used to analyze the futurity data for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("brms")
# library("dplyr")
# library("tibble")
# library("knitr")
#library("marginaleffects")
#library("ggdist")
library("scales")
#library("glue")
# library("cmdstanr")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260911)
# ------------------------------------------------------------------------------


# ----- 3. Load data -----------------------------------------------------------
data_corpus_futurity <- read_csv(here("study_1", "data", 
                                      "corpus_futurity_tidy.csv"))

# Filter and recode form 
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
# ------------------------------------------------------------------------------



# ----- 4. Fit models ----------------------------------------------------------
# ----- 4.1. Intercept-only model ----------------------------------------------
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


# ------------------------------------------------------------------------------

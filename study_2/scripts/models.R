# ----- Build models -----------------------------------------------------------
#
# This script contains the code used to build the models for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("brms")
library("dplyr")
library("tibble")
library("knitr")
library("marginaleffects")
#library("ggdist")
#library("tidyverse")
#library("glue")
# ------------------------------------------------------------------------------

# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260604)

# ----- 3. Load data -----------------------------------------------------------
data_bls <- read.csv(here("study_2", "final_data", "bls_final.csv"))
# ------------------------------------------------------------------------------

# ----- 4. Fit BL models -------------------------------------------------------
# ----- 4.1. Intercept-only model ----------------------------------------------
# Specify model formula
form0 <- bf(rating ~ 0 + Intercept)

# Figure out the priors
get_prior(formula = form0, family = zero_one_inflated_beta(), data = data_bls)

# Set priors
# Not sure, I took the one from my QP
simple_priors <- prior(normal(0, 1.5), class = b)

## Fit simple model
bls_model_0 <- brm(
  formula = form0, 
  prior = simple_priors, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_0"))

## Model output
summary(bls_model_0)

as_draws_df(bls_model_0) |>
  as_tibble() |>
  select(b_Intercept, phi, zoi, coi) |>
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  posterior_summary() |>
  as.data.frame() |>
  rownames_to_column("Parameter") |>
  kable(digits = 2)

  # Obs:
  # b_Intercept is the mean of the beta part of the model. Among ratings that
    # are not at a boundary (0, 1), the avg rating is about 0.54.
  # phi is the precision of that beta part. It is low (low precision = greater
    # variability: the in-between ratings are spread broadly across the interval 
    # rather than tightly clustered. 
  # zoi refers to probability of a value being on a boundary (0 or 1) and not 
    # in between. ~ 33% of the ratings are either 0 or 1; the rest of the ratings 
    # (~ 67%) fall in between.
  # coi refers to the probability of a boundary value of being 1 rather than 0.
    # Of all the boundary responses, ~ 81% are 1s and ~19% are 0s.
    # Of the total ratings, 27% are exactly 1 and about 6% are 0.

data_bls |>
  filter(!is.na(rating)) |>
  summarise(
    mean_nonboundary = mean(rating[rating > 0 & rating < 1]),
    frac_ones        = mean(rating == 1),
    frac_zeros       = mean(rating == 0),
    frac_nonboundary = mean(rating > 0 & rating < 1),
    coi_check        = sum(rating == 1) / sum(rating %in% c(0, 1)),
    zoi_check        = mean(rating %in% c(0, 1))) |>
  round(3)

# ----- 4.3. Model 1 (verb form) -----------------------------------------------
# Specify model formula
# I think I should add phi, etc. in the model. Check Heiss. 
form1 <- bf(rating ~ 0 + Intercept + form +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form1, family = zero_one_inflated_beta(), data = data_bls)

# Set priors
priors1 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
bls_model_1 <- brm(
  formula = form1, 
  prior = priors1, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_1"))

## Average predicted rating by form (response scale, already back-transformed)
mod1_preds <- avg_predictions(
  model     = bls_model_1,
  variables = "form",
  re_formula = NULL)

mod1_preds  # quick look: `estimate` is the predicted mean rating per form level

## Posterior draws of those predictions
mod1_post <- avg_predictions(
  model     = bls_model_1,
  variables = "form",
  re_formula = NULL) %>%
  posterior_draws()

## Formatted table: median [95% CrI] by form
mod1_post %>%
  group_by(form) %>%
  median_qi(x = draw) %>%
  transmute(
    Form     = form,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))




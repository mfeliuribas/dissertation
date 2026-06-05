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

library(tidyverse)
library(bayesplot)
library(tidybayes)

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
# Not sure, I adapted the one from my QP
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
# Should dominance be added to phi?
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

#bls_model_1 <- readRDS(here("study_2", "models", "bls_model_1.rds"))

## Model output
summary(bls_model_1)





m <- bls_model_1
y <- data_bls$rating

# ----- 1. Overall shape: does the simulated distribution match? --------------
# dens_overlay smears the spikes, so pair it with a view that respects [0,1].
pp_check(m, type = "dens_overlay", ndraws = 100) +
  coord_cartesian(xlim = c(0, 1)) +
  ggtitle("Density overlay (interior shape — diagnoses mu and phi)")

# ----- 2. The endpoints: the heart of the matter ------------------------------
# Define test statistics = the fraction of responses exactly at each endpoint.
# These are governed by zoi (alpha) and coi (gamma). If the model's simulated
# fractions don't bracket the observed fraction, those parameters need a formula.
prop_zero <- function(x) mean(x == 0)
prop_one  <- function(x) mean(x == 1)

pp_check(m, type = "stat", stat = "prop_zero", ndraws = 1000) +
  ggtitle("Proportion of ratings exactly 0  (tests zoi & coi)")

pp_check(m, type = "stat", stat = "prop_one", ndraws = 1000) +
  ggtitle("Proportion of ratings exactly 1  (tests zoi & coi)")







as_draws_df(bls_model_1) |>
  as_tibble() |>
  mutate(
    PF_form = plogis(b_Intercept),
    PI_form  = plogis(b_Intercept + b_formPI),
    PP_form  = plogis(b_Intercept + b_formPP)) |>
  select(PF_form, PI_form, PP_form, phi, zoi, coi) |>
  posterior_summary() |>
  as.data.frame() |>
  rownames_to_column("Parameter") |>
  kable(digits = 2)

  # Obs:
  # This is talking about expected ratings between 0 and 1, without accounting 
    # for exact 0s and 1s.
  # b_Intercept refers to the base form, which I recoded for the table using 
    # mutate().
  # The expected rating given the response falls between 0 and 1 (excluding the
    # boundary values - zoi/coi) is the highest for PF (0.70 [0.65, 0.75]), 
    # followed by PP (0.53 [0.46, 0.59]) and PI (0.48 [0.42, 0.54])
  # The intervals for PP and PI overlap. 



# ----- 1. A grid: one row per form, random effects marginalized out ----------
# re_formula = NA  -> population-level prediction (averages over participants,
#                     items, lists), which is what "the effect of form" means.
form_grid <- data_bls |>
  distinct(form) # PF, PI, PP

epred <- form_grid |>
  add_epred_draws(bls_model_1, re_formula = NA)
# .epred is now the FULL expected rating per draw: it folds in the
# Beta mean, the zero/one inflation, everything. This is E[rating | form],
# the marginal expectation, not the conditional interior mu.

# ----- 2. Summarize the per-form expected rating -----------------------------
epred |>
  group_by(form) |>
  median_qi(.epred) |>
  knitr::kable(digits = 3)

# Obs:
  # Expected rating for each verb form.
  # The expected rating for PF (.74) is higher than the PI (.58)and PP
    # (.62) - comparison values are negative and don't cross the 0.
  # PF > PP > PI.

# ----- 3. Pairwise contrasts: the actual "effect of form" ---------------------
# ----- 4. Plot the three contrasts --------------------------------------------
contrasts <- epred |>
  ungroup() |>
  select(form, .draw, .epred) |>
  pivot_wider(names_from = form, values_from = .epred) |>
  mutate(
    PI_vs_PF = PI - PF,
    PP_vs_PF = PP - PF,
    PP_vs_PI = PP - PI)

contrasts |>
  select(PI_vs_PF, PP_vs_PF, PP_vs_PI) |>
  pivot_longer(everything(), names_to = "contrast", values_to = "diff") |>
  group_by(contrast) |>
  median_qi(diff) |>
  knitr::kable(digits = 3)

contrasts |>
  select(PI_vs_PF, PP_vs_PF, PP_vs_PI) |>
  pivot_longer(everything(), names_to = "contrast", values_to = "diff") |>
  ggplot(aes(x = diff, y = contrast)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in expected rating", y = NULL,
       title = "Effect of verb form (pairwise contrasts)")

# Obs:
  # Form has an effect on rating (effect of form on rating).
  # Both PI and PP are rated lower than the PF: 
    # PI sits 0.151 below PF
    # PP sits 0.117 below PF.
    # Both intervals are negative (below 0) and away from it, thus the PI and 
    # the PP reduce the relative rating to PF.
  # PP is rated 0.034 higher than the PI. The interval excludes 0, so there 
    # seems to be a difference (the difference is nonzero), albeit small. 







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




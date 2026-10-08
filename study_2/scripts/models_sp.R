# ----- Build models: Spanish --------------------------------------------------
#
# This script contains the code used to build the Spanish models (bilinguals + 
# monolinguals) for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("brms")
library("dplyr")
library("tibble")
library("knitr")
library("marginaleffects")
library("tidyverse")
library("ggdist")
library("scales")
library("glue")
library("cmdstanr")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20261007)
# ------------------------------------------------------------------------------


# ----- 3. Load and prepare data -----------------------------------------------
data_spanish <- read.csv(here("study_2", "final_data", "spanish_final.csv"))

# Remove NAs and set factor levels
data_spanish_final <- data_spanish |>
  filter(!is.na(rating_beta)) |>
  mutate(
    participant  = factor(participant),
    item         = factor(item),
    list         = factor(list),
    form         = factor(form, levels = c("PF", "PI", "PP")),
    temp_dist    = factor(temp_dist, levels = c("immediate", "near", "distant")),
    plan_cert    = factor(plan_cert, levels = c("certain", "uncertain")),
    ling_profile = factor(ling_profile, levels = c("monolingual", "bilingual")))

# Sum-to-zero coding
contrasts(data_spanish_final$form)         <- contr.sum(3)
contrasts(data_spanish_final$temp_dist)    <- contr.sum(3)
contrasts(data_spanish_final$plan_cert)    <- contr.sum(2)
contrasts(data_spanish_final$ling_profile) <- contr.sum(2)
colnames(contrasts(data_spanish_final$form))         <- c("PF", "PI")
colnames(contrasts(data_spanish_final$temp_dist))    <- c("immediate", "near")
colnames(contrasts(data_spanish_final$plan_cert))    <- c("certain")
colnames(contrasts(data_spanish_final$ling_profile)) <- c("monolingual")

contrasts(data_spanish_final$form)
# ------------------------------------------------------------------------------


# ----- 4. Model 0 (Intercept-only) --------------------------------------------
# Specify model formula
form0_sp <- bf(rating_beta ~ 0 + Intercept + (1 | participant) + (1 | item))

# Figure out the priors
get_prior(formula = form0_sp, family = Beta(), data = data_spanish_final)

# Set priors
priors_sp_0 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(exponential(1), class = sd),
  prior(exponential(0.1), class = phi))

# Fit simple model
model_sp_0 <- brm(
  formula = form0_sp,
  prior = priors_sp_0,
  iter = 2000, warmup = 1000, cores = 4, seed = 20261007,
  data = data_spanish_final,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_0"))

# Model summary
summary(model_sp_0)

# Posterior predictive check: observed vs. simulated ratings
pp_check(model_sp_0, type = "hist", ndraws = 8, binwidth = 0.02)

# Convert the intercept from logit to the 0-1 scale
model_sp_0_post <- as_draws_df(model_sp_0) |>
  mutate(rating = plogis(b_Intercept))

model_sp_0_post |>
  median_qi(rating) |>
  transmute(
    Estimate = glue(
      "{round(rating, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))
# ------------------------------------------------------------------------------


# ----- 5. Model 1 (Form) ------------------------------------------------------
# Specify model formula
form1_sp <- bf(rating_beta ~ 0 + Intercept + form + 
                 (1 + form | participant) + 
                 (1 + form | item))

# Figure out the priors
get_prior(formula = form1_sp, family = Beta(), data = data_spanish_final)

# Set priors
priors_sp_1 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_sp_1 <- brm(
  formula = form1_sp,
  prior = priors_sp_1,
  iter = 2000, warmup = 1000, cores = 4, seed = 20261007,
  data = data_spanish_final,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_1"))

# Model summary
summary(model_sp_1)

# Posterior predictive check
pp_check(model_sp_1, type = "hist", ndraws = 8, binwidth = 0.02)
# ------------------------------------------------------------------------------


# ----- 6. Model 2 (form * temp_dist + form * plan_cert) -----------------------
# Specify model formula
form2_sp <- bf(rating_beta ~ 0 + Intercept + form * temp_dist + form * plan_cert +
                 (1 + form | participant) + 
                 (1 + form | item))

# Figure out the priors
get_prior(formula = form2_sp, family = Beta(), data = data_spanish_final)

# Set priors
priors_sp_2 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_sp_2 <- brm(
  formula = form2_sp,
  prior = priors_sp_2,
  iter = 2000, warmup = 1000, cores = 4, seed = 20261007,
  data = data_spanish_final,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_2"))

# Model summary
summary(model_sp_2)

# Posterior predictive check
pp_check(model_sp_2, type = "hist", ndraws = 8, binwidth = 0.02)
# ------------------------------------------------------------------------------


# ----- Model 3 (form x temp_dist x plan_cert) ---------------------------------
# Specify model formula
form3_sp <- bf(rating_beta ~ 0 + Intercept + form * temp_dist * plan_cert +
                 (1 + form | participant) + 
                 (1 + form | item))

# Figure out the priors
get_prior(formula = form3_sp, family = Beta(), data = data_spanish_final)

# Set priors
priors_sp_3 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_sp_3 <- brm(
  formula = form3_sp,
  prior = priors_sp_3,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261007,
  data = data_spanish_final,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_3"))

# Model summary
summary(model_sp_3)

# Posterior predictive check
pp_check(model_sp_3, type = "hist", ndraws = 8, binwidth = 0.02)
# ------------------------------------------------------------------------------


# ----- Model 4 (Model 3 + group + form x group) -------------------------------
# Specify model formula
form4_sp <- bf(rating_beta ~ 0 + Intercept + form * temp_dist * plan_cert +
                 ling_profile + form:ling_profile +
                 (1 + form | participant) + 
                 (1 + form | item))

# Figure out the priors
get_prior(formula = form4_sp, family = Beta(), data = data_spanish_final)

# Set priors
priors_sp_4 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_sp_4 <- brm(
  formula = form4_sp,
  prior = priors_sp_4,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261007,
  data = data_spanish_final,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_4"))

# Model summary
summary(model_sp_4)

# Posterior predictive check
pp_check(model_sp_4, type = "hist", ndraws = 8, binwidth = 0.02)
# ------------------------------------------------------------------------------


# ----- Model 5: (form x temp_dist x plan_cert x group) ------------------------
# Specify model formula
form5_sp <- bf(rating_beta ~ 0 + form:temp_dist:plan_cert:ling_profile +
                 (1 + form | participant) + 
                 (1 + form | item))

# Figure out the priors
get_prior(formula = form5_sp, family = Beta(), data = data_spanish_final)

# Set up priors
priors_sp_5 <- c(
  prior(normal(0, 1.5), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_sp_5 <- brm(
  formula = form5_sp, 
  prior = priors_sp_5,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261007,
  control = list(adapt_delta = 0.95),
  data = data_spanish_final, 
  family = Beta(), 
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_5"))

# Model summary
summary(model_sp_5)

# Posterior predictive checks
pp_check(model_sp_5, type = "hist", ndraws = 8, binwidth = 0.02)
pp_check(model_sp_5, type = "ecdf_overlay", ndraws = 50)

# Observed vs. predicted mean ratings by form and group
data_spanish_final |>
  group_by(form, ling_profile) |>
  summarize(observed = mean(rating_beta), .groups = "drop")

avg_predictions(
  model = model_sp_5, 
  by = c("form", "ling_profile"))

# Observed vs. predicted mean ratings for all 36 cells
data_spanish_final |>
  group_by(form, temp_dist, plan_cert, ling_profile) |>
  summarzse(observed = mean(rating_beta), .groups = "drop") |>
  print(n = Inf)

avg_predictions(
  model = model_sp_5, 
  by = c("form", "temp_dist", "plan_cert", "ling_profile")) |>
  print(nrows = 36)

# Obs:
  # For MLs, the observed values fall outside the CrIs.
  # PF is predicted "too low"; PI and PP are predicted "too high"
  # Would adding phi (spread/variation) to the model help with ML values?
# ------------------------------------------------------------------------------


# ----- FINAL Model (Model 5 + phi varying by form and group) ------------------
# Specify model formula
form6_sp <- bf(rating_beta ~ 0 + form:temp_dist:plan_cert:ling_profile +
                  (1 + form | participant) + (1 + form | item),
                phi ~ form * ling_profile)

# Figure out the priors
get_prior(formula = form6_sp, family = Beta(), data = data_spanish_final)

# Set up the priors
priors_sp_6 <- c(
  prior(normal(0, 1.5), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(normal(0, 1.5), class = Intercept, dpar = phi),
  prior(normal(0, 1), class = b, dpar = phi))

# Fit model
model_sp_6 <- brm(
  formula = form6_sp, 
  prior = priors_sp_6,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261007,
  control = list(adapt_delta = 0.95),
  data = data_spanish_final, 
  family = Beta(), 
  backend = "cmdstanr",
  file = here("study_2", "models", "spanish", "model_sp_final"))

# Model summary
summary(model_sp_6)

# Posterior predictive checks
pp_check(model_sp_6, type = "hist", ndraws = 8, binwidth = 0.02)
pp_check(model_sp_6, type = "ecdf_overlay", ndraws = 50)

# Observed vs. predicted mean ratings by form and group
data_spanish_final |>
  group_by(form, ling_profile) |>
  summarize(observed = mean(rating_beta), .groups = "drop")

avg_predictions(
  model = model_sp_6, 
  by = c("form", "ling_profile"))

# Observed vs. predicted mean ratings for all 36 cells
data_spanish_final |>
  group_by(form, temp_dist, plan_cert, ling_profile) |>
  summarize(observed = mean(rating_beta), .groups = "drop") |>
  print(n = Inf)

avg_predictions(
  model = model_sp_6, 
  by = c("form", "temp_dist", "plan_cert", "ling_profile")) |>
  print(nrows = 36)

# Obs:
  # The observed and predicted values now are more similar! 

# Get average predictions and posteriors
# mod_final_preds <- avg_predictions(
#   model      = model_sp_6,
#   variables  = c("form", "temp_dist", "plan_cert", "ling_profile"),
#   re_formula = NULL)

# A similar error as in June popped up - 
# Error: vector memory limit of 16.9 Gb reached
# I tried doing some of the stuff listed here but did not work:
# https://wpnext.org/how-to-solve-vector-memory-exhausted-error/


# Evaluate two different methods of getting the posteriors
# 1) Limiting the draws to 2000
mod_sp_final_preds <- avg_predictions(
  model      = model_sp_6,
  variables  = c("form", "temp_dist", "plan_cert", "ling_profile"),
  re_formula = NULL,
  ndraws     = 2000)

mod_sp_final_post <- posterior_draws(mod_sp_final_preds)


# 2) Doing a for loop
post_list <- list()

for (f in c("PF", "PI", "PP")) {
  for (g in c("monolingual", "bilingual")) {
    pred <- avg_predictions(
      model      = model_sp_6,
      variables  = list(form         = f,
                        temp_dist    = c("immediate", "near", "distant"),
                        plan_cert    = c("certain", "uncertain"),
                        ling_profile = g),
      re_formula = NULL)
    post_list[[paste(f, g)]] <- posterior_draws(pred)}}

mod_sp_final_post_loop <- bind_rows(post_list)

# Compare approaches
compare_draws <- bind_rows(
  mod_sp_final_post_loop |> mutate(version = "d8000"),
  mod_sp_final_post |> mutate(version = "d2000")) |>
  group_by(version, form, temp_dist, plan_cert, ling_profile) |>
  median_qi(x = draw * 100) |>
  select(version, form, temp_dist, plan_cert, ling_profile,
         estimate = x, lower = .lower, upper = .upper) |>
  pivot_wider(names_from = version, values_from = c(estimate, lower, upper))

compare_draws |> print(n = Inf)

compare_draws |>
  summarize(
    max_diff_estimate = max(abs(estimate_d8000 - estimate_d2000)),
    max_diff_lower    = max(abs(lower_d8000 - lower_d2000)),
    max_diff_upper    = max(abs(upper_d8000 - upper_d2000)))

# Obs:
  # The estimates given by both approaches are very similar: either they are the 
  # same or they differ by 0.1 (e.g., 6.18 vs 6.19).


# write_csv(mod_sp_final_post, here("study_2", "models", "spanish", 
#                                 "mod_sp_final_post.csv"))
# 
# saveRDS(mod_sp_final_post, here("study_2", "models", "spanish", 
#                               "mod_final_post.rds"))
# ------------------------------------------------------------------------------


# ----- Model 5 and Final Model comparison -------------------------------------
loo(model_sp_6)

loo_mod_sp5 <- add_criterion(model_sp_5, "loo")
loo_mod_sp6 <- add_criterion(model_sp_6, "loo", reloo = TRUE)

loo_compare(loo_mod_sp5, loo_mod_sp6)

# Comments:
  # Model 6 predicts better than 5
  # elpd_diff is 0.0 (mod5) vs -235.9 (mod6)
  # se_diff is 0.0 (mod5) vs 23.1 (mod6).
# ------------------------------------------------------------------------------

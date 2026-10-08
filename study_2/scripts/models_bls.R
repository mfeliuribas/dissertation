# ----- Build models: Bilinguals -----------------------------------------------
#
# This script contains the code used to build the bilingual models (Spanish + 
# English) for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("brms")
library("dplyr")
library("tibble")
library("knitr")
library("bayesplot")
library("marginaleffects")
library("tidyverse")
library("ggdist")
library("scales")
library("glue")
library("cmdstanr")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20261008)
# ------------------------------------------------------------------------------


# ----- 3. Load and prepare data -----------------------------------------------
data_bls <- read.csv(here("study_2", "final_data", "bls_final.csv"))

# Standardize BLP scores over participants (one row each), not over rows
# TO DO --- Move this code to the final_datasets.R script and update datasets
blp_participants <- data_bls |>
  distinct(participant, blp_score_updated) |>
  mutate(
    blp_updated_z = (blp_score_updated - mean(blp_score_updated)) /
      sd(blp_score_updated))

blp_participants |>
  summarize(n = n(),
            mean_z = mean(blp_updated_z),
            sd_z = sd(blp_updated_z))

summary(blp_participants$blp_score_updated)

# BLP values at -1, 0, +1 SD (for labeling the dominance results)
# -88.7, -51.6, -14.4
blp_participants |>
  summarize(mean = mean(blp_score_updated), sd = sd(blp_score_updated)) |>
  mutate(minus_1sd = mean - sd, plus_1sd = mean + sd)

# Set factor levels, replace blp_updated_z with corrected values
data_bls_final <- data_bls |>
  select(-blp_updated_z) |>
  left_join(blp_participants |> select(participant, blp_updated_z),
            by = "participant") |>
  relocate(blp_updated_z, .after = blp_score_updated) |>
  mutate(
    form      = factor(form, levels = c("PF", "PI", "PP")),
    language  = factor(language, levels = c("sp", "en")),
    temp_dist = factor(temp_dist, levels = c("immediate", "near", "distant")),
    plan_cert = factor(plan_cert, levels = c("certain", "uncertain")))

# Filter NAs, create column for language-specific items, group labels for 
# predictive-check plots
data_bls_clean <- data_bls_final |>
  filter(!is.na(rating_beta)) |>
  mutate(
    item_lang = factor(paste(language, item, sep = "_")),
    cell      = interaction(form, temp_dist, plan_cert, language, sep = "_"),
    form_lang = interaction(form, language, sep = "_"))

# Sum-to-zero coding
contrasts(data_bls_clean$form)      <- contr.sum(3)
contrasts(data_bls_clean$language)  <- contr.sum(2)
contrasts(data_bls_clean$temp_dist) <- contr.sum(3)
contrasts(data_bls_clean$plan_cert) <- contr.sum(2)

colnames(contrasts(data_bls_clean$form))      <- c("PF", "PI")
colnames(contrasts(data_bls_clean$language))  <- c("sp")
colnames(contrasts(data_bls_clean$temp_dist)) <- c("immediate", "near")
colnames(contrasts(data_bls_clean$plan_cert)) <- c("certain")

# Check that sum coding works
contrasts(data_bls_clean$form)
contrasts(data_bls_clean$language)
contrasts(data_bls_clean$temp_dist)
contrasts(data_bls_clean$plan_cert)

# Obs:
  # PP should be -1s
  # en should be -1
  # distant should be -1s
  # uncertain should be -1
# ------------------------------------------------------------------------------


# ----- 4. Helper functions ----------------------------------------------------
# Counterfactual draws over a grid: for each row of 'grid', set those variables
# for ALL rows of 'data', get the expected rating per draw x row, and average
# over rows within each draw. Uses all draws, one grid row at a time (memory).
# cf_draws <- function(model, data, grid) {
#   out <- list()
#   for (i in seq_len(nrow(grid))) {
#     nd <- data
#     for (v in names(grid)) {
#       if (is.factor(nd[[v]])) nd[[v]][] <- as.character(grid[[v]][i])
#       else nd[[v]] <- grid[[v]][i]
#     }
#     epred <- posterior_epred(model, newdata = nd, re_formula = NULL)
#     out[[i]] <- grid[rep(i, nrow(epred)), ] |>
#       mutate(drawid = seq_len(nrow(epred)), draw = rowMeans(epred))
#     rm(epred); gc()
#     message("Grid row ", i, " of ", nrow(grid), " done")
#   }
#   bind_rows(out)
# }
# ------------------------------------------------------------------------------


# ----- 5. Model 0 (Intercept-only) --------------------------------------------
# Specify model formula
form0_bl <- bf(rating_beta ~ 0 + Intercept + (1 | participant) + (1 | item_lang))

# Figure out the priors
get_prior(formula = form0_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_0 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(exponential(1), class = sd),
  prior(exponential(0.1), class = phi))

# Fit model
model_bl_0 <- brm(
  formula = form0_bl,
  prior = priors_bl_0,
  iter = 2000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_0"))

# Model summary
summary(model_bl_0)

# Posterior predictive checks
pp_check(model_bl_0, type = "hist", ndraws = 8, binwidth = 0.02)
pp_check(model_bl_0, type = "ecdf_overlay", ndraws = 50)

## LOO (baseline for later comparisons)
model_bl_0 <- add_criterion(model_bl_0, "loo")
loo(model_bl_0)
# ------------------------------------------------------------------------------


# ----- 6. Model 1 (form) ------------------------------------------------------
# Specify model formula
form1_bl <- bf(rating_beta ~ 0 + Intercept + form +
                 (1 + form | participant) + 
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form1_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_1 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_bl_1 <- brm(
  formula = form1_bl,
  prior = priors_bl_1,
  iter = 2000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_1"))

# Model summary
summary(model_bl_1)

# Posterior predictive checks
pp_check(model_bl_1, type = "ecdf_overlay", ndraws = 50)
pp_check(model_bl_1, type = "stat_grouped", stat = "mean", group = "form", 
         ndraws = 500)

# Model comparison
model_bl_1 <- add_criterion(model_bl_1, "loo")
loo_compare(model_bl_0, model_bl_1)
# ------------------------------------------------------------------------------


# ----- 7. Model 2 (form x language) -------------------------------------------
# Specify model formula
form2_bl <- bf(rating_beta ~ 0 + Intercept + form * language +
                 (1 + form * language | participant) +
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form2_bl, family = Beta(), data = data_bls_clean)

# Set priors (same as M1)
priors_bl_2 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_bl_2 <- brm(
  formula = form2_bl,
  prior = priors_bl_2,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_2"))

# Model summary
summary(model_bl_2)

# Posterior predictive checks
pp_check(model_bl_2, type = "ecdf_overlay", ndraws = 50)

# Model comparison
model_bl_2 <- add_criterion(model_bl_2, "loo")
loo_compare(model_bl_1, model_bl_2)
# ------------------------------------------------------------------------------


# ----- 8. Model 3 (form * language + form * temp_dist * plan_cert) ------------
# Specify model formula
form3_bl <- bf(rating_beta ~ 0 + Intercept + form * language + 
                 form * temp_dist * plan_cert +
                 (1 + form * language | participant) +
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form3_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_3 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_bl_3 <- brm(
  formula = form3_bl,
  prior = priors_bl_3,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_3"))

# Model summary
summary(model_bl_3)

# Posterior predictive checks
pp_check(model_bl_3, type = "ecdf_overlay", ndraws = 50)

# Model comparison
model_bl_3 <- add_criterion(model_bl_3, "loo")
loo_compare(model_bl_2, model_bl_3)
# ------------------------------------------------------------------------------


# ----- 9. Model 4 (form x language x temp_dist x plan_cert) -------------------
# Cell-means parameterization: one coefficient per cell (36 cells).
# Under sum coding, the joint prior on the 36 cell means implied extreme means
# (< 5 or > 95) for 24% of simulated cells; with cell means, < 1%.

# Specify model formula: cell-means parameterization (36 cells)
form4_bl <- bf(rating_beta ~ 0 + form:language:temp_dist:plan_cert +
                 (1 + form * language | participant) +
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form4_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_4 <- c(
  prior(normal(0, 1.5), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

validate_prior(priors_bl_4, formula = form4_bl, family = Beta(),
               data = data_bls_clean)

# Prior predictive check: cell-means version
model_bl_4_prior <- brm(
  formula = form4_bl,
  prior = priors_bl_4,
  sample_prior = "only",
  iter = 2000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "prior", "model_bl_4_prior"))

# Prior predictive check: sum-coded version (comparison only, never fit to data)
form4_bl_sum <- bf(rating_beta ~ 0 + Intercept + form * language * temp_dist * plan_cert +
                     (1 + form * language | participant) +
                     (1 + form | item_lang))

model_bl_4_prior_sum <- brm(
  formula = form4_bl_sum,
  prior = priors_bl_3,
  sample_prior = "only",
  iter = 2000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "prior", "model_bl_4_prior_sum"))

# How often does each prior put a cell mean below 5 or above 95?
yrep_bl_4_prior     <- posterior_predict(model_bl_4_prior, ndraws = 500)
yrep_bl_4_prior_sum <- posterior_predict(model_bl_4_prior_sum, ndraws = 500)

cell_means_prior     <- apply(yrep_bl_4_prior, 1,
                              function(x) tapply(x, data_bls_clean$cell, mean))
cell_means_prior_sum <- apply(yrep_bl_4_prior_sum, 1,
                              function(x) tapply(x, data_bls_clean$cell, mean))

tibble(prior         = c("cell_means", "sum_coded"),
       prop_below_05 = c(mean(cell_means_prior < 0.05), mean(cell_means_prior_sum < 0.05)),
       prop_above_95 = c(mean(cell_means_prior > 0.95), mean(cell_means_prior_sum > 0.95)))

# Fit model
model_bl_4 <- brm(
  formula = form4_bl,
  prior = priors_bl_4,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_4"))

# Model summary
summary(model_bl_4)

# Posterior predictive checks
pp_check(model_bl_4, type = "ecdf_overlay", ndraws = 50)

# Model comparison
model_bl_4 <- add_criterion(model_bl_4, "loo")
loo_compare(model_bl_3, model_bl_4)

# Obs:
  # ELPD -5.6 (SE 2.5) vs. M3. 
  # Language differences in form-specific context effects are small (<= 5 points)
  # with intervals including 0 --- NOT KEPT.
# ------------------------------------------------------------------------------


# ----- 10: Model 5 (form * language + form * temp_dist * plan_cert + phi) -----
# Specify model formula
form5_bl <- bf(rating_beta ~ 0 + Intercept + form * language +
                 form * temp_dist * plan_cert +
                 (1 + form * language | participant) +
                 (1 + form | item_lang),
               phi ~ form * language)

# Figure out the priors
get_prior(formula = form5_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_5 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(normal(0, 1.5), class = Intercept, dpar = phi),
  prior(normal(0, 1), class = b, dpar = phi))

# Fit model
model_bl_5 <- brm(
  formula = form5_bl,
  prior = priors_bl_5,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_5"))

# Model summary
summary(model_bl_5)

# Posterior predictive checks
pp_check(model_bl_5, type = "ecdf_overlay", ndraws = 50)

# Model comparison
model_bl_5 <- add_criterion(model_bl_5, "loo")
loo_compare(model_bl_3, model_bl_5)

# Precision (phi) by form and language, for a typical participant and item
avg_predictions(model_bl_5, 
                by = c("form", "language"),
                dpar = "phi", 
                re_formula = NA)

# Obs:
  # Phi varied only between 1.5 and 2.0, approx. 
  # ELPD +8.4 (SE 8.6) vs. M3, and the estimates were basically unchanged. 
  # Phi not kept (unlike the Spanish model, where it improved LOO by 236).
# ------------------------------------------------------------------------------


# ----- 11. Model 6 (mod3 + language x temp_dist + language x plan_cert) -------
# Specify model formula
form6_bl <- bf(rating_beta ~ 0 + Intercept + form * language +
                  form * temp_dist * plan_cert +
                  language:temp_dist + language:plan_cert +
                  (1 + form * language | participant) +
                  (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form6_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_6 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

# Fit model
model_bl_6 <- brm(
  formula = form6_bl,
  prior = priors_bl_6,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_6"))

# Model summary
summary(model_bl_6)

# Model comparison
model_bl_6 <- add_criterion(model_bl_6, "loo")
loo_compare(model_bl_3, model_bl_6)

# Obs:
  # ELPD -1.7 (SE 1.0) vs. M3: practically the same.
# ------------------------------------------------------------------------------


# ----- 12: Model 7 (all predictors, dominance by form and by language only) ---
# Specify model formula
form7_bl <- bf(rating_beta ~ 0 + Intercept + form * language +
                 form * temp_dist * plan_cert +
                 language:temp_dist + language:plan_cert +
                 blp_updated_z + form:blp_updated_z + language:blp_updated_z +
                 (1 + form * language | participant) +
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form7_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_7 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 0.5), class = b, coef = blp_updated_z),
  prior(normal(0, 0.5), class = b, coef = "formPF:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "formPI:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "languagesp:blp_updated_z"),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

validate_prior(priors_bl_7, formula = form7_bl, family = Beta(),
               data = data_bls_clean)

# Fit model
model_bl_7 <- brm(
  formula = form7_bl,
  prior = priors_bl_7,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_7"))

# Model summary
summary(model_bl_7)

# Loo
model_bl_7 <- add_criterion(model_bl_7, "loo")
loo(model_bl_7)

# Obs:
  # All dominance terms small (< 0.1 logit per SD) with intervals including 0.
  # Participant SDs for language and form x language barely changed.
# ------------------------------------------------------------------------------


# ----- 13. FINAL Model 8 (all predictors) -------------------------------------
form8_bl <- bf(rating_beta ~ 0 + Intercept + form * language * blp_updated_z +
                 form * temp_dist * plan_cert +
                 language:temp_dist + language:plan_cert +
                 (1 + form * language | participant) +
                 (1 + form | item_lang))

# Figure out the priors
get_prior(formula = form8_bl, family = Beta(), data = data_bls_clean)

# Set priors
priors_bl_8 <- c(
  prior(normal(0, 1.5), class = b, coef = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 0.5), class = b, coef = blp_updated_z),
  prior(normal(0, 0.5), class = b, coef = "formPF:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "formPI:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "languagesp:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "formPF:languagesp:blp_updated_z"),
  prior(normal(0, 0.5), class = b, coef = "formPI:languagesp:blp_updated_z"),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor),
  prior(exponential(0.1), class = phi))

validate_prior(priors_bl_8, formula = form8_bl, family = Beta(),
               data = data_bls_clean)

# Fit model
model_bl_8 <- brm(
  formula = form8_bl,
  prior = priors_bl_8,
  iter = 3000, warmup = 1000, cores = 4, seed = 20261008,
  data = data_bls_clean,
  family = Beta(),
  backend = "cmdstanr",
  file = here("study_2", "models", "bilinguals", "model_bl_8"))

# Model summary
summary(model_bl_8)

# Posterior predictive checks
pp_check(model_bl_8, type = "hist", ndraws = 8, binwidth = 0.02)
pp_check(model_bl_8, type = "ecdf_overlay", ndraws = 50)

# Proportion of ratings at the top and bottom of the scale, by form x language
yrep_bl_8 <- posterior_predict(model_bl_8, ndraws = 1000)

ppc_stat_grouped(y = data_bls_clean$rating_beta, yrep = yrep_bl_8,
                 group = data_bls_clean$form_lang,
                 stat = function(y) mean(y >= 0.99))

# Observed vs. predicted mean ratings for all 36 cells
data_bls_clean |>
  group_by(form, temp_dist, plan_cert, language) |>
  summarize(observed = mean(rating_beta), .groups = "drop") |>
  print(n = Inf)

avg_predictions(
  model  = model_bl_8,
  by     = c("form", "temp_dist", "plan_cert", "language"),
  ndraws = 2000) |>
  as_tibble() |>
  print(n = Inf)

# Loo
model_bl_8 <- add_criterion(model_bl_8, "loo")
loo(model_bl_8)

# Obs:
  # 0 divergences, Rhat OK, Bulk_ESS OK, Tail_ESS OK.


# Get average predictions and posteriors for linguistic predictors

# mod_bl_final_preds <- avg_predictions(
#   model      = model_bl_8,
#   variables  = c("form", "temp_dist", "plan_cert", "language"),
#   re_formula = NULL)

# Error: vector memory limit of 16.9 Gb reached
# I tried doing some of the stuff listed here but did not work:
# https://wpnext.org/how-to-solve-vector-memory-exhausted-error/
# Same error appeared when limiting the draws to 2,000. 

post_bl_list <- list()

for (f in c("PF", "PI", "PP")) {
  for (l in c("sp", "en")) {
    pred <- avg_predictions(
      model      = model_bl_8,
      variables  = list(form      = f,
                        temp_dist = c("immediate", "near", "distant"),
                        plan_cert = c("certain", "uncertain"),
                        language  = l),
      re_formula = NULL)
    post_bl_list[[paste(f, l)]] <- posterior_draws(pred)
    rm(pred); gc()}}

mod_bl_final_post <- bind_rows(post_bl_list)

# Checks
# 8,000 and 288,000 (8,000 x 36)
n_distinct(mod_bl_final_post$drawid)
nrow(mod_bl_final_post)

# Save posteriors
saveRDS(mod_bl_final_post, here("study_2", "models", "bilinguals", "posteriors",
                                "mod_bl_final_post.rds"))
write_csv(mod_bl_final_post, here("study_2", "models", "bilinguals", "posteriors",
                                 "mod_bl_final_post.csv"))

# Get average predictions and posteriors for language dominance
post_blp_list <- list()

for (f in c("PF", "PI", "PP")) {
  for (l in c("sp", "en")) {
    pred <- avg_predictions(
      model      = model_bl_8,
      variables  = list(form          = f,
                        language      = l,
                        blp_updated_z = c(-1, 0, 1)),
      re_formula = NULL)
    post_blp_list[[paste(f, l)]] <- posterior_draws(pred)
    rm(pred); gc()}}

mod_blp_post <- bind_rows(post_blp_list)

# Checks
# 8,000 and 144,000 (8,000 x 18)
n_distinct(mod_blp_post$drawid)
nrow(mod_blp_post)

# Save posteriors
saveRDS(mod_blp_post, here("study_2", "models", "bilinguals", "posteriors",
                              "mod_blp_post.rds"))
write_csv(mod_blp_post, here("study_2", "models", "bilinguals", "posteriors",
                                  "mod_blp_post.csv"))
# ------------------------------------------------------------------------------


# ----- Model comparison -------------------------------------------------------
loo_compare(model_bl_0, model_bl_1)
loo_compare(model_bl_1, model_bl_2)
loo_compare(model_bl_2, model_bl_3)
loo_compare(model_bl_3, model_bl_4)
loo_compare(model_bl_3, model_bl_5)
loo_compare(model_bl_3, model_bl_6)
loo_compare(model_bl_6, model_bl_7, model_bl_8)

# Obs:
  # M1 vs. M0: +977 (SE 43); M2 vs. M1: +378 (SE 31); M3 vs. M2: +4.3 (SE 4.2)
  # M4 vs. M3: -5.6 (SE 2.5); M5 vs. M3: +8.4 (SE 8.6); M6 vs. M3: -1.7 (SE 1.0)
  # M8 vs. M6: -2.2 (SE 1.8); M7 vs. M6: -3.8 (SE 1.3)
  # Dominance barely changes the prediction of individual ratings (one score
  # per participant), but the dominance contrasts are clear.
# ------------------------------------------------------------------------------

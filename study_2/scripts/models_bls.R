# ----- Build models: Bilinguals -----------------------------------------------
#
# This script contains the code used to build the bilingual models for Study 2.
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
  formula = form0_bls, 
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
# ------------------------------------------------------------------------------



# ----- 4.2. Model 1 (verb form) -----------------------------------------------
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

## Model output
summary(bls_model_1)

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


## Average predicted rating by form (response scale, already back-transformed)
mod1_preds <- avg_predictions(
  model     = bls_model_1,
  variables = "form",
  re_formula = NULL)

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
# ------------------------------------------------------------------------------



# ----- 4.3. Model 2 (verb form, temporal reference) ---------------------------
# Specify model formula
# I think I should add phi, etc. in the model. Check Heiss. 
# Should dominance be added to phi?
form2 <- bf(rating ~ 0 + Intercept + form * temp_dist +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form2, family = zero_one_inflated_beta(), data = data_bls)

# Set priors
priors2 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
bls_model_2 <- brm(
  formula = form2, 
  prior = priors2, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_2"))
# Warning message: 4: Bulk Effective Samples Size (ESS) is too low, indicating 
# posterior means and medians may be unreliable.

## Model output
summary(bls_model_2)

## Average predicted rating by form and temporal distance
mod2_pred <- avg_predictions(
  model      = bls_model_2,
  variables  = c("form", "temp_dist"),
  re_formula = NULL)

mod2_post <- posterior_draws(mod2_pred) # |> write_csv(here("mod2_post.csv"))

## Marginal over form: temp_dist on its own
mod2_post_tempdist <- avg_predictions(
  model      = bls_model_2,
  variables  = "temp_dist",
  re_formula = NULL) |>
  posterior_draws()

## Table: form by temp_dist
mod2_post |>
  group_by(form, temp_dist) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Reference = temp_dist,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

  # Obs:
    # PF: near > distant > immediate
    # PI: near > immediate > distant
    # PP: near > immediate > distant
    # But... the intervals overlap with each other, probably because the model
    # is not considering language yet? 

## Table: temp_dist only
mod2_post_tempdist |>
  group_by(temp_dist) |>
  median_qi(x = draw) |>
  transmute(
    Reference = temp_dist,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

## Plot (mod2_post) - posterior medians of predicted rating with 95% and 65% credible intervals
mod2_post |>
  ggplot() + 
  aes(x = draw, y = form, fill = temp_dist) + 
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") + 
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) + 
  labs(y = NULL, x = "Avg Predicted Rating|Form * Temporal Distance)") + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0), 
        legend.background = element_blank())
# ------------------------------------------------------------------------------



# ----- 4.4. Model 3 (verb form, temporal reference, plan certainty) -----------
# Specify model formula
# I think I should add phi, etc. in the model. Check Heiss. 
# Should dominance be added to phi?
form3 <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form3, family = zero_one_inflated_beta(), data = data_bls)

# Set priors
priors3 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
bls_model_3 <- brm(
  formula = form3, 
  prior = priors3, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_3"))

## Model output
summary(bls_model_3)

## Average predicted rating by form, temporal distance, and plan certainty
mod3_pred <- avg_predictions(
  model      = bls_model_3,
  variables  = c("form", "temp_dist", "plan_cert"),
  re_formula = NULL)

mod3_post <- posterior_draws(mod3_pred) # |> write_csv(here("mod3_post.csv"))

## Marginal over form: plan_cert on its own
mod3_post_cert <- avg_predictions(
  model      = bls_model_3,
  variables  = "plan_cert",
  re_formula = NULL) |>
  posterior_draws()

## Table: plan_cert only
mod3_post_cert |>
  group_by(plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Plan = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

mod3_post |>
  group_by(form, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

  # Obs:
    # Overall, all forms receive higher ratings in the certain condition.
    # The CrI for certain-uncertain conditions by specific form overlap, but the     # estimate does not. 
    # Must perform comparisons to see what this means in terms of stats.
    # When only certainty is considered (and not form), there is no overlap 
    # between the CrI.

## Table: form by temp_dist by plan_cet
mod3_post |>
  group_by(form, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Reference = temp_dist,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
  # Overall it seems that forms get higher ratings the closer and certain the 
    # plan, with the exception of the PF.
  # Interestingly, the near-certain condition gets higher ratings than the 
    # immediate for the PF and the PP. In the PI, the pattern resembles what 
    # would be expected (immediate > near > distant).

## Plot (mod3_post) - posterior medians of predicted rating with 95% and 65% credible intervals
mod3_post |>
  ggplot() + 
  aes(x = draw, y = form, fill = temp_dist) + 
  facet_grid(. ~ plan_cert) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") + 
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) + 
  labs(y = NULL, x = "Avg Predicted Rating|Form * Distance * Certainty)") + 
  theme(legend.position = "bottom", legend.justification = c(1, 0), 
        legend.background = element_blank())
# ------------------------------------------------------------------------------



# ----- 4.5. Model 4 (verb form, temporal reference, plan certainty, language) -
# Specify model formula
# Should dominance be added to phi?
form4 <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert * language +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form4, family = zero_one_inflated_beta(), data = data_bls)


# Set priors
priors4 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"),
             prior(lkj(2),         class = cor),
             prior(gamma(2, 0.5),  class = phi))

## Fit simple model
bls_model_4 <- brm(
  formula = form4, 
  prior = priors4, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_4"))

## Model output
summary(bls_model_4)

## Average predicted rating by form, temporal distance, and plan certainty
mod4_pred <- avg_predictions(
  model      = bls_model_4,
  variables  = c("form", "temp_dist", "plan_cert", "language"),
  re_formula = NULL)

mod4_post <- posterior_draws(mod4_pred) # |> write_csv(here("mod4_post.csv"))

## Marginal over form: language on its own
mod4_post_lang <- avg_predictions(
  model      = bls_model_4,
  variables  = "language",
  re_formula = NULL) |>
  posterior_draws()

## Table: language only
mod4_post_lang |>
  group_by(language) |>
  median_qi(x = draw) |>
  transmute(
    Language = language,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

mod4_post |>
  group_by(form, language) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Language  = language,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# Overall, all forms receive similar ratings across languages.
# The predicted average ratings for the PF and the PI are higher in Spanish; 
  # that of the PP is higher in English.
# The CrI for all forms overlap. 

## Table: form by language, temp_dist and plan_cert
mod4_post |>
  group_by(form, language, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  mutate(
    Form      = factor(form,      levels = c("PF", "PI", "PP")),
    Language  = factor(language,  levels = c("sp", "en")),
    Distance  = factor(temp_dist, levels = c("immediate", "near", "distant")),
    Plan      = factor(plan_cert, levels = c("certain", "uncertain"))) |>
  transmute(
    Form, Language, Distance, Plan,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
   arrange(Language, Form, Distance, Plan) |>
 print(n = 36)

# Obs:
# Overall it seems that certainty might play a role in form rating in both 
  # Spanish and English, as well as temporal distance. It must be checked, 
  # though, if there are statistical differences between immediate and near 
  # conditions. 
# In both Spanish and English the highest rated form is the PF.
# The PF in the distant-certain condition receives the higher ratings in Spanish
  # (estimate = .77), followed by the same form in the near-certain condition 
  # (estimate = .761). 
# In English, the form that receives the highest ratings is also the PF in the 
  # near-certain condition (estimate = .751), followed by the same form in the 
  # distant-certain (estimate = .738) and immediate-certain (estimate =.732).
# In Spanish, the lowest rated form is the PP in the immediate-uncertain 
  # condition (estimate = .552), whereas in English is the PI in the same 
  # condition (estimate = .503).

## Plot (mod4_post) - posterior medians of predicted rating with 95% and 65% 
## credible intervals
mod4_post |>
  filter(language == "sp") |>
  ggplot() +
  aes(x = draw, y = form, fill = temp_dist) +
  facet_grid(. ~ plan_cert) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating | Form * Distance * Certainty",
       title = "Spanish") +
  theme(legend.position = "bottom", legend.justification = c(1, 0),
        legend.background = element_blank())

mod4_post |>
  filter(language == "en") |>
  ggplot() +
  aes(x = draw, y = form, fill = temp_dist) +
  facet_grid(. ~ plan_cert) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating | Form * Distance * Certainty",
       title = "English") +
  theme(legend.position = "bottom", legend.justification = c(1, 0),
        legend.background = element_blank())

mod4_post |>
  ggplot() +
  aes(x = draw, y = form, fill = temp_dist) +
  facet_grid(language ~ plan_cert) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating | Form * Distance * Certainty") +
  theme(legend.position = "bottom", legend.justification = c(1, 0),
        legend.background = element_blank())
# ------------------------------------------------------------------------------



# ----- 4.6. Model 5 (verb form, temporal reference, plan certainty, language, 
#                    language dominance) --------------------------------------
# Specify model formula
# Should dominance be added to phi?
# BLP score should interact with form in order to find out how language dominance
# affects form rating (and not just rating). This is not modified in this model
# but in the final one.
form5 <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert * language + 
              blp_z +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form5, family = zero_one_inflated_beta(), data = data_bls)


# Set priors
priors5 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"),
             prior(lkj(2),         class = cor),
             prior(gamma(2, 0.5),  class = phi))

## Fit model
bls_model_5 <- brm(
  formula = form5, 
  prior = priors5, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_5"))

## Model output
summary(bls_model_5)

## Average predicted rating by form, temporal distance, plan certainty, language,
## and BLP score
## I could not get the posteriors - I tried multiple times and R aborts the 
## session. Apparently there is not enough memory in the computer. Perhaps 
## reducing the number of draws might help? Or would it make sense to get 
## the posteriors multiple times with different predictors (e.g., language 
## dominance just with form).
mod5_pred <- avg_predictions(
  model      = bls_model_5,
  variables  = c("form", "temp_dist", "plan_cert", "language", "blp_z"),
  re_formula = NULL)

mod5_post <- posterior_draws(mod5_pred) # |> write_csv(here("mod5_post.csv"))
# ------------------------------------------------------------------------------



# ----- 4.6. Final Model (verb form, temporal reference, plan certainty, 
#                         language, language dominance, proficiency) -----------
# Specify model formula
# Should dominance be added to phi?
form6 <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert * language + 
              form:blp_z + form:lextale_avg_z:language + 
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form6, family = zero_one_inflated_beta(), data = data_bls)

# Set priors
priors6 <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"),
             prior(lkj(2),         class = cor),
             prior(gamma(2, 0.5),  class = phi))

## Fit model
bls_model_final <- brm(
  formula = form6, 
  prior = priors6, 
  iter = 8000, warmup = 2000, cores = 8, 
  backend = "cmdstanr",
  data = data_bls, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "bls_model_final"))
# Warning messages: 2252 of 24000 (9.0%) transitions ended with a divergence.

## Model output
summary(bls_model_final)

# Parts of the model have not converged (some Rhats are > 1.05). Be careful 
# when analysing the results! We recommend running more iterations and/or 
# setting stronger priors. 

bls_model_final <- readRDS(here("study_2", "models", "bls_model_final.rds"))

## Average predicted rating by form, temporal distance, plan certainty, language,
## BLP score, LexTale score.
## I could not get the posteriors at first - I tried multiple times and R aborts 
## the session. Apparently there is not enough memory in the computer. I tried 
## reducing the number of draws and only getting the posteriors from form, 
## language dominance, lextale, and language and it worked.
# mod_final_pred <- avg_predictions(
#   model      = bls_model_final,
#   variables  = c("form", "temp_dist", "plan_cert", "language", 
#                  "blp_z", "lextale_avg_z"),
#   re_formula = NULL,
#   draw_ids = sample(1:4000, 500))

# Posteriors from only categorical preds
mod_final_pred_1 <- avg_predictions(
  model      = bls_model_final,
  variables  = c("form", "temp_dist", "plan_cert", "language"),
  re_formula = NULL,
  draw_ids = sample(1:4000, 500))

mod_final_post_1 <- posterior_draws(mod_final_pred_1) |> 
  write_csv(here("study_2", "models", "mod_final_bls_post1.csv"))

# Posteriors from form by language dominance
mod_final_pred_2 <- avg_predictions(
  model      = bls_model_final,
  variables  = c("form", "blp_z"),
  re_formula = NULL)

mod_final_post_2 <- posterior_draws(mod_final_pred_2) |> 
  write_csv(here("study_2", "models", "mod_final_bls_post2.csv"))

mod_final_post_2 |>
  group_by(form, blp_z) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Dominance = blp_z,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

mod_final_post_2 |>
  group_by(form, blp_z) |>
  median_qi(draw, .width = c(0.65, 0.95)) |>
  ggplot(aes(x = blp_z, y = draw)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper, fill = factor(.width))) +
  facet_wrap(~ form) +
  scale_fill_brewer(name = "CrI", palette = "Blues") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Bilingual dominance (z)",
       y = "Avg Predicted Rating") +
  theme(legend.position = "bottom")


# Posteriors from form by language by lextale score
# Ask Joseph if the code for the posterior draws is correct. The results are 
# not what I was expecting (especially for English) but when plotting the data
# (not the posteriors) the trend is similar.

# This code was causing problems, that is why we switch to the next chunk.
# mod_final_pred_3 <- avg_predictions(
#   model      = bls_model_final,
#   by         = c("form", "language", "lextale_avg_z"),
#   re_formula = NULL,
#   draw_ids   = sample(1:4000, 500))
# mod_final_post_3 <- posterior_draws(mod_final_pred_3) |> 
#   write_csv(here("study_2", "models", "mod_final_bls_post3.csv"))

mod_final_pred_3 <- avg_predictions(
  model      = bls_model_final,
  variables  = list(
    form          = unique(data_bls$form),
    language      = unique(data_bls$language),
    lextale_avg_z = seq(-3, 2.5, length.out = 25)),
  re_formula = NULL,
  draw_ids   = sample(1:ndraws(bls_model_final), 500))

mod_final_post_3 <- posterior_draws(mod_final_pred_3) |>
  write_csv(here("study_2", "models", "mod_final_bls_post3.csv"))

mod_final_post_3 |>
  group_by(form, language, lextale_avg_z) |>
  median_qi(draw, .width = c(0.65, 0.95)) |>
  left_join(lex_stats, by = "language") |>
  mutate(proficiency = lextale_avg_z * lex_sd + lex_mean) |>
  ggplot(aes(x = proficiency, y = draw)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper, fill = factor(.width))) +
  facet_grid(language ~ form) +
  scale_fill_brewer(name = "CrI", palette = "Blues") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Proficiency (LexTALE score)",
       y = "Avg Predicted Rating") +
  theme(legend.position = "bottom")


ndraws(bls_model_final)

ggplot(data_bls, aes(x = lextale_avg, y = rating)) +
  geom_point(alpha = 0.2, position = position_jitter(height = 0.02)) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_grid(language ~ form) +
  labs(x = "Proficiency (LexTALE score)",
       y = "Rating") +
  theme_minimal()
  






## Code from June 16 (old final model)
## I could not get the posteriors at first - I tried multiple times and R aborts 
## the session. Apparently there is not enough memory in the computer. I tried 
## reducing the number of draws and only getting the posteriors from form, 
## language dominance, lextale, and language and it worked.
mod_final_scores <- avg_predictions(
  model      = bls_model_final,
  variables  = c("form", "blp_z", "lextale_avg_z"),
  re_formula = NULL,
  draw_ids = sample(1:4000, 500))

mod_final_scores_post <- posterior_draws(mod_final_scores) 

mod_final_scores <- avg_predictions(
  model      = bls_model_final,
  variables  = c("form", "blp_z", "lextale_avg_z"),
  re_formula = NULL,
  draw_ids = sample(1:4000, 500)) |>
  posterior_draws()

mod_final_scores |>
  group_by(form, blp_z) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Dominance = blp_z,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

mod_final_scores |>
  group_by(form, blp_z) |>
  median_qi(draw, .width = c(0.65, 0.95)) |>
  ggplot(aes(x = blp_z, y = draw)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper, fill = factor(.width))) +
  facet_wrap(~ form) +
  scale_fill_brewer(name = "CrI", palette = "Blues") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Bilingual dominance (z)",
       y = "Avg Predicted Rating") +
  theme(legend.position = "bottom")


blp_mean <- mean(data_bls$blp_score, na.rm = TRUE)
blp_sd   <- sd(data_bls$blp_score,   na.rm = TRUE)

mod_final_scores |>
  group_by(form, blp_z) |>
  median_qi(draw, .width = c(0.65, 0.95)) |>
  mutate(dominance = blp_z * blp_sd + blp_mean) |>
  ggplot(aes(x = dominance, y = draw)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper, fill = factor(.width))) +
  facet_wrap(~ form) +
  scale_fill_brewer(name = "CrI", palette = "Blues") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Bilingual dominance (BLP score)",
       y = "Avg Predicted Rating") +
  theme(legend.position = "bottom")

# Obs:
  # It seems that language dominance plays a role in the ratings for PI and PP.
  # In the case of the PP, there is a steep, negative slope, which implies that
  # higher Spanish dominance leads to lower average predicted ratings for this 
  # form. 
  # As for the PI, there is also a negative slope, but it is less steep. 
  # The slope for the PF is relatively flat with possibly a slight positive 
  # inclination.

mod_final_lextale <- avg_predictions(
  bls_model_final,
  variables  = c("form", "language", "lextale_avg_z"),
  re_formula = NULL,
  draw_ids   = sample(1:4000, 500)) |>
  posterior_draws()




mod_final_lextale |>
  group_by(form, language, lextale_avg_z) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    language  = language,
    Proficiency = lextale_avg_z,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

lex_stats <- data_bls |>
  group_by(language) |>
  summarize(lex_mean = mean(lextale_avg),
            lex_sd   = sd(lextale_avg),
            .groups = "drop")

mod_final_lextale |>
  group_by(form, language, lextale_avg_z) |>
  median_qi(draw, .width = c(0.65, 0.95)) |>
  left_join(lex_stats, by = "language") |>
  mutate(proficiency = lextale_avg_z * lex_sd + lex_mean) |>
  ggplot(aes(x = proficiency, y = draw)) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper, fill = factor(.width))) +
  facet_grid(language ~ form) +
  scale_fill_brewer(name = "CrI", palette = "Blues") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Proficiency (LexTALE score)",
       y = "Avg Predicted Rating") +
  theme(legend.position = "bottom")

# Obs:
  # It seems that language proficiency plays a role in the ratings for all forms.
  # In all cases and in both Spanish and English there are steep, negative 
  # slopes, which imply that higher proficiency scores (as measured by the 
  # LexTale) lead to lower average predicted ratings for all forms. 
  # If I calculated the estimates correctly for the LexTale and the BLP, these
  # results contradict previous studies that reported no effect whatsoever 
  # (e.g., Zahler and Leguisamon Tolentino, 2024).


# This code must be checked.
mod_final_pred <- avg_predictions(
  model      = bls_model_final,
  variables  = c("form", "temp_dist", "plan_cert", "language", "blp_z", 
                 "lextale_avg_z"),
  re_formula = NULL)

mod_final_post <- posterior_draws(mod_final_pred) # |> 
#   write_csv(here("mod_final_bls_post.csv"))

# TO DO:
  # Think about the priors... 
    # If I expect PF to have higher ratings (perhaps even more 1 than any other 
      # form) would it make sense to add it as a pred. in coi (e.g., coi ~ form)?
      # This might also be motivated by previous studies which report the PF to 
      # be the default form for futurity.
    # If I expect that variability among speakers might be better explained by
      # language dominance, would it make sense to add it as a pred. in phi 
      # (e.g., phi ~ blp_z)?
# ------------------------------------------------------------------------------

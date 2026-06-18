# ----- Build models: English --------------------------------------------------
#
# This script contains the code used to build the English models (bilinguals + 
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
set.seed(20260616)

# ----- 3. Load data -----------------------------------------------------------
data_english <- read.csv(here("study_2", "final_data", "english_final.csv"))
# ------------------------------------------------------------------------------

# ----- 4. Fit English models --------------------------------------------------
# ----- 4.1. Intercept-only model ----------------------------------------------
# Specify model formula
form0_en_en <- bf(rating ~ 0 + Intercept)

# Figure out the priors
get_prior(formula = form0_en, family = zero_one_inflated_beta(), 
          data = data_english)

# Set priors
simple_priors_en <- prior(normal(0, 1.5), class = b)

## Fit simple model
en_model_0 <- brm(
  formula = form0_en, 
  prior = simple_priors_en, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_english, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "en_model_0"))

## Model output
summary(en_model_0)

as_draws_df(en_model_0) |>
  as_tibble() |>
  select(b_Intercept, phi, zoi, coi) |>
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  posterior_summary() |>
  as.data.frame() |>
  rownames_to_column("Parameter") |>
  kable(digits = 2)

data_english |>
  filter(!is.na(rating)) |>
  summarise(
    mean_nonboundary = mean(rating[rating > 0 & rating < 1]),
    frac_ones        = mean(rating == 1),
    frac_zeros       = mean(rating == 0),
    frac_nonboundary = mean(rating > 0 & rating < 1),
    coi_check        = sum(rating == 1) / sum(rating %in% c(0, 1)),
    zoi_check        = mean(rating %in% c(0, 1))) |>
  round(3)

# Obs:
# b_Intercept is the mean of the beta part of the model. Among ratings that
# are not at a boundary (0, 1), the avg rating is about 0.52.
# phi is the precision of that beta part. It is low (low precision = greater
# variability: the in-between ratings are spread broadly across the interval 
# rather than tightly clustered.
# zoi refers to probability of a value being on a boundary (0 or 1) and not 
# in between. ~ 27% of the ratings are either 0 or 1; the rest of the ratings 
# (~ 73%) fall in between.
# coi refers to the probability of a boundary value of being 1 rather than 0.
# Of all the boundary responses, ~ 76% are 1s and ~24% are 0s.
# Of the total ratings, 20% are exactly 1 and about 6% are 0.
# ------------------------------------------------------------------------------



# ----- 4.2. Model 1 (verb form) -----------------------------------------------
# Specify model formula
form1_en <- bf(rating ~ 0 + Intercept + form +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form1_en, family = zero_one_inflated_beta(), 
          data = data_english)

# Set priors
priors1_en <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
en_model_1 <- brm(
  formula = form1_en, 
  prior = priors1_en, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_english, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "en_model_1"))

# Warning messages:
# 2: There were 108 divergent transitions after warmup. Increasing adapt_delta 
#   above 0.8 may help
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means  
#   and medians may be unreliable. Running the chains for more iterations may help. 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances 
#   and tail quantiles may be unreliable. Running the chains for more iterations 
#   may help. 

## Model output
summary(en_model_1)

as_draws_df(en_model_1) |>
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
#   for exact 0s and 1s.
# b_Intercept refers to the base form, which I recoded for the table using 
#   mutate().
# The expected rating given the response falls between 0 and 1 (excluding the
#   boundary values - zoi/coi) is the highest for PF (0.70 [0.62, 0.77]), 
#   followed by PP (0.59 [0.50, 0.67]) and PI (0.37 [0.29, 0.46]). For the 
#   Spanish model it was PF > PI > PP.

## Average predicted rating by form (response scale, already back-transformed)
mod1_preds_en <- avg_predictions(
  model     = en_model_1,
  variables = "form",
  re_formula = NULL)

## Posterior draws of those predictions
mod1_post_en <- avg_predictions(
  model     = en_model_1,
  variables = "form",
  re_formula = NULL) %>%
  posterior_draws()

## Formatted table: median [95% CrI] by form
mod1_post_en %>%
  group_by(form) %>%
  median_qi(x = draw) %>%
  transmute(
    Form     = form,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))
# ------------------------------------------------------------------------------



# ----- 4.3. Model 2 (verb form, temporal reference) ---------------------------
# Specify model formula
form2_en <- bf(rating ~ 0 + Intercept + form * temp_dist +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form2_en, family = zero_one_inflated_beta(), 
          data = data_english)

# Set priors
priors2_en <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
en_model_2 <- brm(
  formula = form2_en, 
  prior = priors2_en, 
  iter = 2000, warmup = 1000, cores = 4, 
  backend = "cmdstanr",
  data = data_english, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "en_model_2"))

# Warning message: 124 of 4000 (3.0%) transitions ended with a divergence.

## Model output
summary(en_model_2)

## Average predicted rating by form and temporal distance
mod2_pred_en <- avg_predictions(
  model      = en_model_2,
  variables  = c("form", "temp_dist"),
  re_formula = NULL)

mod2_post_en <- posterior_draws(mod2_pred_en)

## Table: form by temp_dist
mod2_post_en |>
  group_by(form, temp_dist) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Distance  = temp_dist,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# PF: immediate > near > distant (estimates and CrI are almost identical, 
#   there might be no differences across temporal distance conditions).
# PI: near > immediate > distant.
# PP: near > immediate > distant.
# But... most intervals within form overlap with each other, especially with 
#   the PF and the presents in the immediate-near conditions.

## Plot (mod2_post) - posterior medians of predicted rating with 95% and 65% 
##  credible intervals
mod2_post_en |>
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
form3_en <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form3_en, family = zero_one_inflated_beta(), 
          data = data_english)

# Set priors
priors3_en <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
en_model_3 <- brm(
  formula = form3_en, 
  prior = priors3_en, 
  iter = 2000, warmup = 1000, cores = 4, 
  backend = "cmdstanr",
  data = data_english, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "en_model_3"))

# Warning: 210 of 4000 (5.0%) transitions ended with a divergence.
# Increasing adapt_delta above 0.8 may help.

## Model output
summary(en_model_3)

## Average predicted rating by form, temporal distance, and plan certainty
mod3_pred_en <- avg_predictions(
  model      = en_model_3,
  variables  = c("form", "temp_dist", "plan_cert"),
  re_formula = NULL)

mod3_post_en <- posterior_draws(mod3_pred_en)

mod3_post_en |>
  group_by(form, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# Overall, all forms receive higher ratings in the certain condition.

## Table: form by temp_dist by plan_cet
mod3_post_en |>
  group_by(form, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Reference = temp_dist,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# Overall it seems that forms get higher ratings the closer and certain the 
#   plan, with the exception of the PF. The PF have similar ratings across 
#   conditions.

## Plot (mod3_post) - posterior medians of predicted rating with 95% and 65% 
# credible intervals
mod3_post_en |>
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



# ----- 4.5. Final Model (verb form, temporal reference, plan certainty, profile) -
# Specify model formula
form4_en <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert * ling_profile +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form4_en, family = zero_one_inflated_beta(), 
          data = data_english)

# Set priors
priors4_en <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"),
             prior(lkj(2),         class = cor),
             prior(gamma(2, 0.5),  class = phi))

## Fit simple model
en_model_final <- brm(
  formula = form4_en, 
  prior = priors4_en, 
  iter = 8000, warmup = 2000, cores = 8, 
  backend = "cmdstanr",
  data = data_english, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "en_model_final"))

# Warning messages: 525 of 24000 (2.0%) transitions ended with a divergence.

## Model output
summary(en_model_final)
# Should I increase adapt_delta above 0.8? (due to divergent transitions).


## Average predicted rating by form, temporal distance, plan certainty, profile 
## There is (again) the vector memory limit problem, so I cannot get the 
## posteriors. I used draw_ids to specify the number of draws to be used.

mod_final_post_en <- avg_predictions(
  model      = en_model_final,
  variables  = c("form", "temp_dist", "plan_cert", "ling_profile"),
  re_formula = NULL,
  draw_ids   = sample(1:4000, 500)) |>
  posterior_draws() |> 
  write_csv(here("study_2", "models", "mod_final_en_post.csv"))

mod_final_post_en |>
  group_by(form, ling_profile) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Group     = ling_profile,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# The average predicted rating for the PF is similar between monolinguals (.715)
#   and bilinguals (.713).
# The average predicted ratings for the PP and the PI are lower in the 
#   monolingual group.
# The interval for PP in the monolingual group is considerably wide - perhaps 
#   adding the other preds might better explain the ratings for this form?

## Table: form by group, temp_dist, plan_cert, and ling_profile
mod_final_post_en |>
  group_by(form, ling_profile, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Group     = ling_profile,
    Distance  = temp_dist,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  arrange(Group, Form, Distance, Plan) |>
  print(n = 36)

mod_final_post_en |>
  filter(form == "PF") |>
  group_by(form, ling_profile, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Group    = ling_profile,
    Distance = temp_dist,
    Plan     = plan_cert,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  arrange(Distance, Plan, Group) |>
  print(n = 12)

mod_final_post_en |>
  filter(form == "PI") |>
  group_by(form, ling_profile, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Group    = ling_profile,
    Distance = temp_dist,
    Plan     = plan_cert,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  arrange(Distance, Plan, Group) |>
  print(n = 12)

mod_final_post_en |>
  filter(form == "PP") |>
  group_by(form, ling_profile, temp_dist, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Group    = ling_profile,
    Distance = temp_dist,
    Plan     = plan_cert,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  arrange(Distance, Plan, Group) |>
  print(n = 12)

# Obs:
# The average predicted rating for the PF is similar between the bilingual and 
#   the monolingual group; some of the CrI overlap too. 
# As for the PI, the average predicted rating is always higher in the bilingual 
#   group and no CrI seem to overlap between speaker types.
# With the PP, bilinguals give higher ratings in all conditions, except for the 
#   immediate-certain. However, in this specific condition, the estimates are 
#   similar (mls = .718; bls = .708). This is also true for the near-certain 
#   contexts, where the estimates are similar as well. The interaction between 
#   temporal distance and plan certainty appears to play a stronger role in the 
#   monolingual group, yet this interaction also affects the bilingual ratings.
#   Even though monolinguals accept to some degree the PP, including in the 
#   when the event is supposed to happen (or not) in a distant future (there is 
#   no categorical 0 as for the estimate), they appear to report less preference 
#   for this form compared to bilinguals.
# As always, all these obs must be tested through comparisons!


## Plot (mod_final_post) - posterior medians of predicted rating with 95% and 
## 65% credible intervals
mod_final_post_en |>
  ggplot() +
  aes(x = draw, y = form, fill = temp_dist) +
  facet_grid(ling_profile ~ plan_cert, labeller = label_both) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.6), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, 
       x = "Avg Predicted Rating by Form, Group, Distance and Certainty") +
  theme(legend.position = "bottom",
        legend.justification = c(1, 0),
        legend.background = element_blank())

mod_final_post_en |>
  filter(form == "PF") |>
  ggplot() +
  aes(x = draw, y = temp_dist, fill = ling_profile) + 
  facet_grid(. ~ plan_cert, labeller = label_both) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating by Group, Distance and Certainty", 
       title = "Periphrastic Future") +
  theme(legend.position = "bottom",
        legend.justification = c(1, 0),
        legend.background = element_blank())

mod_final_post_en |>
  filter(form == "PI") |>
  ggplot() +
  aes(x = draw, y = temp_dist, fill = ling_profile) + 
  facet_grid(. ~ plan_cert, labeller = label_both) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating by Group, Distance and Certainty", 
       title = "Futurate Present Indicative") +
  theme(legend.position = "bottom",
        legend.justification = c(1, 0),
        legend.background = element_blank())

mod_final_post_en |>
  filter(form == "PP") |>
  ggplot() +
  aes(x = draw, y = temp_dist, fill = ling_profile) + 
  facet_grid(. ~ plan_cert, labeller = label_both) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = NULL, end = 0.9) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(y = NULL, x = "Avg Predicted Rating by Group, Distance and Certainty", 
       title = "Futurate Present Progressive") +
  theme(legend.position = "bottom",
        legend.justification = c(1, 0),
        legend.background = element_blank())
# ------------------------------------------------------------------------------

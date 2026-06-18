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
set.seed(20260616)

# ----- 3. Load data -----------------------------------------------------------
data_spanish <- read.csv(here("study_2", "final_data", "spanish_final.csv"))
# ------------------------------------------------------------------------------

# ----- 4. Fit Spanish models --------------------------------------------------
# ----- 4.1. Intercept-only model ----------------------------------------------
# Specify model formula
form0_sp <- bf(rating ~ 0 + Intercept)

# Figure out the priors
get_prior(formula = form0_sp, family = zero_one_inflated_beta(), 
          data = data_spanish)

# Set priors
simple_priors_sp_sp <- prior(normal(0, 1.5), class = b)

## Fit simple model
sp_model_0 <- brm(
  formula = form0_sp, 
  prior = simple_priors_sp, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_spanish, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "sp_model_0"))

## Model output
summary(sp_model_0)

as_draws_df(sp_model_0) |>
  as_tibble() |>
  select(b_Intercept, phi, zoi, coi) |>
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  posterior_summary() |>
  as.data.frame() |>
  rownames_to_column("Parameter") |>
  kable(digits = 2)

data_spanish |>
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
  # are not at a boundary (0, 1), the avg rating is about 0.49.
# phi is the precision of that beta part. It is low (low precision = greater
  # variability: the in-between ratings are spread broadly across the interval 
  # rather than tightly clustered.
# zoi refers to probability of a value being on a boundary (0 or 1) and not 
  # in between. ~ 33% of the ratings are either 0 or 1; the rest of the ratings 
  # (~ 67%) fall in between.
# coi refers to the probability of a boundary value of being 1 rather than 0.
  # Of all the boundary responses, ~ 70% are 1s and ~30% are 0s.
# Of the total ratings, 23% are exactly 1 and about 10% are 0.
# ------------------------------------------------------------------------------



# ----- 4.2. Model 1 (verb form) -----------------------------------------------
# Specify model formula
form1_sp <- bf(rating ~ 0 + Intercept + form +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form1_sp, family = zero_one_inflated_beta(), 
          data = data_spanish)

# Set priors
priors1_sp <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
sp_model_1 <- brm(
  formula = form1_sp, 
  prior = priors1_sp, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_spanish, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "sp_model_1"))
# Warning messages:
  # 1: Parts of the model have not converged (some Rhats are > 1.05). Be careful 
    # when analysing the results! We recommend running more iterations and/or 
    # setting stronger priors. 
  # 2: There were 296 divergent transitions after warmup. Increasing adapt_delta 
    # above 0.8 may help.

## Model output
summary(sp_model_1)

as_draws_df(sp_model_1) |>
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
#   This is talking about expected ratings between 0 and 1, without accounting 
#     for exact 0s and 1s.
#   b_Intercept refers to the base form, which I recoded for the table using 
#     mutate().
#   The expected rating given the response falls between 0 and 1 (excluding the
#     boundary values - zoi/coi) is the highest for PF (0.74 [0.65, 0.84]), 
#   followed by PI (0.50 [0.38, 0.65]) and PP (0.40 [0.28, 0.54]). I checked 
#     and for the bilingual model the expected rating was higher for PP than PI. 

## Average predicted rating by form (response scale, already back-transformed)
mod1_preds_sp <- avg_predictions(
  model     = sp_model_1,
  variables = "form",
  re_formula = NULL)

## Posterior draws of those predictions
mod1_post_sp <- avg_predictions(
  model     = sp_model_1,
  variables = "form",
  re_formula = NULL) %>%
  posterior_draws()

## Formatted table: median [95% CrI] by form
mod1_post_sp %>%
  group_by(form) %>%
  median_qi(x = draw) %>%
  transmute(
    Form     = form,
    Estimate = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))
# ------------------------------------------------------------------------------



# ----- 4.3. Model 2 (verb form, temporal reference) ---------------------------
# Specify model formula
form2_sp <- bf(rating ~ 0 + Intercept + form * temp_dist +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form2_sp, family = zero_one_inflated_beta(), 
          data = data_spanish)

# Set priors
priors2_sp <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
sp_model_2 <- brm(
  formula = form2_sp, 
  prior = priors2_sp, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_spanish, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "sp_model_2"))
# Warning messages: 
# 2: There were 129 divergent transitions after warmup.
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means 
#    and medians may be unreliable.
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior 
#    variances and tail quantiles may be unreliable.

## Model output
summary(sp_model_2)

## Average predicted rating by form and temporal distance
mod2_pred_sp <- avg_predictions(
  model      = sp_model_2,
  variables  = c("form", "temp_dist"),
  re_formula = NULL)

mod2_post_sp <- posterior_draws(mod2_pred_sp)

## Marginal over form: temp_dist on its own
mod2_post_tempdist_sp <- avg_predictions(
  model      = sp_model_2,
  variables  = "temp_dist",
  re_formula = NULL) |>
  posterior_draws()

## Table: form by temp_dist
mod2_post_sp |>
  group_by(form, temp_dist) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Distance  = temp_dist,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
#   PF: near > immediate > distant
#   PI: immediate > near > distant
#   PP: near > immediate > distant
#   But... most intervals within form overlap with each other 

## Plot (mod2_post) - posterior medians of predicted rating with 95% and 65% 
##  credible intervals
mod2_post_sp |>
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
form3_sp <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form3_sp, family = zero_one_inflated_beta(), 
          data = data_spanish)

# Set priors
priors3_sp <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"))

## Fit simple model
sp_model_3 <- brm(
  formula = form3_sp, 
  prior = priors3_sp, 
  iter = 2000, warmup = 1000, cores = 4, 
  data = data_spanish, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "sp_model_3"))
# Warning message: There were 98 divergent transitions after warmup.

## Model output
summary(sp_model_3)

## Average predicted rating by form, temporal distance, and plan certainty
mod3_pred_sp <- avg_predictions(
  model      = sp_model_3,
  variables  = c("form", "temp_dist", "plan_cert"),
  re_formula = NULL)

mod3_post_sp <- posterior_draws(mod3_pred_sp)

mod3_post_sp |>
  group_by(form, plan_cert) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Plan      = plan_cert,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
  # Overall, all forms receive higher ratings in the certain condition.
  # The CrI for certain-uncertain conditions by specific form sometimes overlap. 
  #   Must perform comparisons to see what this means.

## Table: form by temp_dist by plan_cet
mod3_post_sp |>
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
#   conditions, which could be explained by the fact that it is generally 
#   reported in the literature as the default form for the future. However, 
#   comparisons must be performed to see if there are really differences or not.

## Plot (mod3_post) - posterior medians of predicted rating with 95% and 65% 
# credible intervals
mod3_post_sp |>
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
form4_sp <- bf(rating ~ 0 + Intercept + form * temp_dist * plan_cert * ling_profile +
              (1 + form * temp_dist * plan_cert | participant) +
              (1 | item) +
              (1 | list))

# Figure out the priors
get_prior(formula = form4_sp, family = zero_one_inflated_beta(), 
          data = data_spanish)

# Set priors
priors4_sp <- c(prior(normal(0, 1.5), class = b),
             prior(normal(0, 2),   class = b, coef = "Intercept"),
             prior(lkj(2),         class = cor),
             prior(gamma(2, 0.5),  class = phi))

## Fit simple model
sp_model_final <- brm(
  formula = form4_sp, 
  prior = priors4_sp, 
  iter = 8000, warmup = 2000, cores = 8, 
  backend = "cmdstanr",
  data = data_spanish, 
  family = zero_one_inflated_beta(), 
  file = here("study_2", "models", "sp_model_final"))
# Warning messages: 355 of 24000 (14.0%) transitions ended with a divergence.
# I increased iter/warmup because when I first ran the model both the Bulk and 
# the Tail Effective Samples Sizes were too low. 

## Model output
summary(sp_model_final)
# 1: Parts of the model have not converged (some Rhats are > 1.05). Be careful 
# when analysing the results! We recommend running more iterations and/or 
# setting stronger priors. 
# Should I increase adapt_delta above 0.8? (due to divergent transitions).


## Average predicted rating by form, temporal distance, plan certainty, profile 
## There is (again) the vector memory limit problem, so I cannot get the 
## posteriors. Run the final model (+ posteriors, etc.) with A's computer - I
## encountered the same problem, so I had to limit the draws using draw_ids.

mod_final_post_sp <- avg_predictions(
  model      = sp_model_final,
  variables  = c("form", "temp_dist", "plan_cert", "ling_profile"),
  re_formula = NULL,
  draw_ids   = sample(1:4000, 500)) |>
  posterior_draws() |> 
write_csv(here("study_2", "models", "mod_final_sp_post.csv"))

mod_final_post_sp |>
  group_by(form, ling_profile) |>
  median_qi(x = draw) |>
  transmute(
    Form      = form,
    Group     = ling_profile,
    Estimate  = glue("{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) 

# Obs:
# The average predicted rating for the PF is similar between monolinguals (.724)
#   and bilinguals (.706).
# The average predicted ratings for the PP and the PI are lower in the 
#   monolingual group, especially for the PP (.327; .53 for bilinguals).
# The interval for PI in the monolingual group is considerably wide - perhaps 
#   adding the other preds might better explain the ratings for this form?

## Table: form by group, temp_dist, plan_cert, and ling_profile
mod_final_post_sp |>
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

mod_final_post_sp |>
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

mod_final_post_sp |>
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

mod_final_post_sp |>
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
#   the monolingual group; some of the CrI overlap too. However, in some 
#   specific conditions (e.g., immediate certain) the ratings seem to differ 
#   more and there is no overlapping between CrI. 
# As for the PI, the average predicted rating is in some conditions considerably 
#   different. For example, bilinguals rate the PI higher in distant-certain 
#   events (.569) compared to monolinguals (.346). Overall, bilinguals give 
#   higher ratings, except in the immediate/near-certain contexts.
# With the PP, bilinguals give higher ratings in all conditions. In fact, 
#   the monolingual estimates are between .30 and .33, with the exception of 
#   near-certain (.37) and immediate-certain (.497) contexts. Even though 
#   monolinguals accept to some degree the PP (there is no categorical 0), 
#   they appear to report less preference for this form compared to bilinguals.
# As always, all these obs must be tested through comparisons!


## Plot (mod_final_post_sp) - posterior medians of predicted rating with 95% and 
## 65% credible intervals
mod_final_post_sp |>
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

mod_final_post_sp |>
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

mod_final_post_sp |>
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

mod_final_post_sp |>
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

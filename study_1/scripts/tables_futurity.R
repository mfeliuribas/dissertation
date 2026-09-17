# ----- Tables: Futurity -------------------------------------------------------
#
# This script contains the code used to create the tables of predictor-level 
# effects from the final model of futurity ('mod3_future') for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("marginaleffects")
library("ggdist")
library("glue")
# ------------------------------------------------------------------------------


# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260911)
# ------------------------------------------------------------------------------


# ----- 3. Load model ----------------------------------------------------------
mod3_future <- readRDS(here("study_1", "models", "mod3_future.rds"))
# ------------------------------------------------------------------------------


# ----- 4. Table: Temporal distance --------------------------------------------
mod3_future_post_temp <- avg_predictions(
  model = mod3_future,
  variables = "temp_dist",
  re_formula = NULL)|>
  posterior_draws()

mod3_future_post_temp |>
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
    values_from = Estimate) |>
  write_csv(here("study_1", "tables", "table_futurity_temp.csv"))
# ------------------------------------------------------------------------------


# ----- 5. Table: Grammatical person -------------------------------------------
mod3_future_post_pers <- avg_predictions(
  model = mod3_future,
  variables = "gram_pers",
  re_formula = NULL)|>
  posterior_draws()

mod3_future_post_pers |>
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
  transmute(
    Person = gram_pers,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Person, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate) |>
  write_csv(here("study_1", "tables", "table_futurity_pers.csv"))
# ------------------------------------------------------------------------------


# ----- 6. Table: Verb Class ---------------------------------------------------
mod3_future_post_verb <- avg_predictions(
  model = mod3_future,
  variables = "verb_class",
  re_formula = NULL)|>
  posterior_draws()

mod3_future_post_verb |>
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
  transmute(
    Verb = verb_class,
    Form = form,
    Estimate = glue(
      "{round(x, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]")) |>
  select(Verb, Form, Estimate) |>
  pivot_wider(
    names_from = Form,
    values_from = Estimate) |>
  write_csv(here("study_1", "tables", "table_futurity_verb.csv"))
# ------------------------------------------------------------------------------


# ----- 6. Table: All predictors -----------------------------------------------
mod3_future_post <- avg_predictions(
  model = mod3_future, 
  variables = c("temp_dist", "gram_pers", "verb_class"), 
  re_formula = NULL) |>
  posterior_draws()

mod3_future_post |>
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
    values_from = Estimate) |>
  write_csv(here("study_1", "tables", "table_futurity_all.csv"))
# ------------------------------------------------------------------------------

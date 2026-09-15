# ----- Comparisons: Futurity --------------------------------------------------
#
# This script contains the code used to perform pairwise comparisons (posterior 
# constrasts) of predictor-level effects from the final model of futurity 
# ('mod3_future') for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("marginaleffects") 
library("ggdist")
library("glue")
# ------------------------------------------------------------------------------


# ----- 2. Load model -----------------------------------------------------------
mod3_future <- readRDS(here("study_1", "models", "mod3_future.rds"))
# ------------------------------------------------------------------------------


# ----- 3. Comparisons: Temporal distance --------------------------------------
# Comparisons against the reference level
post_td <- avg_comparisons(
  mod3_future, 
  variables = "temp_dist",
  re_formula = NULL) |>
  posterior_draws()

post_td |>
  ggplot() +
  aes(x = draw, y = contrast) +
  stat_halfeye(position = position_dodge(0.5)) +
  labs(y = NULL, x = "Posterior diference")

post_td |>
  group_by(contrast) |>
  median_qi(draw) |>
  transmute(
    Comparison = case_when(
      contrast == "mean(distant) - mean(immediate)" ~ "Distant − immediate",
      contrast == "mean(near) - mean(immediate)" ~ "Near − immediate",
      contrast == "mean(nonspecific) - mean(immediate)" ~ 
        "Nonspecific − immediate"),
    Estimate = glue("{round(draw, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

# All comparisons
post_td_all <- avg_comparisons(
  mod3_future,
  variables = list(
    temp_dist = "pairwise"),
  re_formula = NULL) |>
  posterior_draws()

post_td_all |>
  group_by(contrast) |>
  median_qi(draw) |>
  transmute(
    Comparison = case_when(
      contrast == "mean(distant) - mean(immediate)" ~ "Distant − Immediate",
      contrast == "mean(distant) - mean(near)" ~ "Distant − Near",
      contrast == "mean(near) - mean(immediate)" ~ "Near − Immediate",
      contrast == "mean(nonspecific) - mean(distant)" ~ "Nonspecific − Distant",
      contrast == "mean(nonspecific) - mean(immediate)" ~ "Nonspecific − Immediate",
      contrast == "mean(nonspecific) - mean(near)" ~ "Nonspecific − Near"),
    Estimate = glue("{round(draw, 3)} [{round(.lower, 3)}, {round(.upper, 3)}]"))

post_td_all |>
  ggplot() +
  aes(x = draw, y = contrast) +
  stat_halfeye(position = position_dodge(0.5)) +
  labs(y = NULL, x = "Posterior diference MF")

post_td_all_PI <- post_td_all |>
  mutate(
    draw = -draw)

post_td_all_PI |>
  group_by(contrast) |>
  median_qi(draw)

post_td_all_PI |>
  ggplot() +
  aes(x = draw, y = contrast) +
  stat_halfeye(position = position_dodge(0.5)) +
  labs(y = NULL, x = "Posterior diference PI")

# Obs:
  # There is no clear evidence of a difference among immediate vs. near, 
  #   immediate vs. distant, near vs. distant (all CrIs include 0).
  # There is clear evidence that nonspecific contexts have a higher probability 
  #   of MF (over the PI) than every other temporal-distance category.
  # The MF is strongly associated with nonspecific temporal contexts, while the 
  #   three specific temporal-distance categories do not clearly differ from 
  #   one another.
  # Conversely, contexts with a specific temporal reference favor the PI over 
  #   the MF --- I think: if the model is estimating the P(MF), then the opposite 
  #   pattern must be true for the PI? Ask Austin/Joseph.
  #   Also, based on the 'reversed' plot that is not true. I think I am missing 
  #   something --- Revise + Think about it. 
# ------------------------------------------------------------------------------

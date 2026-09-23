# ----- Plots: Futurity --------------------------------------------------------
#
# This script contains the code used to create the plots of predictor-level 
# effects from the final model of futurity ('mod3_future') for Study 1.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("marginaleffects")
library("ggdist")
# ------------------------------------------------------------------------------

# library("tidybayes")

# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260911)
# ------------------------------------------------------------------------------


# ----- 3. Load model ----------------------------------------------------------
mod0_future <- readRDS("here"("study_1", "models", "mod0_future.rds"))
mod3_future <- readRDS(here("study_1", "models", "mod3_future.rds"))
# ------------------------------------------------------------------------------


# ----- 4. Plot of predicted probabilities: MF and PI --------------------------
# Get posteriors for the MF and create posterior probability for the PI
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

# Plot of posterior distribution (intercept-only model)
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
    form = fct_relevel(form, "PI", "MF"))

mod0_future_plot <- mod0_future_plot |>
  ggplot() +
  aes(x = prob, y = form, fill = form) +
  scale_fill_viridis_d(name = NULL, end = 0.5) +
  stat_slab(show.legend = FALSE) +
  stat_pointinterval(
    show.legend = FALSE,
    pch = 21,
    position = position_nudge(y = -0.05)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    y = NULL,
    x = "Posterior probability") +
  theme(
    text = element_text(family = "Palatino"),
    legend.position = "right",
    axis.title.x = element_text(margin = margin(t = 10))) 

ggsave(
  here("study_1", "plots", "plot_bi_choice_futurity.png"),
  plot = mod0_future_plot,
  width = 5,
  height = 5,
  units = "in",
  dpi = 300)

# If I end up using this plot I should add a legend with the CrIs.
# ------------------------------------------------------------------------------


# ----- 5. Plot of predicted probabilities: ALL factors ------------------------
# Get the posteriors for each predictor
mod3_future_post_temp <- avg_predictions(
  model = mod3_future,
  variables = "temp_dist",
  re_formula = NULL)|>
  posterior_draws()

mod3_future_post_pers <- avg_predictions(
  model = mod3_future,
  variables = "gram_pers",
  re_formula = NULL)|>
  posterior_draws()

mod3_future_post_verb <- avg_predictions(
  model = mod3_future,
  variables = "verb_class",
  re_formula = NULL)|>
  posterior_draws()

# Create dataframe with MF posteriors
mod3_future_post_temp <- mod3_future_post_temp |>
  mutate(predictor = "Temporal distance",
         level = as.character(temp_dist)) |>
  select(predictor, level, MF = draw)

mod3_future_post_pers <- mod3_future_post_pers |>
  mutate(predictor = "Grammatical person",
         level = as.character(gram_pers)) |>
  select(predictor, level, MF = draw)

mod3_future_post_verb <- mod3_future_post_verb |>
  mutate(predictor = "Verb type",
         level = as.character(verb_class)) |>
  select(predictor, level, MF = draw)


mod3_future_post_all <- bind_rows(mod3_future_post_temp, mod3_future_post_pers, 
                      mod3_future_post_verb)


# Add PI posteriors to dataframe 
mod3_future_post_all <- mod3_future_post_all |>
  mutate(PI = 1 - MF) |>
  pivot_longer(
    cols = c(MF, PI),
    names_to = "form",
    values_to = "draw")

# Set order of the predictors in the plot
mod3_future_post_all <- mod3_future_post_all |>
  mutate(predictor = factor(predictor,
                            levels = c("Temporal distance",
                                       "Grammatical person",
                                       "Verb type")))


# Plot
plot_futurity_post <- mod3_future_post_all |>
  mutate(level = factor(level,
                        levels = c("nonspecific", "distant", "near", "immediate",
                                   "3pers", "1pers",
                                   "stative", "motion", "dynamic"))) |>
  ggplot() +
  aes(x = draw, y = level, colour = form) +
  stat_pointinterval(position = position_dodge(width = 0.5)) +
  facet_grid(predictor ~ ., scales = "free_y", space = "free_y") +
  scale_color_viridis_d(end = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = "Posterior probability",
       y = NULL,
       colour = "Form") +
  theme(
      text = element_text(family = "Palatino"),
      legend.position = "right",
      axis.title.x = element_text(margin = margin(t = 10))) 

  ggsave(
    here("study_1", "plots", "plot_futurity_post.png"),
    plot = plot_futurity_post,
    width = 7,
    height = 8,
    units = "in",
    dpi = 300)

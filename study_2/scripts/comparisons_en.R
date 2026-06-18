# ----- Comparisons: English ---------------------------------------------------
#
# This script contains the code used to compare the English groups (bilinguals + 
# monolinguals) for Study 2.
#
# ------------------------------------------------------------------------------

# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("marginaleffects")
library("tidyverse")
library("ggdist")
# ------------------------------------------------------------------------------

# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260616)

# ----- 3. Load model ----------------------------------------------------------
#model_sp <- readRDS(here("study_2", "model", "sp_model_final.rds"))
post_en <- read.csv(here("study_2", "models", "mod_final_en_post.csv"))
# ------------------------------------------------------------------------------

# ----- 4. Comparisons ---------------------------------------------------------
# I do not know if any of this makes sense... Must check with someone with a 
# better understanding :)

# I do not have the model in this computer, but the posteriors saved in a csv 
# file. If the comparison is a difference within the draws, then would it make 
# sense to use the posteriors csv and subtract monolingual from bilingual 
# within each posterior draw (if my aim here is to compare these two groups)?

# Pivot the two groups within each draw (take the names from 'group' and values 
#   from 'draw' and create two new columns, 'bilingual' and 'monolingual', adding  
#   the draw values in these two new columns. This way, the same draw-form-cond.
#   for bilinguals and monolinguals is collapsed in the same row.
# Afterwards, substract the monolingual values from the bilingual values in 
#   order to get the difference between the groups. This difference is stored in 
#   a new variable called 'diff'. Every value within 'diff' is one draw of the 
#   difference.
contrast_draws_en <- post_en |>
  select(drawid, form, temp_dist, plan_cert, ling_profile, draw) |>
  pivot_wider(
    names_from = ling_profile, 
    values_from = draw) |>
  mutate(diff = bilingual - monolingual)

# Now that the difference between bilinguals and monolinguals has been 
# established for each combination of draw-form-cond., the difference should be 
# grouped by form-condition (i.e., group the draws within the same conditions) 
# and the estimates and intervals per form-condition should be drawn.
contrast_summary_en <- contrast_draws_en |>
  group_by(form, temp_dist, plan_cert) |>
  summarize(
    estimate  = median(diff),
    conf.low  = quantile(diff, 0.025),
    conf.high = quantile(diff, 0.975),
    .groups   = "drop")

# Plot the posterior difference between bilinguals and monolinguals with 95% CrI 
contrast_draws_en |>
  mutate(temp_dist = factor(temp_dist, c("immediate", "near", "distant"))) |>
  ggplot(aes(x = diff, y = temp_dist, fill = plan_cert)) +
  facet_wrap(form ~ .) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", 
             linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = "Certainty", end = 0.9) +
  labs(x = "Posterior difference (bilingual − monolingual)", y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

contrast_draws_en |>
  mutate(
    temp_dist = factor(temp_dist, c("immediate", "near", "distant")),
    plan_cert = factor(plan_cert, c("certain", "uncertain")),
    form      = factor(form, c("PF", "PI", "PP"))) |>
  ggplot(aes(x = diff, y = temp_dist, fill = temp_dist)) +
  facet_grid(form ~ plan_cert) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  stat_halfeye(point_fill = "white") +
  scale_fill_viridis_d(end = 0.9, guide = "none") +
  coord_cartesian(xlim = c(-0.25, 0.35)) +
  labs(x = "Posterior difference (bilingual − monolingual)", y = NULL) +
  theme_bw() +
  theme(
    panel.spacing.y  = unit(1.4, "lines"),
    panel.spacing.x  = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey90", colour = "grey70"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "none")

# Positive estimate = average rating higher in BLs (the ML values were 
#   subtracted from the BL values in diff = bilingual - monolingual)
# Negative estimate = average rating higher in MLs
# The 95% confidence interval includes 0 = the effect might be zero (i.e., no 
#   reliable evidence of a difference between groups).
# ------------------------------------------------------------------------------

# ----- Comparisons: Bilinguals (OLD) ------------------------------------------
#
# This script contains the code used to compare the bilingual group for Study 2. 
#
# This code is kept here for reference. It is NOT part of the data analysis.
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
#model_sp <- readRDS(here("study_2", "model", "bls_model_final.rds"))
post_ling_bls <- read.csv(here("study_2", "models", "mod_final_bls_post1.csv"))

#post_blp_bls <- read.csv(here("study_2", "models", "mod_final_bls_post2.csv"))

#post_lextale_bls <- read.csv(here("study_2", "models", "mod_final_bls_post3.csv"))
# ------------------------------------------------------------------------------

# ----- 4. Comparisons within language -----------------------------------------
# ----- 4.1. Spanish  ----------------------------------------------------------

# Temp. distance compared in context of certainty
contrast_tempdist_certain_sp <- post_ling_bls |>
  filter(language == "sp", plan_cert == "certain") |>
  select(drawid, form, temp_dist, draw) |>
  pivot_wider(
    names_from  = temp_dist,
    values_from = draw) |>
  mutate(
    imm_near = immediate - near,
    imm_dist = immediate - distant,
    near_dist = near - distant)

# Summary table of temp. distance compared in context of certainty
summary_tempdist_certain_sp <- contrast_tempdist_certain_sp |>
  pivot_longer(
    c(imm_near, imm_dist, near_dist),
    names_to  = "contrast",
    values_to = "diff") |>
  group_by(form, contrast) |>
  summarize(
    estimate  = median(diff),
    conf.low  = quantile(diff, 0.025),
    conf.high = quantile(diff, 0.975),
    .groups   = "drop")

# Plot of temp. distance compared in context of certainty with CrI
contrast_tempdist_certain_sp |>
  pivot_longer(
    c(imm_near, imm_dist, near_dist),
    names_to  = "contrast",
    values_to = "diff") |>
  mutate(
    contrast = factor(contrast,
                      levels = c("imm_near", "imm_dist", "near_dist"),
                      labels = c("immediate − near",
                                 "immediate − distant",
                                 "near − distant")),
    form = factor(form, c("PF", "PI", "PP"))) |>
  ggplot() +
  aes(x = diff, y = contrast, fill = contrast) +
  facet_wrap(~ form) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  stat_halfeye(point_fill = "white") +
  scale_fill_viridis_d(end = 0.9, guide = "none") +
  labs(x = "Posterior difference between temporal distances (Spanish, certain)",
       y = NULL) +
  theme_minimal()

# Temp. distance compared in context of uncertainty
contrast_tempdist_uncertain_sp <- post_ling_bls |>
  filter(language == "sp", plan_cert == "uncertain") |>
  select(drawid, form, temp_dist, draw) |>
  pivot_wider(
    names_from  = temp_dist,
    values_from = draw) |>
  mutate(
    imm_near = immediate - near,
    imm_dist = immediate - distant,
    near_dist = near - distant)

# Summary table of temp. distance compared in context of uncertainty
summary_tempdist_uncertain_sp <- contrast_tempdist_certain_sp |>
  pivot_longer(
    c(imm_near, imm_dist, near_dist),
    names_to  = "contrast",
    values_to = "diff") |>
  group_by(form, contrast) |>
  summarize(
    estimate  = median(diff),
    conf.low  = quantile(diff, 0.025),
    conf.high = quantile(diff, 0.975),
    .groups   = "drop")

# Plot of temp. distance compared in context of uncertainty with CrI
contrast_tempdist_certain_sp |>
  pivot_longer(
    c(imm_near, imm_dist, near_dist),
    names_to  = "contrast",
    values_to = "diff") |>
  mutate(
    contrast = factor(contrast,
                      levels = c("imm_near", "imm_dist", "near_dist"),
                      labels = c("immediate − near",
                                 "immediate − distant",
                                 "near − distant")),
    form = factor(form, c("PF", "PI", "PP"))) |>
  ggplot() +
  aes(x = diff, y = contrast, fill = contrast) +
  facet_wrap(~ form) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  stat_halfeye(point_fill = "white") +
  scale_fill_viridis_d(end = 0.9, guide = "none") +
  labs(x = "Posterior difference between temporal distances (Spanish, uncertain)",
       y = NULL) +
  theme_minimal()

# When the plan is certain, it seems that temporal distance does not play a huge 
# role -- CrI include 0 in all cases. However, the estimate is sometimes negative 
# and others positive, perhaps pointing at some trends. 

# Certainty compared within each temporal distance
contrast_certainty_sp <- post_ling_bls |>
  filter(language == "sp") |>
  select(drawid, form, temp_dist, plan_cert, draw) |>
  pivot_wider(
    names_from  = plan_cert,
    values_from = draw) |>
  mutate(diff = certain - uncertain)

# Summary of certainty compared within each temporal distance
summary_certainty_sp <- contrast_certainty_sp |>
  group_by(form, temp_dist) |>
  summarize(
    estimate  = median(diff),
    conf.low  = quantile(diff, 0.025),
    conf.high = quantile(diff, 0.975),
    .groups   = "drop")

# Plot of certainty compared within each temporal distance with CrI
contrast_certainty_sp |>
  mutate(
    temp_dist = factor(temp_dist, c("immediate", "near", "distant")),
    form      = factor(form, c("PF", "PI", "PP"))) |>
  ggplot() +
  aes(x = diff, y = temp_dist, fill = temp_dist) +
  facet_wrap(~ form) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  stat_halfeye(point_fill = "white") +
  scale_fill_viridis_d(end = 0.9, guide = "none") +
  labs(x = "Posterior difference (certain − uncertain), Spanish", y = NULL) +
  theme_minimal()

# Participants seem to favor the PP in the certain condition when the plan is 
#   immediate or near in the future. The PI is also favored when the plan is 
#   certain and is immediate. As for the PF, it does not seem to be favored  
#   across any certainty domain, albeit in the certain-distant. Overall, all 
#   forms seem to be favored when the plan is certain; yet, most comparisons  
#   include 0 suggesting that there is no reliable evidence of a difference 
#   between certain and uncertain.


# temp_dist * plan_cert
# Does the certainty effect differ across temporal distance?
# How can I do this? I think it will be easier with avg_comparisons() with the
# 'by' argument, but since I do not have the model in this computer I am not 
# sure how to calculate it by hand.



# ------------------------------------------------------------------------------



# ----- 5. Comparisons between languages ---------------------------------------
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

contrast_sp_en_ling <- post_ling_bls |>
  select(drawid, form, temp_dist, plan_cert, language, draw) |>
  pivot_wider(
    names_from = language, 
    values_from = draw) |>
  mutate(diff = sp - en)

# Now that the difference between bilinguals and monolinguals has been 
# established for each combination of draw-form-cond., the difference should be 
# grouped by form-condition (i.e., group the draws within the same conditions) 
# and the estimates and intervals per form-condition should be drawn.
summary_sp_en_ling <- contrast_sp_en_ling |>
  group_by(form, temp_dist, plan_cert) |>
  summarize(
    estimate  = median(diff),
    conf.low  = quantile(diff, 0.025),
    conf.high = quantile(diff, 0.975),
    .groups   = "drop")

# Plot the posterior difference between Spanish and English with 95% CrI 
contrast_sp_en_ling |>
  mutate(temp_dist = factor(temp_dist, c("immediate", "near", "distant"))) |>
  ggplot(aes(x = diff, y = temp_dist, fill = plan_cert)) +
  facet_wrap(form ~ .) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", 
             linewidth = 0.3) +
  stat_halfeye(position = position_dodge(0.5), point_fill = "white") +
  scale_fill_viridis_d(name = "Certainty", end = 0.9) +
  labs(x = "Posterior difference (Spanish − English)", y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

contrast_sp_en_ling |>
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
  labs(x = "Posterior difference (Spanish − English)", y = NULL) +
  theme_bw() +
  theme(
    panel.spacing.y  = unit(1.4, "lines"),
    panel.spacing.x  = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey90", colour = "grey70"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "none")

# Positive estimate = average rating higher in Spanish (the English values were 
#   subtracted from the Spanish values in diff = Spanish - English)
# Negative estimate = average rating higher in English
# The 95% credible interval includes 0 = the effect might be zero (i.e., no 
#   reliable evidence of a difference between languages).
# ------------------------------------------------------------------------------

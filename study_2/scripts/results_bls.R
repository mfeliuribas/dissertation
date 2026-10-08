

# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("ggdist")
library("glue")
# ------------------------------------------------------------------------------

# ----- 2. Load posteriors -----------------------------------------------------
# Linguistic predictors
post_ling_bls <- readRDS(here("study_2", "models", "bilinguals", "posteriors",
                        "mod_bl_final_post.rds")) |>
  as_tibble() |>
  select(drawid, form, language, temp_dist, plan_cert, draw) |>
  mutate(
    form      = factor(form, levels = c("PF", "PI", "PP")),
    language  = factor(language, levels = c("sp", "en"),
                       labels = c("Spanish", "English")),
    temp_dist = factor(temp_dist, levels = c("immediate", "near", "distant")),
    plan_cert = factor(plan_cert, levels = c("certain", "uncertain")))

# Language dominance
post_blp_bls <- readRDS(here("study_2", "models", "bilinguals", "posteriors",
                         "mod_blp_post.rds")) |>
  as_tibble() |>
  select(drawid, form, language, blp_updated_z, draw) |>
  mutate(
    form     = factor(form, levels = c("PF", "PI", "PP")),
    language = factor(language, levels = c("sp", "en"),
                      labels = c("Spanish", "English")))
# ------------------------------------------------------------------------------


# ----- 3. Predicted ratings by form and language ------------------------------
# Average over contexts within each draw
post_bls_form_lang <- post_ling_bls |>
  group_by(drawid, form, language) |>
  summarise(draw = mean(draw), .groups = "drop")

table_bls_form_lang <- post_bls_form_lang |>
  group_by(form, language) |>
  median_qi(x = draw * 100) |>
  mutate(
    Estimate = glue("{round(x, 1)} [{round(.lower, 1)}, {round(.upper, 1)}]")) |>
  select(Form = form, language, Estimate) |>
  pivot_wider(
    names_from = language,
    values_from = Estimate)

# write_csv(table_bls_form_lang, here("study_2", "tables", "bilinguals",
#                                "table_bls_form_lang.csv"))
# ------------------------------------------------------------------------------

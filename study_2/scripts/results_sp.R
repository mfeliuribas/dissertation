

# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("ggdist")
library("glue")
# ------------------------------------------------------------------------------


# ----- 2. Load posteriors ----------------------------------------------------------
post_sp <- readRDS(here("study_2", "models", "spanish", "posteriors", 
                        "mod_sp_final_post_loop.rds")) |>
  as_tibble() |>
  select(drawid, form, temp_dist, plan_cert, ling_profile, draw) |>
mutate(
  form         = factor(form, levels = c("PF", "PI", "PP")),
  temp_dist    = factor(temp_dist, levels = c("immediate", "near", "distant")),
  plan_cert    = factor(plan_cert, levels = c("certain", "uncertain")),
  ling_profile = factor(ling_profile, levels = c("monolingual", "bilingual")))
# ------------------------------------------------------------------------------


# ----- 3. Predicted ratings by form and group ---------------------------------
# Average over contexts within each draw
post_sp_form_group <- post_sp |>
  group_by(drawid, form, ling_profile) |>
  summarize(draw = mean(draw), .groups = "drop")

table_sp_form_group <- post_sp_form_group |>
  group_by(form, ling_profile) |>
  median_qi(x = draw * 100) |>
  mutate(
    Estimate = glue("{round(x, 1)} [{round(.lower, 1)}, {round(.upper, 1)}]")) |>
  select(Form = form, ling_profile, Estimate) |>
  pivot_wider(
    names_from = ling_profile,
    values_from = Estimate)

write_csv(table_sp_form_group, here("study_2", "tables", "spanish", 
                                    "table_sp_form_group.csv"))
# ------------------------------------------------------------------------------


# ----- 4. Predicted ratings by form, context, and group -----------------------
table_sp_ling <- post_sp |>
  group_by(form, temp_dist, plan_cert, ling_profile) |>
  median_qi(x = draw * 100) |>
  mutate(
    Estimate = glue("{round(x, 1)} [{round(.lower, 1)}, {round(.upper, 1)}]")) |>
  select(Group = ling_profile,
         Time = temp_dist, 
         Certainty = plan_cert, 
         form, 
         Estimate) |>
  pivot_wider(
    names_from = form,
    values_from = Estimate) |>
  arrange(Group)

write_csv(table_sp_ling, here("study_2", "tables", "spanish", 
                              "table_sp_ling.csv"))
# ------------------------------------------------------------------------------

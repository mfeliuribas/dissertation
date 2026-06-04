# ----- Do descriptive stats ---------------------------------------------------
#
# This script contains the code used to do descriptive stats for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
data_bls <- read.csv(here("study_2", "final_data", "bls_final.csv"))
# ------------------------------------------------------------------------------



# ----- 3. Check BL data -------------------------------------------------------
# Check participant behavior
data_bls |>
  filter(is.na(rating)) |>
  count(participant)

data_bls |>
  group_by(participant) |>
  summarize(
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    min_rating = min(rating, na.rm = TRUE),
    max_rating = max(rating, na.rm = TRUE)) |>
  arrange(sd_rating) |>
  print(n=89)
# ------------------------------------------------------------------------------


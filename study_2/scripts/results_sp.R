

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
set.seed(20261007)
# ------------------------------------------------------------------------------


# ----- 3. Load model ----------------------------------------------------------
model_sp_6 <- readRDS(here("study_2", "models", "spanish", "model_sp_final.rds"))
# ------------------------------------------------------------------------------

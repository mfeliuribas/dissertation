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
# ------------------------------------------------------------------------------

# ----- 2. Set seed for reproducibility ----------------------------------------
set.seed(20260616)

# ----- 3. Load data -----------------------------------------------------------
data_spanish <- read.csv(here("study_2", "final_data", "spanish_final.csv"))
# ------------------------------------------------------------------------------


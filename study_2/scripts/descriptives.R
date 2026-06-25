# ----- Do descriptive stats ---------------------------------------------------
#
# This script contains the code used to do descriptive stats for Study 2.
#
# ------------------------------------------------------------------------------


# ----- 1. Load libraries ------------------------------------------------------
library("here")
library("tidyverse")
library("dplyr")
library("ggplot2")
# ------------------------------------------------------------------------------



# ----- 2. Load data -----------------------------------------------------------
data_bls <- read.csv(here("study_2", "final_data", "bls_final.csv"))
data_bls_
data_mls_sp <- read.csv(here("study_2", "tidy_data", "spanish_mls_tidy.csv"))
data_mls_en <- read.csv(here("study_2", "tidy_data", "english_mls_tidy.csv"))
# ------------------------------------------------------------------------------



# ----- 3. Descriptive stats for BLs -------------------------------------------
# Total number of participants
data_bls |>
  summarize(total_participants = n_distinct(participant))

data_bls |>
  group_by(participant) %>%
  filter(n_distinct(language) == 1) %>%
  summarise(language = first(language))

# Obs:
  # A total of 89 bilinguals were included in the final dataset. However, of 
  # them, five only completed the English version and six only completed the 
  # Spanish version.

# Family origin
data_bls |>
  group_by(participant) |>
  summarize(
    origin_mom = first(origin_mom),
    origin_dad = first(origin_dad))

data_bls |>
  group_by(participant) |>
  summarize(
    origin_mom = first(origin_mom)) |>
  count(origin_mom, sort = TRUE) 

data_bls |>
  group_by(participant) |>
  summarize(
    origin_dad = first(origin_dad)) |>
  count(origin_dad, sort = TRUE) 

data_bls |>
  group_by(participant) |>
  summarize(origin_mom = first(origin_mom),
            origin_dad = first(origin_dad),
            .groups = "drop") |>
  pivot_longer(c(origin_mom, origin_dad), values_to = "country") |>
  filter(!is.na(country)) |>
  distinct(participant, country) |>
  count(country, sort = TRUE) |>
  mutate(pct = round(100 * n / n_distinct(data_bls$participant), 1))

# Origin self
data_bls |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

# Must calculate how many report not being born in the U.S. 

# Self-reported gender
data_bls |>
  group_by(participant) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

# Obs:
  # Female = 66 (74.2%)
  # Male = 22 (24.7%)
  # Non-binary = 1 (1.1%)

# Age
data_bls |>
  group_by(participant) |>
  summarize(age = first(age), .groups = "drop") |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

# Obs:
  # Range: 18-47
  # Mean: 20.9
  # SD: 4.40

# ------------------------------------------------------------------------------



# ----- 4. Descriptive stats for MLs -------------------------------------------
# Total number of participants
data_mls_sp |>
  summarize(total_participants = n_distinct(participant))

data_mls_en |>
  summarize(total_participants = n_distinct(participant))

# Obs:
  # A total of 35 Spanish monolinguals participated. 
  # A total of 36 Spanish monolinguals participated. 

# Origin self
data_mls_sp |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

data_mls_en |>
  group_by(participant) |>
  summarize(country_origin = first(country_origin), .groups = "drop") |>
  count(country_origin, sort = TRUE)

# Self-reported gender
data_mls_sp |>
  group_by(participant) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

data_mls_en |>
  group_by(participant) |>
  summarize(gender = first(gender), .groups = "drop") |>
  count(gender, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

# Obs:
  # Female = 27 (77.1%) -- SP; 25 (69.4%) -- EN
  # Male = 8 (22.9%) -- SP; 10 (27.8%) -- EN
  # Other = 1 (2.8%) -- EN

# Age
data_mls_sp |>
  group_by(participant) |>
  summarize(age = first(age), .groups = "drop") |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

data_mls_en |>
  group_by(participant) |>
  summarize(age = first(age), .groups = "drop") |>
  summarize(
    mean_age = mean(age),
    sd_age   = sd(age),
    min_age  = min(age),
    max_age  = max(age))

# Obs:
  # Range: 18-32 -- SP; 18-41 -- EN
  # Mean: 24.1 -- SP; 20.4 -- EN
  # SD: 3.71 -- SP; 4.32 -- EN

# Spanish monolinguals: Language
data_mls_sp |>
  group_by(participant) |>
  summarize(
    age_acq_sp = first(age_acq_sp),
    age_comfort_sp = first(age_comfort_sp), 
    .groups = "drop") |>
  count(age_acq_sp, age_comfort_sp, sort = TRUE)

# Obs: 
  # All Spanish monolinguals reported speaking Spanish and feeling comfortable 
  # with this language since birth.

data_mls_sp |>
  group_by(participant) |>
  summarize(age_acq_en = first(age_acq_en)) |>
  summarize(
    mean_age_acq_en = mean(age_acq_en),
    sd_age_acq_en   = sd(age_acq_en),
    min_age_acq_en  = min(age_acq_en),
    max_age_acq_en  = max(age_acq_en))

# Obs: 
  # All Spanish monolinguals started learning English when they started school. 
  # Range: 3-20 
  # Mean: 8.97 
  # SD: 4.29 

data_mls_sp |>
  group_by(participant) |>
  summarize(
    prof_speak_en      = first(prof_speak_en),
    prof_understand_en = first(prof_understand_en),
    prof_read_en       = first(prof_read_en),
    prof_write_en      = first(prof_write_en),
    .groups = "drop") |>
  summarize(
    mean_speak  = mean(prof_speak_en, na.rm = TRUE),
    sd_speak    = sd(prof_speak_en, na.rm = TRUE),
    mean_listen = mean(prof_understand_en, na.rm = TRUE),
    sd_listen   = sd(prof_understand_en, na.rm = TRUE),
    mean_read   = mean(prof_read_en, na.rm = TRUE),
    sd_read     = sd(prof_read_en, na.rm = TRUE),
    mean_write  = mean(prof_write_en, na.rm = TRUE),
    sd_write    = sd(prof_write_en, na.rm = TRUE))

# Obs:
  # 4.03 for speaking (SD = 1.04)
  # 4.57 for listening (SD = 0.655)
  # 4.63 for reading (SD = 0.877)
  # 4.2 for writing (SD = 1.05)

data_mls_sp |>
  group_by(participant) |>
  summarize(years_country_en = first(years_country_en)) |>
  summarize(
    mean_years_country_en = mean(years_country_en),
    sd_years_country_en   = sd(years_country_en),
    min_years_country_en  = min(years_country_en),
    max_years_country_en  = max(years_country_en))

data_mls_sp |>
  group_by(participant) |>
  summarize(years_country_en = first(years_country_en), .groups = "drop") |>
  mutate(
    abroad = if_else(years_country_en > 0, "Some time abroad", "Never")) |>
  count(abroad)

# Obs: 
  # 13 reported having lived some time abroad, whereas 22 have never.
  # Range: 0-5 
  # Mean: 1.03 
  # SD: 1.60 

# English monolinguals: Language
data_mls_en |>
  group_by(participant) |>
  summarize(
    age_acq_en = first(age_acq_en),
    age_comfort_en = first(age_comfort_en), 
    .groups = "drop") |>
  count(age_acq_en, age_comfort_en, sort = TRUE)

# Obs: 
# All English monolinguals reported speaking English and feeling comfortable 
# with this language since birth.

data_mls_en |>
  group_by(participant) |>
  summarize(age_acq_sp = first(age_acq_sp)) |>
  summarize(
    mean_age_acq_sp = mean(age_acq_sp),
    sd_age_acq_sp   = sd(age_acq_sp),
    min_age_acq_sp  = min(age_acq_sp),
    max_age_acq_sp  = max(age_acq_sp))

# Obs: 
  # All English monolinguals started learning Spanish around high school. 
  # Range: 11-20 
  # Mean: 16.1 
  # SD: 2.44 

data_mls_en |>
  group_by(participant) |>
  summarize(
    prof_speak_sp      = first(prof_speak_sp),
    prof_understand_sp = first(prof_understand_sp),
    prof_read_sp       = first(prof_read_sp),
    prof_write_sp      = first(prof_write_sp),
    .groups = "drop") |>
  summarize(
    mean_speak  = mean(prof_speak_sp, na.rm = TRUE),
    sd_speak    = sd(prof_speak_sp, na.rm = TRUE),
    mean_listen = mean(prof_understand_sp, na.rm = TRUE),
    sd_listen   = sd(prof_understand_sp, na.rm = TRUE),
    mean_read   = mean(prof_read_sp, na.rm = TRUE),
    sd_read     = sd(prof_read_sp, na.rm = TRUE),
    mean_write  = mean(prof_write_sp, na.rm = TRUE),
    sd_write    = sd(prof_write_sp, na.rm = TRUE))

# Obs:
# 0.861 for speaking (SD = 0.351)
# 1.14 for listening (SD = 0.351)
# 0.944 for reading (SD = 0.333)
# 0.889 for writing (SD = 0.319)



# ----- 5. Check BL data (Austin) ----------------------------------------------
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

# Data distribution
df <- subset(data_bls, form == "PP")

# Pre-compute stats per language
# Subset to rows where form == "PP"
pp <- df[df$form == "PP", ]

# Compute summary stats
m  <- mean(pp$rating, na.rm = TRUE)
md <- median(pp$rating, na.rm = TRUE)

# Histogram
hist(pp$rating,
     breaks = seq(0, 1, by = 0.1),   # rating ranges 0 to 1
     col    = "lightblue",
     border = "white",
     main   = "Histogram of Ratings (form = PP)",
     xlab   = "Rating",
     xlim   = c(0, 1))

# Vertical lines at mean and median
abline(v = m,  col = "red",  lwd = 2, lty = 2)
abline(v = md, col = "darkgreen", lwd = 2, lty = 3)

# Legend
legend("topright",
       legend = c(paste0("Mean = ",   round(m, 3)),
                  paste0("Median = ", round(md, 3))),
       col    = c("red", "darkgreen"),
       lwd    = 2,
       lty    = c(2, 3),
       bty    = "n")


library(purrr)
frm = "PF"
pp  <- subset(data_bls, form == frm)
bw  <- 0.1
set.seed(42)

# Pre-compute proportions within each language
pp_props <- pp %>%
  mutate(rating_bin = floor(rating / bw) * bw) %>%
  group_by(language, rating_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(language) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Observed + null stats per language
stats <- pp %>%
  group_by(language) %>%
  summarise(
    n            = n(),
    Obs_Mean     = mean(rating, na.rm = TRUE),
    Obs_Median   = median(rating, na.rm = TRUE),
    # simulate null: n random draws from Uniform(0,1)
    Null_Mean    = map_dbl(n, ~ mean(runif(.x, 0, 1))),
    Null_Median  = map_dbl(n, ~ median(runif(.x, 0, 1))),
    .groups      = "drop"
  ) %>%
  mutate(label = paste0(
    "Obs Mean = ",    round(Obs_Mean,   3),
    "\nObs Median = ", round(Obs_Median, 3),
    "\nNull Mean = ",  round(Null_Mean,  3),
    "\nNull Median = ", round(Null_Median, 3)
  ))

ggplot(pp_props, aes(x = rating_bin, y = prop)) +
  geom_col(fill = "lightblue", color = "white", width = bw * 0.9) +
  # Observed lines
  geom_vline(data = stats, aes(xintercept = Obs_Mean,    color = "Obs Mean"),
             linetype = "dashed",  linewidth = 1) +
  geom_vline(data = stats, aes(xintercept = Obs_Median,  color = "Obs Median"),
             linetype = "dotted",  linewidth = 1) +
  # Null lines
  geom_vline(data = stats, aes(xintercept = Null_Mean,   color = "Null Mean"),
             linetype = "dashed",  linewidth = 1) +
  geom_vline(data = stats, aes(xintercept = Null_Median, color = "Null Median"),
             linetype = "dotted",  linewidth = 1) +
  geom_text(data = stats, aes(x = Inf, y = Inf, label = label),
            hjust = 1.05, vjust = 1.2, size = 3, lineheight = 0.95,
            inherit.aes = FALSE) +
  scale_color_manual(
    name   = "Statistic",
    values = c("Obs Mean"    = "red",
               "Obs Median"  = "darkgreen",
               "Null Mean"   = "orange",
               "Null Median" = "purple")
  ) +
  facet_grid(language ~ .) +
  labs(title = paste("Observed vs Null Rating Proportions by Language (form =",frm, ")"),
       x = "Rating", y = "Proportion") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal()

# ------------------------------------------------------------------------------


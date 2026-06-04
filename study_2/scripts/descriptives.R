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


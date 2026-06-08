# 3a_hr_normalisation.R
# ==========================================================================
# Computes age-adjusted maximum HR and %maxHR for every intervals.icu workout.
#
# GENERIC — should work for any user without modification.
# Personal parameters (dob, hr_max_formula, hr_max_offset) come from config.R.
#
# Requires:
#   data/intervals_workouts.rds   — from 2_intervals_ingest.R
#   config.R
#
# Produces:
#   data/intervals_workouts_hr.rds  — intervals_workouts + max_hr_predicted
#                                     + pct_max_hr columns
#
# Tuning tip: after running, call check_hr_calibration() and inspect the plot.
# If predicted max HR is consistently above/below your empirical peaks, adjust
# hr_max_offset in config.R.
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)

message("🏃 Starting HR normalisation...")

# --- 1. Load ------------------------------------------------------------------
intervals_workouts <- read_rds("data/intervals_workouts.rds")

# --- 2. Max HR formula --------------------------------------------------------
# Returns predicted HRmax for a given age in years.
# Formula and offset are set in config.R.
predict_max_hr <- function(age_years,
                           formula = hr_max_formula,
                           offset  = hr_max_offset) {
  base <- switch(formula,
                 tanaka = 208 - 0.7 * age_years,
                 fox    = 220 - age_years,
                 gelish = 207 - 0.7 * age_years,
                 stop(sprintf("Unknown HR formula '%s'. Choose: tanaka, fox, gelish.", formula))
  )
  base + offset
}

# --- 3. Apply to workouts -----------------------------------------------------
intervals_workouts_hr <- intervals_workouts |>
  mutate(
    age_at_workout   = as.numeric(difftime(start_day, dob, units = "days")) / 365.25,
    max_hr_predicted = predict_max_hr(age_at_workout),
    # %maxHR: use recorded max HR from device where available
    pct_max_hr       = if_else(
      !is.na(max_hr) & max_hr > 0,
      max_hr / max_hr_predicted,
      NA_real_
    ),
    # Also normalise average HR
    pct_avg_hr       = if_else(
      !is.na(average_hr) & average_hr > 0,
      average_hr / max_hr_predicted,
      NA_real_
    )
  )

# --- 4. Calibration check -----------------------------------------------------
# Prints a summary comparing predicted max HR to empirically observed peaks.
# Useful for tuning hr_max_offset in config.R.
check_hr_calibration <- function(df = intervals_workouts_hr) {
  df |>
    filter(!is.na(max_hr), max_hr > 0) |>
    mutate(year = year(start_day)) |>
    group_by(year) |>
    summarise(
      n_workouts        = n(),
      observed_peak_hr  = max(max_hr, na.rm = TRUE),
      mean_predicted    = mean(max_hr_predicted, na.rm = TRUE),
      mean_pct_max      = mean(pct_max_hr, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      headroom_bpm = mean_predicted - observed_peak_hr
    ) |>
    print()
  
  message(
    sprintf("\nFormula: %s | Offset: %+.1f bpm | DOB: %s",
            hr_max_formula, hr_max_offset, dob),
    "\nIf 'headroom_bpm' is consistently negative, increase hr_max_offset in config.R.",
    "\nIf consistently large positive, decrease hr_max_offset."
  )
}

message("   Calibration summary (predicted vs observed peak HR by year):")
check_hr_calibration()

# --- 5. Save ------------------------------------------------------------------
write_rds(intervals_workouts_hr, "data/intervals_workouts_hr.rds")

message(sprintf(
  "✔ HR normalisation complete. %d workouts with pct_max_hr -> data/intervals_workouts_hr.rds",
  sum(!is.na(intervals_workouts_hr$pct_max_hr))
))
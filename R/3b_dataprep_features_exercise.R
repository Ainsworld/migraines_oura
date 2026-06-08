# 3b_exercise_features.R
# ==========================================================================
# Builds daily and per-workout HR stress features from intervals.icu data.
#
# GENERIC — should work for any user without modification.
# Thresholds and parameters come from config.R.
#
# Key design choice: features are expressed as %maxHR (from 3a) rather than
# raw BPM, so they remain comparable across age and between individuals.
# The hr_pct_thresholds vector in config.R controls which bands are computed.
#
# Requires:
#   data/intervals_workouts_hr.rds      — from 3a_hr_normalisation.R
#   data/intervals_hr_dist.rds          — from 2_intervals_ingest.R
#   data/intervals_hr_dist_activity.rds — from 2_intervals_ingest.R (optional)
#   config.R
#
# Produces:
#   data/exercise_daily.rds      — one row per day, HR stress + activity summary
#   data/exercise_per_workout.rds — one row per workout, HR stress features
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)

message("🏋️  Starting exercise feature engineering...")

# --- 1. Load ------------------------------------------------------------------
intervals_workouts_hr <- read_rds("data/intervals_workouts_hr.rds")

hr_activity_path <- "data/intervals_hr_dist_activity.rds"
have_activity_hr <- file.exists(hr_activity_path)
intervals_hr_dist_activity <- if (have_activity_hr) read_rds(hr_activity_path) else NULL

intervals_hr_dist <- read_rds("data/intervals_hr_dist.rds")

if (!have_activity_hr)
  message("   ⚠ Per-activity HR not found; using daily-level fallback. ",
          "Re-run 2_intervals_ingest.R to enable per-workout accuracy.")

# --- 2. Helper: compute HR stress features from a bpm/seconds table -----------
# Thresholds are supplied as fractions of max HR; max_hr_predicted per row
# is used to convert to absolute BPM on the fly.
#
# For daily aggregation, max_hr_predicted is the day's mean predicted max HR.
# For per-workout, it's the individual workout's predicted max HR.

build_hr_features <- function(df, max_hr_col, thresholds = hr_pct_thresholds,
                              min_secs = peak_min_secs) {
  # df must have columns: bpm, seconds, plus whatever grouping vars are needed.
  # max_hr_col is a scalar or per-group value supplied at summarise time.
  #
  # We compute features for each threshold t in `thresholds`:
  #   hr_mins_gtXX   : minutes with bpm >= t * max_hr
  #   hr_impulse_XX  : sum of (bpm - threshold) * seconds / 60  (linear stress)
  #   hr_strain_XX   : sum of (bpm - threshold)^2 * seconds / 6000 (quadratic)
  # Plus threshold-agnostic:
  #   peak_hr        : highest bpm sustained >= min_secs
  #   net_cardiac_impulse : sum of (bpm - rhr) * seconds / 3600
  #   homeostatic_strain  : sum of (bpm - rhr)^2 * seconds / 360000
  NULL  # implementation inline in summarise below for clarity
}

# --- 3. Daily HR features -----------------------------------------------------
# Join per-day mean predicted max HR from workouts to the HR distribution.
daily_max_hr <- intervals_workouts_hr |>
  group_by(start_day) |>
  summarise(max_hr_predicted = mean(max_hr_predicted, na.rm = TRUE), .groups = "drop")

# Build threshold-labelled feature names dynamically from config
pct_labels <- paste0(as.integer(hr_pct_thresholds * 100))  # e.g. "85", "90", "95"

daily_hr_features <- intervals_hr_dist |>
  left_join(daily_max_hr, by = "start_day") |>
  mutate(max_hr_predicted = coalesce(
    max_hr_predicted,
    predict_max_hr(as.numeric(difftime(start_day, dob, units = "days")) / 365.25)
  )) |>
  group_by(start_day) |>
  summarise(
    peak_hr = max(bpm[seconds >= peak_min_secs], -Inf, na.rm = TRUE),
    # Dynamic threshold features
    across(
      .cols = NULL,  # placeholder — computed via map below
    ),
    net_cardiac_impulse = sum(pmax(0, bpm - rhr_default) * seconds, na.rm = TRUE) / 3600,
    homeostatic_strain  = sum(((bpm - rhr_default)^2) * seconds, na.rm = TRUE) / 360000,
    max_hr_predicted    = first(max_hr_predicted),
    .groups = "drop"
  ) |>
  mutate(peak_hr = if_else(is.finite(peak_hr), peak_hr, NA_real_),
         peak_pct_max = peak_hr / max_hr_predicted)

# Build threshold columns via map (cleaner than hard-coding)
threshold_features <- intervals_hr_dist |>
  left_join(daily_max_hr, by = "start_day") |>
  mutate(max_hr_predicted = replace_na(max_hr_predicted, 170)) |>  # fallback
  group_by(start_day) |>
  summarise(
    across(everything(), ~ first(.x)),   # carry through for reference
    .groups = "drop"
  )

# Build one feature tibble per threshold then bind columns
thr_cols <- map(hr_pct_thresholds, function(t) {
  label <- as.integer(t * 100)
  intervals_hr_dist |>
    left_join(daily_max_hr, by = "start_day") |>
    mutate(
      max_hr_predicted = replace_na(max_hr_predicted, 170),
      thr_bpm = t * max_hr_predicted
    ) |>
    group_by(start_day) |>
    summarise(
      "{paste0('hr_mins_gt', label)}"  := sum(seconds[bpm >= thr_bpm], na.rm = TRUE) / 60,
      "{paste0('hr_impulse_', label)}" := sum(pmax(0, bpm - thr_bpm) * seconds, na.rm = TRUE) / 60,
      "{paste0('hr_strain_', label)}"  := sum((pmax(0, bpm - thr_bpm)^2) * seconds, na.rm = TRUE) / 6000,
      .groups = "drop"
    )
}) |>
  reduce(left_join, by = "start_day")

daily_hr_features <- intervals_hr_dist |>
  left_join(daily_max_hr, by = "start_day") |>
  mutate(max_hr_predicted = replace_na(max_hr_predicted, 170)) |>
  group_by(start_day) |>
  summarise(
    peak_hr             = max(bpm[seconds >= peak_min_secs], -Inf, na.rm = TRUE),
    net_cardiac_impulse = sum(pmax(0, bpm - rhr_default) * seconds, na.rm = TRUE) / 3600,
    homeostatic_strain  = sum(((bpm - rhr_default)^2) * seconds, na.rm = TRUE) / 360000,
    max_hr_predicted    = first(max_hr_predicted),
    .groups = "drop"
  ) |>
  mutate(
    peak_hr      = if_else(is.finite(peak_hr), peak_hr, NA_real_),
    peak_pct_max = peak_hr / max_hr_predicted
  ) |>
  left_join(thr_cols, by = "start_day") |>
  rename(date = start_day)

# --- 4. Activity harmonisation (Intervals + Oura workout summary) -------------
acts_intervals <- intervals_workouts_hr |>
  transmute(
    source         = "Intervals",
    workout_id     = id,
    start_time, end_time,
    date           = start_day,
    activity       = activity_type,
    duration_mins  = as.numeric(duration, units = "mins"),
    calories,
    intensity_score = coalesce(icu_intensity, 0),
    peak_hr_reported = max_hr,
    max_hr_predicted,
    pct_max_hr,
    pct_avg_hr
  )

# Oura workouts — only included where no intervals entry overlaps
oura_tidy_path <- "data/oura_tidy.rds"
if (file.exists(oura_tidy_path)) {
  oura_workouts <- read_rds(oura_tidy_path)$oura_workouts
  acts_oura <- oura_workouts |>
    transmute(
      source        = "Oura",
      workout_id    = NA_character_,
      start_time, end_time,
      date          = start_day,
      activity      = activity_type,
      duration_mins = as.numeric(duration, units = "mins"),
      calories,
      intensity_score = case_match(str_to_lower(intensity),
                                   "easy" ~ 40, "moderate" ~ 60, "hard" ~ 80,
                                   .default = NA_real_),
      peak_hr_reported = NA_real_,
      max_hr_predicted = NA_real_,
      pct_max_hr = NA_real_,
      pct_avg_hr = NA_real_
    )
  
  acts_oura_unique <- acts_oura |>
    anti_join(acts_intervals,
              by = join_by(overlaps(start_time, end_time, start_time, end_time)))
  
  acts_all <- bind_rows(acts_intervals, acts_oura_unique)
} else {
  message("   ℹ No Oura data found — using Intervals only.")
  acts_all <- acts_intervals
}

daily_activity_harmonised <- acts_all |>
  group_by(date) |>
  summarise(
    total_duration_mins    = sum(duration_mins, na.rm = TRUE),
    total_active_cals      = sum(calories, na.rm = TRUE),
    has_tracked_sport      = any(source == "Intervals"),
    sport_duration_mins    = sum(duration_mins[source == "Intervals"], na.rm = TRUE),
    life_duration_mins     = sum(duration_mins[source == "Oura"], na.rm = TRUE),
    activity_duration_mins = sport_duration_mins + life_duration_mins,
    .groups = "drop"
  )

# --- 5. Per-workout HR features -----------------------------------------------
if (have_activity_hr) {
  message("   ✔ Building per-workout HR features from activity-level data...")
  
  workout_thr_cols <- map(hr_pct_thresholds, function(t) {
    label <- as.integer(t * 100)
    intervals_hr_dist_activity |>
      left_join(select(intervals_workouts_hr, id, max_hr_predicted),
                by = c("activity_id" = "id")) |>
      mutate(
        max_hr_predicted = replace_na(max_hr_predicted, 170),
        thr_bpm = t * max_hr_predicted
      ) |>
      group_by(activity_id) |>
      summarise(
        "{paste0('hr_mins_gt', label)}"  := sum(seconds[bpm >= thr_bpm], na.rm = TRUE) / 60,
        "{paste0('hr_impulse_', label)}" := sum(pmax(0, bpm - thr_bpm) * seconds, na.rm = TRUE) / 60,
        "{paste0('hr_strain_', label)}"  := sum((pmax(0, bpm - thr_bpm)^2) * seconds, na.rm = TRUE) / 6000,
        .groups = "drop"
      )
  }) |>
    reduce(left_join, by = "activity_id")
  
  exercise_per_workout <- intervals_hr_dist_activity |>
    left_join(select(intervals_workouts_hr, id, max_hr_predicted, pct_max_hr),
              by = c("activity_id" = "id")) |>
    group_by(activity_id, start_time, end_time, start_day, max_hr_predicted) |>
    summarise(
      peak_hr = max(bpm[seconds >= peak_min_secs], -Inf, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      peak_hr      = if_else(is.finite(peak_hr), peak_hr, NA_real_),
      peak_pct_max = peak_hr / max_hr_predicted
    ) |>
    left_join(workout_thr_cols, by = "activity_id") |>
    rename(workout_id = activity_id, date = start_day)
  
} else {
  message("   ⚠ Per-workout HR unavailable — using daily-attributed fallback.")
  # Assign daily features to the longest workout of each day
  exercise_per_workout <- intervals_workouts_hr |>
    group_by(start_day) |>
    slice_max(duration, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(workout_id = id, start_time, end_time, date = start_day) |>
    left_join(daily_hr_features, by = "date") |>
    filter(!is.na(peak_hr))
}

# --- 6. Save ------------------------------------------------------------------
write_rds(daily_hr_features,        "data/exercise_daily_hr.rds")
write_rds(daily_activity_harmonised,"data/exercise_daily_activity.rds")
write_rds(exercise_per_workout,     "data/exercise_per_workout.rds")
write_rds(acts_all,                 "data/acts_all.rds")

message("✔ Exercise features complete.")
message(sprintf("   - Daily HR features:    %d days  -> data/exercise_daily_hr.rds", nrow(daily_hr_features)))
message(sprintf("   - Daily activity:       %d days  -> data/exercise_daily_activity.rds", nrow(daily_activity_harmonised)))
message(sprintf("   - Per-workout features: %d workouts -> data/exercise_per_workout.rds", nrow(exercise_per_workout)))

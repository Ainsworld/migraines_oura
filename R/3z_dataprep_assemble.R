# 3f_assemble.R
# ==========================================================================
# Spine join: assembles all feature outputs into the final analysis-ready
# daily dataset and the continuous-time substrate.
#
# GENERIC — no feature logic here, only joins. If you have skipped any of
# the 3a-3e scripts, comment out the corresponding left_join below.
#
# Requires (all from prior scripts):
#   data/oura_tidy.rds
#   data/exercise_daily_hr.rds        — 3b
#   data/exercise_daily_activity.rds  — 3b
#   data/exercise_per_workout.rds     — 3b
#   data/drink_days.rds               — 3c  (comment out join if skipped)
#   data/migraine_features.rds        — 3d  (comment out join if skipped)
#   data/other_features.rds           — 3e
#   config.R
#
# Produces:
#   data/analysis_ready.rds       — daily spine for frequentist/Bayesian models
#   data/continuous_substrate.rds — event-level tables + hourly hazard grid
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)

message("🔗 Assembling final datasets...")

# --- 1. Load all feature artifacts --------------------------------------------
oura_data    <- read_rds("data/oura_tidy.rds")
readiness    <- oura_data$oura_daily_readiness
sleep        <- oura_data$oura_daily_sleep

daily_hr       <- read_rds("data/exercise_daily_hr.rds")
daily_activity <- read_rds("data/exercise_daily_activity.rds")
per_workout    <- read_rds("data/exercise_per_workout.rds")
acts_all       <- read_rds("data/acts_all.rds")

drink_days     <- if (file.exists("data/drink_days.rds"))     read_rds("data/drink_days.rds")     else tibble()
migraine_data  <- if (file.exists("data/migraine_features.rds")) read_rds("data/migraine_features.rds") else NULL
other          <- read_rds("data/other_features.rds")

event_days      <- other$event_days
trigger_events  <- other$trigger_events
altitude_blocks <- other$altitude_blocks
in_any_block    <- other$in_any_block

# Migraine components (NULL-safe)
migraine_days       <- migraine_data$migraine_days
missed_med_events   <- migraine_data$missed_med_events
migraine_episodes   <- migraine_data$migraine_episodes
medication_timeline <- migraine_data$medication_timeline
med_dose_at         <- migraine_data$med_dose_at
triptan_events      <- migraine_data$triptan_events
atypical_migraines  <- migraine_data$atypical_migraines

# Sentinel date: last known migraine before data window (for days_since_last)
# ▶ PERSONAL — set this to the date of the last migraine before your Oura start
migraine_before_data <- as.Date("2022-11-02")

# --- 2. Daily spine -----------------------------------------------------------
spine <- tibble(
  date = seq.Date(min(readiness$start_day), max(readiness$start_day), by = "day")
)

# --- 3. Build analysis_df -----------------------------------------------------
analysis_df <- spine |>
  
  # Migraine days (▶ comment out if 3d skipped)
  left_join(migraine_days, by = "date") |>
  
  # Weekend / weekday flags
  mutate(
    is_weekend  = wday(date, week_start = 1) >= 6,
    is_saturday = wday(date, week_start = 1) == 6,
    is_sunday   = wday(date, week_start = 1) == 7,
    weekday     = wday(date, week_start = 1, label = TRUE)
  ) |>
  
  # Days since last migraine (▶ comment out if 3d skipped)
  left_join(
    migraine_days |> transmute(prev_migraine = date) |> distinct(),
    by = join_by(closest(date > prev_migraine))
  ) |>
  mutate(
    prev_migraine        = coalesce(prev_migraine, migraine_before_data),
    days_since_last      = as.numeric(difftime(date, prev_migraine, units = "days")),
    weeks_since_last     = days_since_last / 7,
    weeks_since_last_max1 = pmin(1, weeks_since_last),
    migraine_yesterday   = coalesce(days_since_last == 1, FALSE)
  ) |>
  
  # Medication timeline (▶ comment out if 3d skipped)
  left_join(medication_timeline,
            by = join_by(between(date, treatment_start, treatment_end))) |>
  mutate(any_medication = !is.na(treatment)) |>
  rename(medication_dose = treatment_dose) |>
  select(-starts_with("treatment")) |>
  
  # Alcohol (▶ comment out if 3c skipped)
  left_join(drink_days, by = "date") |>
  
  # Other tag features
  left_join(event_days, by = "date") |>
  
  # Oura readiness and sleep
  left_join(readiness |> select(start_day, readiness_score),
            by = join_by(date == start_day)) |>
  left_join(sleep |> select(start_day, sleep_score,
                            total_sleep = contributors.total_sleep),
            by = join_by(date == start_day)) |>
  
  # Exercise
  left_join(daily_activity, by = "date") |>
  left_join(daily_hr,       by = "date") |>
  
  # Zero-fill NAs for additive features
  mutate(
    across(
      c(migraine_n, migraine_episodes_n,
        drink_any_wine, drink_any_beer, drinks_count, drinks_amount,
        drink_sessions, peak_ebac_daily, peak_ebac_proxy_daily, peak_bac_rate_daily,
        total_duration_mins, total_active_cals,
        sport_duration_mins, life_duration_mins, activity_duration_mins,
        net_cardiac_impulse, homeostatic_strain, medication_dose,
        starts_with("hr_")),
      ~ replace_na(., 0)
    ),
    across(c(has_tracked_sport),  ~ replace_na(., FALSE)),
    across(starts_with("any_"),   ~ replace_na(., FALSE)),
    across(starts_with("extra_"), ~ replace_na(., FALSE)),
    across(ends_with("_score"),   ~ replace_na(., as.integer(mean(., na.rm = TRUE))))
  ) |>
  mutate(
    badsleep_d    = 0.1 * (mean(sleep_score,     na.rm = TRUE) - sleep_score),
    totalsleep_d  = 0.1 * (mean(total_sleep,     na.rm = TRUE) - total_sleep),
    unreadiness_d = 0.1 * (mean(readiness_score, na.rm = TRUE) - readiness_score),
    migraine      = coalesce(migraine_n >= 1, FALSE)
  )

write_rds(analysis_df, "data/analysis_ready.rds")
message(sprintf("✔ Daily artifact: %d rows -> data/analysis_ready.rds", nrow(analysis_df)))

# --- 4. Continuous-time substrate ---------------------------------------------
message("   ⏱  Assembling continuous-time substrate...")

# Unified exposure-event table
workout_events <- per_workout  # alias for clarity

# Identify first %maxHR threshold column name for the primary magnitude
first_thr_label <- as.integer(hr_pct_thresholds[2] * 100)  # middle threshold
mins_col   <- paste0("hr_mins_gt",  first_thr_label)
impulse_col <- paste0("hr_impulse_", first_thr_label)

exposure_events <- bind_rows(
  workout_events |>
    transmute(
      event_time       = end_time,
      event_time_local = end_time,
      event_type       = "workout",
      magnitude        = .data[[mins_col]],
      peak_hr,
      peak_pct_max,
      hr_impulse       = .data[[impulse_col]],
      workout_id
    ),
  if (!is.null(migraine_data)) {
    read_rds("data/alcohol_sessions.rds") |>
      transmute(
        event_time       = drink_first,
        event_time_local = peak_time_local,
        event_type       = "alcohol",
        magnitude        = peak_ebac,
        peak_bac_rate,
        drinks_amount    = drinks_amount_sum,
        drink_series
      )
  } else tibble(),
  trigger_events
) |>
  arrange(event_time_local)

# Hourly hazard spine
grid_start   <- floor_date(min(readiness$start_day),   "day")
grid_end     <- ceiling_date(max(readiness$start_day), "day")
hourly_spine <- tibble(
  t_local = seq(as_datetime(grid_start), as_datetime(grid_end), by = "1 hour")
)
floor_h <- function(x) floor_date(x, "hour")

# Migraine onsets on the hourly grid (▶ comment out if 3d skipped)
mig_hour <- if (!is.null(migraine_episodes)) {
  migraine_episodes |>
    mutate(t_local = floor_h(onset_est_local)) |>
    group_by(t_local) |>
    summarise(migraine_onset  = n(),
              onset_atypical  = sum(any_atypical),
              .groups = "drop")
} else tibble(t_local = as.POSIXct(character()))

exp_workout <- exposure_events |>
  filter(event_type == "workout") |>
  transmute(t_local          = floor_h(event_time_local),
            exp_wkt_mins     = magnitude,
            exp_wkt_impulse  = hr_impulse,
            exp_wkt_peakhr   = peak_hr,
            exp_wkt_pctmax   = peak_pct_max) |>
  group_by(t_local) |>
  summarise(exp_wkt_mins    = sum(exp_wkt_mins,    na.rm = TRUE),
            exp_wkt_impulse = sum(exp_wkt_impulse, na.rm = TRUE),
            exp_wkt_peakhr  = max(exp_wkt_peakhr,  na.rm = TRUE),
            exp_wkt_pctmax  = max(exp_wkt_pctmax,  na.rm = TRUE),
            .groups = "drop") |>
  mutate(exp_wkt_peakhr = if_else(is.finite(exp_wkt_peakhr), exp_wkt_peakhr, 0),
         exp_wkt_pctmax = if_else(is.finite(exp_wkt_pctmax), exp_wkt_pctmax, 0))

exp_alc <- exposure_events |>
  filter(event_type == "alcohol") |>
  transmute(t_local          = floor_h(event_time_local),
            exp_alc_peakebac = magnitude,
            exp_alc_peakrate = peak_bac_rate,
            exp_alc_units    = drinks_amount) |>
  group_by(t_local) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

exp_trig <- exposure_events |>
  filter(event_type %in% c("plane","coffee","stress","hydration","missed_medication")) |>
  transmute(t_local = floor_h(event_time_local), event_type, magnitude) |>
  group_by(t_local, event_type) |>
  summarise(magnitude = sum(magnitude), .groups = "drop") |>
  pivot_wider(names_from = event_type, names_prefix = "exp_",
              values_from = magnitude, values_fill = 0)

onset_lookup <- if (!is.null(migraine_episodes)) {
  migraine_episodes |> transmute(prev_onset = onset_est_local) |> distinct()
} else {
  tibble(prev_onset = as.POSIXct(character()))}

hourly_df <- hourly_spine |>
  left_join(mig_hour,    by = "t_local") |>
  left_join(exp_workout, by = "t_local") |>
  left_join(exp_alc,     by = "t_local") |>
  left_join(exp_trig,    by = "t_local") |>
  mutate(
    migraine_onset   = replace_na(migraine_onset, 0L),
    onset_atypical   = replace_na(onset_atypical, 0L),
    across(starts_with("exp_"), ~ replace_na(.x, 0)),
    state_highaltitude = in_any_block(t_local, altitude_blocks),
    date       = as.Date(t_local),
    hour       = hour(t_local),
    dow        = wday(t_local, week_start = 1, label = TRUE),
    is_weekend = wday(t_local, week_start = 1) >= 6,
    circ_sin   = sin(2 * pi * hour / 24),
    circ_cos   = cos(2 * pi * hour / 24),
    medication_dose = if (!is.null(med_dose_at))
      med_dose_at(date, medication_timeline) else 0
  ) |>
  left_join(onset_lookup, by = join_by(closest(t_local > prev_onset))) |>
  mutate(
    prev_onset = coalesce(prev_onset, as_datetime(migraine_before_data)),
    hours_since_last_migraine = as.numeric(difftime(t_local, prev_onset, units = "hours"))
  ) |>
  select(-prev_onset)

# --- 5. Save continuous substrate ---------------------------------------------
continuous_substrate <- list(
  triptan_events      = triptan_events,
  migraine_episodes   = migraine_episodes,
  atypical_migraines  = atypical_migraines,
  exposure_events     = exposure_events,
  workout_events      = workout_events,
  alcohol_sessions    = if (file.exists("data/alcohol_sessions.rds"))
    read_rds("data/alcohol_sessions.rds") else NULL,
  trigger_events      = trigger_events,
  altitude_blocks     = altitude_blocks,
  missed_med_events   = missed_med_events,
  medication_timeline = medication_timeline,
  hourly_df           = hourly_df,
  params              = as.list(environment(config))   # snapshot of config at run time
)

write_rds(continuous_substrate, "data/continuous_substrate.rds")

message(sprintf("✔ Continuous substrate -> data/continuous_substrate.rds"))
message(sprintf("   - hourly_df: %d rows | workout_events: %d | exposure_events: %d",
                nrow(hourly_df), nrow(workout_events), nrow(exposure_events)))
message("✔ Assembly complete.")

# 3d_migraine_features.R
# ==========================================================================
# Triptan administrations, migraine episode detection, onset estimation,
# and missed medication events.
#
# ▶▶ PERSONAL — this script is specific to Mark's data and health context.
#    If you are adapting this codebase for your own condition:
#      - Replace the triptan/migraine tag logic with your own condition tags
#      - Adapt or remove the medication_timeline tribble (in config.R)
#      - The episode-merging and onset-estimation patterns are reusable as-is
#      - If you have no health condition to model, skip this script and remove
#        migraine_days and missed_med_events from 3f_assemble.R
#
# Data model notes:
#   * A 'migraine' tag = moment a SUMATRIPTAN was taken (not onset).
#   * Triptan ~= onset in practice (~1h buildup), except for a handful of
#     logged spans where start = onset, end = triptan (flagged as atypical).
#   * Closely-successive tags = re-dose (same episode), collapsed below.
#   * medication_timeline = PRESCRIBED dose, not adherence.
#     Missed doses are captured via Oura 'missed_med' tags.
#
# Requires:
#   data/oura_tidy.rds   — from 1_oura_ingest.R
#   config.R
#
# Produces:
#   data/migraine_features.rds  — list containing:
#       triptan_events, migraine_episodes, atypical_migraines,
#       migraine_days, missed_med_events, medication_timeline
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)

message("🤕 Starting migraine feature engineering...")

# --- 1. Load ------------------------------------------------------------------
oura_data <- read_rds("data/oura_tidy.rds")
tags      <- oura_data$oura_tags

if (!"spans_date"       %in% names(tags)) tags$spans_date       <- FALSE
if (!"end_time_imputed" %in% names(tags)) tags$end_time_imputed <- FALSE
if (!"end_day"          %in% names(tags)) tags$end_day          <- as.Date(NA)

# --- 2. Medication timeline ---------------------------------------------------
# medication_timeline is defined in config.R — edit it there.
# ▶ If you have no medication to model, set medication_timeline to an empty
#   tibble in config.R (see the comment there) and this section still runs safely.

med_dose_at <- function(dates, timeline) {
  idx <- map_int(as.Date(dates), function(d) {
    hit <- which(d >= timeline$treatment_start & d < timeline$treatment_end)
    if (length(hit) == 0) NA_integer_ else hit[1]
  })
  timeline$treatment_dose[idx] |> replace_na(0)
}

# --- 3. Triptan administrations -----------------------------------------------
triptan_events <- tags |>
  filter(tag == "migraine") |>
  arrange(start_time) |>
  mutate(
    span_hrs  = as.numeric(difftime(end_time_local, start_time_local, units = "hours")),
    real_span = !is.na(span_hrs) &
      span_hrs > migraine_min_span_hrs &
      span_hrs < migraine_max_span_hrs,
    onset_known    = real_span | spans_date,
    atypical       = onset_known,  # every span is unrepresentative (slow-onset/untreated)
    triptan_local  = case_when(
      real_span  ~ end_time_local,
      spans_date ~ end_time_local,   # imputed; see end_time_imputed flag
      TRUE       ~ start_time_local
    ),
    onset_local = if_else(onset_known, start_time_local, ymd_hms(NA_character_)),
    # Daily attribution anchor: true onset if known, else tag time
    attr_local  = coalesce(onset_local, start_time_local),
    # Continuous-time onset estimate: true if known, else triptan minus typical buildup
    onset_est_local = if_else(onset_known, onset_local,
                              start_time_local - dhours(typical_buildup_hrs)),
    side = str_extract(str_to_lower(coalesce(comment, "")), "left|right|bilateral")
  )

atypical_migraines <- triptan_events |>
  filter(atypical) |>
  select(start_time_local, end_time_local, span_hrs, spans_date,
         end_time_imputed, side, comment)

message(sprintf("   > Triptan administrations: %d | Atypical (spanned): %d",
                nrow(triptan_events), nrow(atypical_migraines)))

# --- 4. Episode merging -------------------------------------------------------
# Re-doses within episode_merge_hrs are collapsed into one episode.
migraine_episodes <- triptan_events |>
  arrange(triptan_local) |>
  mutate(
    gap_hrs     = as.numeric(difftime(triptan_local, lag(triptan_local), units = "hours")),
    new_episode = is.na(gap_hrs) | gap_hrs > episode_merge_hrs,
    episode_id  = cumsum(new_episode)
  ) |>
  group_by(episode_id) |>
  summarise(
    n_triptans          = n(),
    multi_dose          = n() > 1,
    onset_known         = any(onset_known),
    any_atypical        = any(atypical),
    triptan_first_local = min(triptan_local),
    triptan_last_local  = max(triptan_local),
    onset_est_local     = min(onset_est_local),
    .groups = "drop"
  ) |>
  arrange(onset_est_local) |>
  mutate(
    prev_onset           = lag(onset_est_local),
    episode_interval_hrs = as.numeric(difftime(onset_est_local, prev_onset, units = "hours")),
    date = as.Date(floor_date(onset_est_local - hours(day_transition_hrs), unit = "day"))
  )

message(sprintf("   > Episodes: %d (%d multi-dose)",
                nrow(migraine_episodes), sum(migraine_episodes$multi_dose)))

# --- 5. Daily migraine summary ------------------------------------------------
triptan_attr <- triptan_events |>
  mutate(date = as.Date(floor_date(attr_local - hours(day_transition_hrs), unit = "day")))

migraine_days <- triptan_attr |>
  group_by(date) |>
  summarise(
    migraine_first    = min(start_time),
    migraine_n        = n(),
    .groups = "drop"
  ) |>
  left_join(
    migraine_episodes |> count(date, name = "migraine_episodes_n"),
    by = "date"
  ) |>
  mutate(migraine_episodes_n = replace_na(migraine_episodes_n, 0L))

# --- 6. Missed medication events ----------------------------------------------
missed_med_events <- tags |>
  filter(tag %in% tag_sets$missed_med) |>
  transmute(
    event_time       = start_time,
    event_time_local = start_time_local,
    event_type       = "missed_medication",
    magnitude        = 1
  )

message(sprintf("   > Migraine days: %d | Missed med events: %d",
                nrow(migraine_days), nrow(missed_med_events)))

# --- 7. Save ------------------------------------------------------------------
migraine_features <- list(
  triptan_events     = triptan_events,
  migraine_episodes  = migraine_episodes,
  atypical_migraines = atypical_migraines,
  migraine_days      = migraine_days,
  missed_med_events  = missed_med_events,
  medication_timeline = medication_timeline,
  med_dose_at        = med_dose_at   # helper function — used in 3f_assemble.R
)

write_rds(migraine_features, "data/migraine_features.rds")
message("✔ Migraine features complete -> data/migraine_features.rds")

# 3_data_prep.R
# ==========================================================================
# Feature engineering for migraine analysis.
#
# Artifacts:
#   (1) data/analysis_ready.rds       -- daily spine `analysis_df` (unchanged
#                                        contract for 5_model_frequentist/bayesian).
#   (2) data/continuous_substrate.rds -- event-level tables + hourly hazard grid
#                                        for a future 5c continuous-time model.
#
# IMPORTANT data-model semantics (clarified by Mark):
#   * A `migraine` tag = the moment a SUMATRIPTAN was taken (when the migraine
#     became "clearly real"), NOT the onset.
#   * In practice the triptan is taken AT onset (~1h buildup), so triptan ~= onset.
#   * EXCEPTION: a handful (~6) of migraine tags have a real start/end span. These
#     are ATYPICAL (slow-onset / untreated, e.g. one 16h "ran out of sumatriptan"
#     attack). Start = onset, end = triptan; flagged as outliers, NOT used to
#     calibrate a population lag.
#   * Some date-spanning tags arrive (from 1_oura_ingest.R) with a NULL end_time
#     but a distinct end_day -> `spans_date` + a flagged imputed end_time_local.
#   * Closely-successive migraine tags = the first triptan didn't work (re-dose),
#     i.e. the SAME episode, not new attacks. Collapsed into `migraine_episodes`.
#   * `highaltitude` tags = skiing holidays => sustained multi-day STATE, not an
#     impulse. Merged into `altitude_blocks`.
#
# EBAC fixes (carried from prior revision): simulated Widmark peak is used (not
# the proxy); simulator gets the per-drink duration vector; elimination_rate is
# 0.015/hr for the g/100ml scale; explicit logged spans and inherited last-drink
# cadence feed durations.
# ==========================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(readr)

# --- 0. Parameters (single source of truth) -----------------------------------

params <- list(
  # Migraine attribution / episodes
  day_transition_hrs   = 8,          # "day" rolls over at 08:00 local
  episode_merge_hrs     = 8,         # triptans within this gap = same episode (re-dose)
  migraine_min_span_hrs = 5/60,      # treat a logged span as a real span if > ~5 min
  migraine_max_span_hrs = 72,        # ...and < this (attacks can last days)
  typical_buildup_hrs   = 1,         # typical onset->triptan buildup for point tags
  
  # HR stress
  rhr_default   = 50,
  peak_min_secs = 10,                # ignore HR blips shorter than this for "peak"
  
  # Alcohol / EBAC (Widmark, BAC in g/100ml = %)
  drink_series_hrs         = 2.1,
  body_weight_kg           = 74,
  widmark_r                = 0.68,
  elimination_rate         = 0.015,
  grams_per_drink_amount   = 8.0,
  default_amount           = 2,
  default_consumption_time = 0.5,
  min_span_hrs             = 1/60,
  max_span_hrs             = 8,
  
  # Altitude / continuous grid
  altitude_merge_hrs = 48,           # altitude tags within 2 days = one holiday block
  grid_unit          = "hour"
)
params$widmark_factor <- params$body_weight_kg * params$widmark_r * 10

# Tag vocabularies (tolerant of tags that don't exist yet)
tag_sets <- list(
  coffee     = c("coffee"),
  stress     = c("stress", "anxiety", "party", "socialgathering"),
  travel     = c("travel", "airplane", "car", "train"),
  plane      = c("airplane"),
  highalt    = c("highaltitude"),
  lowpress   = c("airplane", "highaltitude"),
  hydration  = c("hydration", "electrolyte", "electrolytes", "rehydrate"),  # FUTURE experiment
  missed_med = c("Missed Migraine medicine", "missed migraine medicine")
)

# --- 1. Load Data -------------------------------------------------------------
oura_data          <- read_rds("data/oura_tidy.rds")
intervals_workouts <- read_rds("data/intervals_workouts.rds")
intervals_hr_dist  <- read_rds("data/intervals_hr_dist.rds")

hr_activity_path <- "data/intervals_hr_dist_activity.rds"
have_activity_hr <- file.exists(hr_activity_path)
intervals_hr_activity <- if (have_activity_hr) read_rds(hr_activity_path) else NULL

tags      <- oura_data$oura_tags
activity  <- oura_data$oura_daily_activity
readiness <- oura_data$oura_daily_readiness
sleep     <- oura_data$oura_daily_sleep
oura_workouts <- oura_data$oura_workouts

# Tolerate an older 1_oura_ingest.R that predates the date-spanning-tag fix.
if (!"spans_date" %in% names(tags))       tags$spans_date <- FALSE
if (!"end_time_imputed" %in% names(tags)) tags$end_time_imputed <- FALSE
if (!"end_day" %in% names(tags))          tags$end_day <- as.Date(NA)

message("⚙️  Starting Feature Engineering...")
if (!have_activity_hr)
  message("   ⚠ Per-activity HR not found; using daily-attributed fallback. ",
          "Add the hr_dist_activity save to 2_intervals_ingest.R for per-workout accuracy.")

# --- 2. Helper functions ------------------------------------------------------

extract_amount <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  suppressWarnings(readr::parse_number(as.character(x)))
}

# Grid-integrated BAC trajectory: peak level, peak +ve rate, time-of-peak (hrs).
simulate_bac_path <- function(start_hrs, dur_hrs, amounts, p = params, dt = 1/60) {
  if (length(amounts) == 0) return(c(peak_bac = 0, peak_rate = 0, peak_time = 0))
  ord <- order(start_hrs)
  start_hrs <- start_hrs[ord]; dur_hrs <- dur_hrs[ord]; amounts <- amounts[ord]
  t <- seq(0, max(start_hrs + dur_hrs) + 3, by = dt)
  grams <- p$grams_per_drink_amount * amounts
  ingest <- numeric(length(t))
  for (i in seq_along(amounts)) {
    active <- t >= start_hrs[i] & t < (start_hrs[i] + dur_hrs[i])
    ingest[active] <- ingest[active] + grams[i] / dur_hrs[i]
  }
  rate <- pmax(0, ingest / p$widmark_factor - p$elimination_rate)
  bac <- numeric(length(t))
  for (k in 2:length(t))
    bac[k] <- max(0, bac[k - 1] + (ingest[k - 1] / p$widmark_factor - p$elimination_rate) * dt)
  c(peak_bac = max(bac), peak_rate = max(rate), peak_time = t[which.max(bac)])
}

med_dose_at <- function(dates, timeline) {
  idx <- map_int(as.Date(dates), function(d) {
    hit <- which(d >= timeline$treatment_start & d < timeline$treatment_end)
    if (length(hit) == 0) NA_integer_ else hit[1]
  })
  timeline$treatment_dose[idx] |> replace_na(0)
}

# Merge timestamped tags into blocks where successive gaps <= merge_hrs.
make_blocks <- function(starts, ends, merge_hrs) {
  if (length(starts) == 0) return(tibble(block_id = integer(), block_start = as.POSIXct(character()),
                                         block_end = as.POSIXct(character()), n_tags = integer()))
  ord <- order(starts); starts <- starts[ord]; ends <- coalesce(ends[ord], starts)
  gap <- as.numeric(difftime(starts, lag(ends), units = "hours"))
  block_id <- cumsum(is.na(gap) | gap > merge_hrs)
  tibble(block_id, block_start = starts, block_end = ends) |>
    group_by(block_id) |>
    summarise(block_start = min(block_start), block_end = max(block_end),
              n_tags = n(), .groups = "drop")
}

in_any_block <- function(times, blocks) {
  if (nrow(blocks) == 0) return(rep(0L, length(times)))
  map_int(times, ~ as.integer(any(.x >= blocks$block_start & .x <= blocks$block_end)))
}

# --- 3. Activity Harmonisation ------------------------------------------------
message("   🔨 Harmonising activity feeds...")

acts_intervals <- intervals_workouts |>
  transmute(source = "Intervals", workout_id = id,
            start_time, end_time, date = start_day, activity = activity_type,
            duration_mins = as.numeric(duration, units = "mins"),
            calories, intensity_score = coalesce(icu_intensity, 0),
            peak_hr_reported = max_hr)

acts_oura <- oura_workouts |>
  transmute(source = "Oura", workout_id = NA_character_,
            start_time, end_time, date = start_day, activity = activity_type,
            duration_mins = as.numeric(duration, units = "mins"), calories,
            intensity_score = case_match(str_to_lower(intensity),
                                         "easy" ~ 40, "moderate" ~ 60, "hard" ~ 80,
                                         .default = NA),
            peak_hr_reported = NA_real_)

acts_oura_unique <- acts_oura |>
  anti_join(acts_intervals,
            by = join_by(overlaps(start_time, end_time, start_time, end_time)))

acts_all <- bind_rows(acts_intervals, acts_oura_unique)

daily_activity_harmonised <- acts_all |>
  group_by(date) |>
  summarise(
    total_duration_mins = sum(duration_mins, na.rm = TRUE),
    total_active_cals   = sum(calories, na.rm = TRUE),
    has_tracked_sport   = max(source == "Intervals") == 1,
    sport_duration_mins = sum(duration_mins[source == "Intervals"], na.rm = TRUE),
    life_duration_mins  = sum(duration_mins[source == "Oura"], na.rm = TRUE),
    activity_duration_mins = sport_duration_mins + life_duration_mins,
    .groups = "drop"
  )

# --- 4. HR Stress Features (daily) --------------------------------------------
message("   ❤️  Calculating HR stress features...")

daily_rhr_lookup <- readiness |>
  mutate(rhr = params$rhr_default) |>
  select(start_day, rhr) |>
  distinct(start_day, .keep_all = TRUE)

daily_hr_features <- intervals_hr_dist |>
  left_join(daily_rhr_lookup, by = "start_day") |>
  mutate(rhr = replace_na(rhr, params$rhr_default)) |>
  group_by(start_day) |>
  summarise(
    peak_hr        = max(bpm[seconds >= params$peak_min_secs], -Inf, na.rm = TRUE),
    hr_mins_gt_140 = sum(seconds[bpm >= 140], na.rm = TRUE) / 60,
    hr_mins_gt_150 = sum(seconds[bpm >= 150], na.rm = TRUE) / 60,
    hr_mins_gt_160 = sum(seconds[bpm >= 160], na.rm = TRUE) / 60,
    hr_mins_140_150 = sum(seconds[bpm >= 140 & bpm < 150], na.rm = TRUE) / 60,
    hr_mins_150_160 = sum(seconds[bpm >= 150 & bpm < 160], na.rm = TRUE) / 60,
    hr_impulse_140 = sum(pmax(0, bpm - 140) * seconds, na.rm = TRUE) / 60,
    hr_impulse_150 = sum(pmax(0, bpm - 150) * seconds, na.rm = TRUE) / 60,
    hr_impulse_160 = sum(pmax(0, bpm - 160) * seconds, na.rm = TRUE) / 60,
    hr_strain_140_sq = sum((pmax(0, bpm - 140)^2) * seconds, na.rm = TRUE) / 6000,
    hr_strain_150_sq = sum((pmax(0, bpm - 150)^2) * seconds, na.rm = TRUE) / 6000,
    hr_strain_160_sq = sum((pmax(0, bpm - 160)^2) * seconds, na.rm = TRUE) / 6000,
    net_cardiac_impulse = sum(pmax(0, bpm - rhr) * seconds, na.rm = TRUE) / 3600,
    homeostatic_strain  = sum(((bpm - rhr)^2) * seconds, na.rm = TRUE) / 360000,
    .groups = "drop"
  ) |>
  mutate(peak_hr = if_else(is.finite(peak_hr), peak_hr, NA_real_)) |>
  rename(date = start_day)

# --- 5. Per-workout HR features (continuous-time substrate) -------------------
if (have_activity_hr) {
  message("   🏋️  Building per-workout HR features (activity-level)...")
  workout_hr_hist <- intervals_hr_activity |>
    transmute(workout_id = activity_id, start_time, end_time, bpm, seconds)
  
  workout_events <- workout_hr_hist |>
    group_by(workout_id, start_time, end_time) |>
    summarise(
      peak_hr        = max(bpm[seconds >= params$peak_min_secs], -Inf, na.rm = TRUE),
      hr_mins_gt_140 = sum(seconds[bpm >= 140], na.rm = TRUE) / 60,
      hr_mins_gt_150 = sum(seconds[bpm >= 150], na.rm = TRUE) / 60,
      hr_mins_gt_160 = sum(seconds[bpm >= 160], na.rm = TRUE) / 60,
      hr_impulse_150 = sum(pmax(0, bpm - 150) * seconds, na.rm = TRUE) / 60,
      hr_strain_150_sq = sum((pmax(0, bpm - 150)^2) * seconds, na.rm = TRUE) / 6000,
      .groups = "drop"
    ) |>
    mutate(peak_hr = if_else(is.finite(peak_hr), peak_hr, NA_real_))
} else {
  message("   🏋️  Building per-workout HR features (daily-attributed fallback)...")
  main_workout <- intervals_workouts |>
    group_by(start_day) |>
    slice_max(duration, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(workout_id = id, start_time, end_time, date = start_day)
  
  workout_events <- main_workout |>
    left_join(daily_hr_features, by = "date") |>
    transmute(workout_id, start_time, end_time,
              peak_hr, hr_mins_gt_140, hr_mins_gt_150, hr_mins_gt_160,
              hr_impulse_150, hr_strain_150_sq) |>
    filter(!is.na(hr_mins_gt_150))
  workout_hr_hist <- NULL
}

# --- 6. Medication timeline ---------------------------------------------------
# NB: this is the PRESCRIBED dose. Actual adherence is imperfect; see
# `missed_med_events` (derived below) for logged missed doses.
medication_timeline <- tribble(
  ~treatment_start, ~treatment_end, ~treatment, ~treatment_dose,
  "2023-06-14", "2023-08-20", "Candesartan 8mg", 0.5,
  "2024-12-18", "2024-12-22", "Candesartan 8mg", 0.5,
  "2024-12-23", "2024-12-28", "Candesartan 8mg", 1,
  "2024-12-29", "2025-01-12", "Candesartan 8mg", 1.5,
  "2025-01-13", "2025-08-28", "Candesartan 8mg", 2,
  "2025-08-29", "2026-03-19", "Candesartan 8mg", 1.5,
  "2026-03-19", "2026-05-17", "Candesartan 8mg", 2,
  "2026-05-17", as.character(Sys.Date()), "Candesartan 8mg", 1.5,
) |>
  mutate(treatment_start = ymd(treatment_start), treatment_end = ymd(treatment_end))

# --- 7. Migraine logic: triptan administrations, onsets, episodes -------------
message("   🤕 Resolving triptan administrations, onsets, and episodes...")

# Each migraine tag = a triptan administration. A real span => start is ONSET,
# end is the triptan moment. Otherwise onset is latent (earlier than triptan).
# Each migraine tag is a triptan administration, taken AT onset essentially every
# time (~1h buildup) -> triptan ~= onset for the typical population. The only
# exceptions are a few logged spans, which are ATYPICAL (slow-onset / untreated)
# and are flagged, NOT used to calibrate a population lag. Date-spanning tags
# (NULL end_time, distinct end_day) are handled via `spans_date` from 1.R.
triptan_events <- tags |>
  filter(tag == 'migraine') |>
  arrange(start_time) |>
  mutate(
    span_hrs  = as.numeric(difftime(end_time_local, start_time_local, units = "hours")),
    real_span = !is.na(span_hrs) &
      span_hrs > params$migraine_min_span_hrs &
      span_hrs < params$migraine_max_span_hrs,
    onset_known = real_span | spans_date,        # onset = tag start in both cases
    atypical    = onset_known,                   # per Mark: every span is unrepresentative
    triptan_local = case_when(real_span  ~ end_time_local,
                              spans_date ~ end_time_local,   # imputed; see end_time_imputed
                              TRUE       ~ start_time_local),
    onset_local = if_else(onset_known, start_time_local, ymd_hms(NA_character_)),
    # Daily attribution anchor (unchanged contract): true onset if known, else tag time.
    attr_local  = coalesce(onset_local, start_time_local),
    # Continuous-time onset estimate: true onset if known, else triptan minus the
    # typical ~1h buildup. Immaterial daily; matters only at hourly resolution.
    onset_est_local = if_else(onset_known, onset_local,
                              start_time_local - hours(params$typical_buildup_hrs)),
    side = str_extract(str_to_lower(coalesce(comment, "")), "left|right|bilateral")
  )

# The few atypical spans — kept for inspection; EXCLUDE or model as a separate
# class in 5c. NB: outliers, NOT a lag-calibration sample.
atypical_migraines <- triptan_events |>
  filter(atypical) |>
  select(start_time_local, end_time_local, span_hrs, spans_date, end_time_imputed, side, comment)
message(sprintf("      > Atypical spanned migraines: %d (treated as outliers, not a lag prior)",
                nrow(atypical_migraines)))

# Collapse re-doses into episodes (the unit of a "new attack").
migraine_episodes <- triptan_events |>
  arrange(triptan_local) |>
  mutate(
    gap_hrs = as.numeric(difftime(triptan_local, lag(triptan_local), units = "hours")),
    new_episode = is.na(gap_hrs) | gap_hrs > params$episode_merge_hrs,
    episode_id = cumsum(new_episode)
  ) |>
  group_by(episode_id) |>
  summarise(
    n_triptans      = n(),
    multi_dose      = n() > 1,
    onset_known     = any(onset_known),
    any_atypical    = any(atypical),
    triptan_first_local = min(triptan_local),
    triptan_last_local  = max(triptan_local),
    onset_est_local = min(onset_est_local),
    .groups = "drop"
  ) |>
  arrange(onset_est_local) |>
  mutate(
    prev_onset = lag(onset_est_local),
    episode_interval_hrs = as.numeric(difftime(onset_est_local, prev_onset, units = "hours")),
    date = as.Date(floor_date(onset_est_local - hours(params$day_transition_hrs), unit = "day"))
  )

# Daily target: a migraine day = any triptan administration attributed to it.
triptan_attr <- triptan_events |>
  mutate(date = as.Date(floor_date(attr_local - hours(params$day_transition_hrs), unit = "day")))

migraine_days <- triptan_attr |>
  group_by(date) |>
  summarise(
    migraine_first = min(start_time),
    migraine_n     = n(),                              # triptan administrations
    .groups = "drop"
  ) |>
  left_join(
    migraine_episodes |> count(date, name = "migraine_episodes_n"),
    by = "date"
  ) |>
  mutate(migraine_episodes_n = replace_na(migraine_episodes_n, 0L))

# Missed-medication events (adherence signal; not folded into dose timeline).
missed_med_events <- tags |>
  filter(tag %in% tag_sets$missed_med) |>
  transmute(event_time = start_time, event_time_local = start_time_local,
            event_type = "missed_medication", magnitude = 1)

# --- 8. Alcohol & EBAC --------------------------------------------------------
message("   🍺 Calculating Alcohol & EBAC...")

drinks_raw <- tags |>
  filter(tag == 'alc_drink') |>
  arrange(start_time) |>
  mutate(
    explicit_amount = extract_amount(comment),
    explicit_amount = if_else(explicit_amount == 0, NA_real_, explicit_amount),
    type_clean = str_to_lower(tag_type)
  )

modes <- drinks_raw |>
  filter(!is.na(explicit_amount)) |>
  group_by(type_clean) |>
  summarise(modal_amount = as.numeric(names(sort(table(explicit_amount), decreasing = TRUE)[1])),
            .groups = "drop")

message("      > Inferred amount defaults from history:")
modes |>
  mutate(msg = sprintf("        - %s: %.1f amount", type_clean, modal_amount)) |>
  pull(msg) |> walk(message)

drinks_enriched <- drinks_raw |>
  left_join(modes, by = "type_clean") |>
  mutate(
    final_drink_amount = coalesce(explicit_amount, modal_amount, params$default_amount),
    drink_last = lag(start_time),
    time_diff  = as.numeric(difftime(start_time, drink_last, units = "hours")),
    drink_series = cumsum(is.na(time_diff) | time_diff > params$drink_series_hrs),
    explicit_span_hrs = as.numeric(difftime(end_time_local, start_time_local, units = "hours")),
    explicit_span_hrs = if_else(explicit_span_hrs > params$min_span_hrs &
                                  explicit_span_hrs < params$max_span_hrs,
                                explicit_span_hrs, NA_real_)
  ) |>
  group_by(drink_series) |>
  mutate(
    next_start_time   = lead(start_time),
    gap_to_next       = as.numeric(difftime(next_start_time, start_time, units = "hours")),
    prev_gap          = time_diff,
    same_as_prev      = type_clean == lag(type_clean),
    is_last_in_series = is.na(next_start_time),
    duration_hrs = case_when(
      !is.na(explicit_span_hrs)                                            ~ explicit_span_hrs,
      !is.na(gap_to_next)                                                  ~ gap_to_next,
      is_last_in_series & coalesce(same_as_prev, FALSE) & !is.na(prev_gap) ~ prev_gap,
      TRUE ~ final_drink_amount * params$default_consumption_time
    ),
    duration_hrs = pmax(0.17, duration_hrs)
  ) |>
  ungroup()

alcohol_sessions <- drinks_enriched |>
  group_by(drink_series) |>
  arrange(start_time, .by_group = TRUE) |>
  summarise(
    drink_first       = min(start_time),
    drink_last        = max(start_time),
    drink_first_local = min(start_time_local),
    drink_last_local  = max(start_time_local),
    drinks_amount_sum = sum(final_drink_amount),
    drinks_count      = n(),
    session_span_hrs  = pmax(1, as.numeric(difftime(max(start_time_local),
                                                    min(start_time_local), units = "hours"))),
    alcohol_grams_total = drinks_amount_sum * params$grams_per_drink_amount,
    sim = list(simulate_bac_path(
      start_hrs = as.numeric(difftime(start_time, min(start_time), units = "hours")),
      dur_hrs   = duration_hrs,
      amounts   = final_drink_amount)),
    any_wine = max(type_clean == 'wine'),
    any_beer = max(type_clean == 'beer'),
    .groups = "drop"
  ) |>
  mutate(
    peak_ebac       = map_dbl(sim, "peak_bac"),
    peak_bac_rate   = map_dbl(sim, "peak_rate"),
    peak_time_hrs   = map_dbl(sim, "peak_time"),
    peak_time_local = drink_first_local + dhours(peak_time_hrs),
    peak_ebac_proxy = pmax(0, alcohol_grams_total / params$widmark_factor -
                             session_span_hrs * 0.015)
  ) |>
  select(-sim)

drink_days <- alcohol_sessions |>
  mutate(date = as.Date(floor_date(drink_first_local, unit = "day"))) |>
  group_by(date) |>
  summarise(
    drinks_count          = sum(drinks_count),
    drinks_amount         = sum(drinks_amount_sum),
    drink_sessions        = n(),
    peak_ebac_daily       = max(peak_ebac),
    peak_ebac_proxy_daily = max(peak_ebac_proxy),
    peak_bac_rate_daily   = max(peak_bac_rate),
    drink_any_wine        = max(any_wine),
    drink_any_beer        = max(any_beer),
    .groups = "drop"
  )

# --- 9. Other events: impulse triggers + altitude STATE blocks ----------------
event_days <- tags |>
  group_by(date = start_day) |>
  summarise(
    extra_coffee     = max(tag %in% tag_sets$coffee) == 1,
    any_stress       = max(tag %in% tag_sets$stress) == 1,
    any_travel       = max(tag %in% tag_sets$travel) == 1,
    any_plane        = max(tag %in% tag_sets$plane) == 1,
    any_highaltitude = max(tag %in% tag_sets$highalt) == 1,
    any_lowpressure  = max(tag %in% tag_sets$lowpress) == 1,
    any_hydration    = max(tag %in% tag_sets$hydration) == 1,
    .groups = "drop"
  )

# Impulse-type triggers (point-ish events). Altitude handled separately as state.
trigger_events <- tags |>
  mutate(trigger = case_when(
    tag %in% tag_sets$plane     ~ "plane",
    tag %in% tag_sets$coffee    ~ "coffee",
    tag %in% tag_sets$stress    ~ "stress",
    tag %in% tag_sets$travel    ~ "travel",
    tag %in% tag_sets$hydration ~ "hydration",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(trigger)) |>
  transmute(event_time = start_time, event_time_local = start_time_local,
            event_type = trigger, magnitude = 1) |>
  bind_rows(missed_med_events)

# Sustained altitude exposure (skiing holidays) as start/end blocks.
alt_tags <- tags |> filter(tag %in% tag_sets$highalt)
altitude_blocks <- make_blocks(alt_tags$start_time_local, alt_tags$end_time_local,
                               params$altitude_merge_hrs)
message(sprintf("      > Altitude blocks (ski holidays) detected: %d", nrow(altitude_blocks)))

# --- 10. Master daily join (analysis_df) — existing-model contract ------------
spine <- tibble(date = seq.Date(min(readiness$start_day), max(readiness$start_day), by = "day"))
migraine_before_data <- as.Date("2022-11-02")

analysis_df <- spine |>
  left_join(migraine_days, by = "date") |>
  mutate(
    is_weekend  = wday(date, week_start = 1) >= 6,
    is_saturday = wday(date, week_start = 1) == 6,
    is_sunday   = wday(date, week_start = 1) == 7,
    weekday     = wday(date, week_start = 1, label = TRUE)
  ) |>
  left_join(
    migraine_days |> transmute(prev_migraine = date) |> distinct(),
    by = join_by(closest(date > prev_migraine))
  ) |>
  mutate(
    prev_migraine = coalesce(prev_migraine, migraine_before_data),
    days_since_last = as.numeric(difftime(date, prev_migraine, units = "days")),
    weeks_since_last = days_since_last / 7,
    weeks_since_last_max1 = pmin(1, weeks_since_last),
    migraine_yesterday = coalesce(days_since_last == 1, FALSE)
  ) |>
  left_join(medication_timeline, by = join_by(between(date, treatment_start, treatment_end))) |>
  mutate(any_medication = !is.na(treatment)) |>
  rename(medication_dose = treatment_dose) |>
  select(-starts_with('treatment')) |>
  left_join(drink_days, by = "date") |>
  left_join(event_days, by = "date") |>
  left_join(readiness |> select(start_day, readiness_score), by = join_by(date == start_day)) |>
  left_join(sleep |> select(start_day, sleep_score, total_sleep = contributors.total_sleep),
            by = join_by(date == start_day)) |>
  left_join(daily_activity_harmonised, by = "date") |>
  left_join(daily_hr_features, by = "date") |>
  mutate(
    across(c(migraine_n, migraine_episodes_n, drink_any_wine, drink_any_beer,
             drinks_count, drinks_amount, drink_sessions,
             peak_ebac_daily, peak_ebac_proxy_daily, peak_bac_rate_daily,
             total_duration_mins, total_active_cals,
             sport_duration_mins, life_duration_mins, activity_duration_mins,
             net_cardiac_impulse, homeostatic_strain, medication_dose,
             starts_with("hr_")),
           ~replace_na(., 0)),
    across(c(has_tracked_sport), ~replace_na(., FALSE)),
    across(starts_with('any_'),   ~replace_na(., FALSE)),
    across(starts_with('extra_'), ~replace_na(., FALSE)),
    across(ends_with('_score'),   ~replace_na(., as.integer(mean(., na.rm = TRUE))))
  ) |>
  mutate(
    badsleep_d    = 0.1 * (mean(sleep_score) - sleep_score),
    totalsleep_d  = 0.1 * (mean(total_sleep) - total_sleep),
    unreadiness_d = 0.1 * (mean(readiness_score) - readiness_score),
    migraine = (migraine_n >= 1)
  )

write_rds(analysis_df, "data/analysis_ready.rds")
message(sprintf("✔ Daily artifact: %d rows -> data/analysis_ready.rds", nrow(analysis_df)))

# --- 11. Continuous-time substrate (for future 5c) ----------------------------
message("   ⏱  Assembling continuous-time substrate...")

# 11a. Unified long exposure-event table (one primary magnitude per row).
#      Anchors:  workout -> end_time ; alcohol -> simulated BAC peak time.
exposure_events <- bind_rows(
  workout_events |>
    transmute(event_time = end_time, event_time_local = end_time,  # intervals ts are local-naive
              event_type = "workout", magnitude = hr_mins_gt_150,
              peak_hr, hr_impulse_150, workout_id),
  alcohol_sessions |>
    transmute(event_time = drink_first, event_time_local = peak_time_local,
              event_type = "alcohol", magnitude = peak_ebac,
              peak_bac_rate, drinks_amount = drinks_amount_sum, drink_series),
  trigger_events
) |>
  arrange(event_time_local)

# 11b. Hourly hazard spine -----------------------------------------------------
# migraine_onset fires on EPISODE onset (not every triptan re-dose). Exposures
# are impulse-coded at their hour for kernel convolution; altitude is a STATE.
grid_start <- floor_date(min(readiness$start_day), "day")
grid_end   <- ceiling_date(max(readiness$start_day), "day")
hourly_spine <- tibble(t_local = seq(as_datetime(grid_start), as_datetime(grid_end), by = "1 hour"))
floor_h <- function(x) floor_date(x, "hour")

mig_hour <- migraine_episodes |>
  mutate(t_local = floor_h(onset_est_local)) |>
  group_by(t_local) |>
  summarise(migraine_onset = n(),
            onset_atypical = sum(any_atypical),   # slow-onset/untreated outliers
            .groups = "drop")

exp_workout <- exposure_events |>
  filter(event_type == "workout") |>
  transmute(t_local = floor_h(event_time_local),
            exp_wkt_mins150 = magnitude, exp_wkt_impulse150 = hr_impulse_150,
            exp_wkt_peakhr = peak_hr) |>
  group_by(t_local) |>
  summarise(exp_wkt_mins150 = sum(exp_wkt_mins150, na.rm = TRUE),
            exp_wkt_impulse150 = sum(exp_wkt_impulse150, na.rm = TRUE),
            exp_wkt_peakhr = max(exp_wkt_peakhr, na.rm = TRUE), .groups = "drop") |>
  mutate(exp_wkt_peakhr = if_else(is.finite(exp_wkt_peakhr), exp_wkt_peakhr, 0))

exp_alc <- exposure_events |>
  filter(event_type == "alcohol") |>
  transmute(t_local = floor_h(event_time_local),
            exp_alc_peakebac = magnitude, exp_alc_peakrate = peak_bac_rate,
            exp_alc_units = drinks_amount) |>
  group_by(t_local) |>
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop")

exp_trig <- exposure_events |>
  filter(event_type %in% c("plane", "coffee", "stress", "hydration", "missed_medication")) |>
  transmute(t_local = floor_h(event_time_local), event_type, magnitude) |>
  group_by(t_local, event_type) |>
  summarise(magnitude = sum(magnitude), .groups = "drop") |>
  pivot_wider(names_from = event_type, names_prefix = "exp_",
              values_from = magnitude, values_fill = 0)

onset_lookup <- migraine_episodes |> transmute(prev_onset = onset_est_local) |> distinct()

hourly_df <- hourly_spine |>
  left_join(mig_hour,    by = "t_local") |>
  left_join(exp_workout, by = "t_local") |>
  left_join(exp_alc,     by = "t_local") |>
  left_join(exp_trig,    by = "t_local") |>
  mutate(
    migraine_onset  = replace_na(migraine_onset, 0L),
    onset_atypical  = replace_na(onset_atypical, 0L),
    across(starts_with("exp_"), ~replace_na(.x, 0)),
    state_highaltitude = in_any_block(t_local, altitude_blocks),
    date = as.Date(t_local),
    hour = hour(t_local),
    dow  = wday(t_local, week_start = 1, label = TRUE),
    is_weekend = wday(t_local, week_start = 1) >= 6,
    circ_sin = sin(2 * pi * hour / 24),
    circ_cos = cos(2 * pi * hour / 24),
    medication_dose = med_dose_at(date, medication_timeline)
  ) |>
  left_join(onset_lookup, by = join_by(closest(t_local > prev_onset))) |>
  mutate(
    prev_onset = coalesce(prev_onset, as_datetime(migraine_before_data)),
    hours_since_last_migraine = as.numeric(difftime(t_local, prev_onset, units = "hours"))
  ) |>
  select(-prev_onset)

# --- 12. Save continuous substrate --------------------------------------------
# FUTURE (pre-Oura extension, NOT built): you have ~10y of workout + migraine/
# triptan times (calendar) and HR inside Heart Graph. To extend, ingest those
# into the SAME schemas (workout_events, triptan_events/migraine_episodes) and
# union; every Oura-era covariate (alcohol, sleep, readiness) becomes NA before
# the Oura start, so 5c must allow type-specific missingness / an era indicator.
continuous_substrate <- list(
  triptan_events      = triptan_events,     # every administration (onset known iff spanned)
  migraine_episodes   = migraine_episodes,  # collapsed attacks (re-doses merged)
  atypical_migraines  = atypical_migraines, # spanned outliers (exclude/separate-class in 5c)
  exposure_events     = exposure_events,
  workout_events      = workout_events,
  workout_hr_hist     = workout_hr_hist,    # NULL if per-activity HR unavailable
  alcohol_sessions    = alcohol_sessions,
  trigger_events      = trigger_events,
  altitude_blocks     = altitude_blocks,    # sustained-state exposures
  missed_med_events   = missed_med_events,
  medication_timeline = medication_timeline,
  hourly_df           = hourly_df,
  params              = params
)
write_rds(continuous_substrate, "data/continuous_substrate.rds")

message(sprintf("✔ Continuous substrate -> data/continuous_substrate.rds"))
message(sprintf("   - triptan_events: %d | episodes: %d (%d multi-dose) | hourly_df: %d rows",
                nrow(triptan_events), nrow(migraine_episodes),
                sum(migraine_episodes$multi_dose), nrow(hourly_df)))
message(sprintf("   - workout_hr_hist: %s",
                if (is.null(workout_hr_hist)) "absent (re-ingest 2.R for fitted-threshold support)"
                else sprintf("%d rows", nrow(workout_hr_hist))))
message("✔ Data prep complete.")
# 3c_alcohol_features.R
# ==========================================================================
# Alcohol session detection, EBAC simulation, and daily drink features.
#
# GENERIC — should work for any user without modification provided their
# Oura tags include drink events. Tag vocabulary is configured in config.R.
#
# ▶ SKIPPABLE: if you do not log alcohol in Oura tags, skip this script and
#   remove drink_days from the spine join in 3f_assemble.R.
#
# EBAC model: Widmark equation with grid-integrated BAC trajectory.
# Parameters (body_weight_kg, widmark_r, elimination_rate, etc.) in config.R.
#
# Requires:
#   data/oura_tidy.rds   — from 1_oura_ingest.R
#   config.R
#
# Produces:
#   data/alcohol_sessions.rds  — one row per drinking session with EBAC sim
#   data/drink_days.rds        — one row per day, aggregated drink features
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)
library(readr)

message("🍺 Starting alcohol feature engineering...")

# --- 1. Load ------------------------------------------------------------------
oura_data <- read_rds("data/oura_tidy.rds")
tags      <- oura_data$oura_tags

# Tolerate older ingest format
if (!"spans_date"       %in% names(tags)) tags$spans_date       <- FALSE
if (!"end_time_imputed" %in% names(tags)) tags$end_time_imputed <- FALSE

# --- 2. Helper: Widmark EBAC trajectory ---------------------------------------
# Returns peak BAC, peak positive absorption rate, and time-of-peak (hrs).
# Inputs: vectors of drink start times (hrs from session start), drink durations,
#         and drink amounts (in your tag unit, converted via grams_per_drink_amount).
simulate_bac_path <- function(start_hrs, dur_hrs, amounts, dt = 1/60) {
  if (length(amounts) == 0) return(c(peak_bac = 0, peak_rate = 0, peak_time = 0))
  ord <- order(start_hrs)
  start_hrs <- start_hrs[ord]; dur_hrs <- dur_hrs[ord]; amounts <- amounts[ord]
  t <- seq(0, max(start_hrs + dur_hrs) + 3, by = dt)
  grams  <- grams_per_drink_amount * amounts
  ingest <- numeric(length(t))
  for (i in seq_along(amounts)) {
    active <- t >= start_hrs[i] & t < (start_hrs[i] + dur_hrs[i])
    ingest[active] <- ingest[active] + grams[i] / dur_hrs[i]
  }
  rate <- pmax(0, ingest / widmark_factor - elimination_rate)
  bac  <- numeric(length(t))
  for (k in 2:length(t))
    bac[k] <- max(0, bac[k-1] + (ingest[k-1] / widmark_factor - elimination_rate) * dt)
  c(peak_bac = max(bac), peak_rate = max(rate), peak_time = t[which.max(bac)])
}

extract_amount <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  suppressWarnings(readr::parse_number(as.character(x)))
}

# --- 3. Filter and enrich drink tags ------------------------------------------
drinks_raw <- tags |>
  filter(tag == "alc_drink") |>
  arrange(start_time) |>
  mutate(
    explicit_amount = extract_amount(comment),
    explicit_amount = if_else(explicit_amount == 0, NA_real_, explicit_amount),
    type_clean      = str_to_lower(tag_type)
  )

if (nrow(drinks_raw) == 0) {
  message("   ⚠ No alcohol tags found. Saving empty artifacts.")
  write_rds(tibble(), "data/alcohol_sessions.rds")
  write_rds(tibble(), "data/drink_days.rds")
  message("✔ Alcohol features complete (no data).")
  stop("No alcohol tags — skipping remainder of script.", call. = FALSE)
}

# Infer modal amount per drink type from logged history
modes <- drinks_raw |>
  filter(!is.na(explicit_amount)) |>
  group_by(type_clean) |>
  summarise(
    modal_amount = as.numeric(names(sort(table(explicit_amount), decreasing = TRUE)[1])),
    .groups = "drop"
  )

message("   > Inferred amount defaults from history:")
modes |>
  mutate(msg = sprintf("       %s: %.1f units", type_clean, modal_amount)) |>
  pull(msg) |> walk(message)

drinks_enriched <- drinks_raw |>
  left_join(modes, by = "type_clean") |>
  mutate(
    final_drink_amount = coalesce(explicit_amount, modal_amount, default_amount),
    drink_last         = lag(start_time),
    time_diff          = as.numeric(difftime(start_time, drink_last, units = "hours")),
    drink_series       = cumsum(is.na(time_diff) | time_diff > drink_series_hrs),
    explicit_span_hrs  = as.numeric(difftime(end_time_local, start_time_local, units = "hours")),
    explicit_span_hrs  = if_else(
      explicit_span_hrs > min_span_hrs & explicit_span_hrs < max_span_hrs,
      explicit_span_hrs, NA_real_
    )
  ) |>
  group_by(drink_series) |>
  mutate(
    next_start_time   = lead(start_time),
    gap_to_next       = as.numeric(difftime(next_start_time, start_time, units = "hours")),
    prev_gap          = time_diff,
    same_as_prev      = type_clean == lag(type_clean),
    is_last_in_series = is.na(next_start_time),
    duration_hrs = case_when(
      !is.na(explicit_span_hrs)                                             ~ explicit_span_hrs,
      !is.na(gap_to_next)                                                   ~ gap_to_next,
      is_last_in_series & coalesce(same_as_prev, FALSE) & !is.na(prev_gap) ~ prev_gap,
      TRUE ~ final_drink_amount * default_consumption_time
    ),
    duration_hrs = pmax(0.17, duration_hrs)
  ) |>
  ungroup()

# --- 4. Session aggregation + EBAC simulation ---------------------------------
alcohol_sessions <- drinks_enriched |>
  group_by(drink_series) |>
  arrange(start_time, .by_group = TRUE) |>
  summarise(
    drink_first         = min(start_time),
    drink_last          = max(start_time),
    drink_first_local   = min(start_time_local),
    drink_last_local    = max(start_time_local),
    drinks_amount_sum   = sum(final_drink_amount),
    drinks_count        = n(),
    session_span_hrs    = pmax(1, as.numeric(difftime(
      max(start_time_local), min(start_time_local), units = "hours"))),
    alcohol_grams_total = drinks_amount_sum * grams_per_drink_amount,
    sim = list(simulate_bac_path(
      start_hrs = as.numeric(difftime(start_time, min(start_time), units = "hours")),
      dur_hrs   = duration_hrs,
      amounts   = final_drink_amount
    )),
    any_wine = any(type_clean == "wine"),
    any_beer = any(type_clean == "beer"),
    .groups = "drop"
  ) |>
  mutate(
    peak_ebac       = map_dbl(sim, "peak_bac"),
    peak_bac_rate   = map_dbl(sim, "peak_rate"),
    peak_time_hrs   = map_dbl(sim, "peak_time"),
    peak_time_local = drink_first_local + dhours(peak_time_hrs),
    peak_ebac_proxy = pmax(0, alcohol_grams_total / widmark_factor -
                             session_span_hrs * elimination_rate)
  ) |>
  select(-sim)

# --- 5. Daily aggregation -----------------------------------------------------
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
    drink_any_wine        = any(any_wine),
    drink_any_beer        = any(any_beer),
    .groups = "drop"
  )

# --- 6. Save ------------------------------------------------------------------
write_rds(alcohol_sessions, "data/alcohol_sessions.rds")
write_rds(drink_days,       "data/drink_days.rds")

message("✔ Alcohol features complete.")
message(sprintf("   - %d sessions | %d drink-days -> data/alcohol_sessions.rds, data/drink_days.rds",
                nrow(alcohol_sessions), nrow(drink_days)))


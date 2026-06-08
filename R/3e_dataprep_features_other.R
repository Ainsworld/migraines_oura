# 3e_other_features.R
# ==========================================================================
# Tag-based features: coffee, stress, travel, altitude, hydration.
# Also builds altitude exposure blocks (sustained-state, not impulse).
#
# PARTIALLY GENERIC:
#   - The feature engineering pattern is generic
#   - Tag vocabulary is personal — configured in config.R (tag_sets)
#   ▶ Review tag_sets in config.R and add/remove tags to match your own usage
#
# ▶ SKIPPABLE sections (comment out if not relevant):
#   - Altitude blocks: only relevant if you log high-altitude exposure
#   - Any tag_set whose tags you don't use
#
# Requires:
#   data/oura_tidy.rds   — from 1_oura_ingest.R
#   config.R
#
# Produces:
#   data/other_features.rds  — list containing:
#       event_days, trigger_events, altitude_blocks
# ==========================================================================

source("R/config.R")
library(tidyverse)
library(lubridate)

message("🏷️  Starting other tag features...")

# --- 1. Load ------------------------------------------------------------------
oura_data <- read_rds("data/oura_tidy.rds")
tags      <- oura_data$oura_tags

if (!"spans_date"       %in% names(tags)) tags$spans_date       <- FALSE
if (!"end_time_imputed" %in% names(tags)) tags$end_time_imputed <- FALSE

# --- 2. Helper: merge timestamped events into blocks --------------------------
# Used for sustained-state exposures (altitude, multi-day states).
# Successive tags within merge_hrs of each other are merged into one block.
make_blocks <- function(starts, ends, merge_hrs) {
  if (length(starts) == 0)
    return(tibble(block_id    = integer(),
                  block_start = as.POSIXct(character()),
                  block_end   = as.POSIXct(character()),
                  n_tags      = integer()))
  ord     <- order(starts)
  starts  <- starts[ord]
  ends    <- coalesce(ends[ord], starts)
  gap     <- as.numeric(difftime(starts, lag(ends), units = "hours"))
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

# --- 3. Daily tag summary (binary flags per day) ------------------------------
# Extend or reduce this list by editing tag_sets in config.R.
event_days <- tags |>
  group_by(date = start_day) |>
  summarise(
    extra_coffee     = any(tag %in% tag_sets$coffee),
    any_stress       = any(tag %in% tag_sets$stress),
    any_travel       = any(tag %in% tag_sets$travel),
    any_plane        = any(tag %in% tag_sets$plane),
    any_highaltitude = any(tag %in% tag_sets$highalt),
    any_lowpressure  = any(tag %in% tag_sets$lowpress),
    any_hydration    = any(tag %in% tag_sets$hydration),
    .groups = "drop"
  )

# --- 4. Impulse-type trigger events (timestamped, for continuous-time model) --
# Missed medication events are added here from migraine_features if available;
# otherwise omitted.
trigger_events <- tags |>
  mutate(trigger = case_when(
    tag %in% tag_sets$plane     ~ "plane",
    tag %in% tag_sets$coffee    ~ "coffee",
    tag %in% tag_sets$stress    ~ "stress",
    tag %in% tag_sets$travel    ~ "travel",
    tag %in% tag_sets$hydration ~ "hydration",
    TRUE                         ~ NA_character_
  )) |>
  filter(!is.na(trigger)) |>
  transmute(
    event_time       = start_time,
    event_time_local = start_time_local,
    event_type       = trigger,
    magnitude        = 1
  )

# Append missed medication events if migraine features have been run
mig_path <- "data/migraine_features.rds"
if (file.exists(mig_path)) {
  missed_med_events <- read_rds(mig_path)$missed_med_events
  trigger_events    <- bind_rows(trigger_events, missed_med_events)
  message("   ✔ Missed medication events appended from migraine_features.rds")
} else {
  message("   ℹ migraine_features.rds not found; missed_med events omitted.")
}

# --- 5. Altitude blocks (sustained state) -------------------------------------
# ▶ SKIPPABLE: remove this section if you don't log high-altitude exposure
alt_tags <- tags |> filter(tag %in% tag_sets$highalt)
altitude_blocks <- make_blocks(alt_tags$start_time_local, alt_tags$end_time_local,
                               altitude_merge_hrs)
message(sprintf("   > Altitude blocks detected: %d", nrow(altitude_blocks)))

# --- 6. Save ------------------------------------------------------------------
other_features <- list(
  event_days      = event_days,
  trigger_events  = trigger_events,
  altitude_blocks = altitude_blocks,
  # Pass helpers through so 3f_assemble.R can use them without re-sourcing
  in_any_block    = in_any_block,
  make_blocks     = make_blocks
)

write_rds(other_features, "data/other_features.rds")
message("✔ Other features complete -> data/other_features.rds")
message(sprintf("   - %d event-days | %d trigger events | %d altitude blocks",
                nrow(event_days), nrow(trigger_events), nrow(altitude_blocks)))


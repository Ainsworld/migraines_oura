source("R/api_clients.R")
library(tidyverse)
library(lubridate)
library(purrr)

# --- Setup --------------------------------------------------------------------
first_date <- as.Date("2022-11-03") 
last_date  <- Sys.Date()

message(sprintf("🚀 Starting Intervals Ingest (%s to %s)", first_date, last_date))

# --- 1. Fetch Activity List ---------------------------------------------------
intervals <- IntervalsClient$new()

# Get the index of all activities
raw_activities <- intervals$get_activities(
  oldest_iso = as.character(first_date), 
  newest_iso = as.character(last_date)
)

message(sprintf("   ✔ Index fetched: %d activities found.", nrow(raw_activities)))

# --- 2. Create 'Strava-Like' Workouts Table -----------------------------------
# Goal: Complement the Oura 'workouts' table.
# We map Intervals fields to match the Oura schema you provided.

intervals_workouts <- raw_activities |>
  mutate(
    start_time = ymd_hms(start_date_local),
    end_time = start_time + seconds(moving_time),
    start_day = as.Date(start_time)
  ) |>
  select(
    # Core Keys
    id,
    start_time,
    end_time,
    start_day,
    
    # Dimensions (Renamed to match Oura 'workouts' glimpse where possible)
    activity_type = type,
    duration = moving_time,    # Intervals uses seconds, same as Oura
    distance = distance,
    calories = calories,
    
    # Metrics specific to Training
    average_hr = average_heartrate,
    max_hr = max_heartrate,
    average_watts = average_watts,
    norm_power = normalized_power,
    training_load = icu_training_load,
    intensity = intensity # e.g. "85" (score) or zone label
  ) |>
  mutate(source = "Intervals.icu") |>
  as_tibble()

# --- 3. Fetch Granular HR Distributions ---------------------------------------
# Goal: Test "HR Stress" hypothesis by getting exact minutes at specific HRs.
# We fetch 'time-at-hr' which gives seconds spent in every HR bucket.

message("   ⬇ Fetching HR distributions (this may take a moment)...")

# Filter to only activities that actually HAVE heart rate data
hr_activities <- intervals_workouts |> 
  filter(!is.na(average_hr), average_hr > 0)

# Safe fetcher function
fetch_hr_dist <- function(id) {
  tryCatch({
    # Add small delay to respect API rate limits during bulk fetch
    Sys.sleep(0.1) 
    intervals$get_time_at_hr(id)
  }, error = function(e) return(NULL))
}

# Iterate and bind
# This creates a long table: Activity ID | HR Bucket | Seconds
hr_dist_raw <- hr_activities$id |>
  map_df(function(id) {
    res <- fetch_hr_dist(id)
    if (is.null(res)) return(NULL)
    
    # Intervals returns a list of buckets. We bind them and tag with ID.
    as_tibble(res) |> mutate(activity_id = id)
  })

# --- 4. Transform to Daily HR Distributions -----------------------------------
# Aggregating up to the Day level for the Migraine Model.
# We create a 'Histogram' per day.

daily_hr_dist <- hr_dist_raw |>
  left_join(select(intervals_workouts, id, start_day), by = c("activity_id" = "id")) |>
  # Intervals buckets are often 1-bpm width. We can bin them if needed later.
  # For now, we keep the raw "HR -> Seconds" mapping.
  group_by(start_day, bpm) |>
  summarise(seconds = sum(secs, na.rm = TRUE), .groups = "drop") |>
  arrange(start_day, bpm)

# --- Save Artifacts -----------------------------------------------------------
write_rds(intervals_workouts, "data/intervals_workouts.rds")
write_rds(daily_hr_dist, "data/intervals_hr_dist.rds")

message(sprintf("✔ Intervals ingest complete."))
message(sprintf("   - Workouts: %d saved to data/intervals_workouts.rds", nrow(intervals_workouts)))
message(sprintf("   - HR Data:  %d daily records saved to data/intervals_hr_dist.rds", nrow(daily_hr_dist)))
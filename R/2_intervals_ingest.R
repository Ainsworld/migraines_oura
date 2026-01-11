source("R/api_clients.R")
library(tidyverse)
library(lubridate)
library(purrr)

# --- Setup --------------------------------------------------------------------
first_date <- as.Date("2022-11-03") 
last_date  <- Sys.Date() - 1

message(sprintf("🚀 Starting Intervals Ingest (%s to %s)", first_date, last_date))

# --- 1. Fetch Activity List ---------------------------------------------------
intervals <- IntervalsClient$new()

# Get the index of all activities
raw_activities <- intervals$get_activities(
  oldest_iso = as.character(first_date), 
  newest_iso = as.character(last_date)
)

message(sprintf("   ✔ Index fetched: %d activities found.", nrow(raw_activities)))

# --- 2. Clean & Normalize Workouts --------------------------------------------
intervals_workouts <- raw_activities |>
  # Filter: Keep only valid activities with duration
  filter(!is.na(moving_time), moving_time > 0) |>
  mutate(
    start_time = ymd_hms(start_date_local),
    end_time = start_time + seconds(moving_time),
    start_day = as.Date(start_time),
    type_clean = str_to_lower(type) 
  ) |>
  select(
    # --- Core Identifiers ---
    id, 
    start_time, 
    end_time, 
    start_day, 
    activity_type = type_clean,
    source = source,
    label = name, # Useful for manual spot-checking (e.g. "Morning Ride")
    
    # --- Key Metrics (Used in Model) ---
    duration = moving_time, 
    distance = distance,
    calories = calories,
    
    # --- Physiological Load ---
    average_hr = average_heartrate,
    max_hr = max_heartrate,          # Added: Useful for checking intensity limits
    
    # --- Power Data (Cycling Specific) ---
    # Using 'icu_' prefixes as they are often more reliable in the export
    average_watts = icu_average_watts,
    norm_power = icu_weighted_avg_watts, # 'Normalized Power' equivalent
    
    # --- Training Load Calculations ---
    icu_training_load = icu_training_load,
    icu_intensity = icu_intensity        # Internal intensity score
    
    # --- NOTES ON UNUSED FIELDS (Future Me) ---
    # 1. Power Curves (icu_pm_*, icu_rolling_*): Detailed CP/W' models. Overkill for daily aggregation.
    # 2. Weather (average_temp, wind_*): Available but inconsistent across devices/indoors.
    # 3. Gear (gear_*): Bike/Shoe tracking. Not relevant for physio stress.
    # 4. Streams/Bytes (skyline_*, stream_types): Binary blob data for charts.
    # 5. RPE/Feel: Subjective data often missing or inconsistent.
  ) |>
  mutate(
    duration = as.difftime(duration, units = "secs")
  ) |>
  as_tibble()

message(sprintf("   ✔ Cleaned: %d valid activities retained.", nrow(intervals_workouts)))


# --- 3. Fetch Granular HR Distributions ---------------------------------------
# Goal: Test "HR Stress" hypothesis by getting exact minutes at specific HRs.
# We fetch 'time-at-hr' which gives seconds spent in every HR bucket.

message("   ⬇ Fetching HR distributions (this may take a moment)...")

#  Only attempt fetch for activities that actually claim to have HR
hr_candidates <- intervals_workouts |> 
  filter(!is.na(average_hr), average_hr > 0)

# Initialize Progress Bar
total_fetches <- nrow(hr_candidates)
pb <- txtProgressBar(min = 0, max = total_fetches, style = 3)
counter <- 0

fetch_hr_dist <- function(id) {
  tryCatch({
    Sys.sleep(0.1) # Set this to something nonzero like 0.2 if lots go missing
    intervals$get_time_at_hr(id)
  }, error = function(e) return(NULL))
}

hr_dist_raw <- hr_candidates$id |>
  map_df(function(id) {
    # Update progress bar
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    
    res <- fetch_hr_dist(id)
    if (is.null(res)) return(NULL)
    
    df <- as_tibble(res)
    
    # FIX: Explicitly handle the List structure (min_bpm scalar + secs vector)
    # The API returns a list like $min_bpm = 110, $secs = c(15, 5, 1...)
    # DIAGNOSIS: The distribution is REVERSED. 
    # secs[1] corresponds to the HIGHEST BPM (max_bpm).
    
    if (!is.null(res$secs) && !is.null(res$min_bpm)) {
      # Calculate the max bpm implied by the vector length
      implied_max <- res$min_bpm + length(res$secs) - 1
      
      # Construct sequence descending from Max to Min
      bpm_seq <- seq(from = implied_max, to = res$min_bpm, by = -1)
      
      tibble(
        activity_id = id,
        bpm = bpm_seq,
        seconds = as.integer(res$secs)
      )
    } else if (is.data.frame(res)) {
      # Fallback: Sometimes it might return a DF (e.g. from JSON array)
      as_tibble(res) |> mutate(activity_id = id)
    } else {
      NULL 
    }
  })

close(pb)

# --- 4. Transform to Daily HR Distributions -----------------------------------
# Aggregating up to the Day level for the Migraine Model.
# We create a 'Histogram' per day.

daily_hr_dist <- hr_dist_raw |>
  left_join(select(intervals_workouts, id, start_day), by = c("activity_id" = "id")) |>
  # Intervals buckets are often 1-bpm width. We can bin them if needed later.
  # For now, we keep the raw "HR -> Seconds" mapping.
  group_by(start_day, bpm) |>
  summarise(seconds = sum(seconds, na.rm = TRUE), .groups = "drop") |>
  arrange(start_day, bpm)

# --- Save Artifacts -----------------------------------------------------------
write_rds(intervals_workouts, "data/intervals_workouts.rds")
write_rds(daily_hr_dist, "data/intervals_hr_dist.rds")

message(sprintf("✔ Intervals ingest complete."))
message(sprintf("   - Workouts: %d saved to data/intervals_workouts.rds", nrow(intervals_workouts)))
message(sprintf("   - HR Data:  %d daily records saved to data/intervals_hr_dist.rds", 
                daily_hr_dist |> distinct(start_day) |> nrow()))

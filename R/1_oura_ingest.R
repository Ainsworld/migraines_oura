source("R/api_clients.R")
library(tidyverse)
library(lubridate)

# --- Setup --------------------------------------------------------------------
first_date <- as.Date("2022-11-03")
last_date  <- Sys.Date() - 1

message(sprintf("🚀 Starting Oura Ingest (%s to %s)", first_date, last_date))

# --- Execution ----------------------------------------------------------------
oura <- OuraClient$new()

# 1. Fetch Raw Endpoints
# ----------------------
raw_sessions  <- oura$get_endpoint("session", first_date, last_date)
raw_tags      <- oura$get_endpoint("enhanced_tag", first_date, last_date)
raw_workouts  <- oura$get_endpoint("workout", first_date, last_date)
raw_activity  <- oura$get_endpoint("daily_activity", first_date, last_date)
raw_readiness <- oura$get_endpoint("daily_readiness", first_date, last_date)
raw_sleep     <- oura$get_endpoint("daily_sleep", first_date, last_date)

# 2. Tidy & Transform (Replicating '1 - Oura API.R')
# --------------------------------------------------

# --- Tags ---
alcohol_variations <- c('alcohol','beer','wine','liquor','Homebrew')

tags <- raw_tags |>
  mutate(
    # Handle timezone offsets
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    start_time_local = ymd_hms(substr(start_time_local, 1, 19)),
    end_time_local = ymd_hms(substr(end_time_local, 1, 19)),
    start_day = ymd(start_day),
    
    # Logic: Clean tag types and normalize alcohol
    tag_type = coalesce(custom_name, tag_type_code),
    tag_type = str_remove(tag_type, "tag_generic_|tag_sleep_"),
    tag = if_else(tag_type %in% alcohol_variations, 'alc_drink', tag_type),
    
    tz_offset = as.numeric(difftime(start_time_local, start_time, units = "hours"))
  ) |>
  arrange(start_time)

# --- Workouts ---
workouts <- raw_workouts |>
  mutate(
    start_time = ymd_hms(start_datetime),
    end_time = ymd_hms(end_datetime),
    duration = end_time - start_time,
    start_day = ymd(day)
  ) |>
  select(activity_type = activity, calories, start_time, end_time, 
         duration, start_day, distance, intensity, source, label)

# --- Activity ---
activity <- raw_activity |>
  mutate(
    start_day = ymd(day),
    # Critical: Your logic converted these to Hours
    activity_high_hrs = high_activity_time / 3600,
    activity_med_hrs = medium_activity_time / 3600,
    activity_low_hrs = low_activity_time / 3600,
    activity_sedentary_hrs = sedentary_time / 3600,
    activity_resting_hrs = resting_time / 3600
  ) |>
  select(start_day, score, active_calories, ends_with("hrs"))

# --- Readiness ---
readiness <- raw_readiness |>
  mutate(start_day = ymd(day)) |>
  select(start_day, readiness_score = score, temperature_deviation, everything(), -day, -id)

# --- Sleep ---
sleep <- raw_sleep |>
  mutate(start_day = ymd(day)) |>
  select(start_day, sleep_score = score, everything(), -day, -id)


# --- Save Artifacts -----------------------------------------------------------
# Saving as a list mimics your 'oura_dataframes.RData' but in a more portable format
list(
  tags = tags,
  workouts = workouts,
  activity = activity,
  readiness = readiness,
  sleep = sleep,
  sessions = raw_sessions
) |> 
  write_rds("data/oura_tidy.rds")

message("✔ Oura ingest complete. Saved tidy list to data/oura_tidy.rds")
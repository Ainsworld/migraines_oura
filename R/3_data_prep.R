library(tidyverse)
library(lubridate)
library(zoo)
library(readr) # For parse_number

# --- 1. Load Data -------------------------------------------------------------
oura_data <- read_rds("data/oura_tidy.rds")

# Load new Intervals artifacts
intervals_workouts <- read_rds("data/intervals_workouts.rds")
intervals_hr_dist  <- read_rds("data/intervals_hr_dist.rds")

# Extract Oura list items
tags <- oura_data$oura_tags
activity <- oura_data$oura_daily_activity
readiness <- oura_data$oura_daily_readiness
sleep <- oura_data$oura_daily_sleep
oura_workouts <- oura_data$oura_workouts

message("⚙️  Starting Feature Engineering...")

# --- 2. Activity Harmonization (The Consolidated View) ------------------------
message("   🔨 Harmonizing Activity Feeds...")

# A. Prepare Intervals (Master)
acts_intervals <- intervals_workouts |> 
  transmute(
    source = "Intervals",
    start_time, end_time, 
    date = start_day,
    activity = activity_type,
    duration_mins = as.numeric(duration, units = "mins"),
    calories,
    intensity_score = coalesce(icu_intensity, 0)
  )

# B. Prepare Oura (Candidates)
acts_oura <- oura_workouts |> 
  transmute(
    source = "Oura",
    start_time, end_time,
    date = start_day,
    activity = activity_type,
    duration_mins = as.numeric(duration, units = "mins"),
    calories,
    intensity_score = case_match(str_to_lower(intensity),
                                 "easy" ~ 40,
                                 "moderate" ~ 60,
                                 "hard" ~ 80,
                                 .default = NA # Rest/Unknown/NA
    )
  )

# C. Deduplication (Intervals wins overlaps)
acts_oura_unique <- acts_oura |>
  anti_join(
    acts_intervals,
    by = join_by(overlaps(start_time, end_time, start_time, end_time))
  )

# D. Consolidated Daily Stats
daily_activity_harmonised <- bind_rows(acts_intervals, acts_oura_unique) |>
  group_by(date) |>
  summarise(
    total_duration_mins = sum(duration_mins, na.rm = TRUE),
    total_active_cals = sum(calories, na.rm = TRUE),
    has_tracked_sport = max(source == "Intervals") == 1,
    sport_duration_mins = sum(duration_mins[source == "Intervals"], na.rm = TRUE),
    life_duration_mins = sum(duration_mins[source == "Oura"], na.rm = TRUE),
    activity_duration_mins = sport_duration_mins + life_duration_mins
  )

# --- 3. HR Stress Features ----------------------------------------------------
message("   ❤️  Calculating HR Stress Features...")

# Join RHR to allow relative calculations
rhr_default = 50         

daily_rhr_lookup <- readiness |> 
  mutate(rhr = 50) |> 
  #mutate(rhr = resting_heart_rate) |> 
  select(start_day, rhr) |> 
  distinct(start_day, .keep_all = TRUE)

daily_hr_features <- intervals_hr_dist |>
  left_join(daily_rhr_lookup, by = "start_day") |>
  # Fallback for missing RHR: Use global mean to prevent data loss
  mutate(rhr = replace_na(rhr, rhr_default)) |>
  group_by(start_day) |>
  summarise(
    # A. Simple Duration (The Base Rate)
    # "Hours spent in the danger zone"
    hr_hrs_gt_140 = sum(seconds[bpm >= 140], na.rm = TRUE) / 3600,
    hr_hrs_gt_150 = sum(seconds[bpm >= 150], na.rm = TRUE) / 3600,
    hr_hrs_gt_160 = sum(seconds[bpm >= 160], na.rm = TRUE) / 3600,
    hr_hrs_140_150 = sum(seconds[bpm >= 140 & bpm < 150], na.rm = TRUE) / 3600,
    hr_hrs_150_160 = sum(seconds[bpm >= 150 & bpm < 160], na.rm = TRUE) / 3600,
  
    # B. Linear Impulse (Excess Beats)
    # "How many EXTRA heartbeats did I spend above the threshold?"
    # Captures intensity: 1 min at 170 counts double 1 min at 160 (vs 150 baseline)
    # Unit: Beat-Hours. 
    # Value of 10 = "10 beats over threshold for 1 hour" (or 20 beats for 30 mins)
    hr_impulse_140 = sum(pmax(0, bpm - 140) * seconds, na.rm = TRUE) / 3600,
    hr_impulse_150 = sum(pmax(0, bpm - 150) * seconds, na.rm = TRUE) / 3600,
    hr_impulse_160 = sum(pmax(0, bpm - 160) * seconds, na.rm = TRUE) / 3600,
 
    # C. Quadratic Strain (Non-Linear Cost)
    # Penalizes the extreme efforts heavily.
    # Unit: 100-Beat²-Hours.
    # Scaled by 360,000 to keep range 0-100 approx.
    hr_strain_140_sq = sum((pmax(0, bpm - 140)^2) * seconds, na.rm = TRUE) / 360000,
    hr_strain_150_sq = sum((pmax(0, bpm - 150)^2) * seconds, na.rm = TRUE) / 360000,
    hr_strain_160_sq = sum((pmax(0, bpm - 160)^2) * seconds, na.rm = TRUE) / 360000,

    # D. Physiological Load (Relative to Homeostasis)
    # "Net Cardiac Impulse": Total extra beats above resting baseline.
    # This normalizes effort: 130bpm is harder if RHR is 40 vs 60.
    # Unit: Excess Beat-Hours above RHR.
    # Range: Typically 10 (Rest day) to 300 (Hard Training day)
    net_cardiac_impulse = sum(pmax(0, bpm - rhr) * seconds, na.rm = TRUE) / 3600,

    # "Homeostatic Strain": Quadratic penalty for deviation from RHR.
    # Aligns with Allostatic Load theory: distance from equilibrium is costly.
    # Scaled by 360,000 to keep comparable to other indices
    homeostatic_strain = sum(((bpm - rhr)^2) * seconds, na.rm = TRUE) / 360000
  ) |>
  rename(date = start_day)


# --- 4. Custom Feature Logic --------------------------------------------------

# A. Treatment Dates
treatment_dates <- tribble(
  ~treatment_start, ~treatment_end, ~treatment, ~treatment_dose,
  "2023-06-14", "2023-08-20", "Candesartan 8mg", 0.5,
  "2024-12-18", "2024-12-22", "Candesartan 8mg", 0.5,
  "2024-12-23", "2024-12-28", "Candesartan 8mg", 1,
  "2024-12-29", "2025-01-12", "Candesartan 8mg", 1.5,
  "2025-01-13", "2025-08-28", "Candesartan 8mg", 2,
  "2025-08-29", as.character(Sys.Date()), "Candesartan 8mg", 1.5,
) |>
  mutate(treatment_start = ymd(treatment_start),
         treatment_end = ymd(treatment_end))

# B. Migraine Day Transition Logic
day_transition <- 8 

migraines <- tags |> 
  filter(tag == 'migraine') |> 
  select(migraine_time = start_time, migraine_time_local = start_time_local) |> 
  arrange(migraine_time) |> 
  mutate(
    migraine_last = lag(migraine_time),
    migraine_last_hrs = interval(migraine_last, migraine_time) / hours(1),
    date = as.Date(floor_date(migraine_time_local - hours(day_transition), unit = "day"))
  )

migraine_days <- migraines |> 
  group_by(date) |> 
  summarise(
    migraine_first = min(migraine_time),
    migraine_n = n(),
    migraine_interval = min(migraine_last_hrs)
  )

# -- C. Alcohol & Peak EBAC Logic (Advanced Unit Extraction) -----

# Time between successive drink tags to be considered part of a singular drinks series
drink_series_hrs <- 2.1 

# These are used for modelling Estimated Blood Alcohol Concentration (EBAC)
body_weight_kg <- 74    
widmark_r <- 0.68
widmark_factor <- body_weight_kg * widmark_r * 10 
elimination_rate <- 0.15 
grams_per_drink_amount <- 8.0
# ^ Use 8 if counting alcoholic units (~8g of pure ethanol = 1% alcohol in 1 litre drink), 
#   use 18 for US standard 'drink'
default_amount <- 2 # fallback if no amount and no observed avg for that tag
default_consumption_time <- 0.5 # time to consume one amount - applied when 
  # there's no follow-on drink, which would otherwise define the consumption rate as
  # 'sipping over the duration' is assumed.

message("   🍺 Calculating Alcohol & EBAC (with Modal Imputation)...")

# I adopted the convention of adding the units alcohol in the comment of a tag
# each time I had one, so this extract that and applies fallbacks if not present.

# Helper to extract numbers from comments like "1.5 white" or "2"
extract_amount <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  # parse_number extracts the first numeric value found
  suppressWarnings(readr::parse_number(as.character(x)))
}

# 1. Prepare raw drinks with explicit amounts
drinks_raw <- tags |> 
  filter(tag == 'alc_drink') |>
  arrange(start_time) |>
  mutate(
    # Extract number from comment (if any)
    explicit_amount = extract_amount(comment),
    # Treat 0 extraction as NA to avoid errors, assuming tags imply at least something
    explicit_amount = if_else(explicit_amount == 0, NA_real_, explicit_amount),
    type_clean = str_to_lower(tag_type)
  )

# 2. Calculate Modal Amounts per Type (The "Inconsistent Tagger" Fix)
# Logic: Find the most frequent unit count for each alcohol type
modes <- drinks_raw |>
  filter(!is.na(explicit_amount)) |>
  group_by(type_clean) |>
  summarise(
    # Sort table of counts, pick top name, convert to numeric
    modal_amount = as.numeric(names(sort(table(explicit_amount), decreasing=TRUE)[1])),
    .groups = "drop"
  )

# Print inferred defaults for user verification
message("      > Inferred Amount Defaults based on your history:")
modes |> 
  mutate(msg = sprintf("        - %s: %.1f amount", type_clean, modal_amount)) |> 
  pull(msg) |> 
  walk(message)

# 3. Apply Defaults & Cluster Sessions
drinks_enriched <- drinks_raw |>
  left_join(modes, by = "type_clean") |>
  mutate(
    final_drink_amount = coalesce(explicit_amount, modal_amount, default_amount),
    drink_last = lag(start_time),
    time_diff = as.numeric(difftime(start_time, drink_last, units = "hours")),
    drink_series = cumsum(is.na(time_diff) | time_diff > drink_series_hrs)
  ) |>
  group_by(drink_series) |>
  mutate(
    next_start_time = lead(start_time),
    duration_hrs = if_else(!is.na(next_start_time), 
                           as.numeric(difftime(next_start_time, start_time, units = "hours")), 
                           final_drink_amount * default_consumption_time),
    duration_hrs = pmax(0.17, duration_hrs) # Fallback of 10 mins
  ) |> ungroup()

simulate_peak_ebac <- function(amounts, durations) {
  current_bac <- 0
  peak_bac <- 0
  for (i in seq_along(amounts)) {
    grams <- grams_per_drink_amount * amounts[i]
    ingestion_slope <- (grams / widmark_factor) / durations[i]
    next_bac <- max(0, current_bac + ((ingestion_slope - elimination_rate) * durations[i]))
    peak_bac <- max(peak_bac, current_bac, next_bac)
    current_bac <- next_bac
  }
  return(peak_bac)
}

drink_days <- drinks_enriched |> 
  group_by(drink_series) |> 
  summarise(
    drink_first_local = min(start_time_local),
    drink_last_local = max(start_time_local),
    
    drinks_amount_sum = sum(final_drink_amount),
    drinks_count = n(),
    duration_hrs = pmax(1, as.numeric(difftime(drink_last_local, drink_first_local, units = "hours"))),
    
    alcohol_grams_total = drinks_amount_sum * grams_per_drink_amount,
    peak_ebac_proxy = pmax(0, (alcohol_grams_total / (body_weight_kg * widmark_r * 10)) - (duration_hrs * 0.015)),
    peak_ebac = simulate_peak_ebac(final_drink_amount, duration_hrs),
    
    any_wine = max(if_else(type_clean == 'wine', 1, 0)),
    any_beer = max(if_else(type_clean == 'beer', 1, 0))
  ) |> 
  mutate(date = as.Date(floor_date(drink_first_local, unit = "day"))) |> 
  group_by(date) |> 
  summarise(
    drinks_count = sum(drinks_count), # Sum of volumes, not tags
    drinks_amount = sum(drinks_amount_sum), # Sum of volumes, not tags
    drink_sessions = n(),
    peak_ebac_daily = max(peak_ebac_proxy),
    drink_any_wine = max(any_wine),
    drink_any_beer = max(any_beer)
  )

# D. Other Events
event_days <- tags |> 
  #filter(tag %in% c('travel','airplane','car', 'stress','anxiety','socialgathering','party', 'coffee','highaltitude')) |> 
  group_by(date = start_day) |> 
  summarise(
    extra_coffee = max(tag == 'coffee') == 1,
    any_stress = max(tag %in% c('stress','anxiety','party','socialgathering')) == 1,
    any_travel = max(tag %in% c('travel','airplane','car','train')) == 1,
    any_plane = max(tag == 'airplane') == 1,
    any_highaltitude = max(tag == 'highaltitude') == 1,
    any_lowpressure = max(tag %in% c('airplane','highaltitude')) == 1
  )

# --- 5. Master Join -----------------------------------------------------------

spine <- tibble(date = seq.Date(from = min(readiness$start_day), 
                                to = max(readiness$start_day), 
                                by = "day"))

migraine_before_data <- as.Date("2022-11-02")

analysis_df <- spine |>
  # 1. Join Migraines & Lags
  left_join(migraine_days, by = "date") |>
  mutate(
    is_weekend = wday(date, week_start = 1) >= 6,
    is_saturday = wday(date, week_start = 1) == 6,
    weekday = wday(date, week_start = 1, label = TRUE)
  ) |>
  left_join(
    migraines |> rename(prev_migraine = date) |> select(prev_migraine) |> distinct(), 
    by = join_by(closest(date > prev_migraine))
  ) |>
  mutate(
    prev_migraine = coalesce(prev_migraine, migraine_before_data),
    days_since_last = as.numeric(difftime(date, prev_migraine, units = "days")),
    weeks_since_last = days_since_last / 7,
    weeks_since_last_max1 = pmin(1, weeks_since_last)
  ) |>
  
  # 2. Join Treatments, Drinks, Events
  left_join(treatment_dates, by = join_by(between(date, treatment_start, treatment_end))) |>
  mutate(any_medication = !is.na(treatment)) |>
  select(-starts_with('treatment')) |>
  left_join(drink_days, by = "date") |>
  left_join(event_days, by = "date") |>
  
  # 3. Join Oura Daily Data
  left_join(readiness |> select(start_day, readiness_score), by = join_by(date == start_day)) |>
  left_join(sleep |> select(start_day, sleep_score, total_sleep = contributors.total_sleep), by = join_by(date == start_day)) |>
  
  # 4. Join Harmonized Activity & HR Features
  left_join(daily_activity_harmonised, by = "date") |>
  left_join(daily_hr_features, by = "date") |>
  
  # 5. Imputation & Cleanup
  mutate(
    across(c(migraine_n, 
             drink_any_wine, drink_any_beer,
             drinks_count, drinks_amount, drink_sessions, peak_ebac_daily, 
             total_duration_mins, total_active_cals, 
             sport_duration_mins, life_duration_mins, activity_duration_mins,
             net_cardiac_impulse, homeostatic_strain, 
             starts_with("hr_")), # Picks up loads of the HR-distribution metrics
           ~replace_na(., 0)),
    across(c(has_tracked_sport), ~replace_na(., FALSE)),
    across(starts_with('any_'), ~replace_na(., FALSE)),
    across(starts_with('extra_'), ~replace_na(., FALSE)),
    across(ends_with('_score'), ~replace_na(., as.integer(mean(., na.rm = TRUE))))
  ) |>
  
  # 6. Feature Engineering (Scaling/Centering)
  mutate(
    badsleep_d = 0.1 * (mean(sleep_score) - sleep_score),
    totalsleep_d = 0.1 * (mean(total_sleep) - total_sleep),
    unreadiness_d = 0.1 * (mean(readiness_score) - readiness_score),
    
    # Target Variable
    migraine = (migraine_n >= 1)
  )

# --- 6. Save Final Artifact ---------------------------------------------------
write_rds(analysis_df, "data/analysis_ready.rds")

message(sprintf("✔ Data Prep Complete. Final dataset: %d rows.", nrow(analysis_df)))

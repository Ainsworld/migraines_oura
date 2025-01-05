library(tidyverse)


fetch_data = FALSE

if (fetch_data) { 
  source("1 - Oura API.R")
  } else {
  load("oura_dataframes.RData")
  }






#' Some general principles
#' - Use UTC for interval calculations, use local time for anything relating to
#'   thinking about 'today' or time of day.
#' - Variable suffixes
#'   _c = centred, i.e. in original units but relative to the mean
#'   _d = dec centred, i.e. relative to mean, in 10s of units.
#'   _z = normalised, i.e. 0 = mean, 1 = +1 SD
#' - Migraine events in middle of night attached to previous day.
#' - Assumed that migraine tags are a consistent timing and when the migraine
#'   has got significant, not the very start of it.


# Bit of EDA of the tags --------------------------------------------------


# tags |> group_by(tag_type) |> count() |> arrange(desc(n)) |> print(n = 40)
# tags |> filter(tag_type == 'custom') |> print(n = 100)
# 
# 
# model_data_days  |>  summarise(across(everything(), mean, na.rm = TRUE)) |> glimpse()
# colSums(is.na(model_data_days))

tag_summary <- tags %>%
  group_by(start_day, tag_type) %>%
  summarise(tag_count = n(), .groups = "drop") %>%
  # Add total frequencies and reorder tag_type by total count
  group_by(tag_type) %>%
  mutate(total_count = sum(tag_count)) %>%
  ungroup() %>%
  mutate(tag_type = factor(tag_type, levels = unique(tag_type[order(-total_count)])))


tags %>%
  group_by(start_day, tag_type) %>%
  summarise(tag_count = n(), .groups = "drop") %>%
  group_by(tag_type) %>%
  mutate(total_count = sum(tag_count)) %>%
  ungroup() %>%
  mutate(tag_type = factor(tag_type, levels = unique(tag_type[order(total_count)]))) |> 
  ggplot(aes(x = start_day, y = tag_type, size = tag_count)) +
  geom_point(alpha = 0.3) +  
  scale_size_area(max_size = 5) +  
  labs(title = "Tag Type Frequency Over Time")


# Define custom tags for medical interventions ------------------------------

treatment_dates <- tribble(
  ~treatment_start, ~treatment_end, ~treatment, ~treatment_dose,
  "2023-06-14", "2023-08-20", "Candesartan", "4mg",
  "2024-12-18", "2025-01-09", "Candesartan", "escalating 4mg to 12mg") |>
  mutate(treatment_start = ymd(treatment_start),
         treatment_end = ymd(treatment_end))


# Gather up data - Migraines ---------------------------------------------------


day_transition = 5  
 # What hour of the day is considered the boundary between one day and the next
 # i.e. a migraine before this hour in the night is assigned to the previous day

migraines <- tags |> 
  filter(tag == 'migraine') |> 
  select(migraine_time = start_time,
         migraine_time_local = start_time_local,
         migraine_comment = comment) |> 
  arrange(migraine_time) |> 
  mutate(migraine_last = lag(migraine_time),
         migraine_last_hrs = interval(migraine_last, migraine_time) / hours(1),
         date = as.Date(floor_date(migraine_time_local - hours(day_transition), unit = "day")))

#migraines

migraine_days <- migraines |> 
  group_by(date) |> 
  summarise(migraine_first = min(migraine_time),
            migraine_last = max(migraine_time),
            migraine_n = n(),
            migraine_interval = min(migraine_last_hrs))

#migraine_days

# ANALYSIS: to decide on 5am as reasonable transition time
migraines |>
  mutate(first_of_series = (migraine_last_hrs > (2 * 24)) |> coalesce(FALSE)) |> 
  mutate(time = hms::as_hms(migraine_time_local)) |>
  ggplot(aes(x = time)) +
  geom_histogram(bins = 12, boundary = 0) +
  scale_x_time(breaks = scales::breaks_timespan(unit = "hours", n = 8)) +
  facet_wrap( ~ first_of_series, labeller = "label_both") +
  labs(title = "Most migraines in evening, especially first of a run")

# # ANALYSIS: of intervals
# # Shows clear peak around 24h, and another 
# migraines |> 
#   ggplot(aes(x = migraine_last_hrs / 24)) +
#   geom_density(adjust = 0.4) +
#   scale_x_log10(breaks = scales::breaks_log(n = 10)) +
#   labs(title = "Two peaks - at 24h, and 3-7 days")
# 
# # ANALYSIS: of days of week
# migraines |> 
#   mutate(first_of_series = (migraine_last_hrs > (2 * 24)) |> coalesce(FALSE)) |> 
#   ggplot(aes(x = wday(migraine_time, label = TRUE, week_start = 1))) +
#   geom_bar() +
#   facet_wrap( ~ first_of_series, labeller = "label_both") +
#   labs(title = "Migraines more common at the weekend",
#        x = "Day of week",
#   )


# Gather up data - other interesting tags ---------------------------------

travel_tags = c('travel','airplane','car')
stress_tags = c('stress','anxiety','socialgathering','party')
coffee_tags = c('coffee')

event_days <- tags |> 
  filter(tag %in% c(travel_tags, stress_tags, coffee_tags)) |> 
  group_by(date = start_day) |> 
  summarise(extra_coffee = max(tag %in% coffee_tags) == 1,
            any_stress = max(tag %in% stress_tags) == 1,
            any_travel = max(tag %in% travel_tags) == 1,
            any_plane = max(tag %in% c('airplane')) == 1)


# Gather up data - Drinks -------------------------------------------------


drink_series_hrs = 2.1 # interval between drinks to consider part of single session
# Analysis of intervals showed a clear cluster where gap 0.5-2h)
# Also, 2h is about how long an average drink's alcohol takes to be metabolised,
# so drinks closer than this will lead to higher concentrations of alcohol in blood

drinks <- tags |> 
  filter(tag == 'alc_drink') |> 
  select(drink_time = start_time,
         drink_time_local = start_time_local,
         type = tag_type,
         comment) |> 
  arrange(drink_time) |> 
  mutate(drink_last = lag(drink_time)) |> 
  mutate(
    time_diff = as.numeric(difftime(drink_time, drink_last, units = "hours")),
    drink_series = cumsum(is.na(time_diff) | time_diff > drink_series_hrs)
  ) %>%
  select(-time_diff) # Optional: remove intermediate column if not needed

drink_series <- drinks |> 
  group_by(drink_series) |> 
  summarise(drink_first = min(drink_time),
            drink_last = max(drink_time),
            drink_first_local = min(drink_time_local),
            drink_last_local = max(drink_time_local),
            drinks_n = n(),
            any_wine = max(if_else(type == 'wine', 1, 0)),
            any_beer = max(if_else(type == 'beer', 1, 0))
  ) |> 
  mutate(date = as.Date(floor_date(drink_first_local, unit = "day")))

#drink_series |> print(n = 50)

drink_days <- 
  drink_series |> 
  group_by(date) |> 
  summarise(drink_first = min(drink_first),
            drink_last = max(drink_last),
            drinks_total = sum(drinks_n),
            drink_sessions = n(),
            drink_session_biggest = max(drinks_n),
            drink_session_biggest_gt1 = pmax(0, drink_session_biggest - 1),
            drink_sessions_gt1 = sum(drinks_n >=2),
            drink_any_wine = max(any_wine),
            drink_any_beer = max(any_beer)
  )

drink_days |> glimpse()

# #ANALYSIS: the time intervals to define series...
# drinks |>
#   mutate(time_diff = as.numeric(difftime(drink_time, drink_last, units = "hours")),
#          time = hms::as_hms(drink_last),
#          evening = if_else(time >= hms('17:00:00'), 'Prev drink in evening', 'Prev drink before evening')) |>
#   filter(time_diff < 12) |>
#   ggplot(aes(x = time_diff)) +
#   geom_histogram(bins = 25) +
#   #geom_density(adjust = 0.4) +
#   geom_vline(xintercept = drink_series_hrs, colour = "darkred", linetype = "dashed") +
#   scale_x_log10(breaks = scales::breaks_log(n = 10)) +
#   facet_wrap(~ evening) +
#   labs(title = "Time between drinks", x = "Hours from previous drink")





# Combine up by day for modelling -----------------------------------------

first_date <- readiness |> summarise(min(start_day)) |> pull()
last_date <- readiness |> summarise(max(start_day)) |> pull()

spine <- tibble(date = seq.Date(from = ymd(first_date), to = ymd(last_date), by = "day"))

migraine_before_data <- ymd_hms("2022-11-02T17:00:00+00:00") |> as.Date()


model_data_days <- spine |> 
  left_join(migraine_days) |> 
  # Features: calendrical
  mutate(is_weekend = wday(date, week_start = 1) >= 6,
         is_saturday = wday(date, week_start = 1) == 6,
         is_sunday = wday(date, week_start = 1) == 7,
         weekday = wday(date, week_start = 1, label = TRUE), 
         .after = date) |> 
  # Calculate hours since last migraine
  left_join(
    migraines |> rename(prev_migraine = date), 
    by = join_by(closest(date > prev_migraine))) |> 
  mutate(prev_migraine = coalesce(prev_migraine, migraine_before_data),
         days_since_last = as.numeric(difftime(date, prev_migraine, units = "days")),
         weeks_since_last = days_since_last / 7,
         weeks_since_last_max1 = pmin(1, weeks_since_last),
         migraine_yesterday = coalesce(days_since_last == 1, FALSE)) |> 
  
  # Bring in treatments
  left_join(treatment_dates, by = join_by(between(date, treatment_start, treatment_end))) |>
  mutate(any_medication = !is.na(treatment)) |>
  select(-starts_with('treatment')) |>
  
  # Bring in drinks data
  left_join(drink_days) |>
  
  # Bring in Oura daily data
  left_join(activity, by = join_by(date == start_day)) |> 
  left_join(readiness |> select(start_day, readiness_score), 
            by = join_by(date == start_day)) |> 
  left_join(sleep |> select(start_day, sleep_score, total_sleep), 
            by = join_by(date == start_day)) |> 
  
  # Bring in other Oura tags
  left_join(event_days) |> 
  
  # Handle non-joins or other gaps
    # replace with zero...
  mutate(across(c(migraine_n, drinks_total, drink_sessions, drink_sessions_gt1,
                  drink_any_wine, drink_any_beer, drink_session_biggest, drink_session_biggest_gt1), 
                ~replace_na(., 0))) |> 
    # replace with FALSE... (
  mutate(across(starts_with('any_'), ~replace_na(., FALSE))) |> 
  mutate(across(starts_with('extra_'), ~replace_na(., FALSE))) |> 
    # replace with mean of others...
  mutate(across(contains('hrs'), ~replace_na(., mean(., na.rm = TRUE)))) |> 
  mutate(across(ends_with('_d'), ~replace_na(., mean(., na.rm = TRUE)))) |> 
  mutate(across(ends_with('_score'), ~replace_na(., as.integer(mean(., na.rm = TRUE))))) |> 
  mutate(across(c(active_calories), ~replace_na(., as.integer(mean(., na.rm = TRUE))))) |> 
  
  # Other useful features
  mutate(badsleep_d = 0.1 * (mean(sleep_score, na.rm = TRUE) - sleep_score),
         totalsleep_d = 0.1 * (mean(total_sleep, na.rm = TRUE) - total_sleep)) |> 
    # Sleep scaled as per 10 units
  mutate(unreadiness_d = 0.1 * (mean(readiness_score, na.rm = TRUE) - readiness_score)) |> 
    # Unreadiness scaled as per 10 units
  mutate(activity_d = 0.1 * (activity_score - mean(activity_score, na.rm = TRUE)),
         active_cals100 = active_calories / 100,
         activity_sedentary_hrs_c = activity_sedentary_hrs - mean(activity_sedentary_hrs, na.rm = TRUE),
         activity_resting_hrs_c = activity_resting_hrs - mean(activity_resting_hrs, na.rm = TRUE),
  ) |> 
    # Activity scaled as per 10 units
  mutate(drinks_any = (drinks_total > 0),
         activity_medhigh_hrs = activity_high_hrs + activity_med_hrs) |> 
  
  # Define key indicator
  mutate(migraine = (migraine_n >= 1))



#model_data_days |> glimpse()

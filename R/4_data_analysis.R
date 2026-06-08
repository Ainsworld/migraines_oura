
# Assumed that 3. will have been run




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


# Some visuals for trends, etc. -------------------------------------------


# Faceted by year
tags |> 
  filter(tag == 'migraine') |> 
  arrange(start_day) |> 
  mutate(
    day_of_week = wday(start_day, label = TRUE, week_start = 1),
    year = year(start_day),
    week_number = isoweek(start_day),
    year_week_date = floor_date(start_day, unit = "week"),
    time_of_day_numeric = hour(start_time) + minute(start_time) / 60,
    previous_day = lag(start_day), 
    consecutive = as.numeric(start_day - previous_day == 1),
    first_of_run = if_else(consecutive == 0, TRUE, FALSE)  # Flag for first of run
  ) |> 
  ggplot(aes(x = week_number, y = day_of_week, fill = time_of_day_numeric)) +
  geom_hline(yintercept = seq(0.5, 7.5, by = 1), color = "gray70", size = 0.5) +
  geom_tile(aes(color = factor(first_of_run)), linewidth = 0.8) +  # Outline for 'first of run'
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = NA), 
    na.translate = FALSE, 
    name = "First of Run",  # Legend title
    labels = c("No", "Yes")  # Legend labels
  ) +
  scale_fill_gradientn(
    colors = c("#3333AA", "#1d77a0", "#AADD00", "#EECC00", "#CC2200", "#772222"),
    breaks = seq(0, 24, by = 4),  # Breaks at 6-hour intervals
    values = scales::rescale(c(0, 4, 8, 16, 20, 24)),
    name = "Time of Day"
  ) +
  scale_x_continuous(
    breaks = seq(1, 52, by = 4),  # Show labels for every 4 weeks
    labels = function(x) {
      # Create labels that show month names for the first week of each month
      month_labels <- format(as.Date(paste0("2025-W", sprintf("%02d", x), "-1"), format = "%Y-W%U-%u"), "%b")
      return(month_labels)
    },
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    limits = rev(levels(wday(Sys.Date(), label = TRUE, week_start = 1))),
    expand = c(0, 0)
  ) +
  labs(
    x = "", y = "",
    title = "Migraine Occurrences Over Time",
    fill = "Time of Day"
  ) +
  facet_grid(year ~ .) +  # Facet by year
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.spacing = unit(3, "lines"),  # Add more space between facets
    strip.text = element_text(face = "bold", size = 14),  # Make year labels bold
    legend.position = "bottom",  # Position the legend at the bottom
    legend.key.width = unit(2, "lines")
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = "grey90"))  # Make sure the inner of the legend key is grey
  )



# Numbers over time -------------------------------------------------------

# Simple smoothed line plot showing migraines per week as metric 
library(tidyquant)

# Prepare treatment data for plotting (step function)
treatment_plot_data <- treatment_dates |>
  select(start = treatment_start, end = treatment_end, dose = treatment_dose) |>
  pivot_longer(cols = c(start, end), names_to = "type", values_to = "date") |>
  arrange(date) |>
  group_by(dose) 

coeff <- 1 # Scale factor to align dose (0-2mg) with migraine counts (0-4) visually

tags |> 
  filter(tag == 'migraine') |> 
  mutate(week_date = floor_date(as.Date(start_day), unit = "week", week_start = 1)) |>
  count(week_date) |>  # Count the number of migraines per week
  # This adds missing weeks and sets n to 0
  complete(week_date = seq.Date(min(week_date), max(week_date), by = "week"), 
           fill = list(n = 0)) |>
  ggplot() +  # Convert year_week to Date
  # Secondary Layer: Candesartan Dose (Area/Step)
  geom_rect(data = treatment_dates,
            aes(xmin = treatment_start, xmax = treatment_end, 
                ymin = 0, ymax = treatment_dose / coeff),
            fill = "#DD5511", alpha = 0.4) +
  geom_bar(aes(x = week_date, y = n),
           stat = "identity", fill = "grey60", alpha = 0.7) +  # Bar plot (with grey fill
  geom_ma(aes(x = week_date, y = n),
          ma_fun = EMA, n = 20, color = "black", linetype = "solid", size = 1) +
  
  # Dual Axis Scaling
  scale_y_continuous(
    name = "Migraines per Week",
    sec.axis = sec_axis(~ . * coeff, name = "Candesartan Dose (mg) - Orange Area")
  ) +
  labs(title = "Migraine Frequency vs Candesartan Treatment",
       subtitle = "Line = EMA of Migraines | Orange Area = Medication Dose",
       x = "Date") +
  theme_minimal()




#FF5F17
#FE892D

# ANALYSIS: to decide on 8am as reasonable transition time
migraines |>
  mutate(first_of_series = (migraine_last_hrs > (2 * 24)) |> coalesce(FALSE)) |> 
  mutate(time = hms::as_hms(migraine_time_local)) |>
  ggplot(aes(x = time)) +
  geom_histogram(bins = 25, boundary = 0, fill = "#DD5511") +
  scale_x_time(breaks = scales::breaks_timespan(unit = "hours", n = 8)) +
  facet_wrap( ~ first_of_series, labeller = "label_both") +
  labs(title = "Most migraines in evening, especially first of a run")

# ANALYSIS: of intervals
# Shows clear peak around 24h, and another
migraines |>
  ggplot(aes(x = migraine_last_hrs / 24)) +
  geom_density(adjust = 0.5, colour = "#DD5511", linewidth = 2) +
  scale_x_log10(breaks = scales::breaks_log(n = 10)) +
  labs(title = "Two peaks - at 24h, and 3-7 days",
       x = "Number of days since previous migraine (log scale)")

# ANALYSIS: of days of week
migraines |>
  mutate(first_of_series = if_else((migraine_last_hrs > (2 * 24)) |> coalesce(FALSE),
                                   'First of series', 'Follow-ons'),
         evening = if_else((hms::as_hms(migraine_time_local) >= hms('18:00:00')),
                           'Evening', ' Morning + Afternoon')) |>
  ggplot(aes(x = wday(migraine_time, label = TRUE, week_start = 1))) +
  geom_bar(fill = "#DD5511") +
  facet_grid(evening ~ first_of_series) +
  labs(title = "Evening migraines more common at the weekend, daytime ones often start on Thursdays",
       x = "Day of week",
  )



# Some new charts ---------------------------------------------------------

# --- 2. Raw HR Distribution Analysis (Sport Days vs Migraine) ----------------

# Objective: Compare the cumulative distribution of HR time on Sport Days 
# that DID result in a migraine vs those that DID NOT.
# Split by Era: Pre-2025 (Pre-Regular Meds) vs 2025+ (Medicated).

# A. Prepare Data
# We need to join the granular HR distributions to the daily outcome label.

hr_analysis_df <- intervals_hr_dist |>
  # Join with the Master Analysis DF to get 'migraine' outcome and 'treatment' era
  inner_join(analysis_df |> select(date, migraine, has_tracked_sport), 
             by = c("start_day" = "date")) |>
  # Filter for Sport Days only
  filter(has_tracked_sport) |>
  # Define Era
  mutate(
    era = if_else(year(start_day) >= 2025, "2025+ (Candesartan Era)", "Pre-2025 (Unmedicated/Sporadic)"),
    outcome_label = if_else(migraine, "Migraine Followed", "Safe Day")
  )

# B. Calculate ECDF (Empirical Cumulative Distribution Function) data
# We want to see: "On Migraine days, did I spend more time at X bpm?"

# Normalize seconds to proportion of daily activity time to handle different ride lengths
hr_cdf_data <- hr_analysis_df |>
  group_by(start_day) |>
  mutate(total_secs = sum(seconds)) |>
  ungroup() |>
  mutate(prop_time = seconds / total_secs)

# C. Density Plot 
# Looking for the "fat tail" at high HRs
ggplot(hr_cdf_data, aes(x = bpm, weight = prop_time, fill = outcome_label)) +
  geom_density(alpha = 0.4, adjust = 0.2) + # Smoothed
  facet_wrap(~ era) +
  scale_fill_manual(values = c("Safe Day" = "grey60", "Migraine Followed" = "#DD5511")) +
  coord_cartesian(xlim = c(120, 180)) +
  labs(
    title = "HR Density Comparison",
    subtitle = "Check for 'fat tails' (excess time) in the 150+ zone on Migraine days",
    x = "Heart Rate (BPM)",
    y = "Density"
  ) +
  theme_minimal()


# D. Calculate Risk Probability (P(Migraine | Peak HR in Band X))
# Logic: Classify each Sport Day by its PEAK HR band (e.g., Peak was 150-155).
# Then calculate migraine probability for that band.

# 1. Identify Peak HR per Day
peak_min_secs <- 10

daily_peaks <- hr_analysis_df |>
  group_by(start_day, era, migraine) |>
  summarise(
    peak_hr = max(bpm[seconds > peak_min_secs], na.rm = TRUE), # Ignore blips < 5s
    .groups = "drop"
  ) |>
  filter(peak_hr > 100) # Filter out recovery/walking days


# hr_analysis_df |> 
#   group_by(start_day) |> 
#   summarise(hrs = sum(seconds)/3600) |> 
#   view()


# 2. Bin Peaks and Calculate Risk
bin_width <- 8
conf_level <- 0.67 # or 0.95 is classic

risk_data_binned <- daily_peaks |>
  mutate(
    hr_bin = floor(peak_hr / bin_width) * bin_width
  ) |>
  group_by(era, hr_bin) |>
  summarise(
    prob_migraine = mean(migraine),
    n_days = n(),
    n_successes = sum(migraine),
    .groups = "drop"
  ) |>
  # Filter for robustness: need at least 2 observations in a bin to plot it
  filter(n_days >= 2) |> 
  # Robust Error Calculation: Clopper-Pearson Exact CI
  # This handles p=0 correctly (unlike simple SE which collapses to 0).
  # conf.level = 0.68 approximates +/- 1 Standard Error.
  mutate(
    ci_results = purrr::map2(n_successes, n_days, ~binom.test(.x, .y, conf.level = conf_level)),
    ci_low = purrr::map_dbl(ci_results, ~.$conf.int[1]),
    ci_high = purrr::map_dbl(ci_results, ~.$conf.int[2])
  )

# E. Plot Risk Curve
ggplot(risk_data_binned, aes(x = hr_bin, y = prob_migraine, color = era)) +
  # Confidence Interval Ribbon (Robust)
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = era), 
              alpha = 0.15, color = NA) +
  
  # Main Line & Points
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n_days), alpha = 0.7) +
  
  # Scales & Styling
  scale_color_manual(values = c("Pre-2025 (Unmedicated/Sporadic)" = "grey50", "2025+ (Candesartan Era)" = "#DD5511")) +
  scale_fill_manual(values = c("Pre-2025 (Unmedicated/Sporadic)" = "grey50", "2025+ (Candesartan Era)" = "#DD5511")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(100, 200, by = 10)) +
  coord_cartesian(xlim = c(110, 180)) +
  
  labs(
    title = "Migraine Risk by Peak Heart Rate",
    subtitle = "Probability of Migraine given the Peak HR reached (with 68% Exact CI)",
    x = "Peak Heart Rate (5-bpm bands)",
    y = "Probability of Migraine (Next Day)",
    color = "Era", fill = "Era", size = "Days observed"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# --- VARIABLE WIDTH BINNING (Staircase Plot) ---
# Objective: Equalize statistical power by binning based on N (sample size), 
# rather than fixed BPM width.

risk_data_variable <- daily_peaks |>
  group_by(era) |>
  # Create bins with ~20 observations each
  mutate(
    # cut_number divides data into n groups of approx equal size
    # We estimate n groups by dividing total rows by target bin size (20)
    n_groups = pmax(2, round(n() / 20)),
    hr_bin_factor = cut_number(peak_hr, n = n_groups)
  ) |>
  group_by(era, hr_bin_factor) |>
  summarise(
    prob_migraine = mean(migraine),
    n_days = n(),
    n_successes = sum(migraine),
    # Extract numeric bounds from the factor (e.g., "(150,155]" -> 150, 155)
    # Using regex to pull the numbers out
    bin_min = as.numeric(str_extract(as.character(hr_bin_factor), "(?<=\\().+?(?=,)")),
    bin_max = as.numeric(str_extract(as.character(hr_bin_factor), "(?<=,).+?(?=\\])")),
    .groups = "drop"
  ) |>
  # Bayesian Credible Interval (Beta)
  mutate(
    alpha_post = 0.5 + n_successes,
    beta_post = 0.5 + (n_days - n_successes),
    ci_low = qbeta(0.16, alpha_post, beta_post), 
    ci_high = qbeta(0.84, alpha_post, beta_post)
  )

# Plot: Variable Width Staircase / Rectangles
ggplot(risk_data_variable) +
  # Uncertainty Rectangles (background)
  geom_rect(aes(xmin = bin_min, xmax = bin_max, 
                ymin = ci_low, ymax = ci_high, 
                fill = era), 
            alpha = 0.5) +
  
  # Main Probability Steps (foreground)
  geom_segment(aes(x = bin_min, xend = bin_max, 
                   y = prob_migraine, yend = prob_migraine, 
                   color = era), 
               linewidth = 1.2) +
  
  # Connect the steps (optional, for visual flow) - plotting midpoints
  # geom_point(aes(x = (bin_min + bin_max)/2, y = prob_migraine, color = era)) +
  
  scale_color_manual(values = c("Pre-2025 (Unmedicated/Sporadic)" = "grey50", "2025+ (Candesartan Era)" = "#DD5511")) +
  scale_fill_manual(values = c("Pre-2025 (Unmedicated/Sporadic)" = "grey50", "2025+ (Candesartan Era)" = "#DD5511")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(100, 200, by = 10)) +
  coord_cartesian(xlim = c(110, 185)) +
  
  labs(
    title = "Migraine Risk by Peak HR (Variable Width Bins)",
    subtitle = "Each step represents ~20 sport days. Width indicates data density.",
    x = "Peak Heart Rate Range (BPM)",
    y = "Probability of Migraine",
    color = "Era", fill = "Era"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




# Workout-to-migraine timing investigations -------------------------------

# ==========================================================================
# ANALYSIS: Workout-to-migraine interval — delay hypothesis EDA
#
# Each point = one typical migraine episode. Value = hours between the
# end of the most recent preceding workout (within 36h) and migraine onset.
# Densities layered by the workout's HR band (highest threshold sustained
# for >= min_sustained_mins, so brief spikes don't define the band).
#
# What a positive result looks like: the ≥150/160 densities peak at a
# specific lag (e.g. 4–16h) distinct from the uniform-ish lower bands.
# What a null looks like: all bands similar, or all peaking near 0.
# ==========================================================================

min_sustained_mins <- 0.5    # minutes above threshold to count as "that band"
window_hrs         <- 20   # look-back window

{
  workout_banded <- workout_events |>
    mutate(
      hr_band = case_when(
        hr_mins_gt_160 >= min_sustained_mins ~ "≥160 bpm",
        hr_mins_gt_150 >= min_sustained_mins ~ "150–160 bpm",
        hr_mins_gt_140 >= min_sustained_mins ~ "140–150 bpm",
        TRUE                                 ~ "<140 bpm"
      ),
      hr_band = fct_relevel(hr_band, "<140 bpm", "140–150 bpm",
                            "150–160 bpm", "≥160 bpm")
    )
  
  # Cross-join migraines × workouts, filter to window, keep only the
  # most-recent workout per episode.
  episodes_typical <- migraine_episodes #|> filter(!any_atypical)
  
  episode_workout_lag <- episodes_typical |>
    select(episode_id, onset_est_local) |>
    cross_join(
      workout_banded |> select(workout_id, wkt_end = end_time, hr_band)
    ) |>
    mutate(lag_hrs = as.numeric(difftime(onset_est_local, wkt_end, units = "hours"))) |>
    filter(lag_hrs >= 0, lag_hrs <= window_hrs) |>
    group_by(episode_id) |>
    slice_min(lag_hrs, n = 1, with_ties = FALSE) |>
    mutate(era = if_else(onset_est_local > '2025-01-01', 'Candesartan', 'Pre-Candesartan')) |> 
    ungroup()
  
  n_total     <- nrow(episodes_typical)
  n_matched   <- n_distinct(episode_workout_lag$episode_id)
  n_no_recent <- n_total - n_matched
  
  band_n <- episode_workout_lag |>
    count(hr_band) |>
    mutate(label = sprintf("%s  (n=%d)", hr_band, n))
  
  label_lookup <- setNames(band_n$label, band_n$hr_band)
  
  band_colours <- c(
    "<140 bpm"    = "grey60",
    "140–150 bpm" = "#74A9CF",
    "150–160 bpm" = "#F1A340",
    "≥160 bpm"    = "#DD5511"
  )
  
  episode_workout_lag |> 
    ggplot(aes(x = lag_hrs, fill = hr_band, colour = hr_band)) +
    geom_density(alpha = 0.22, bw = 0.75, linewidth = 0.9) +
    geom_rug(alpha = 0.7, length = unit(0.03, "npc"), linewidth = 0.5) +
    annotate("text",
             x = window_hrs * 0.97, y = Inf, vjust = 1.6,
             label = sprintf("%d of %d episodes had\nno workout in prior %dh",
                             n_no_recent, n_total, window_hrs),
             size = 3, colour = "grey45", hjust = 1) +
    scale_x_continuous(
      limits  = c(0, window_hrs),
      breaks  = seq(0, window_hrs, 6),
      labels  = \(x) paste0(x, "h"),
      expand  = expansion(mult = c(0, 0.01))
    ) +
    scale_fill_manual(values = band_colours, labels = label_lookup) +
    scale_colour_manual(values = band_colours, labels = label_lookup) +
    labs(
      title    = "Workout–migraine interval: delay hypothesis EDA",
      subtitle = sprintf("%.0f typical episodes | Most-recent workout in %.0fh window | HR band = highest threshold sustained ≥%.0f min",
        n_total, window_hrs, min_sustained_mins
      ),
      x       = "Hours between workout end and migraine onset",
      y       = "Density",
      fill    = "Workout HR band",
      colour  = "Workout HR band",
      caption = paste(
        "Bands: highest threshold (140/150/160 bpm) sustained ≥", min_sustained_mins,
        "min. Atypical/spanned episodes excluded."
      )
    ) +
    #facet_wrap(~ era, ncol = 1) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "right",
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      plot.caption     = element_text(size = 8, colour = "grey50"),
      panel.grid.minor = element_blank()
    )
}



# From workout to migraine... ---------------------------------------------

# ==========================================================================
# ANALYSIS: Workout → migraine — probability-scaled density
#
# FORWARD direction: for each workout, find the FIRST migraine onset
# within [0, window_hrs]. Area under each band's curve =
# P(migraine within window | workout in HR band).
#
# Compare to the original backward plot: same pairs, different denominator.
# Here the denominator is ALL workouts, so areas are absolute probabilities,
# directly comparable across bands.
# ==========================================================================

min_sustained_mins <- 0.5
window_hrs         <- 24
bw                 <- 1
n_grid             <- 240

{
  # --- 1. Band workouts (identical to backward version) ---------------------
  workout_banded <- workout_events |>
    mutate(
      hr_band = case_when(
        hr_mins_gt_160 >= min_sustained_mins ~ "≥160 bpm",
        hr_mins_gt_150 >= min_sustained_mins ~ "150–160 bpm",
        hr_mins_gt_140 >= min_sustained_mins ~ "140–150 bpm",
        TRUE                                 ~ "<140 bpm"
      ),
      hr_band = fct_relevel(hr_band, "<140 bpm", "140–150 bpm",
                            "150–160 bpm", "≥160 bpm")
    ) #|> filter(start_time < '2025-08-01') # When I started thinking this was a thing
  
  episodes_typical <- migraine_episodes # |> filter(!any_atypical)
  
  # --- 2. Forward join: each workout → its FIRST migraine within window -----
  workout_fwd <- workout_banded |>
    select(workout_id, wkt_time = start_time, hr_band) |>
    cross_join(
      episodes_typical |> select(episode_id, onset_est_local)
    ) |>
    mutate(
      lag_hrs = as.numeric(difftime(onset_est_local, wkt_time, units = "hours")),
      era     = if_else(onset_est_local > ymd("2025-01-01"),
                        "Candesartan", "Pre-Candesartan")
    ) |>
    filter(lag_hrs >= 0, lag_hrs <= window_hrs) |>
    group_by(workout_id) |>
    slice_min(lag_hrs, n = 1, with_ties = FALSE) |>
    ungroup()
  
  # --- 3. Per-band: n workouts (denominator) and n matched (numerator) ------
  band_stats <- workout_banded |>
    count(hr_band, name = "n_workouts") |>
    left_join(
      workout_fwd |> count(hr_band, name = "n_matched"),
      by = "hr_band"
    ) |>
    mutate(
      n_matched  = replace_na(n_matched, 0L),
      p_migraine = n_matched / n_workouts
    )
  
  # --- 4. KDE helper: normalise to integrate to 1 over [0, window], --------
  #        then scale by P(migraine | band). Integral of result = P(migraine).
  scaled_kde <- function(v, p_scale, bw, from, to, n) {
    if (length(v) < 2)
      return(tibble(x = c(from, to), y_scaled = c(0, 0)))
    d  <- density(v, bw = bw, from = from, to = to, n = n)
    dx <- d$x[2] - d$x[1]
    y_norm <- d$y / (sum(d$y) * dx)   # now integrates to ~1 over [from, to]
    tibble(x = d$x, y_scaled = y_norm * p_scale)
  }
  
  band_densities <- band_stats |>
    mutate(
      lag_vals = map(hr_band, \(b) filter(workout_fwd, hr_band == b)$lag_hrs),
      dens     = pmap(list(lag_vals, p_migraine), scaled_kde,
                      bw = bw, from = 0, to = window_hrs, n = n_grid)
    ) |>
    select(hr_band, n_workouts, n_matched, p_migraine, dens) |>
    unnest(dens)
  
  # --- 5. Colours & legend labels with absolute probabilities ---------------
  band_colours <- c(
    "<140 bpm"    = "grey60",
    "140–150 bpm" = "#74A9CF",
    "150–160 bpm" = "#F1A340",
    "≥160 bpm"    = "#DD5511"
  )
  
  # Replace the deframe_from_cols line with:
  label_lookup <- band_stats |>
    mutate(label = sprintf("%s  (%d wkts, %.0f%% → migraine)",
                           hr_band, n_workouts, p_migraine * 100)) |>
    { \(d) setNames(d$label, as.character(d$hr_band)) }()
  
  # label_lookup <- band_stats |>
  #   mutate(label = sprintf("%s  (%d wkts, %.0f%% → migraine)",
  #                          hr_band, n_workouts, p_migraine * 100)) |>
  #   deframe_from_cols(hr_band, label)   # = setNames(.$label, .$hr_band)
  
  # --- 6. Plot --------------------------------------------------------------
  # y = probability density per hour. A peak of 4%/hr means: if you picked
  # a random 1h window at that lag after a workout of this band, you'd have
  # ~4% chance of migraine onset in that hour.
  ggplot(band_densities, aes(x, y_scaled, fill = hr_band, colour = hr_band)) +
    geom_area(alpha = 0.22, position = "identity") +
    geom_line(linewidth = 0.9) +
    geom_rug(data = workout_fwd,
             aes(x = lag_hrs, colour = hr_band),
             inherit.aes = FALSE,
             alpha = 0.8, length = unit(0.03, "npc"), linewidth = 1) +
    scale_x_continuous(
      limits = c(0, window_hrs),
      breaks = seq(0, window_hrs, 4),
      labels = \(x) paste0(x, "h"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = band_colours,
                      labels = label_lookup, drop = FALSE) +
    scale_colour_manual(values = band_colours,
                        labels = label_lookup, drop = FALSE) +
    labs(
      title    = "Workout → migraine: probability-scaled delay density",
      subtitle = paste0(
        sprintf("%d workouts | %d typical episodes | %.0fh forward window | ",
                nrow(workout_banded), nrow(episodes_typical), window_hrs),
        sprintf("threshold sustained \u2265%.1f min\n", min_sustained_mins),
        "Area under each curve = P(migraine within window | HR band)"
      ),
      x      = "Hours from workout end to migraine onset",
      y      = "Migraine probability  (per hour;  area = total P)",
      fill   = "HR band",
      colour = "HR band",
      caption = paste0(
        "Denominator = all workouts in study period. ",
        "Slight downward bias for workouts near period end (no follow-up window). ",
        "Bandwidth = ", bw, "h."
      )
    ) +
    # facet_wrap(~ era, ncol = 1) +   # uncomment to split by Candesartan era
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "right",
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      plot.caption     = element_text(size = 8, colour = "grey50"),
      panel.grid.minor = element_blank()
    )
}



# Version with sampled 'no workout' times ---------------------------------

set.seed(42)   # reproducible no-workout sampling

window_hrs         <- 24
n_grid             <- 240
n_boot             <- 10
hr_thresholds      <- c(150, 160)  # <-- only thing you ever need to change
min_sustained_mins <- 0.25
bw                 <- 1.5

{
    # --- Compute threshold columns from per-activity HR histogram ----------------
  if (is.null(workout_hr_hist))
    stop("workout_hr_hist is NULL.\n",
         "Re-run 2_intervals_ingest.R with the per-activity HR save ",
         "(see 3_data_prep.R header for the one-liner).")
  
  th_asc <- sort(unique(hr_thresholds))
  n_th   <- length(th_asc)
  
  hr_thresh_cols <- map(th_asc, \(th) {
    workout_hr_hist |>
      group_by(workout_id) |>
      summarise(!!sprintf("hr_mins_gt_%d", th) :=
                  sum(seconds[bpm >= th], na.rm = TRUE) / 60,
                .groups = "drop")
  }) |>
    reduce(left_join, by = "workout_id")
  
  workout_events_analysed <- workout_events |>
    select(-any_of(sprintf("hr_mins_gt_%d", th_asc))) |>
    left_join(hr_thresh_cols, by = "workout_id") |>
    mutate(across(starts_with("hr_mins_gt_"), \(x) replace_na(x, 0)))
  
  # --- Band structure (all derived from hr_thresholds) -------------------------
  band_lbls <- c(
    sprintf("\u2265%d bpm", th_asc[n_th]),
    if (n_th > 1) map_chr(seq(n_th - 1, 1),
                          \(i) sprintf("%d\u2013%d bpm", th_asc[i], th_asc[i+1])),
    sprintf("<%d bpm", th_asc[1])
  )
  band_levels <- rev(band_lbls)
  
  band_colours <- setNames(
    rev(colorRampPalette(c("#EEBB00", "#E8650A", "#C0230E", "#6B0504"))(n_th + 1)),
    band_lbls
  )
  
  assign_bands <- function(df) {
    out <- rep(sprintf("<%d bpm", th_asc[1]), nrow(df))
    for (i in seq_len(n_th)) {
      col <- sprintf("hr_mins_gt_%d", th_asc[i])
      lbl <- if (i < n_th) sprintf("%d\u2013%d bpm", th_asc[i], th_asc[i+1]) else
        sprintf("\u2265%d bpm", th_asc[i])
      out[df[[col]] >= min_sustained_mins] <- lbl
    }
    factor(out, levels = band_levels)
  }

  # --- 1. Band workouts -------------------------------------------------------
  workout_banded <- workout_events_analysed |>          # was workout_events
    mutate(hr_band = assign_bands(pick(everything())))
  
  band_levels      <- levels(workout_banded$hr_band)
  episodes_typical <- migraine_episodes
  
  # --- 2. Workout forward join (anchor = start) --------------------------------
  workout_fwd <- workout_banded |>
    select(workout_id, wkt_anchor = start_time, hr_band) |>
    cross_join(episodes_typical |> select(episode_id, onset_est_local)) |>
    mutate(lag_hrs = as.numeric(difftime(onset_est_local, wkt_anchor,
                                         units = "hours"))) |>
    filter(lag_hrs >= 0, lag_hrs <= window_hrs) |>
    group_by(workout_id) |>
    slice_min(lag_hrs, n = 1, with_ties = FALSE) |>
    ungroup()
  
  # --- 3. Per-band no-workout sampling ----------------------------------------
  # For each HR band, sample rest-day times from THAT band's ToD distribution.
  # This controls for the confound that e.g. ≥160 workouts may all happen at 7am.
  study_dates  <- seq.Date(min(as_date(workout_banded$start_time)),
                           max(as_date(workout_banded$start_time)), by = "day")
  no_wkt_dates <- study_dates[!study_dates %in% as_date(workout_banded$start_time)]
  
  tod_of <- function(times)
    hour(times) + minute(times) / 60 + second(times) / 3600
  
  tod_dow_of <- function(times)
    tibble(tod = hour(times) + minute(times)/60 + second(times)/3600,
           dow = wday(times, week_start = 1))
  
  # Build synthetic anchors per band
  # In the nowkt_per_band block, instead of sampling from all no_wkt_dates,
  # sample within matching day-of-week per band
  
  sample_by_dow = FALSE
  if (sample_by_dow) {
    nowkt_per_band <- map_dfr(seq_along(band_levels), \(bi) {
      band      <- band_levels[bi]
      band_ref  <- tod_dow_of(filter(workout_banded, hr_band == band)$start_time)
      
      map_dfr(seq_len(n_boot), \(i) {
        set.seed(i * 17 + bi * 31)
        tibble(date = no_wkt_dates,
               dow  = wday(no_wkt_dates, week_start = 1)) |>
          rowwise() |>
          mutate(
            # Sample time-of-day from workouts of the SAME day-of-week for this band
            sampled_tod = {
              pool <- band_ref$tod[band_ref$dow == dow]
              # Fall back to all days if this dow has no workouts in this band
              if (length(pool) == 0) pool <- band_ref$tod
              sample(pool, 1)
            },
            workout_id = paste0("nw_b", bi, "_s", i, "_", row_number()),
            wkt_anchor = as_datetime(date) + dseconds(round(sampled_tod * 3600)),
            hr_band    = factor(band, levels = band_levels),
            sample_id  = i
          ) |>
          ungroup() |>
          select(workout_id, wkt_anchor, hr_band, sample_id)
      })
    })
  } else {
    nowkt_per_band <- map_dfr(seq_along(band_levels), \(bi) {
      band     <- band_levels[bi]
      band_tod <- tod_of(filter(workout_banded, hr_band == band)$start_time)
      map_dfr(seq_len(n_boot), \(i) {
        set.seed(i * 17 + bi * 31)
        tibble(
          workout_id = paste0("nw_b", bi, "_s", i, "_", seq_along(no_wkt_dates)),
          wkt_anchor = as_datetime(no_wkt_dates) +
            dseconds(round(sample(band_tod, length(no_wkt_dates),
                                  replace = TRUE) * 3600)),
          hr_band    = factor(band, levels = band_levels),
          sample_id  = i
        )
      })
    })
  }
  
  # Forward join per band (avoids one giant 37k × 228 cross_join in memory)
  nowkt_fwd_all <- map_dfr(band_levels, \(band) {
    nowkt_per_band |>
      filter(hr_band == band) |>
      select(workout_id, wkt_anchor, hr_band, sample_id) |>
      cross_join(episodes_typical |> select(episode_id, onset_est_local)) |>
      mutate(lag_hrs = as.numeric(difftime(onset_est_local, wkt_anchor,
                                           units = "hours"))) |>
      filter(lag_hrs >= 0, lag_hrs <= window_hrs) |>
      group_by(workout_id) |>
      slice_min(lag_hrs, n = 1, with_ties = FALSE) |>
      ungroup()
  })
  
  # --- 4. Band stats ----------------------------------------------------------
  band_stats <- workout_banded |>
    count(hr_band, name = "n_workouts") |>
    left_join(workout_fwd |> count(hr_band, name = "n_matched"), by = "hr_band") |>
    mutate(n_matched  = replace_na(n_matched, 0L),
           p_migraine = n_matched / n_workouts)
  
  # Per-band no-workout reference stats (p_migraine = average across n_boot)
  nowkt_band_stats <- nowkt_per_band |>
    count(hr_band, name = "n_total") |>
    left_join(nowkt_fwd_all |> count(hr_band, name = "n_matched"), by = "hr_band") |>
    mutate(n_matched      = replace_na(n_matched, 0L),
           p_migraine_ref = n_matched / n_total)
  
  # --- 5. KDE -----------------------------------------------------------------
  scaled_kde <- function(v, p_scale, bw, from, to, n) {
    if (length(v) < 2) return(tibble(x = c(from, to), y_scaled = c(0, 0)))
    d  <- density(v, bw = bw, from = from, to = to, n = n)
    dx <- d$x[2] - d$x[1]
    tibble(x = d$x, y_scaled = d$y / (sum(d$y) * dx) * p_scale)
  }
  
  # Main densities (workout lags per band)
  band_densities <- band_stats |>
    mutate(
      lag_vals = map(hr_band, \(b) filter(workout_fwd, hr_band == b)$lag_hrs),
      dens     = pmap(list(lag_vals, p_migraine), scaled_kde,
                      bw = bw, from = 0, to = window_hrs, n = n_grid)
    ) |>
    select(hr_band, n_workouts, n_matched, p_migraine, dens) |>
    unnest(dens)
  
  # Reference densities (per-band no-workout, pooled across n_boot for smooth shape)
  ref_densities <- nowkt_band_stats |>
    mutate(
      lag_vals = map(hr_band, \(b) filter(nowkt_fwd_all, hr_band == b)$lag_hrs),
      dens     = pmap(list(lag_vals, p_migraine_ref), scaled_kde,
                      bw = bw, from = 0, to = window_hrs, n = n_grid)
    ) |>
    select(hr_band, p_migraine_ref, dens) |>
    unnest(dens)
  
  # --- 6. Sheet rain: vertical segments just below y = 0 ---------------------
  y_max_overall <- max(band_densities$y_scaled, na.rm = TRUE)
  rain_top      <- -y_max_overall * 0.020   # just below the x-axis
  rain_bottom   <- -y_max_overall * 0.070   # tick height
  
  rain_data <- workout_fwd |>
    select(hr_band, lag_hrs) |>
    mutate(hr_band = factor(hr_band, levels = band_levels))
  
  # --- 7. Reference line annotation (first panel only) -----------------------
  first_band <- band_levels[1]
  ref_first  <- ref_densities |> filter(as.character(hr_band) == first_band)
  
  # Position: just above the reference line near its peak on the right shoulder
  ann_x     <- ref_first$x[which.max(ref_first$y_scaled)] + 1.5
  ann_x     <- 1
  ann_y_ref <- max(ref_first$y_scaled)
  ann_y     <- ann_y_ref + y_max_overall * 0.07
  
  ref_annotation <- tibble(
    hr_band = factor(first_band, levels = band_levels),
    x = ann_x, y = ann_y
  )
  
  # --- 7b. Per-panel no-workout probability label ----------------------------
  # Anchored just above the reference line in the bottom-right of each panel.
  ref_label_pos <- ref_densities |>
    group_by(hr_band) |>
    filter(x >= window_hrs * 0.70) |>              # right-hand portion of curve
    summarise(y_ref_right = mean(y_scaled), .groups = "drop") |>
    left_join(nowkt_band_stats |> select(hr_band, p_migraine_ref),
              by = "hr_band") |>
    mutate(
      label = sprintf("p(migraine)\n= %.0f%%",
                      p_migraine_ref * 100),
      x     = window_hrs * 0.97,
      y     = y_ref_right + y_max_overall * 0.035  # just above the line
    )
  
  # --- 8. Panel labels (replaces legend) -------------------------------------
  panel_labels <- band_stats |>
    mutate(
      label = sprintf("Workouts: %d\nMigraines: %d\np(migraine) = %.0f%%", n_workouts, n_matched, p_migraine * 100),
      x     = window_hrs * 0.97,
      y     = y_max_overall * 0.97
    )
  
  # --- 10. Plot ---------------------------------------------------------------
  band_densities |> 
    ggplot(aes(x, y_scaled, fill = hr_band, colour = hr_band)) +
    geom_area(alpha = 0.20, position = "identity") +
    geom_line(linewidth = 0.9) +
    # Per-band matched no-workout reference
    geom_line(
      data        = ref_densities,
      aes(x = x, y = y_scaled),
      colour      = "black", alpha= 0.3, linetype = "21", linewidth = 1,
      inherit.aes = FALSE
    ) +
    # Sheet rain: vertical tick segments below y = 0
    geom_segment(
      data        = rain_data,
      aes(x = lag_hrs, xend = lag_hrs,
          y = rain_top, yend = rain_bottom,
          colour = hr_band),
      alpha = 0.7, linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    # Panel annotation: n + P (colour matched, top-right)
    geom_text(
      data        = panel_labels,
      aes(x = x, y = y, label = label, colour = hr_band),
      hjust = 1, vjust = 1, size = 3.5, fontface = "bold",
      inherit.aes = FALSE
    ) +
    # Reference line label: first panel only
    geom_text(
      data        = ref_annotation,
      aes(x = x, y = y),
      label       = "matched\ndistribution\nfrom non-\nworkout days",
      colour      = "grey40", size = 2.7, hjust = 0, lineheight = 0.9,
      inherit.aes = FALSE
    ) +
    # No-workout probability label: bottom-right of each panel
    geom_text(
      data        = ref_label_pos,
      aes(x = x, y = y, label = label),
      colour      = "grey45",
      hjust       = 1, vjust = 0,
      size        = 2.8, fontface = "plain",
      inherit.aes = FALSE
    ) +
    facet_wrap(~ hr_band, ncol = 4) +
    coord_cartesian(
      xlim = c(0, window_hrs),
      ylim = c(rain_bottom * 1.8, y_max_overall * 1.08)
    ) +
    scale_x_continuous(
      breaks = seq(0, window_hrs, 4),
      labels = \(x) paste0(x, "h"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(5)(c(0, y_max_overall)),
      labels = \(x) ifelse(x < 0, "", scales::percent(x, accuracy = 0.1)),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values   = band_colours, drop = FALSE) +
    scale_colour_manual(values = band_colours, drop = FALSE) +
    labs(
      title    = "My migraines happen more often 6-8h after an intense workout",
      subtitle = paste0(
        sprintf("%d workouts | %d typical episodes | ", nrow(workout_banded),
                nrow(episodes_typical)),
        sprintf("%.0fh window | anchor = workout start | ", window_hrs),
        "matched timings for non-workout days\n",
        sprintf("Threshold sustained \u2265%.2f min | ", min_sustained_mins),
        "Area under curve = P(migraine within window | HR band)"
      ),
      x       = "Hours from workout start to migraine onset",
      y       = "Migraine probability  (per hour;  area = total P)",
      fill    = NULL, colour = NULL,
      caption = paste0(
        sprintf("Dashed reference: %d-sample pooled KDE per HR band, ", n_boot),
        "rest-day times sampled from that band's own workout time-of-day distribution ",
        "(controls for time-of-day confounding between bands). ",
        "Bandwidth = ", bw, "h."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "none",
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      plot.caption     = element_text(size = 8, colour = "grey50"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}




# Check: distribution of timings of migraines and of workouts. ------------

bind_rows(
  migraine_episodes |>
    transmute(hour = hour(onset_est_local) + minute(onset_est_local)/60,
              type = "Migraine onsets"),
  workout_banded |>
    transmute(hour = hour(start_time) + minute(start_time)/60,
              type = hr_band |> as.character() |> paste("workouts"))
) |>
  ggplot(aes(hour, fill = type)) +
  geom_histogram(binwidth = 1, boundary = 0, colour = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 23, 3),
                     labels = \(x) sprintf("%02d:00", x)) +
  scale_fill_manual(values = c(
    "Migraine onsets"       = "#DD5511",
    "≥160 bpm workouts"     = "#6B0504",
    "150–160 bpm workouts"  = "#C0230E",
    "140–150 bpm workouts"  = "#E8650A",
    "<140 bpm workouts"     = "#EEBB00"
  )) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(title = "Time-of-day distribution: migraines vs workouts",
       x = "Hour of day", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))



# --- Plot 1: hour × day-of-week heatmap — migraines and workouts side by side
bind_rows(
  migraine_episodes |>
    transmute(hour = hour(onset_est_local),
              dow  = wday(onset_est_local, label = TRUE, week_start = 1),
              type = "Migraine onsets"),
  workout_banded |>
    transmute(hour = hour(start_time),
              dow  = wday(start_time, label = TRUE, week_start = 1),
              type = paste0(hr_band, " workouts"))
) |>
  count(type, dow, hour) |>
  ggplot(aes(hour, dow, fill = n)) +
  geom_tile() +
  scale_x_continuous(breaks = seq(0, 23, 3),
                     labels = \(x) sprintf("%02d:00", x)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  facet_wrap(~ type, ncol = 1, scales = "free") +
  labs(title = "Time-of-day × day-of-week",
       x = "Hour", y = NULL, fill = "Count") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"))

# --- Plot 2: migraine rate by day-of-week
# Denominator = total days of each type in the study period
study_spine <- tibble(
  date = seq.Date(min(as_date(workout_banded$start_time)),
                  max(as_date(workout_banded$start_time)), by = "day"),
  dow  = wday(date, label = TRUE, week_start = 1)
)

migraine_dates <- as_date(migraine_episodes$onset_est_local)

study_spine |>
  mutate(had_migraine = date %in% migraine_dates) |>
  group_by(dow) |>
  summarise(n_days = n(),
            n_migraines = sum(had_migraine),
            p = n_migraines / n_days,
            .groups = "drop") |>
  ggplot(aes(dow, p)) +
  geom_col(fill = "#DD5511", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", p * 100)),
            vjust = -0.4, size = 3.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Daily migraine rate by day of week",
       subtitle = "Denominator = all days in study period",
       x = NULL, y = "P(migraine)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


workout_fwd |>
  filter(hr_band == "≥160 bpm") |>
  mutate(dow = wday(as_date(wkt_anchor), label = TRUE, week_start = 1)) |>
  count(dow) |>
  mutate(pct = n / sum(n))

workout_banded |>
  filter(hr_band == "≥160 bpm") |>
  mutate(dow = wday(start_time, label = TRUE, week_start = 1)) |>
  count(dow) |>
  mutate(pct = n / sum(n))

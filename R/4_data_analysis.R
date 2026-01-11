
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

library(tidyverse)


fetch_data = FALSE

if (fetch_data) { 
  source("1 - Oura API.R")
} else {
  load("oura_dataframes.RData")
}



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
  geom_tile(aes(color = factor(first_of_run)), size = 0.8) +  # Outline for 'first of run'
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = NA), 
    na.translate = FALSE, 
    name = "First of Run",  # Legend title
    labels = c("No", "Yes")  # Legend labels
  ) +
  scale_fill_gradientn(
    colors = c("#3333AA", "#1d77a0", "#AADD00", "#EECC00", "#CC2200", "#772222"),
    breaks = seq(0, 24, by = 4),  # Breaks at 6-hour intervals
    values = rescale(c(0, 4, 8, 16, 20, 24)),
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


# Simple smoothed line plot showing migraines per week as metric 
tags |> 
  filter(tag == 'migraine') |> 
  mutate(
    year_week = paste0(year(start_day), "-W", sprintf("%02d", isoweek(start_day)))
  ) |> 
  count(year_week) |>  # Count the number of migraines per week
  ggplot(aes(x = as.Date(paste0(year_week, "-1"), format = "%Y-W%U-%u"), y = n)) +  # Convert year_week to Date
  geom_bar(stat = "identity", fill = "grey70") +  # Bar plot (with grey fill
  geom_smooth(se = FALSE, method = "loess", color = "black") +  # Smoothed line
  labs(
    x = "Week", 
    y = "Number of Migraines", 
    title = "Number of Migraines Per Week"
  ) +
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
  geom_density(adjust = 0.6, colour = "#DD5511", linewidth = 2) +
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

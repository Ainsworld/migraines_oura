# 4b_alcohol_analysis.R
# --------------------------------------------------------------------------
# Exploratory analysis of alcohol consumption.
# ASSUMES 3_data_prep.R has been run in the same session, with the UPDATED
# drink logic, so the following objects exist:
#   - analysis_df    (daily spine; includes drinks_amount, peak_ebac_daily,
#                     peak_ebac_proxy_daily, peak_bac_rate_daily, migraine, ...)
#   - drinks_enriched (per-drink rows with type_clean, comment, start_time_local,
#                     final_drink_amount, duration_hrs, drink_series)
#   - series_metrics  (per-session rows; peak_ebac, peak_bac_rate, ...)
# --------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tidyquant)   # geom_ma / EMA, matching your migraine-frequency chart
library(scales)

orange <- "#DD5511"
grey   <- "grey60"

# Consistent era split (matches 4_data_analysis.R)
era_of <- function(d) if_else(year(d) >= 2025, "2025+ (Candesartan era)",
                              "Pre-2025 (Unmedicated/Sporadic)")

# Rank-based AUC (Mann-Whitney), no pROC dependency.
auc_fast <- function(score, label) {
  ok <- !is.na(score) & !is.na(label)
  score <- score[ok]; label <- as.integer(label[ok])
  n_pos <- sum(label == 1); n_neg <- sum(label == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[label == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# Beta credible interval (matches the qbeta(0.16/0.84) convention in file 4).
beta_ci <- function(successes, n, lo = 0.16, hi = 0.84) {
  a <- 0.5 + successes; b <- 0.5 + (n - successes)
  tibble(prob = successes / n,
         ci_low  = qbeta(lo, a, b),
         ci_high = qbeta(hi, a, b))
}


# ==========================================================================
# 0. Daily drinking frame (true zeros come from analysis_df spine)
# ==========================================================================




drink_daily <- analysis_df |>
  transmute(
    date, migraine,
    era = era_of(date),
    units            = drinks_amount,                  # 0 on dry days
    is_drink_day     = drinks_amount > 0,
    peak_ebac        = peak_ebac_daily,                # simulated Widmark peak
    peak_ebac_proxy  = peak_ebac_proxy_daily,          # old crude proxy (kept for A/B)
    peak_rate        = peak_bac_rate_daily,            # peak dBAC/dt
    any_wine         = drink_any_wine == 1,
    any_beer         = drink_any_beer == 1
  ) |> 
  rowwise() |>
  mutate(
    # Find matching vacation row where current date falls within boundaries
    matching_vacation = list(vacation_intervals |> 
                               filter(date >= start_day & date <= end_day)),
    is_holiday       = nrow(matching_vacation) > 0,
    holiday_comment  = if_else(is_holiday, matching_vacation$comment[1], NA_character_)
  ) |>
  ungroup()


# ==========================================================================
# A. Units per week  (weekly bars + EMA, mirroring migraine-frequency chart)
# ==========================================================================

weekly_units <- drink_daily |>
  mutate(week_date = floor_date(date, "week", week_start = 1)) |>
  group_by(week_date) |>
  summarise(units = sum(units), .groups = "drop") |>
  complete(week_date = seq.Date(min(week_date), max(week_date), by = "week"),
           fill = list(units = 0))

ggplot(weekly_units, aes(week_date, units)) +
  geom_hline(yintercept = 14, linetype = "dashed", colour = orange) +
  annotate("text", x = min(weekly_units$week_date), y = 14.6,
           label = "UK CMO low-risk: 14 units/wk", hjust = 0, size = 3, colour = orange) +
  geom_bar(stat = "identity", fill = grey, alpha = 0.7) +
  geom_ma(ma_fun = EMA, n = 13, colour = "black", linetype = "solid", linewidth = 1) +
  labs(title = "Alcohol units per week",
       subtitle = "Bars = weekly units | Line = 8-week EMA",
       x = "Date", y = "Units / week") +
  theme_minimal()


# ==========================================================================
# B. Drink types over time (monthly stacked bars, with sub-tags where present)
# ==========================================================================

# Sub-tag = the non-numeric descriptor in the comment (e.g. "1.5 white" -> "white").
k <- 5 

drinks_typed <- drinks_enriched |>
  mutate(
    sub_type    = str_squish(str_remove_all(coalesce(comment, ""), "[0-9.]+")) |> str_to_lower(),
    drink_label = if_else(sub_type == "" | is.na(sub_type),
                          type_clean, paste0(type_clean, ": ", sub_type)),
    month_date  = floor_date(as.Date(start_time_local), "month")
  ) |>
  # Count occurrences of each constructed drink_label
  add_count(drink_label, name = "label_count") |>
  # Consolidate labels that don't meet the frequency threshold k
  mutate(
    drink_label = if_else(label_count >= k, drink_label, type_clean)
  ) |>
  # Clean up the intermediate count column
  select(-label_count)

monthly_type <- drinks_typed |>
  group_by(month_date, drink_label) |>
  summarise(units = sum(final_drink_amount), .groups = "drop")

drink_palette <- c(
  # --- THE AMBER/BROWN FAMILY (Beers & Brews) ---
  # Bound by an organic, malty hue-line; distinctly separated from the orange whisky
  "beer"            = "#FFBB00",  # Rich copper ale amber
  "homebrew"        = "#EEAA00",  # Dark, heavy malt brown (stout/porter tone)
  
  # --- THE WINE FAMILY (Red, White, and Rosé clusters) ---
  "wine: red"       = "#780516",  # Deep, unmistakable burgundy
  "wine: port"      = "#4A000B",  # Ultra-dark, high-saturation oxblood
  "wine"            = "#E06666",  # Distinctive vibrant Rosé (serving as the generic anchor)
  "wine: white"     = "#FFF2B2",  # Bright straw-yellow
  "wine: prosecco"  = "#EBF5FB",  # Crisp, effervescent platinum-white
  
  # --- THE SPIRITS & NEUTRALS FAMILY ---
  "alcohol: whisky" = "#EE7712",  # Vivid, luminous honey gold (clean spirit signal)
  "alcohol: gin"    = "#5DADE2",  # Clear, clean botanical ice blue
  "alcohol"         = "#BDC3C7"   # Neutral cool grey for unclassified baseline
)

# 3. Visualization
ggplot(monthly_type, aes(x = month_date, y = units, fill = drink_label)) +
  geom_col(color = "#FFFFFF", size = 0.2) + # Crisp white border to separate stacked blocks
  # Use manual scale with named vector to lock colors to specific string labels
  scale_fill_manual(values = drink_palette) +
  geom_hline(yintercept = 14*52/12, linetype = "dashed", colour = orange) +
  annotate("text", x = min(weekly_units$week_date), y = 14*52/12 * 1.05,
           label = "UK CMO low-risk: 14 units/wk", hjust = 0, size = 3, colour = orange) +
  labs(
    title = "Units by drink type (monthly)",
    subtitle = "Stacked by type and sub-tag where a descriptor was logged",
    x = "Month", 
    y = "Units", 
    fill = "Drink"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Drop vertical grids to let bars breathe chronologically
  )


# ==========================================================================
# C. UK guideline compliance
#    Formal 2016 CMO low-risk metric: <=14 units/week, spread over 3+ days.
#    The "2 (consecutive) alcohol-free days" rule is from PRE-2016 guidance,
#    retained here as an optional, separately-toggled heuristic.
# ==========================================================================

afd_threshold       <- 2   # alcohol-free days target (heuristic)
require_consecutive <- TRUE

longest_dry_run <- function(is_dry) {
  if (length(is_dry) == 0) return(0L)
  r <- rle(is_dry)
  max(c(0L, r$lengths[r$values]), na.rm = TRUE)
}

# Helper function: Find the first Monday of any given year (ISO 8601 standard)
# Jan 4th is mathematically guaranteed to always fall in ISO Week 1
get_first_monday <- function(y) {
  floor_date(as.Date(paste0(y, "-01-04")), "week", week_start = 1)
}

anchor_monday_2024 <- as.Date("2024-01-01")

weekly_compliance <- drink_daily |>
  arrange(date) |>
  mutate(week_date = floor_date(date, "week", week_start = 1)) |> # 1=Monday, 7=Sunday
  group_by(week_date) |>
  summarise(
    units   = sum(units),
    afd     = sum(!is_drink_day),
    afd_run = longest_dry_run(!is_drink_day),
    n_days  = n(),
    .groups = "drop"
  ) |>
  filter(n_days == 7) |>
  mutate(
    afd_ok   = if (require_consecutive) afd_run >= afd_threshold else afd >= afd_threshold,
    units_ok = units <= 14,
    
    # Map explicit 2x2 states into a single clean factor
    status   = case_when(
      units_ok &  afd_ok ~ "Units OK & Dry Days OK",
      units_ok & !afd_ok ~ "Units OK, too few Dry Days",
      !units_ok &  afd_ok ~ "Over 14 Units, but Dry Days OK",
      !units_ok & !afd_ok ~ "Over 14 Units & too few Dry Days"
    ),
    status = factor(status, levels = c(
      "Units OK & Dry Days OK",
      "Units OK, too few Dry Days",
      "Over 14 Units, but Dry Days OK",
      "Over 14 Units & too few Dry Days"
    )),
    status      = factor(status, levels = c("Units OK & Dry Days OK", "Units OK, too few Dry Days", "Over 14 Units, but Dry Days OK", "Over 14 Units & too few Dry Days")),
    # Prepare spatial anchors for plotting
    year        = year(week_date),
    
    # Apply the same exact offset projection to the bars
    normal_date = anchor_monday_2024 + as.numeric(week_date - get_first_monday(year))
  )

# Holidays
vacation_bounds <- tags |>
  filter(tag_type == "vacation") |>
  mutate(
    year = year(start_day),
    
    # Calculate exact days from that year's first Monday, project onto 2024
    xmin_normal = anchor_monday_2024 + as.numeric(start_day - get_first_monday(year)),
    xmax_normal = anchor_monday_2024 + as.numeric(end_day - get_first_monday(year(end_day))),
    
    clean_label = if_else(is.na(comment) | comment == "", "Vacation", comment)
  )

# 2x2 Named Color Palette (Blue/Orange X Dark/Pale Matrix)
compliance_palette <- c(
  "Units OK & Dry Days OK"           = "#A9C9E2",  
  "Units OK, too few Dry Days"       = "#1F77B4",  
  "Over 14 Units, but Dry Days OK"   = "#F5B041", 
  "Over 14 Units & too few Dry Days" = "#D35400"   
)

# --------------------------------------------------------------------------
# Plot 1: Longitudinal Linear Chart
# --------------------------------------------------------------------------
ggplot(weekly_compliance, aes(week_date, units, fill = status)) +
  geom_hline(yintercept = 14, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_col(width = 7) + # Fixed scaling gap
  scale_fill_manual(values = compliance_palette) +
  labs(
    title = "Weekly Guideline Compliance Matrix",
    subtitle = sprintf("Volume cut-off: 14 units | Dry-day target: %d%s alcohol-free days/week",
                       afd_threshold, if (require_consecutive) " consecutive" else ""),
    x = "Timeline", y = "Units / week", fill = "Compliance Status"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# --------------------------------------------------------------------------
# Plot 2: Horizon/Seasonal Linear Calendar Chart
# --------------------------------------------------------------------------

ggplot() +
  # --- LAYER 1: Raw sub-week vacation rectangles (Drawn from raw dates) ---
  geom_rect(
    data = vacation_bounds,
    aes(xmin = xmin_normal, xmax = xmax_normal, ymin = 0, ymax = Inf),
    fill = "#7F8C8D", alpha = 0.12
  ) +
  
  # --- LAYER 2: Guidelines and reference limits ---
  geom_area(data = weekly_compliance, aes(x = normal_date, y = 14), fill = "grey95", alpha = 0.4) +
  geom_hline(yintercept = 14, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  
  # --- LAYER 3: Weekly Compliance Bars ---
  geom_col(data = weekly_compliance, aes(x = normal_date, y = units, fill = status), width = 6) + 
  scale_fill_manual(values = compliance_palette) +
  
  # --- LAYER 4: Clean, vertical annotations tucked in the top-left of the boxes ---
  geom_text(
    data = vacation_bounds,
    aes(
      x = xmin_normal + 1.5,   # Nudge slightly right from the exact start edge
      y = 58,                  # Anchor high near the ceiling of the facet plot area
      label = clean_label
    ),
    angle    = 90,             # Make text perfectly vertical
    hjust    = 1,              # Top-justify text downward from y anchor point
    vjust    = 0.5,            # Center text across the point horizontal
    size     = 2.8, 
    fontface = "italic", 
    colour   = "grey30"
  ) +
  
  # Layout Constraints
  facet_wrap(~year, ncol = 1, strip.position = "right") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) + # Explicit y roof to normalize label heights
  labs(
    title = "Seasonal Trajectory & Ground-Truth Vacation Context",
    subtitle = "Vertical text displays exact entry boundaries and logs from Oura tags.",
    x = "Calendar Season", y = "Total Weekly Units", fill = "Compliance Status"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.y     = element_text(angle = 0, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y  = unit(0.8, "lines"),
    legend.position  = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


# Alcohol-free days per week over time (separate panel, EMA)
ggplot(weekly_compliance, aes(week_date, afd)) +
  geom_hline(yintercept = afd_threshold, linetype = "dashed", colour = orange) +
  geom_bar(stat = "identity", fill = grey, alpha = 0.7) +
  geom_ma(ma_fun = EMA, n = 13, colour = "black", linewidth = 1, linetype = "solid", alpha = 0.7) +
  labs(title = "Alcohol-free days per week",
       subtitle = "Bars = AFDs | Line = 8-week EMA | Dashed = target",
       x = "Date", y = "Alcohol-free days") +
  theme_minimal()


# ==========================================================================
# EXTRA 1. Peak level vs peak RATE-of-rise as migraine predictors
#   Directly tests your mechanistic hypothesis: is it the peak BAC, or the
#   peak rate of change (disequilibrating force) that carries the signal?
# ==========================================================================

drink_days_only <- drink_daily |> filter(is_drink_day)

# Univariate discrimination (next-/same-analysis-day migraine, per your 8am logic)
auc_tbl <- tibble(
  feature = c("Peak EBAC (simulated)", "Peak EBAC (proxy)", "Peak dBAC/dt"),
  auc = c(
    auc_fast(drink_days_only$peak_ebac,       drink_days_only$migraine),
    auc_fast(drink_days_only$peak_ebac_proxy, drink_days_only$migraine),
    auc_fast(drink_days_only$peak_rate,       drink_days_only$migraine)
  )
)
print(auc_tbl)

# Scatter: where do migraine days sit in (level, rate) space?
ggplot(drink_days_only, aes(peak_ebac, peak_rate, colour = migraine)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_colour_manual(values = c(`FALSE` = grey, `TRUE` = orange),
                      labels = c("No migraine", "Migraine")) +
  labs(title = "Peak BAC level vs peak rate of rise",
       subtitle = sprintf("On drinking days | AUC level=%.2f, rate=%.2f",
                          auc_tbl$auc[1], auc_tbl$auc[3]),
       x = "Peak EBAC (g/100ml, simulated)", y = "Peak dBAC/dt (per hr)",
       colour = NULL) +
  theme_minimal()


# ==========================================================================
# EXTRA 2. Migraine risk by drink type (wine / beer / both / other)
#   Same Beta-CI machinery as your HR risk curve. NB: confounded with dose
#   and peak BAC -- treat as descriptive, not causal (see chat notes).
# ==========================================================================

type_risk <- drink_days_only |>
  mutate(drink_type = case_when(
    any_wine &  any_beer ~ "Both",
    any_wine & !any_beer ~ "Wine only",
    !any_wine & any_beer ~ "Beer only",
    TRUE                 ~ "Other"
  )) |>
  group_by(drink_type) |>
  summarise(n = n(), successes = sum(migraine), .groups = "drop") |>
  filter(n >= 5) |>
  # Execute row-by-row on the existing columns
  rowwise() |>
  mutate(ci = list(beta_ci(successes, n))) |>
  ungroup() |>
  # Unpack the list-column into explicit result columns
  unnest_wider(ci)

ggplot(type_risk, aes(reorder(drink_type, prob), prob)) +
  geom_col(fill = grey, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, colour = orange) +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = percent) +
  labs(title = "Migraine probability by drink type",
       subtitle = "68% Beta credible intervals | drinking days only",
       x = NULL, y = "P(migraine)") +
  theme_minimal()


# ==========================================================================
# EXTRA 3. Timing: does a LATE session finish raise risk?
#   Alcohol fragments late sleep; a late last-drink is a plausible distinct
#   pathway from peak BAC. Uses last-drink local hour per session-day.
# ==========================================================================

last_drink_hour <- drinks_enriched |>
  mutate(date = as.Date(floor_date(start_time_local, "day"))) |>
  group_by(date) |>
  summarise(last_hour = hour(max(start_time_local)) +
              minute(max(start_time_local)) / 60, .groups = "drop")

timing_risk <- drink_daily |>
  filter(is_drink_day) |>
  inner_join(last_drink_hour, by = "date") |>
  mutate(hour_bin = cut(last_hour,
                        breaks = c(-Inf, 18, 20, 22, Inf),
                        labels = c("<18:00", "18-20", "20-22", "22:00+"))) |>
  group_by(hour_bin) |>
  summarise(n = n(), successes = sum(migraine), .groups = "drop") |>
  filter(n >= 5) |>
  # Compute the CI row-by-row on the summarized metrics
  rowwise() |>
  mutate(ci = list(beta_ci(successes, n))) |>
  ungroup() |>
  # Unpack the list-column containing the data frame or named list into columns
  unnest_wider(ci)

ggplot(timing_risk, aes(hour_bin, prob)) +
  geom_col(fill = grey, alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, colour = orange) +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = percent) +
  labs(title = "Migraine probability by last-drink time",
       subtitle = "68% Beta credible intervals | drinking days only",
       x = "Local time of last drink", y = "P(migraine)") +
  theme_minimal()

# config.R
# ==========================================================================
# Single source of truth for all personal and tunable parameters.
#
# ▶ NEW USER CHECKLIST:
#   1. Set your date of birth (dob)
#   2. Set your body metrics (weight, sex for Widmark r)
#   3. Review the HR formula and offset — run 3a_hr_normalisation.R and
#      compare predicted max HR against your empirical peaks to tune offset
#   4. Review tag_sets — these match YOUR Oura tag vocabulary; rename to
#      match whatever tags you use
#   5. Review medication_timeline in 3d_migraine_features.R (or delete that
#      script entirely if you are not modelling a health condition)
#
# API keys do NOT go here. They live in ~/.Renviron:
#   OURA_API_KEY=...
#   INTERVALS_API_KEY=...
#   INTERVALS_ATHLETE_ID=...
# ==========================================================================

# --- Identity -----------------------------------------------------------------
# Used for age-adjusted HR calculations in 3a_hr_normalisation.R
dob <- as.Date("1976-04-09")   # ← CHANGE THIS

# --- Date range ---------------------------------------------------------------
# Oura and Intervals ingests use these bounds.
first_date <- as.Date("2022-11-03")   # ← date your Oura ring data starts
last_date  <- Sys.Date() - 1

# --- Day transition -----------------------------------------------------------
# "Today" rolls over at this hour (local time). Affects migraine attribution,
# alcohol session dating, and the daily spine.
day_transition_hrs <- 8   # 08:00 local

# --- Heart Rate normalisation -------------------------------------------------
# Formula choices: "tanaka" (recommended), "fox", "gelish"
#   Tanaka (2001): 208 - 0.7 * age   — meta-analytic, best validated
#   Fox    (1971): 220 - age         — ubiquitous but high individual variance
#   Gelish (2007): 207 - 0.7 * age   — near-identical to Tanaka
#
# offset: additive shift (bpm) applied after the formula to match your
#   empirical peak HR. Start at 0, then adjust if predicted max HR is
#   consistently above or below your observed peaks.
#   Example: if Tanaka predicts 172 but you routinely hit 177, set offset = 5.
hr_max_formula <- "tanaka"
hr_max_offset  <- 0

# %maxHR thresholds used in exercise feature engineering (3b).
# Default bands align approximately with classic zone boundaries:
#   ~82% maxHR ≈ high aerobic / zone 4
#   ~88% maxHR ≈  the VT2/LT2 boundary — the theoretically most 
#             interesting threshold for a migraine trigger hypothesis, since 
#             this is where sympathetic activation, cortisol, and catecholamine 
#             response escalate sharply
#   ~94% maxHR ≈ severe domain, capturing genuinely maximal efforts
hr_pct_thresholds <- c(0.82, 0.88, 0.94)

# Minimum sustained seconds at threshold to count as a "peak" (not a spike)
peak_min_secs <- 15

# Resting HR default (used when Oura readiness data is unavailable)
rhr_default <- 50

# --- Body metrics (Alcohol / EBAC) --------------------------------------------
# Used in the Widmark EBAC simulation (3c_alcohol_features.R).
body_weight_kg <- 74      # ← CHANGE THIS
# Widmark r distribution factor: ~0.68 male, ~0.55 female
widmark_r      <- 0.68    # ← adjust for sex
# Alcohol elimination rate in g/100ml per hour (standard Widmark)
elimination_rate <- 0.015
# Grams of pure alcohol per "drink amount unit" in your tag system
grams_per_drink_amount <- 8.0
# Defaults when amount/duration not logged
default_amount           <- 2
default_consumption_time <- 0.5   # hours

# Drink session parameters
drink_series_hrs <- 2.1   # gap > this = new session
min_span_hrs     <- 1/60
max_span_hrs     <- 8

# --- Migraine / episode logic -------------------------------------------------
# Only relevant if running 3d_migraine_features.R.
# ▶ If you are not modelling migraines, skip that script entirely.
episode_merge_hrs     <- 8      # triptans within this gap = same episode
migraine_min_span_hrs <- 5/60   # minimum logged span to treat as real onset data
migraine_max_span_hrs <- 72
typical_buildup_hrs   <- 1.5      # assumed onset-to-triptan lag for point tags

# --- Altitude -----------------------------------------------------------------
altitude_merge_hrs <- 48   # altitude tags within this gap = one exposure block

# --- Medication timeline ------------------------------------------------------
# ▶ PERSONAL — edit or replace for your own medication history.
# treatment_dose is a relative scale (1 = standard dose); adapt as needed.
# ▶ If you are not modelling a health condition, leave this as an empty tibble:
#   medication_timeline <- tibble(treatment_start = as.Date(character()),
#                                 treatment_end   = as.Date(character()),
#                                 treatment       = character(),
#                                 treatment_dose  = numeric())
library(tibble)   # loaded here so config.R is self-contained
medication_timeline <- tribble(
  ~treatment_start, ~treatment_end,          ~treatment,        ~treatment_dose,
  "2023-06-14",     "2023-08-20",            "Candesartan 8mg", 0.5,
  "2024-12-18",     "2024-12-22",            "Candesartan 8mg", 0.5,
  "2024-12-23",     "2024-12-28",            "Candesartan 8mg", 1,
  "2024-12-29",     "2025-01-12",            "Candesartan 8mg", 1.5,
  "2025-01-13",     "2025-08-28",            "Candesartan 8mg", 2,
  "2025-08-29",     "2026-03-19",            "Candesartan 8mg", 1.5,
  "2026-03-19",     "2026-05-17",            "Candesartan 8mg", 2,
  "2026-05-17",     as.character(Sys.Date()), "Candesartan 8mg", 1.5,
) |>
  mutate(treatment_start = as.Date(treatment_start),
         treatment_end   = as.Date(treatment_end))

# --- Tag vocabularies ---------------------------------------------------------
# ▶ Edit these to match your own Oura tag names.
# Keys are used throughout the pipeline; values are the tag strings from Oura.
tag_sets <- list(
  coffee     = c("coffee"),
  stress     = c("stress", "anxiety", "party", "socialgathering"),
  travel     = c("travel", "airplane", "car", "train"),
  plane      = c("airplane"),
  highalt    = c("highaltitude"),
  lowpress   = c("airplane", "highaltitude"),
  hydration  = c("hydration", "electrolyte", "electrolytes", "rehydrate"),
  missed_med = c("Missed Migraine medicine", "missed migraine medicine")
)

# Alcohol tag variations — any tag matching these is treated as a drink event
alcohol_variations <- c("alcohol", "beer", "wine", "liquor", "Homebrew")

# --- Continuous-time grid -----------------------------------------------------
grid_unit <- "hour"   # resolution of the hourly hazard spine in 3f_assemble.R

# --- Derived (do not edit) ----------------------------------------------------
widmark_factor <- body_weight_kg * widmark_r * 10
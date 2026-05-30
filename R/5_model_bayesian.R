library(tidyverse)
library(brms)       # The engine
library(bayesplot)  # For diagnostics
library(tidybayes)  # For tidy posterior manipulation
library(modelr)     # For data grid generation

# --- 1. Load & Scale Data -----------------------------------------------------
analysis_df <- read_rds("data/analysis_ready.rds")

# Prepare scaling identical to frequentist approach for comparability
model_df <- analysis_df |>
  mutate(
    # Scaling to "Human Units"
    hr_10mins_gt_150 = hr_mins_gt_150 / 10,
    impulse_10bh = net_cardiac_impulse / 10,     
    ebac_drink   = peak_ebac_daily / 0.02,       
    badsleep_10  = badsleep_d,                   
  )

# --- 2. Define Priors (Evidence-Based) ----------------------------------------

#' Here's what Claude thought as a summary of research evidence, useful to inform priors:
#'Let me help analyze the potential priors for your Bayesian model based on published research. Note that I'll be drawing from research up to early 2024, and you should verify these specific numbers against current literature.
# For episodic migraines (most research focuses on these rather than chronic migraines):
#   Frequency and Clustering:
#   
#   Research suggests migraines do tend to cluster, with increased probability of occurrence within 1-3 days of a previous migraine
# Baseline frequency prior could center around 1-4 migraines per month (median from population studies)
# After 2 weeks without a migraine, probability tends toward the baseline rather than increasing
# 
# Day of Week:
#   
#   Studies show a slight increase in probability for weekdays vs weekends (about 20% higher)
# Monday typically shows the highest relative risk (around 1.2x compared to Sunday)
# 
# Transportation:
#   
#   Long car/train journeys: Studies suggest about 25-30% of migraineurs report these as triggers
# Air travel: Research links this to increased risk, particularly during descent (pressure changes), with roughly 20-40% reporting it as a trigger
# Consider using a relative risk multiplier of 1.5-2x for days with extended travel
# 
# Coffee:
#   
#   Complex relationship - both protective and triggering effects reported
# Withdrawal is a strong trigger (relative risk ~2-3x)
# Regular moderate consumption (1-2 cups) shows slight protective effect
# High consumption (>3 cups) shows increased risk
# 
# Candesartan:
#   
#   Clinical trials show reduction in migraine days by 30-50%
# Consider a multiplier of 0.5-0.7 on baseline probability when medication is consistent
# Takes 4-6 weeks to reach full effect
# 
# Activity Levels:
#   
#   High intensity: Mixed evidence - can be protective or triggering
# Moderate activity: Generally protective, reducing frequency by 20-30%
# Sedentary behavior: Associated with increased risk (~1.2-1.5x)
# Sleep: Strong U-shaped relationship
# 
# Optimal 7-8 hours shows lowest risk
# Both <6 and >9 hours associated with ~2x risk increase
# 
# Alcohol:
#   
#   Overall relative risk increase of 1.5-2x for any alcohol consumption
# Wine shows strongest association (2-3x risk increase)
# Beer shows lower but still significant risk (1.3-1.5x)
# Dose-dependent relationship: Each drink increases risk by approximately 40%
# Time course: Risk peaks 4-12 hours after consumption


# Rationale derived from migraine epidemiology literature:
# 1. Base Rate: ~5-10% daily probability -> Intercept ~ -2.5.
# 2. Cluster Effect: Migraines cluster. 'Yesterday' is a strong predictor -> Positive Prior.
# 3. Weekend Effect: "Let-down" headaches are common -> Weak Positive Prior.
# 4. Medication: Candesartan is effective -> Strong Negative Prior.
# 5. Alcohol: Established trigger -> Moderate Positive Prior.
# 6. HR Stress: Ambiguous in literature (trigger vs protective) -> Skeptical (Mean 0).

priors_main <- c(
  # Intercept: Centered on ~7% probability, allowing variance.
  set_prior("normal(-2.5, 1)", class = "Intercept"),
  
  # Known strong predictors (Informative)
  set_prior("normal(1.0, 0.5)", class = "b", coef = "migraine_yesterdayTRUE"),
  set_prior("normal(-1.0, 0.5)", class = "b", coef = "medication_dose"),
  
  # Suspected Lifestyle Factors (Weakly Informative)
  set_prior("normal(0.5, 0.5)", class = "b", coef = "ebac_drink"),
  
  # Physiological Stress (Skeptical / Regularizing)
  # We assume no effect (0) unless data proves otherwise, to avoid overfitting noise.
  set_prior("normal(0, 0.5)", class = "b", coef = "hr_10mins_gt_150"),
  set_prior("normal(0, 0.5)", class = "b", coef = "badsleep_10"),
  
  # Default for others
  set_prior("normal(0, 1)", class = "b") 
)


# --- 3. Model Fitting ---------------------------------------------------------

# M1: Physiology & Controls (The "Vascular Load" Hypothesis)
# Iterations: 4000 (2000 warm-up) usually sufficient for logistic.
m_bayes_physio <- brm(
  migraine ~ 
    is_saturday + 
    migraine_yesterday + 
    weeks_since_last_max1 +
    medication_dose +
    hr_10mins_gt_150 + 
    ebac_drink + 
    badsleep_10,
  data = model_df,
  family = bernoulli(link = "logit"),
  prior = priors_main,
  chains = 4, cores = 4, seed = 123,
  file = "models/m_bayes_physio" # Cache the model
)

# M2: The "Threshold Shift" Interaction
# Does Medication flatten the slope of HR and Alcohol?
m_bayes_inter <- brm(
  migraine ~ 
    is_saturday + 
    migraine_yesterday + 
    weeks_since_last_max1 +
    badsleep_10 +
    # Interaction Terms implicitly include main effects
    hr_10mins_gt_150 * medication_dose +
    ebac_drink * medication_dose,
  data = model_df,
  family = bernoulli(link = "logit"),
  prior = priors_main,
  chains = 4, cores = 4, seed = 123,
  file = "models/m_bayes_inter"
)

# --- 4. Diagnostics & Comparison ----------------------------------------------

# Compare models using LOO-CV (Leave-One-Out Cross-Validation)
# This estimates out-of-sample predictive accuracy.
loo_m1 <- loo(m_bayes_physio)
loo_m2 <- loo(m_bayes_inter)

loo_compare(loo_m1, loo_m2) |> print()

# Posterior Predictive Check (Bars)
pp_check(m_bayes_physio, type = "bars", ndraws = 100) +
  labs(title = "Posterior Predictive Check", subtitle = "Does the model simulate realistic migraine counts?")

# --- 5. Posterior Analysis (The "So What?") -----------------------------------

# A. Visualizing Coefficients (Forest Plot equivalent)
# We plot the 67% and 95% credible intervals of the Odds Ratios.
m_bayes_physio |>
  gather_draws(`b_.*`, regex = TRUE) |>
  filter(!.variable %in% c("b_Intercept")) |>
  mutate(odds_ratio = exp(.value)) |>
  ggplot(aes(y = .variable, x = odds_ratio)) +
  stat_pointinterval(.width = c(0.67, 0.95)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  labs(
    title = "Bayesian Posterior Distributions",
    x = "Odds Ratio",
    y = "Parameter"
  ) +
  theme_minimal()

# B. Hypothesis Testing (Probability of Direction)
# "What is the probability that Medication reduces the HR effect?"
# We inspect the interaction term `b_impulse_10bh:any_medicationTRUE`.
post_samples <- as_draws_df(m_bayes_inter)

prob_med_reduces_hr_risk <- mean(post_samples$`b_hr_10mins_gt_150:medication_dose` < 0)
prob_med_reduces_alc_risk <- mean(post_samples$`b_medication_dose:ebac_drink` < 0)

message(sprintf("Probability that Medication reduces HR sensitivity: %.1f%%", prob_med_reduces_hr_risk * 100))
message(sprintf("Probability that Medication reduces Alcohol sensitivity: %.1f%%", prob_med_reduces_alc_risk * 100))

# --- 6. Conditional Effects Plot (The "Risk Curve") ---------------------------

# We generate the "Risk Curve" for HR, split by Medication Status.
# This replaces the frequentist `interact_plot`.

# Generate grid of data for plotting
viz_grid <- model_df |>
  data_grid(
    hr_10mins_gt_150 = seq_range(hr_10mins_gt_150, n = 50),
    medication_dose = seq(0,2,0.5),
    # Set other predictors to median/mode baseline
    is_saturday = FALSE,
    migraine_yesterday = FALSE,
    weeks_since_last_max1 = median(weeks_since_last_max1),
    ebac_drink = 0,
    badsleep_10 = 0
  )

# Add fitted draws (Predictive Uncertainty)
viz_draws <- viz_grid |>
  add_epred_draws(m_bayes_inter, ndraws = 100) # epred = Expectation of the Posterior (Probability)

# Plot
ggplot(viz_draws, aes(x = hr_10mins_gt_150, y = .epred, color = medication_dose, fill = medication_dose)) +
  stat_lineribbon(.width = 0.67, alpha = 0.2) + # 67% CI ribbon
  labs(
    title = "Bayesian Interaction Effect",
    subtitle = "Does medication flatten the risk curve of High Cardiac Impulse?",
    x = "Cardiac Impulse (10 Beat-Hours)",
    y = "Probability of Migraine",
    color = "Status", fill = "Status"
  ) +
  theme_minimal()

library(tidyverse)
library(jtools)   # For summ() and plotting
library(broom)    # For tidy model outputs
library(ggstance) # For horizontal effect plots if needed

# --- 1. Load Data -------------------------------------------------------------
analysis_df <- read_rds("data/analysis_ready.rds")

# --- 2. Pre-processing & Scaling ----------------------------------------------
# We scale continuous variables to "Human Units" so coefficients are readable.
# - Cardiac Impulse: Unit = 10 Excess Beat-Hours (a significant workout)
# - EBAC: Unit = 0.02% (approx 1 standard drink spike)
# - Sleep: Unit = 10 points (a full letter grade change in score)

model_df <- analysis_df |>
  mutate(
    # Scaling
    impulse_10bh = net_cardiac_impulse / 10,     # Coeff = Risk per 10 beat-hours
    ebac_drink   = peak_ebac_daily / 0.02,       # Coeff = Risk per ~1 drink
    badsleep_10  = badsleep_d,                   # Already scaled 10-points in prep
    hr_10mins_140_150 = hr_mins_140_150 / 10,
    hr_10mins_150_160 = hr_mins_150_160 / 10,
    hr_10mins_gt_150 = hr_mins_gt_150 / 10,
    hr_10mins_gt_160 = hr_mins_gt_160 / 10,
    hr_impulse_140_s = 2 * hr_impulse_140 / max(hr_impulse_140),
    hr_impulse_150_s = 2 * hr_impulse_150 / max(hr_impulse_150),
    hr_impulse_160_s = 2 * hr_impulse_160 / max(hr_impulse_160),
    
    
    # Ensure factors are clean
    is_saturday = as.logical(is_saturday),
    migraine_yesterday = as.logical(migraine_yesterday)
  )

# --- 3. Model Specification ---------------------------------------------------

# M1: Base Controls (Known Factors)
# Testing the "Life Context" hypothesis.
m1_base <- glm(
  migraine ~ 
    is_saturday + 
    migraine_yesterday + 
    weeks_since_last_max1,
  data = model_df,
  family = binomial(link = "logit")
)

# M2: Physiology 1 - Addition of ingested items
m2_physio1 <- update(m1_base, . ~ . + 
                      medication_dose + 
                      any_plane + 
                      any_highaltitude +
                      extra_coffee + 
                      ebac_drink 
)

# M3: Physiology 2 - Activity
m3_physio2 <- update(m2_physio1, . ~ .  +
                       activity_duration_mins + 
                       hr_10mins_gt_150
                       
)

# M3: Physiology 2 - Activity
m3_physio2b <- update(m2_physio1, . ~ .  +
                        activity_duration_mins + 
                        hr_impulse_150_s
                     
)

# M4: Interaction (The "Perfect Storm" Hypothesis)
# Does Alcohol lower the threshold for HR triggers?
m4_inter <- update(m2_physio1, . ~ . +
                     hr_10mins_gt_150 * ebac_drink
)

# --- 4. Model Comparison & Diagnostics ----------------------------------------

# A. Statistical Summary (Table)
# VIF checks for multicollinearity
export_summs(m1_base, m2_physio1, m3_physio2, m3_physio2b, m4_inter, 
             scale = TRUE, 
             error_format = "[{conf.low}, {conf.high}]",
             model.names = c("Base Controls", "Substances", "Activity", "Activity v2", "Inter"))

# B. Visual Coefficient Plot (Forest Plot)
# Shows Odds Ratios. If CI crosses 1, effect is not significant.
plot_summs(m1_base, m2_physio1, m3_physio2, m3_physio2b, 
           exp = TRUE, # Plot Odds Ratios
           inner_ci_level = 0.8, # Thick line = 80% CI
           colors = c("grey50", "#1f77b4", "#d62728", "darkorange")) +
  labs(
    title = "Migraine Trigger Analysis",
    subtitle = "Odds Ratios (Right = Risk Increase, Left = Protective)",
    x = "Odds Ratio (Log Scale)"
  ) +
  scale_x_log10(limits = c(0.1, NA), oob = scales::squish) +
  theme_minimal()

# --- 5. Effect Interpretation (Key Variables) ---------------------------------

# Visualizing the "Dose-Response" for Cardiac Impulse
effect_plot(m3_physio2, pred = hr_10mins_gt_150, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05) +
  labs(title = "Effect of Cardiac Load", 
       x = "10s of mins HR > 150 ", y = "Migraine Probability")

# Visualizing the "Dose-Response" for Alcohol
effect_plot(m3_physio2, pred = ebac_drink, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05) +
  labs(title = "Effect of Peak Alcohol (EBAC)", 
       x = "Drinks Equivalent (approx)", y = "Migraine Probability")

# If Interaction is significant (p < 0.10), plot it
if (summary(m4_inter)$coefficients["impulse_10bh:ebac_drink", "Pr(>|z|)"] < 0.15) {
  interact_plot(m4_inter, pred = hr_10mins_gt_150, modx = ebac_drink, interval = TRUE) +
    labs(
      title = "Interaction: Does Alcohol amplify HR Risk?",
      x = "10s of mins HR > 150",
      y = "Predicted Probability"
    )
}

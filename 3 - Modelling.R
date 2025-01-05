source("2 - Data Prep.R")


# For model interpretation
library(jtools)
library(patchwork)

# For LASSO modelling
library(glmnet)



#library(MASS)

# Define functions used elsewhere
all_effect_plots <- function(model, partial = TRUE) {
  # Get the list of predictors from the model
  predictors <- attr(terms(model), "term.labels")
  
  # Generate effect plots for each predictor
  effect_plots <- map(predictors, function(var) {
    effect_plot(
      model,
      pred = !!sym(var),  # Use rlang to refer to variable name
      jitter = c(0.2, 0.05),
      interval = TRUE,
      plot.points = TRUE,
      partial.residuals = partial
    ) +
      labs(title = paste0(if_else(partial, "Partial ", ""), "Effect: ", var), x = var)
  })
  
  # Display the plots (e.g., print the first one)
  #print(effect_plots[[1]])
  
  # Optionally, arrange plots in a grid
  combined_plot <- wrap_plots(effect_plots)
  print(combined_plot)
}



# Fit logistic regression model -------------------------------------------




# Correlations
library(GGally)
ggpairs(model_data_days, 
        columns = c("is_weekend", "migraine_yesterday", "days_since_last", 
                    "high_activity_hrs", "med_activity_hrs", "low_activity_hrs", "sedentary_hrs", "resting_hrs", 
                    "drink_sessions_gt1", "drinks_any" ))



#' Notes of things found...
#' - Using multiple drink-related variables dilutes the significance levels to insignificant for all
#' - The two with strongest relationships (though collinear with each other)
#'   are any_drink and drink_sessions_gt1  
#' - When added in, looks like beer marginally lowers risk, but not significant.


# Fit the logistic regression model
model <- glm(
  migraine ~ 
    #is_weekend + 
    is_saturday +
    is_sunday +
    extra_coffee +
    any_stress +
    any_medication +
    #any_travel +
    any_plane +
    #weekday +
    migraine_yesterday +
    weeks_since_last +
    weeks_since_last_max1 +
    unreadiness_d +
    badsleep_d +
    activity_d +
    #active_cals100 +
    activity_high_hrs +
    #activity_medhigh_hrs +
    activity_med_hrs +
    activity_low_hrs +
    activity_sedentary_hrs_c +
    activity_resting_hrs_c +
    #drinks_total + 
    drink_sessions_gt1 +
    drink_session_biggest +
    #drink_session_biggest_gt1 +
    drinks_any +
    drink_any_wine +
    drink_any_beer 
  , data = model_data_days, 
  family = binomial(link = "logit")
)



# Summarize the model
summary(model)

# Jtools summary
summ(model, exp = TRUE, vif = TRUE)
# Exponentiated coeffients, which makes them odds ratios (1 = no effect)

plot_summs(model, exp = TRUE, inner_ci_level = 0.67) +
  scale_x_log10(breaks = scales::breaks_log(n = 10)) +
  labs(title = "Migraine model", x = "Odds ratio")

all_effect_plots(model)

# effect_plot(model, 
#             pred = weekday, 
#             jitter = c(0.35, 0.02),
#             interval = TRUE, plot.points = TRUE,
#             partial.residuals = TRUE) 




# Select more parsimonious model ------------------------------------------

# Perform stepwise selection with AIC
  # (Avoid loading MASS as conflicts with dplyr on use of select())
stepwise_model <- MASS::stepAIC(model, direction = "both")

# Summarize the result
summ(stepwise_model, exp = TRUE, vif = TRUE)


plot_summs(stepwise_model, model, 
           model.names = c("AIC Stepwise", "Full"),
           exp = TRUE, inner_ci_level = 0.67) +
  scale_x_log10(breaks = scales::breaks_log(n = 10)) +
  labs(title = "Migraine model", x = "Odds ratio")


# LASSO version -----------------------------------------------------------

#  Key insights from using LASSO:
# - Model complexity indicated from k-fold cross-validation (~16 substantive vars)
# - Drinks: GT1 is strongest, also in are Wine and having anything
#

# Prepare data
X <- model_data_days %>%
  select(-c(starts_with("migraine"), 
            starts_with("days"), 
            date, prev_migraine), 
         migraine_yesterday) %>%
  makeX(na.impute = TRUE)

Y <- model_data_days$migraine

# Fit the LASSO model
lasso_model <- cv.glmnet(X, Y, 
                         nfolds = 20,
                         family = "binomial", alpha = 1)

# View results
plot(lasso_model)

# View variable importance and coefficients
coef(lasso_model, s = "lambda.min")

# Extract non-zero coefficients for the optimal lambda
lasso_coef <- coef(lasso_model, s = "lambda.min") |> 
  as.matrix() |> 
  as_tibble(rownames = "variable") |> 
  rename(value = s1) |> 
  filter(abs(value) >= 1E-3)

# Create a bar plot
lasso_coef |> 
  filter(variable != "(Intercept)") |> 
  mutate(abs_value = abs(value)) |> 
  arrange(desc(abs_value)) |> 
  ggplot(aes(x = reorder(variable, abs_value), y = value, fill = value > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "LASSO Coefficients",
    x = "Variable",
    y = "Coefficient Value"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  theme(legend.position = "top")


# Hand-crafted model -----------------------------------------------------
# Variables in here for the following reasons:
# - Selected clearly by AIC stepwise or LASSO (e.g. Saturday, drink_sessions_gt1)
# - Stablemates to those variables (e.g. Sunday, drink_session_biggest)
#   (either collinear or conceptually related)

model_final <- glm(
  migraine ~ 
    is_saturday + 
    is_sunday + 
    any_plane +
    any_medication +
    extra_coffee +
    migraine_yesterday +
    weeks_since_last +
    activity_high_hrs +
    activity_med_hrs +
    activity_sedentary_hrs_c +
    activity_resting_hrs_c +
    drink_session_biggest +
    drinks_any +
    drink_any_wine,
  
    data = model_data_days, 
  family = binomial(link = "logit")
)


# Jtools summary
summ(model_final, exp = TRUE, vif = TRUE)

plot_summs(model_final, 
           model, 
           stepwise_model, 
           model.names = c("Final", "Full", "AIC Stepwise"),
           exp = TRUE, inner_ci_level = 0.67) +
  scale_x_log10(breaks = scales::breaks_log(n = 15)) +
  coord_cartesian(xlim = c(0.3, 7)) +
  labs(title = "Migraine model", x = "Odds ratio")


all_effect_plots(model_final)







# Looking at interactions -------------------------------------------------

model_inter <- glm(
  migraine ~ 
    is_saturday *
    migraine_yesterday *
    drinks_any,
  data = model_data_days, 
  family = binomial(link = "logit")
)


summary(model_inter)
summ(model_inter, exp = TRUE, vif = TRUE)

plot_summs(model_inter, 
           exp = TRUE, inner_ci_level = 0.67) +
  scale_x_log10(breaks = scales::breaks_log(n = 8)) +
  labs(title = "Migraine model", x = "Odds ratio")


all_effect_plots(model_final)

all_effect_plots(model_final, partial = FALSE)




# View calibration / accuracy of model ------------------------------------

# Get predicted probabilities
pred <- model_data_days %>%
  mutate(predicted_prob = predict(model_final, type = "response"))

# Create bins for predicted probabilities
calibration_data <- pred %>%
  mutate(bin = cut(predicted_prob, breaks = seq(0.025, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(
    mean_predicted = mean(predicted_prob),
    observed_proportion = mean(migraine),
    n = n()
  ) %>%
  filter(!is.na(bin))  # Remove empty bins

# Calculate binomial confidence intervals for each bin
calibration_data <- calibration_data %>%
  mutate(
    # Calculate the number of successes in each bin
    successes = round(observed_proportion * n),
    # Get binomial confidence intervals using binom.test
    ci = purrr::pmap(list(successes, n), ~binom.test(.x, .y, conf.level = 0.67)$conf.int),  # Use pmap to apply binom.test correctly
    ci_low = purrr::map_dbl(ci, 1),   # Lower bound of confidence interval
    ci_high = purrr::map_dbl(ci, 2)   # Upper bound of confidence interval
  )


# Plot calibration
ggplot(calibration_data, aes(x = mean_predicted, y = observed_proportion, label = n)) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_text(size = 3, hjust = 0, vjust = 0.5, nudge_x = 0.01) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.00,
    color = "black", alpha = 0.2, linewidth = 1
  ) +
  geom_point(aes(size = n), alpha = 0.5) +
  scale_size_area(max_size = 10) +
  scale_x_continuous(breaks = scales::breaks_width(0.2)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Calibration Plot",
    subtitle = "Error bars show 67% confidence intervals\n(A well-calibrated model will see ~67% of confidence lines overlap the diagonal)",
    x = "Mean Predicted Probability",
    y = "Observed Proportion"
  ) +
  theme_minimal()





# Bayesian version --------------------------------------------------------

library(brms)
library(bayesplot)

prior <- c(
  set_prior("normal(0, 2)", class = "b"),      # Priors for coefficients
  set_prior("normal(0, 3)", class = "Intercept") # Prior for intercept
)

# priors <- c(
#   set_prior("normal(1, 1)", class = "b", coef = "high_activity_hrs"),  # Positive effect
#   set_prior("normal(-1, 1)", class = "b", coef = "weeks_since_last"), # Negative effect
#   set_prior("normal(0.5, 1)", class = "b", coef = "is_saturday"),     # Positive
#   set_prior("normal(0, 2)", class = "b")                              # Weakly informative for others
#)


bayes_model <- brm(
  migraine ~ 
    is_saturday + 
    is_sunday + 
    any_plane +
    any_medication +
    extra_coffee +
    migraine_yesterday +
    weeks_since_last +
    activity_high_hrs +
    activity_med_hrs +
    activity_low_hrs +
    activity_sedentary_hrs_c +
    activity_resting_hrs_c +
    drink_sessions_gt1 +
    drink_session_biggest +
    drinks_any +
    drink_any_wine
  ,
  data = model_data_days,
  family = bernoulli(link = "logit"),
  prior = prior
)

# Check priors
prior_summary(bayes_model)       

# Summary of posterior distributions
summary(bayes_model)

# Plot posterior distributions
plot(bayes_model)

# Posterior predictive checks
pp_check(bayes_model, type = "bars")



# Posterior samples

posterior_samples <- as.array(bayes_model)
parameter_names <- dimnames(posterior_samples)$variable
filtered_parameters <- parameter_names[!parameter_names %in% c("Intercept", "lprior", "lp__")]
posterior_clean <- posterior_samples[,,filtered_parameters]

posterior_samples |> view()

# Plot with bayesplot
mcmc_areas(posterior_clean, prob = 0.67, prob_outer = 0.99, area_method = "scaled height") +
  scale_x_continuous(breaks = scales::breaks_width(1), limits = c(-3, 2))

# Odds ratio version
mcmc_areas(posterior_clean, transformations = exp,
           point_est = "median",
           prob = 0.67, prob_outer = 0.95, area_method = "scaled height") +
  #scale_x_continuous(breaks = scales::breaks_width(0.5), limits = c(0, 3.5)) +
  scale_x_log10(breaks = scales::breaks_log(n = 10), limits = c(0.1, 4)) +
  geom_vline(xintercept = 1) +
  labs(x = "Odds ratio (1 = no change)")
  

# Generate a pairs plot for the first few parameters
params = c("b_Intercept",
           "b_is_saturdayTRUE", 
           "b_drinks_anyTRUE",
           "b_migraine_yesterdayTRUE",
           "b_med_activity_hrs",
           "b_high_activity_hrs")
mcmc_pairs(posterior_clean, pars = params)

mcmc_pairs(as_draws_matrix(posterior_clean)[1:100,,]) # Use fewer obs to make rendering quicker

mcmc_scatter(posterior_samples, pars = c("b_Intercept","b_sedentary_hrs")) 


# Overlaid prior and posterior for multiple parameters (e.g., "b_is_saturday", "b_any_travel")
prior <- get_prior(bayes_model)



mcmc_intervals(posterior_samples, regex_pars = "^b_")

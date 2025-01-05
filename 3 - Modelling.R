source("2 - Data Prep.R")


# For model interpretation
library(jtools)
library(patchwork)

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
    any_travel +
    any_plane +
    #weekday +
    migraine_yesterday +
    weeks_since_last +
    weeks_since_last_max1 +
    unreadiness_d +
    badsleep_d +
    activity_d +
    #active_cals100 +
    high_activity_hrs +
    #medhigh_activity_hrs +
    med_activity_hrs +
    low_activity_hrs +
    #sedentary_hrs_c +
    #resting_hrs_c +
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




# Select more parsimonious model

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



# Hand-crafted model

model_final <- glm(
  migraine ~ 
    is_saturday + 
    is_sunday + 
    any_plane +
    extra_coffee +
    migraine_yesterday +
    weeks_since_last +
    high_activity_hrs +
    med_activity_hrs +
    drink_sessions_gt1 +
    drinks_any,
    data = model_data_days, 
  family = binomial(link = "logit")
)


# Jtools summary
summ(model_final, exp = TRUE, vif = TRUE)

plot_summs(model, 
           model_final, 
           stepwise_model, 
           model.names = c("Full", "Custom", "AIC Stepwise"),
           exp = TRUE, inner_ci_level = 0.67) +
  scale_x_log10(breaks = scales::breaks_log(n = 15)) +
  coord_cartesian(xlim = c(0.3, 7)) +
  labs(title = "Migraine model", x = "Odds ratio")


all_effect_plots(model_final)



# LASSO version -----------------------------------------------------------

library(glmnet)

# Prepare data
X <- model_data_days %>%
  select(-migraine) %>%
  as.matrix()

y <- model_data_days$migraine

# Fit the LASSO model
lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)

# View results
plot(lasso_model)




# Interactive tool --------------------------------------------------------

ui <- fluidPage(
  titlePanel("GLM Prediction Explorer"),
  
  fluidRow(
    column(12, h4("Interactive Control Panels")),
    uiOutput("control_panels")
  )
)

server <- function(input, output) {
  
  # Define all controls and their ranges
  controls <- list(
    list(name = "migraine_yesterday", type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "is_saturday",        type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "any_travel",         type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "extra_coffee",       type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "weeks_since_last",   type = "slider", default = 0, range = seq(0, 2, by = 1/7)),
    list(name = "high_activity_hrs",  type = "slider", default = 0, range = seq(0, 10, by = 1)),
    list(name = "med_activity_hrs",   type = "slider", default = 1, range = seq(0, 10, by = 1)),
    list(name = "sedentary_hrs",      type = "slider", default = 9, range = seq(0, 12, by = 1)),
    list(name = "drink_session_biggest_gt1", type = "slider", default = 0, range = seq(0, 4, by = 1)),
    list(name = "drinks_any",         type = "checkbox", default = 9, range = c(FALSE, TRUE))
  )
  
  # Generate UI for each control
  output$control_panels <- renderUI({
    control_panels <- lapply(controls, function(control) {
      input_control <- switch(
        control$type,
        "checkbox" = checkboxInput(control$name, label = control$name, value = inputs[[control$name]]),
        "slider" = sliderInput(control$name, label = control$name, min = control$range[1],
                               max = tail(control$range, n=1), value = control$default)
      )
      
      plot_output <- plotOutput(paste0(control$name, "_plot"), height = "150px")
      
      # Arrange control and plot in a small panel
      column(
        4,
        div(style = "border: 1px solid #ccc; padding: 10px; margin: 5px; border-radius: 5px;",
            input_control,
            plot_output)
      )
    })
    
    # Arrange panels in a grid
    fluidRow(control_panels)
  })
  
  # Generate prediction plots for each control
  lapply(controls, function(control) {
    output[[paste0(control$name, "_plot")]] <- renderPlot({
      base_data <- as.data.frame(reactiveValuesToList(inputs))
      
      # Generate predictions for the range of the current control
      pred_data <- data.frame(
        value = control$range,
        prediction = sapply(control$range, function(val) {
          base_data[[control$name]] <- val
          predict(model_final, newdata = base_data, type = "response")
        })
      )
      
      # Filter the pred_data where the value matches the current input
      selected_value <- input[[control$name]]
      pred_data$highlight <- ifelse(pred_data$value == selected_value, "red", "black")
      
      # Plot the prediction results
      ggplot(pred_data, aes(x = value, y = prediction)) +
        geom_point(aes(colour = highlight), size = 3) +
        scale_color_identity() +
        labs(title = control$name, x = NA, y = "Prediction") +
        scale_y_continuous(limits = c(0,1)) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    })
  })
  
  # Update reactive values when inputs change
  observe({
    lapply(controls, function(control) {
      inputs[[control$name]] <- input[[control$name]]
    })
  })
}



# Run the Shiny app
shinyApp(ui = ui, server = server)

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
  mutate(bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.075), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(
    mean_predicted = mean(predicted_prob),
    observed_proportion = mean(migraine),
    n = n()
  ) %>%
  filter(!is.na(bin))  # Remove empty bins

# Plot calibration
ggplot(calibration_data, aes(x = mean_predicted, y = observed_proportion, size = n, label = n)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 10) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_text(size = 3, hjust = 0, vjust = 0.5, nudge_x = 0.01) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Calibration Plot",
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
    
    any_travel +
    extra_coffee +
    
    migraine_yesterday +
    weeks_since_last +
    
    high_activity_hrs +
    med_activity_hrs +
    
    drink_sessions_gt1 +
    drinks_any
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

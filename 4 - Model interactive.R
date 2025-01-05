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
    list(name = "any_medication",     type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "any_plane",          type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "extra_coffee",       type = "checkbox", default = FALSE, range = c(FALSE, TRUE)),
    list(name = "weeks_since_last",   type = "slider", default = 0, range = seq(0, 2, by = 1/7)),
    list(name = "activity_high_hrs",  type = "slider", default = 0, range = seq(0, 10, by = 1)),
    list(name = "activity_med_hrs",   type = "slider", default = 1, range = seq(0, 10, by = 1)),
    list(name = "activity_resting_hrs",type = "slider", default = 9, range = seq(0, 12, by = 1)),
    list(name = "drink_sessions_gt1", type = "slider", default = 0, range = seq(0, 4, by = 1)),
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
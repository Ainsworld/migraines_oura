library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)



# OURA API functions ------------------------------------------------------
# Written mostly by Claude on 26/12/24, guided by me

#' Oura API Client class
#' @description A client for interacting with the Oura API
OuraClient <- R6::R6Class("OuraClient",
  public = list(
    #' @field token Authentication token
    token = NULL,
    #' @field base_url Base URL for the API
    base_url = "https://api.ouraring.com/v2/usercollection",
    
    #' @description Initialize a new OuraClient
    #' @param token Authentication token
    initialize = function(token) {
      self$token <- token
    },
    
    #' @description Make a GET request to the API
    #' @param endpoint API endpoint
    #' @param params Query parameters
    #' @return Response content
    get = function(endpoint, params = list()) {
      url <- paste(self$base_url, endpoint, sep = "/")
      
      response <- GET(
        url = url,
        query = params,
        add_headers(
          Authorization = paste("Bearer", self$token),
          Accept = "application/json"
        )
      )
      
      self$handle_errors(response)
      fromJSON(rawToChar(response$content))
    },
    
    #' @description Handle API errors
    #' @param response HTTP response
    handle_errors = function(response) {
      if (http_error(response)) {
        status <- status_code(response)
        error_message <- switch(as.character(status),
                                "400" = "Invalid query parameters",
                                "401" = "Unauthorized access - token may be expired or invalid",
                                "403" = "Access forbidden - subscription may have expired",
                                "404" = "Resource not found",
                                "429" = "Rate limit exceeded (5000 requests/5 minutes)",
                                sprintf("HTTP %d - Unknown error", status)
        )
        stop(error_message)
      }
    },
    
    #' @description Fetch paginated data from an endpoint
    #' @param endpoint API endpoint
    #' @param start_date Start date (optional)
    #' @param end_date End date (optional)
    #' @return List of results
    fetch_paginated = function(endpoint, start_date = NULL, end_date = NULL) {
      params <- list()
      if (!is.null(start_date)) params$start_date <- as.character(as.Date(start_date))
      if (!is.null(end_date)) params$end_date <- as.character(as.Date(end_date))
      
      all_results <- list()
      repeat {
        response <- self$get(endpoint, params)
        all_results <- c(all_results, response$data)
        
        if (is.null(response$next_token) || response$next_token == "") break
        params$next_token <- response$next_token
      }
      
      all_results
    },
    
    #' @description Get a single document by ID
    #' @param endpoint API endpoint
    #' @param document_id Document ID
    #' @return Document data
    get_document = function(endpoint, document_id) {
      self$get(paste(endpoint, document_id, sep = "/"))
    }
  )
)

#' Create data fetching functions
#' @param client OuraClient instance
#' @param endpoint API endpoint name
#' @return List of functions for the endpoint
create_endpoint_functions <- function(client, endpoint) {
  list(
    fetch = function(start_date = NULL, end_date = NULL) {
      client$fetch_paginated(endpoint, start_date, end_date)
    },
    get_single = function(document_id) {
      client$get_document(endpoint, document_id)
    }
  )
}

#' Create a new Oura API client
#' @param token Authentication token
#' @return List of functions for interacting with the API
create_oura_client <- function(token) {
  client <- OuraClient$new(token)
  
  list(
    tags = create_endpoint_functions(client, "enhanced_tag"),
    workouts = create_endpoint_functions(client, "workout"),
    sessions = create_endpoint_functions(client, "session"),
    daily_activity = create_endpoint_functions(client, "daily_activity"),
    daily_readiness = create_endpoint_functions(client, "daily_readiness"),
    daily_sleep = create_endpoint_functions(client, "daily_sleep")
  )
}





# Fetch the data ----------------------------------------------------------

# Examples of unused methods

#single_tag <- oura$tags$get_single("tag_id")
#single_workout <- oura$workouts$get_single("workout_id")
#single_activity <- oura$daily_activity$get_single("activity_id")



source("secrets.R") # this just sets the API key used below. Not included in github.
oura <- create_oura_client(oura_api_key)

first_date <- "2022-11-03"
last_date <- "2025-01-04"



# Fetch SESSIONS
fetch <- oura$sessions$fetch(first_date, last_date)



# Fetch TAGS
fetch <- oura$tags$fetch(first_date, last_date)

alcohol_variations <- c('alcohol','beer','wine','liquor', # Native Oura tags
                        'Homebrew' # A custom tag I created
                        )

# Convert list to tibble
tags <- tibble(
  #id = fetch$id,
  tag_type = fetch$tag_type_code,
  # These converts times to to UTC...
  start_time = ymd_hms(fetch$start_time),
  end_time = ymd_hms(fetch$end_time),
  # These convert to local time, which is more appropriate here though will create some interval oddities
  start_time_local = ymd_hms(substr(fetch$start_time, 1, 19)),
  end_time_local = ymd_hms(substr(fetch$end_time, 1, 19)),
  start_day = ymd(fetch$start_day),
  end_day = ymd(fetch$end_day),
  comment = fetch$comment,
  custom_name = fetch$custom_name
)  |> 
  # Clean up tag types 
  mutate(tag_type = coalesce(custom_name, tag_type)) |> 
  mutate(tag_type = str_remove(tag_type, "tag_generic_|tag_sleep_"),
         tag = if_else(tag_type %in% alcohol_variations,
                                    'alc_drink',
                                    tag_type),
         .after = tag_type) |> 
  mutate(tz_offset = as.numeric(difftime(start_time_local, start_time, units = "hours")),
         .before = start_time_local) |> 
  arrange(start_time)



# Fetch WORKOUTS (only seems to be those originated in Oura)
fetch <- oura$workouts$fetch(first_date, last_date)

intense_activities <- c('running','cycling','downhillSkiing')
# ... this unused as my workouts of this type which were imported from Strava
# or similar don't actually appear in the data

workouts <- tibble(
  #id = fetch$id,
  activity_type = fetch$activity,
  calories = fetch$calories,
  start_time = ymd_hms(fetch$start_datetime),
  end_time = ymd_hms(fetch$end_datetime),
  duration = end_time - start_time,
  start_day = ymd(fetch$day),
  distance = fetch$distance,
  intensity = fetch$intensity,
  source = fetch$source,
  label = fetch$label
)



# Fetch daily ACTIVITY
fetch <- oura$daily_activity$fetch(first_date, last_date)

# # Code to view the history of MET for a single day
# # Looks like the values a set very precisely from the Oura ring and as a single
# # value for a longer duration imported workout.

# obs <- unlist(fetch$met$items[761])
# times <- seq(from = 0, by = 1, length.out = length(obs)) / 60
# df <-  tibble(time = times, met = obs)
# ggplot(df, aes(x = time, y = met)) +
#   geom_line()

activity <- tibble(
  #id = fetch$id,
  start_day = ymd(fetch$day),
  activity_score = fetch$score,
  active_calories = fetch$active_calories,
  activity_high_hrs = fetch$high_activity_time / (60 * 60),
  activity_med_hrs = fetch$medium_activity_time / (60 * 60),
  activity_low_hrs = fetch$low_activity_time / (60 * 60),
  activity_sedentary_hrs = fetch$sedentary_time / (60 * 60),
  activity_resting_hrs = fetch$resting_time / (60 * 60)
)
  


# Fetch Daily READINESS
fetch <- oura$daily_readiness$fetch(first_date, last_date)

readiness <- tibble(
  #id = fetch$id,
  start_day = ymd(fetch$day),
  readiness_score = fetch$score,
  temperature_deviation = fetch$temperature_deviation,
  fetch$contributors
)



# Fetch Daily SLEEP
fetch <- oura$daily_sleep$fetch(first_date, last_date)

sleep <- tibble(
  #id = fetch$id,
  start_day = ymd(fetch$day),
  sleep_score = fetch$score,
  fetch$contributors
)
  


# Save dataframes to disk
save(tags, workouts, activity, readiness, sleep,
       file = "oura_dataframes.RData")


  
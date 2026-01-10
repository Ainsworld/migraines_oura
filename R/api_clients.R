library(tidyverse)
library(httr)
library(jsonlite)
library(R6)

# --- Configuration Helper -----------------------------------------------------

#' Securely retrieve environment variables
#' @param key_name String. Name of the env var.
#' @return Value of the key. Stops if missing.
get_secret <- function(key_name) {
  val <- Sys.getenv(key_name)
  if (identical(val, "")) {
    stop(sprintf(
      "❌ Missing Secret: '%s'.\n   Please ensure this is set in your .Renviron file.\n   See .Renviron.example for guidance.",
      key_name
    ), call. = FALSE)
  }
  return(val)
}

# --- Base Client --------------------------------------------------------------

#' Base Client for Health APIs
ApiClient <- R6::R6Class("ApiClient",
                         public = list(
                           base_url = NULL,
                           token = NULL,
                           
                           initialize = function(base_url, token) {
                             self$base_url <- base_url
                             self$token <- token
                           },
                           
                           handle_errors = function(response) {
                             if (http_error(response)) {
                               status <- status_code(response)
                               msg <- content(response, "text", encoding = "UTF-8")
                               stop(sprintf("API Error: %d - %s", status, msg), call. = FALSE)
                             }
                           }
                         )
)

# --- Oura Client (V2) ---------------------------------------------------------

#' Oura API V2 Client
OuraClient <- R6::R6Class("OuraClient",
                          inherit = ApiClient,
                          public = list(
                            #' @param token Defaults to OURA_PAT from .Renviron
                            initialize = function(token = get_secret("OURA_API_KEY")) {
                              super$initialize("https://api.ouraring.com/v2/usercollection", token)
                            },
                            
                            #' Fetch paginated data from Oura
                            #' @param endpoint String. e.g., "daily_activity"
                            get_paginated = function(endpoint, start_date, end_date) {
                              all_data <- list()
                              next_token <- NULL
                              
                              message(sprintf("Running Oura Ingest [%s]: %s to %s", endpoint, start_date, end_date))
                              
                              repeat {
                                params <- list(
                                  start_date = as.character(start_date),
                                  end_date = as.character(end_date),
                                  next_token = next_token
                                )
                                
                                # Remove nulls to keep query clean
                                params <- params[!sapply(params, is.null)]
                                
                                res <- GET(
                                  url = paste(self$base_url, endpoint, sep = "/"),
                                  query = params,
                                  add_headers(Authorization = paste("Bearer", self$token))
                                )
                                
                                self$handle_errors(res)
                                
                                # Parse
                                payload <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
                                
                                if (!is.null(payload$data) && length(payload$data) > 0) {
                                  all_data <- bind_rows(all_data, as_tibble(payload$data))
                                }
                                
                                next_token <- payload$next_token
                                if (is.null(next_token)) break
                              }
                              
                              message(sprintf("✔ Complete: %d records fetched.", nrow(all_data)))
                              return(all_data)
                            }
                          )
)

# --- Intervals.icu Client -----------------------------------------------------

#' Intervals.icu API Client
IntervalsClient <- R6::R6Class("IntervalsClient",
                               inherit = ApiClient,
                               public = list(
                                 athlete_id = NULL,
                                 
                                 #' @param athlete_id Defaults to INTERVALS_ATHLETE_ID from .Renviron
                                 #' @param api_key Defaults to INTERVALS_API_KEY from .Renviron
                                 initialize = function(athlete_id = get_secret("INTERVALS_ATHLETE_ID"), 
                                                       api_key = get_secret("INTERVALS_API_KEY")) {
                                   self$athlete_id <- athlete_id
                                   super$initialize("https://intervals.icu/api/v1", api_key)
                                 },
                                 
                                 #' Get activities between dates
                                 get_activities = function(oldest_iso, newest_iso) {
                                   url <- sprintf("%s/athlete/%s/activities", self$base_url, self$athlete_id)
                                   
                                   message(sprintf("Running Intervals Ingest: %s to %s", oldest_iso, newest_iso))
                                   
                                   res <- GET(
                                     url, 
                                     query = list(oldest = oldest_iso, newest = newest_iso), 
                                     authenticate("API_KEY", self$token) # Basic Auth: User="API_KEY", Pass=Token
                                   )
                                   
                                   self$handle_errors(res)
                                   dat <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
                                   
                                   message(sprintf("✔ Complete: %d activities fetched.", nrow(dat)))
                                   return(as_tibble(dat))
                                 },
                                 
                                 #' Get Time in Zone for specific activity
                                 get_time_at_hr = function(activity_id) {
                                   url <- sprintf("%s/activity/%s/time-at-hr", self$base_url, activity_id)
                                   res <- GET(url, authenticate("API_KEY", self$token))
                                   self$handle_errors(res)
                                   fromJSON(content(res, "text", encoding = "UTF-8"))
                                 }
                               )
)
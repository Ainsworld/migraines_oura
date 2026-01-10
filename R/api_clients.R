library(tidyverse)
library(httr)
library(jsonlite)
library(R6)

# --- Configuration Helper -----------------------------------------------------
get_secret <- function(key_name) {
  # Sys.getenv can return "" for missing keys, ensuring we handle clean strings
  val <- Sys.getenv(key_name, unset = NA)
  
  if (is.na(val) || identical(val, "")) {
    stop(sprintf(
      "❌ Missing Secret: '%s'.\n   Please check your .Renviron file (run `usethis::edit_r_environ()`).\n   Line should look like: %s='your_token_here'",
      key_name, key_name
    ), call. = FALSE)
  }
  
  # Trim whitespace just in case of copy-paste errors
  return(trimws(val))
}

# --- Base Client --------------------------------------------------------------
ApiClient <- R6::R6Class("ApiClient",
                         public = list(
                           base_url = NULL,
                           token = NULL,
                           
                           initialize = function(base_url, token) {
                             self$base_url <- base_url
                             self$token <- token
                           },
                           
                           handle_errors = function(response, source_label) {
                             if (http_error(response)) {
                               status <- status_code(response)
                               msg <- content(response, "text", encoding = "UTF-8")
                               stop(sprintf("❌ %s API Error [%d]: %s", source_label, status, msg), call. = FALSE)
                             }
                           }
                         )
)

# --- Oura Client (V2) ---------------------------------------------------------
OuraClient <- R6::R6Class("OuraClient",
                          inherit = ApiClient,
                          public = list(
                            initialize = function(token = get_secret("OURA_API_KEY")) {
                              # Pass to parent initialize
                              super$initialize("https://api.ouraring.com/v2/usercollection", token)
                              
                              # Debug: Verify inheritance worked immediately upon creation
                              if (is.null(self$handle_errors)) {
                                stop("❌ CRITICAL ERROR: Inheritance failed. 'handle_errors' method is missing from OuraClient.", call. = FALSE)
                              }
                              message("✅ OuraClient initialized successfully.")
                            },
                            
                            #' Generic paginated fetcher
                            get_endpoint = function(endpoint, start_date, end_date) {
                              all_data <- list()
                              next_token <- NULL
                              
                              message(sprintf("   ⬇ Oura [%s]: Fetching...", endpoint))
                              
                              repeat {
                                params <- list(
                                  start_date = as.character(start_date),
                                  end_date = as.character(end_date), 
                                  next_token = next_token
                                )
                                params <- params[!sapply(params, is.null)]
                                
                                # Ensure token is valid before call
                                if (is.null(self$token) || self$token == "") stop("❌ Token is missing in OuraClient instance.")
                                
                                res <- GET(
                                  url = paste(self$base_url, endpoint, sep = "/"),
                                  query = params,
                                  add_headers(Authorization = paste("Bearer", self$token))
                                )
                                
                                # Critical Point: This is where 'attempt to apply non-function' likely occurred
                                # if 'self$handle_errors' was somehow NULL.
                                self$handle_errors(res, "Oura")
                                
                                payload <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
                                
                                if (!is.null(payload$data) && length(payload$data) > 0) {
                                  all_data <- bind_rows(all_data, as_tibble(payload$data))
                                }
                                
                                next_token <- payload$next_token
                                if (is.null(next_token)) break
                              }
                              return(all_data)
                            }
                          )
)

# --- Intervals.icu Client -----------------------------------------------------
IntervalsClient <- R6::R6Class("IntervalsClient",
                               inherit = ApiClient,
                               public = list(
                                 athlete_id = NULL,
                                 
                                 initialize = function(athlete_id = get_secret("INTERVALS_ATHLETE_ID"), 
                                                       api_key = get_secret("INTERVALS_API_KEY")) {
                                   self$athlete_id <- athlete_id
                                   super$initialize("https://intervals.icu/api/v1", api_key)
                                 },
                                 
                                 #' Get list of activities
                                 get_activities = function(oldest_iso, newest_iso) {
                                   url <- sprintf("%s/athlete/%s/activities", self$base_url, self$athlete_id)
                                   message("   ⬇ Intervals: Fetching Activities...")
                                   
                                   res <- GET(
                                     url, 
                                     query = list(oldest = oldest_iso, newest = newest_iso), 
                                     authenticate("API_KEY", self$token)
                                   )
                                   
                                   self$handle_errors(res, "Intervals")
                                   dat <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
                                   return(as_tibble(dat))
                                 },
                                 
                                 #' Get Time in Zone for specific activity
                                 get_time_at_hr = function(activity_id) {
                                   url <- sprintf("%s/activity/%s/time-at-hr", self$base_url, activity_id)
                                   res <- GET(url, authenticate("API_KEY", self$token))
                                   self$handle_errors(res, "Intervals Time-at-HR")
                                   return(fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE))
                                 },
                                 
                                 #' Get streams (HR, Watts, Cadence) for granular analysis
                                 get_activity_streams = function(activity_id) {
                                   url <- sprintf("%s/activity/%s/streams", self$base_url, activity_id)
                                   res <- GET(url, authenticate("API_KEY", self$token))
                                   self$handle_errors(res, "Intervals Streams")
                                   return(fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE))
                                 }
                               )
)
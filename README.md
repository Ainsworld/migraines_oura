# Migraine & Physiology Analysis

**Author:** Mark Ainsworth  
**Status:** Active Research  

A rigorous data pipeline analyzing the relationship between physiological stressors (High HR duration, Sleep fragmentation, Alcohol) and Migraine onset. This project integrates data from **Oura** (Recovery/Sleep) and **Intervals.icu** (Training Stress/HR Zones).

---

## 🔐 1. Secrets Management (Security Protocol)

This repository uses a **"Twin-File"** system to protect API credentials. 

**Do NOT commit your keys to GitHub.**

### Setup Instructions:
1.  Locate the file `.Renviron.example` in the root directory.
2.  Duplicate this file and rename it to `.Renviron` (note the dot prefix).
3.  Open `.Renviron` and paste your actual API keys:
    ```bash
    OURA_PAT="your_oura_personal_access_token"
    INTERVALS_API_KEY="your_intervals_api_key"
    INTERVALS_ATHLETE_ID="your_athlete_id"
    ```
4.  Restart your R session (`Session > Restart R`) to load these variables.

**Note:** The `.Renviron` file is explicitly ignored by git to prevent accidental leaks.

---

## ⚠️ 2. The "Strava Lock" & Workaround

**The Problem:** Due to Strava's API Terms of Service (Nov 2024), third-party platforms like Intervals.icu are prohibited from re-exporting Strava-synced data via their API. 
* *Symptom:* The `2_intervals_ingest.R` script returns `NA` or blank data for all Strava-recorded rides.

**The Fix (Manual "Data Washing"):** To unlock this data for analysis, you must replace the "synced" activities with "uploaded" files.

1.  **Download Archive:** Go to Strava -> Settings -> My Account -> Download or Delete Your Account -> Request Your Archive.
2.  **Upload to Intervals:** Take the `activities` folder from the archive and upload it to Intervals.icu.
3.  **Result:** Intervals treats these as direct file uploads, bypassing the third-party API restriction. The R scripts in this repo will now successfully fetch granular HR data for these rides.

---

## 🏗️ 3. Project Structure

### A. The "Plumbing"
* **`R/api_clients.R`**: Contains the `R6` class definitions for `OuraClient` and `IntervalsClient`. Handles authentication, rate limiting, and error parsing.

### B. The Pipeline
The analysis runs in a strict sequence:

1.  **`R/1_oura_ingest.R`**
    * **Intent:** Fetches raw Sleep, Readiness, Activity, and Tag data.
    * **Logic:** Applies timezone corrections and harmonizes tag names (e.g., mapping "Homebrew" to "Alcohol").
    * **Output:** `data/oura_tidy.rds`

2.  **`R/2_intervals_ingest.R`**
    * **Intent:** Fetches Training Load and Granular HR distributions.
    * **Logic:** Iterates through history to fetch "Time-at-HR" (seconds per bpm) for detailed physiological stress modeling.
    * **Prerequisite:** Requires the "Strava Archive" workaround described above.
    * **Output:** `data/intervals_workouts.rds`, `data/intervals_hr_dist.rds`

3.  **`R/3_prep_model.R`**
    * **Intent:** Feature Engineering & Hypothesis Generation.
    * **Logic:**
        * Calculates **Chronic Training Load** (42-day rolling avg).
        * Calculates **Sleep Fragmentation** (Awake / Total Sleep).
        * Implements **Day Shifting** (Migraines < 8am assigned to previous day).
        * Clusters alcohol intake into **"Sessions"** (drinks < 2.1h apart).
    * **Output:** `data/analysis_ready.rds`

### C. Analysis
* **`3 - Modelling.R`**: Logistic Regression and Bayesian models testing the "High HR Duration" hypothesis.
* **`4 - Model interactive.R`**: A Shiny gadget for exploring model predictions.

---

## 🛠️ Dependencies
* **Core:** `tidyverse`, `lubridate`, `R6`, `jsonlite`, `httr`
* **Modelling:** `brms` (Bayesian), `glmnet` (LASSO), `jtools`
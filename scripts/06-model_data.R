#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Set-up ####
# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)


#### Prepare dataset ####
# Read in the data and clean variable names
data <- read_csv("data/01-raw_data/poll_raw_data.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.8,
    transparency_score >= 5,
    pollscore <= 0 # Need to investigate this choice - come back and fix. 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state), # Hacky fix for national polls - come back and check
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # When Harris declared
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) # Need number not percent for some models
  )

swing_states <- c("Arizona", "Florida", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania")

harris_models <- list()
states <- c("National", swing_states)
for (state in states) {
  # Filter data for each state
  state_data <- just_harris_high_quality %>%
    filter(state == state)
  
  # Fit linear model
  model <- lm(pct ~ end_date + pollster, data = state_data)
  
  # Store model in list with state name
  harris_models[[state]] <- model
}

just_trump_high_quality <- data %>%
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.8,
    transparency_score >= 5,
    pollscore <= 0  # Placeholder for pollscore filter, adjust as necessary
  ) %>%
  mutate(
    state = if_else(is.na(state), "National", state), # Replace NA with "National" for national polls
    end_date = mdy(end_date)
  ) %>%
  filter(end_date >= as.Date("2024-07-21")) %>% # Date after Harris declared
  mutate(
    num_trump = round((pct / 100) * sample_size, 0) # Convert pct to actual number
  )

# Fit models for Donald Trump for national and swing states
trump_models <- list()
for (state in states) {
  # Filter data for each state
  state_data <- just_trump_high_quality %>%
    filter(state == state)
  
  # Fit linear model
  model <- lm(pct ~ end_date + pollster, data = state_data)
  
  # Store model in list with state name
  trump_models[[state]] <- model
}


#### Save model ####
# Save both lists of models in an .rds file
saveRDS(list(harris_models = harris_models, trump_models = trump_models), file = "models/models_harris_trump.rds")

# To load these models back later, use:
# loaded_models <- readRDS("models_harris_trump.rds")



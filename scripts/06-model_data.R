#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor) # Load the janitor package for clean_names()

# Load and clean data
data <- read_csv("data/01-raw_data/poll_raw_data.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
# Load required packages
library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor)

# Load and clean data
data <- read_csv("data/01-raw_data/poll_raw_data.csv") |>
  clean_names()

# Filter and prepare data for Harris model
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21"),
         state == "National") |>
  mutate(
    num_harris = round((pct / 100) * sample_size, 0)
  ) |>
  drop_na(pct, end_date)  # Drop rows with NA in pct or end_date

# Filter and prepare data for Trump model
just_trump_high_quality <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21"),
         state == "National") |>
  mutate(
    num_trump = round((pct / 100) * sample_size, 0)
  ) |>
  drop_na(pct, end_date)  # Drop rows with NA in pct or end_date

write_csv(just_harris_high_quality, "data/02-analysis_data/Harris.csv")
write_csv(just_trump_high_quality, "data/02-analysis_data/Trump.csv")
# Fit the model for Harris
harris_model <- stan_glm(
  formula = pct ~ end_date,
  data = just_harris_high_quality,
  family = gaussian(),
  prior = normal(location = 0, scale = 0.1),
  prior_intercept = normal(location = 50, scale = 5),
  prior_aux = exponential(rate = 1),
  seed = 853
)

# Fit the model for Trump
trump_model <- stan_glm(
  formula = pct ~ end_date,
  data = just_trump_high_quality,
  family = gaussian(),
  prior = normal(location = 0, scale = 0.1),
  prior_intercept = normal(location = 50, scale = 5),
  prior_aux = exponential(rate = 1),
  seed = 853
)

# Save models individually
saveRDS(harris_model, file = "models/models_harris.rds")
saveRDS(trump_model, file = "models/models_trump.rds")


                               
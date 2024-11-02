#### Preamble ####
# Purpose: Models the relatioship between the supprt rate of Harris and Trump with respect to time
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data has been downloaded from the website
# Any other information needed? None

# Filter data to Harris estimates based on high-quality polls after she declared
# Load required packages
library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor)

just_harris_high_quality <- read.csv("data/02-analysis_data/Harris.csv")
just_trump_high_quality <- read.csv("data/02-analysis_data/Trump.csv")

# Fit the model for Harris
harris_model <- stan_glm(
  formula = pct ~ days_after_earliest,
  data = just_harris_high_quality,
  family = gaussian(),
  prior = normal(location = 0, scale = 0.1),
  prior_intercept = normal(location = 50, scale = 5),
  prior_aux = exponential(rate = 1),
  seed = 853
)

# Fit the model for Trump
trump_model <- stan_glm(
  formula = pct ~ days_after_earliest,
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


                               
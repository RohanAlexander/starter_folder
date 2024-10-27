#### Preamble ####
# Purpose: Cleans the raw data from 538 by removing missing values
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data has been downloaded from the website
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(dplyr)
set.seed(853)

#### Simulate data ####

# Define the sample size as an adjustable parameter
sample_size <- 100  # Change this to any desired number of rows

# Simulate data
simulated_data <- tibble(
  pollster = sample(c("InsiderAdvantage", "TIPP", "YouGov", "Ipsos", "Gallup"), sample_size, replace = TRUE),
  numeric_grade = round(runif(sample_size, 1, 5), 1),
  pollscore = round(rnorm(sample_size, mean = 0, sd = 1), 1),
  methodology = sample(c("Online", "Phone", "Mixed"), sample_size, replace = TRUE),
  transparency_score = round(runif(sample_size, 0, 10), 1),
  state = sample(state.abb, sample_size, replace = TRUE),
  start_date = sample(seq(as.Date('2024-01-01'), as.Date('2024-11-01'), by="day"), sample_size, replace = TRUE),
  population_full = sample(c("Registered Voters", "Likely Voters"), sample_size, replace = TRUE),
  candidate_name = sample(c("Kamala Harris", "Donald Trump"), sample_size, replace = TRUE),
  pct = round(runif(sample_size, 45, 55), 1)
)

# View the simulated data
print(simulated_data)


#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")

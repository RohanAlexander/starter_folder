#### Preamble ####
# Purpose: Simulates a dataset of Australian electoral divisions, including the 
  # state and party that won each division.
# Author: Rohan Alexander
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
  # - `polars` must be installed (pip install polars)
  # - `numpy` must be installed (pip install numpy)


#### Workspace setup ####
import polars as pl
import numpy as np
np.random.seed(853)


#### Simulate data ####
# State names
states = [
    "New South Wales", "Victoria", "Queensland", "South Australia", 
    "Western Australia", "Tasmania", "Northern Territory", 
    "Australian Capital Territory"
]

# Political parties
parties = ["Labor", "Liberal", "Greens", "National", "Other"]

# Probabilities for state and party distribution
state_probs = [0.25, 0.25, 0.15, 0.1, 0.1, 0.1, 0.025, 0.025]
party_probs = [0.40, 0.40, 0.05, 0.1, 0.05]

# Generate the data using numpy and polars
divisions = [f"Division {i}" for i in range(1, 152)]
states_sampled = np.random.choice(states, size=151, replace=True, p=state_probs)
parties_sampled = np.random.choice(parties, size=151, replace=True, p=party_probs)

# Create a polars DataFrame
analysis_data = pl.DataFrame({
    "division": divisions,
    "state": states_sampled,
    "party": parties_sampled
})


#### Save data ####
analysis_data.write_csv("data/00-simulated_data/simulated_data.csv")

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

#### Clean data ####
raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# Remove rows with any missing values in the relevant columns
data_clean <- na.omit(data[, c("pct", "pollscore", "methodology", "transparency_score", "hypothetical")])

#### Save data ####
write_csv(data_clean, "data/02-analysis_data/analysis_data.csv")

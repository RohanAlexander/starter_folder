#### Preamble ####
# Purpose: Downloads and saves the data from 538 president general 
# Authors: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: Dataset found from 538 website
# Any other information needed? None


#### Workspace setup ####

library(tidyverse)
library(readr)

#### Download data ####

president_polls <- read_csv("~/Downloads/president_polls.csv")
data <- president_polls


#### Save data ####

write_csv(data, "data/01-raw_data/raw_data.csv") 

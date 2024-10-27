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

#### Download data ####
raw_data <- read_csv("https://projects.fivethirtyeight.com/polls/data/president_polls.csv")


#### Save data ####
write_csv(raw_data, "data/01-raw_data/poll_raw_data.csv") 

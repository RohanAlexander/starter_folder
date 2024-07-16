#### Preamble ####
# Purpose: Downloads and saves the data from Excel file.
# Author: Luca Carnegie
# Date: 16 July 2024
# Contact: luca.carnegie@mail.utoronto.ca or rohan.alexander@utoronto.ca 
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)
library(readr)

#### Download data ####
sta302_survey <- read_excel("data/raw_data/raw_STA302_postcourse_survey_w24.xlsx")


#### Save data ####
write_csv(sta302_survey, "data/raw_data/raw_STA302_postcourse_survey_w24.csv") 

         

#### Preamble ####
# Purpose: Cleans the raw survey data recored by Rohan at the end of STA302
# Author: Luca Carnegie
# Date: 16 July 2024
# Contact: luca.carnegie@mail.utoronto.ca or rohan.alexander@utoronto.ca
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_STA302_postcourse_survey_w24.csv")

raw_data <- as_tibble(raw_data)

# Drop unnecessary columns

raw_data <- raw_data |>
  select(-c(Email, 
            Name, 
            `Last modified time`, 
            `What is your full name on Quercus?`, #anonymized
            `What is your Student ID?`, #anonymized 
            `What is/are your major/s?`, #no answer for any 
            `What is/are your minor/s?`, #no answer for any 
            ))


# Clean specialization column

# Clean GPA column

# Fix text errors in self-perception question

# Replace erroneous characters with NA, etc. 

# Fix "Any other comments?" column


#### Save data ####
write_csv(cleaned_data, "data/analysis_data/clean_STA302_postcourse_survey_w24.csv")

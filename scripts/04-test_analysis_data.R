#### Preamble ####
# Purpose: Tests for cleaned data
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data has been downloaded from the website
# Any other information needed? None


#### Workspace setup ####
# Load libraries
library(testthat)
library(tibble)
library(readr)


simulated_data <- read_parquet('data/02-analysis_data/Harris.parquet')
# For Trump: simulated_data <- read_parquet('data/02-analysis_data/Trump.parquet')

# Define the test cases
test_that("Simulated data has correct structure and values", {
  # Check that the data frame has the expected number of rows and columns
  expect_equal(nrow(simulated_data), sample_size)
  expect_equal(ncol(simulated_data), 10)
  
  # Check column names
  expect_equal(
    colnames(simulated_data),
    c("pollster", "numeric_grade", "pollscore", "methodology", 
      "transparency_score", "state", "start_date", 
      "population_full", "candidate_name", "pct")
  )
})

test_that("Pollster values are from expected categories", {
  expect_true(all(simulated_data$pollster %in% c("InsiderAdvantage", "TIPP", "YouGov", "Ipsos", "Gallup")))
})

test_that("Numeric grade is within the range 1 to 5", {
  expect_true(all(simulated_data$numeric_grade >= 1 & simulated_data$numeric_grade <= 5))
})

test_that("Transparency score is within the range 0 to 10", {
  expect_true(all(simulated_data$transparency_score >= 0 & simulated_data$transparency_score <= 10))
})

test_that("Percentage (pct) is within the range 45 to 55", {
  expect_true(all(simulated_data$pct >= 45 & simulated_data$pct <= 55))
})

test_that("Pollscore column is numeric", {
  expect_true(is.numeric(simulated_data$pollscore))
})

test_that("Methodology values are from expected categories", {
  expect_true(all(simulated_data$methodology %in% c("Online", "Phone", "Mixed")))
})

test_that("State values are valid US state abbreviations", {
  expect_true(all(simulated_data$state %in% state.abb))
})

test_that("Start date is within the specified date range", {
  expect_true(all(simulated_data$start_date >= as.Date('2024-01-01') & 
                    simulated_data$start_date <= as.Date('2024-11-01')))
})

test_that("Population full values are from expected categories", {
  expect_true(all(simulated_data$population_full %in% c("Registered Voters", "Likely Voters")))
})

test_that("Candidate name values are from expected categories", {
  expect_true(all(simulated_data$candidate_name %in% c("Kamala Harris", "Donald Trump")))
})

test_that("Pct column is numeric", {
  expect_true(is.numeric(simulated_data$pct))
})
#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 26 September 2024 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(testthat)

data <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Test data ####
# Test that the dataset has 151 rows - there are 151 divisions in Australia
test_that("dataset has 151 rows", {
  expect_equal(nrow(analysis_data), 151)
})

# Test that the dataset has 3 columns
test_that("dataset has 3 columns", {
  expect_equal(ncol(analysis_data), 3)
})

# Test that the 'division' column is character type
test_that("'division' is character", {
  expect_type(analysis_data$division, "character")
})

# Test that the 'party' column is character type
test_that("'party' is character", {
  expect_type(analysis_data$party, "character")
})

# Test that the 'state' column is character type
test_that("'state' is character", {
  expect_type(analysis_data$state, "character")
})

# Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(analysis_data)))
})

# Test that 'division' contains unique values (no duplicates)
test_that("'division' column contains unique values", {
  expect_equal(length(unique(analysis_data$division)), 151)
})

# Test that 'state' contains only valid Australian state or territory names
valid_states <- c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", 
                  "Tasmania", "Northern Territory", "Australian Capital Territory")
test_that("'state' contains valid Australian state names", {
  expect_true(all(analysis_data$state %in% valid_states))
})

# Test that there are no empty strings in 'division', 'party', or 'state' columns
test_that("no empty strings in 'division', 'party', or 'state' columns", {
  expect_false(any(analysis_data$division == "" | analysis_data$party == "" | analysis_data$state == ""))
})

# Test that the 'party' column contains at least 2 unique values
test_that("'party' column contains at least 2 unique values", {
  expect_true(length(unique(analysis_data$party)) >= 2)
})
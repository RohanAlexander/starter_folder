#### Preamble ####
# Purpose: Models
# Author: Rohan Alexander
# Date: 4 August 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: -
# Any other information needed? -


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(readxl)


#### Read data ####
all_data <-
  read_excel(here::here("data/merged_data.xlsx"), sheet = "Sheet1") |>
  janitor::clean_names() |>
  mutate(mark = if_else(mark == 1, 0.99, mark)) |> # I'm a monster
  mutate(llm_usage =
           if_else(llm_usage %in% c("Minimal", "None"), "None or minimal", llm_usage)) |>
  filter(!is.na(what_is_your_gpa))


### Model data ####
# Using rstanarm
fit1rstanarm <-
  stan_betareg(mark ~ llm_usage,
               data = all_data,
               link = "logit",
               seed = 853)

fit2rstanarm <-
  stan_betareg(
    mark ~ llm_usage + what_is_your_gpa,
    data = all_data,
    link = "logit",
    seed = 853
  )

# Using brms
fit1brms <- brm(
  data = all_data,
  family = Beta,
  mark ~ llm_usage,
  prior = prior(normal(0, 1), class = b) +
    prior(gamma(4, 0.1), class = phi),
  cores = 4,
  seed = 853
)

fit2brms <- brm(
  data = all_data,
  family = Beta,
  mark ~ llm_usage + what_is_your_gpa,
  prior = prior(normal(0, 1), class = b) +
    prior(gamma(4, 0.1), class = phi),
  cores = 4,
  seed = 853
)


#### Save model ####
saveRDS(fit1rstanarm, file = "models/fit1rstanarm.rds")

saveRDS(fit2rstanarm, file = "models/fit2rstanarm.rds")

saveRDS(fit1brms, file = "models/fit1brms.rds")

saveRDS(fit2brms, file = "models/fit2brms.rds")

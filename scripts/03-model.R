#### Preamble ####
# Purpose: Models
# Author: Rohan Alexander
# Date: 17 March 2025
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: -
# Any other information needed? -


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(arrow)


#### Read data ####
all_data <-
  read_parquet(here::here("data/analysis_data.parquet"))


# Change the reference level so it's in terms of none
all_data <-
  all_data |>
  mutate(llm_usage = relevel(llm_usage, ref = "None or minimal"))


#### Model data ####
#### Simplified data ####
# Change the 1s to 0.99 so we can just use Beta initially
all_data <-
  all_data |>
  mutate(mark_no_ones = if_else(mark == 1, 0.99, mark))

# Using rstanarm
fit1rstanarm <-
  stan_betareg(
    mark_no_ones ~ llm_usage,
    data = all_data,
    link = "logit",
    seed = 853
  )

fit2rstanarm <-
  stan_betareg(
    mark_no_ones ~ llm_usage + what_is_your_gpa,
    data = all_data,
    link = "logit",
    seed = 853
  )

# Using brms
fit1brms <- brm(
  data = all_data,
  family = Beta,
  mark_no_ones ~ llm_usage,
  prior = prior(normal(0, 1), class = b) +
    prior(gamma(4, 0.1), class = phi),
  cores = 4,
  seed = 853
)

fit2brms <- brm(
  data = all_data,
  family = Beta,
  mark_no_ones ~ llm_usage + what_is_your_gpa,
  prior = prior(normal(0, 1), class = b) +
    prior(gamma(4, 0.1), class = phi),
  cores = 4,
  seed = 853
)


#### Real data ####
# There are no zeros, but there are ones so need one-inflated Beta
# Achieve by using zero-one-inflated Beta and setting coi to 1 following:
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#special-case-2-one-inflated-beta-regression

fit_one_inflated <- brm(
  formula = bf(mark ~ llm_usage, phi ~ llm_usage, zoi ~ llm_usage, coi ~ 1),
  family = zero_one_inflated_beta(),
  data = all_data,
  cores = 4,
  seed = 853
)

fit_one_inflated_gpa <- brm(
  formula = bf(
    mark ~ llm_usage + what_is_your_gpa,
    phi ~ llm_usage + what_is_your_gpa,
    zoi ~ llm_usage + what_is_your_gpa,
    coi ~ 1
  ),
  family = zero_one_inflated_beta(),
  data = all_data,
  cores = 4,
  seed = 853
)


#### Save models ####
saveRDS(fit1rstanarm, file = "models/fit1rstanarm.rds")

saveRDS(fit2rstanarm, file = "models/fit2rstanarm.rds")

saveRDS(fit1brms, file = "models/fit1brms.rds")

saveRDS(fit2brms, file = "models/fit2brms.rds")

saveRDS(fit_one_inflated, file = "models/fit_one_inflated.rds")

saveRDS(fit_one_inflated_gpa, file = "models/fit_one_inflated_gpa.rds")





#### Redo with STA304 ####
#### Read data ####
sta304_data <-
  read_parquet(here("data/sta304_analysis_data_classified_cleaned.parquet"))

# Change the reference level so it's in terms of none
sta304_data <-
  sta304_data |>
  mutate(llm_usage = relevel(llm_usage, ref = "None or minimal"))


#### Model data ####
#### Simplified data ####
# Change the 1s to 0.99 so we can just use Beta initially
sta304_data <-
  sta304_data |>
  mutate(mark_no_ones = if_else(mark == 1, 0.99, mark))

# Using rstanarm
fit1rstanarm_sta304 <-
  stan_betareg(
    mark_no_ones ~ llm_usage,
    data = sta304_data,
    link = "logit",
    seed = 853
  )

fit2rstanarm_sta304 <-
  stan_betareg(
    mark_no_ones ~ llm_usage + what_is_your_gpa,
    data = sta304_data,
    link = "logit",
    seed = 853
  )

# Using brms
fit1brms_sta304 <-
  brm(
    data = sta304_data,
    family = Beta,
    mark_no_ones ~ llm_usage,
    prior = prior(normal(0, 1), class = b) +
      prior(gamma(4, 0.1), class = phi),
    cores = 4,
    seed = 853
  )

fit2brms_sta304 <-
  brm(
    data = sta304_data,
    family = Beta,
    mark_no_ones ~ llm_usage + what_is_your_gpa,
    prior = prior(normal(0, 1), class = b) +
      prior(gamma(4, 0.1), class = phi),
    cores = 4,
    seed = 853
  )


#### Real data ####
# There are no zeros, but there are ones so need one-inflated Beta
# Achieve by using zero-one-inflated Beta and setting coi to 1 following:
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#special-case-2-one-inflated-beta-regression

fit_one_inflated_sta304 <-
  brm(
    formula = bf(mark ~ llm_usage, phi ~ llm_usage, zoi ~ llm_usage, coi ~ 1),
    family = zero_one_inflated_beta(),
    data = sta304_data,
    cores = 4,
    seed = 853
  )

fit_one_inflated_gpa_sta304 <- brm(
  formula = bf(
    mark ~ llm_usage + what_is_your_gpa,
    phi ~ llm_usage + what_is_your_gpa,
    zoi ~ llm_usage + what_is_your_gpa,
    coi ~ 1
  ),
  family = zero_one_inflated_beta(),
  data = sta304_data,
  cores = 4,
  seed = 853
)

## Add in whether they have ESL
fit_one_inflated_gpa_english_sta304 <- brm(
  formula = bf(
    mark ~ llm_usage + what_is_your_gpa + native_english,
    phi ~ llm_usage + what_is_your_gpa + native_english,
    zoi ~ llm_usage + what_is_your_gpa + native_english,
    coi ~ 1
  ),
  family = zero_one_inflated_beta(),
  data = sta304_data,
  cores = 4,
  seed = 853
)






#### Combine both and then add a flag ####
sta304_data_reduced <-
  sta304_data |>
  select(mark, llm_usage, what_is_your_gpa) |>
  mutate(class = "sta304")

sta302_data_reduced <-
  all_data |>
  select(mark, llm_usage, what_is_your_gpa) |>
  mutate(class = "sta302")

both <-
  rbind(sta302_data_reduced, sta304_data_reduced)

fit_one_inflated_gpa_both <- brm(
  formula = bf(
    mark ~ llm_usage + what_is_your_gpa + class,
    phi ~ llm_usage + what_is_your_gpa + class,
    zoi ~ llm_usage + what_is_your_gpa + class,
    coi ~ 1
  ),
  family = zero_one_inflated_beta(),
  data = both,
  cores = 4,
  seed = 853
)






#### Save models ####
saveRDS(fit1rstanarm_sta304, file = "models/fit1rstanarm_sta304.rds")

saveRDS(fit2rstanarm_sta304, file = "models/fit2rstanarm_sta304.rds")

saveRDS(fit1brms_sta304, file = "models/fit1brms_sta304.rds")

saveRDS(fit2brms_sta304, file = "models/fit2brms_sta304.rds")

saveRDS(fit_one_inflated_sta304, file = "models/fit_one_inflated_sta304.rds")

saveRDS(fit_one_inflated_gpa_sta304, file = "models/fit_one_inflated_gpa_sta304.rds")

saveRDS(fit_one_inflated_gpa_english_sta304, file = "models/fit_one_inflated_gpa_english_sta304.rds")

saveRDS(fit_one_inflated_gpa_both, file = "models/fit_one_inflated_gpa_both.rds")

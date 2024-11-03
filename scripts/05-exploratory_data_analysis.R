#### Preamble ####
# Purpose: Explore the dataset 
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 4 November 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data used from data folder
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")
data <- read.csv("data/01-raw_data/raw_data.csv")

hist(data$pct)

### Filter through the data we want 
trump <- data |> filter(
  candidate_name == "Donald Trump",
  numeric_grade >= 2.7,
  pollscore <= 0,
  transparency_score >= 5.5
)|>
  mutate(
    state = if_else(is.na(state), "National", state), 
    end_date = mdy(end_date)
  ) |>
  mutate(
    num_trump = round((pct / 100) * sample_size, 0) # Need number not percent for some models
  )

### Only keeping the rows that we want to investigate
trump <- trump |> select(sample_size, pollster, numeric_grade, pollscore, methodology, transparency_score, state, start_date, 
                         end_date, population, party, answer, candidate_name, pct)
### Summary of the selected dataset
summary(trump)

### Histogram ### 
hist(trump$pct, main = "Histogram of Poll", xlab = "The pencentage of the vote for Trump in the poll", ylab = "Frequency")

### Summary statistics for pct
mean_pct <- mean(data$pct)
sd_pct <- sd(data$pct)
min_pct <- min(data$pct)
max_pct <- max(data$pct)

summary_table <- data.frame( Metric = c("Mean", "Standard Deviation", "Minimum", "Maximum"),
                             Value = c(mean_pct, sd_pct, min_pct, max_pct))

kable(summary_table, caption = "Statistics for Poll Percentage")

### Distribution of poll by pollster
pollster_selected <- data |> filter(
  numeric_grade >= 2.7,
  pollscore <= 0,
  transparency_score >= 5.5
)|>
  mutate(
    state = if_else(is.na(state), "National", state), 
  ) |>
  mutate(
    number_pct = round((pct / 100) * sample_size, 0)
  )|>
  group_by(pollster) |>
  filter(n() > 50)

pollster_selected$donald_trump <- ifelse(pollster_selected$answer == "Harris", 0,
                                         ifelse(pollster_selected$answer == "Trump", 1, NA))

ggplot(pollster_selected, aes(x = reorder(pollster, -table(pollster)[pollster]))) +
  geom_bar() +
  labs(title = "Distribution of Polls by Pollster", x ="Pollster", y = "Number of Polls")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

### Distribution of poll by state
ggplot(pollster_selected, aes(x = reorder(state, -table(state)[state]))) + 
  geom_bar() + 
  labs(title = "Distribution of Polls by State", x = "State", y = "Number of Polls") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

### Plot Trump Variable 

ggplot(pollster_selected, aes(x = factor(donald_trump))) +
  geom_bar() +
  labs(title = "Distribution of Polls by Trump vs Harris", x = "Voter for Donald Trump", y = "Number of Polls") +
  scale_x_discrete(labels = c("0" = "Harris", "1" = "Trump"))

### Model data ####
first_model <-
  stan_glm(
    formula = pct ~ length + width,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)



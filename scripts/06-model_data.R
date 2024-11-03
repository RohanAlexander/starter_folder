#### Preamble ####
# Purpose: Models the relatioship between the supprt rate of Harris and Trump with respect to time
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data has been downloaded from the website
# Any other information needed? None
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor) # Load the janitor package for clean_names()
library(ggplot2)

# Load and clean data
data <- read_csv("data/01-raw_data/poll_raw_data.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
# Load required packages
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor)
library(ggplot2)

just_harris_high_quality <- read.csv("data/02-analysis_data/Harris.csv")
just_trump_high_quality <- read.csv("data/02-analysis_data/Trump.csv")

# Filter and prepare data for Harris model
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(state == "National") |>  # Remove the filter on end_date here
  drop_na(pct, end_date) |>  # Drop rows with NA in pct or end_date
  mutate(
    # Create a new column for days after the earliest date
    days_after_earliest = as.numeric(end_date - min(end_date))
  )

# Define `earliest_date` globally based on `just_harris_high_quality`
earliest_date <- min(just_harris_high_quality$end_date)

# Repeat the process for Trump data
just_trump_high_quality <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(state == "National") |>  # Remove the filter on end_date here
  drop_na(pct, end_date) |>  # Drop rows with NA in pct or end_date
  mutate(
    # Create a new column for days after the earliest date
    days_after_earliest = as.numeric(end_date - earliest_date)
  )

# Write data to CSV
write_csv(just_harris_high_quality, "data/02-analysis_data/Harris.csv")
write_csv(just_trump_high_quality, "data/02-analysis_data/Trump.csv")

# Fit the model for Harris
harris_model <- stan_glm(
  formula = pct ~ days_after_earliest,
  data = just_harris_high_quality,
  family = gaussian(),
  prior = normal(location = 0, scale = 0.1),
  prior_intercept = normal(location = 50, scale = 5),
  prior_aux = exponential(rate = 1),
  seed = 853
)

# Fit the model for Trump
trump_model <- stan_glm(
  formula = pct ~ days_after_earliest,
  data = just_trump_high_quality,
  family = gaussian(),
  prior = normal(location = 0, scale = 0.1),
  prior_intercept = normal(location = 50, scale = 5),
  prior_aux = exponential(rate = 1),
  seed = 853
)

# Define the target end date
last_date <- as.Date("2024-11-05")

# Calculate the number of days from earliest_date to last_date
days_to_last_date <- as.numeric(last_date - earliest_date)

# Generate prediction data frame with days_after_earliest up to days_to_last_date
days_range <- data.frame(days_after_earliest = seq(0, days_to_last_date, by = 1))

# Convert days_after_earliest to actual dates by adding them to earliest_date
days_range$end_date <- earliest_date + days_range$days_after_earliest

# Harris predictions
harris_predictions <- predict(harris_model, newdata = days_range, se.fit = TRUE)
days_range$harris_pct <- harris_predictions$fit
days_range$harris_se <- harris_predictions$se.fit

# Trump predictions
trump_predictions <- predict(trump_model, newdata = days_range, se.fit = TRUE)
days_range$trump_pct <- trump_predictions$fit
days_range$trump_se <- trump_predictions$se.fit

# Add actual poll data to plot
just_harris_high_quality$Candidate <- "Harris"
just_trump_high_quality$Candidate <- "Trump"
poll_data <- rbind(just_harris_high_quality, just_trump_high_quality)

# Plot
ggplot(data = days_range, aes(x = end_date)) +
  # Harris line and confidence interval
  geom_line(aes(y = harris_pct, color = "Harris"), linewidth = 1) +
  geom_ribbon(aes(ymin = harris_pct - 1.96 * harris_se, ymax = harris_pct + 1.96 * harris_se, fill = "Harris"), alpha = 0.2) +
  
  # Trump line and confidence interval
  geom_line(aes(y = trump_pct, color = "Trump"), linewidth = 1) +
  geom_ribbon(aes(ymin = trump_pct - 1.96 * trump_se, ymax = trump_pct + 1.96 * trump_se, fill = "Trump"), alpha = 0.2) +
  
  # Actual poll data points
  geom_point(data = poll_data, aes(x = end_date, y = pct, color = Candidate), alpha = 0.5) +
  
  # Axis labels and title
  labs(title = "Polling Trends for Harris and Trump",
       x = "Date",
       y = "Polling Percentage (%)",
       color = "Candidate",
       fill = "Candidate") +
  
  # Custom x-axis to show every 2 months
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b",  # Shows only the month (e.g., "Aug", "Oct")
    limits = c(earliest_date, as.Date("2024-11-05"))
  ) +
  
  # Highlight Nov 5th specifically with a dashed line
  geom_vline(xintercept = as.numeric(as.Date("2024-11-05")), linetype = "dashed", color = "black") +
  
  # Color scheme
  scale_color_manual(values = c("Harris" = "blue", "Trump" = "red")) +
  scale_fill_manual(values = c("Harris" = "blue", "Trump" = "red")) +
  
  # Theme for cleaner look
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate labels slightly to 45 degrees
  )



### PLot the swing states Trend ###

# # Define a list of swing states
# swing_states <- c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin")
# 
# # Filter for swing states from the original dataset and assign them to Harris and Trump datasets
# state_poll_data <- data %>%
#   filter(state %in% swing_states) %>%
#   mutate(
#     end_date = mdy(end_date),
#     Candidate = if_else(candidate_name == "Kamala Harris", "Harris", "Trump")
#   )
# 
# # Generate the prediction data frame with dates up to November 5, 2024
# days_range <- data.frame(days_after_earliest = seq(0, days_to_last_date, by = 1))
# days_range$end_date <- earliest_date + days_range$days_after_earliest
# 
# # Harris predictions
# harris_predictions <- predict(harris_model, newdata = days_range, se.fit = TRUE)
# days_range$harris_pct <- harris_predictions$fit
# days_range$harris_se <- harris_predictions$se.fit
# 
# # Trump predictions
# trump_predictions <- predict(trump_model, newdata = days_range, se.fit = TRUE)
# days_range$trump_pct <- trump_predictions$fit
# days_range$trump_se <- trump_predictions$se.fit
# 
# # Repeat the prediction data for each state
# state_days_range <- days_range %>%
#   tidyr::crossing(state = swing_states)
# 
# # Prepare for faceted plot by merging actual poll data with the predictions
# state_poll_data <- state_poll_data %>%
#   filter(Candidate %in% c("Trump", "Harris"))  # Filter for only Trump and Harris
# 
# # Combine actual poll data with predictions for plotting
# plot_data <- state_days_range %>%
#   left_join(state_poll_data, by = c("end_date", "state"))
# 
# # Plot with faceting by state
# ggplot(data = plot_data, aes(x = end_date)) +
#   # Harris line and confidence interval
#   geom_line(aes(y = harris_pct, color = "Harris"), linewidth = 1) +
#   geom_ribbon(aes(ymin = harris_pct - 1.96 * harris_se, ymax = harris_pct + 1.96 * harris_se, fill = "Harris"), alpha = 0.2) +
#   
#   # Trump line and confidence interval
#   geom_line(aes(y = trump_pct, color = "Trump"), linewidth = 1) +
#   geom_ribbon(aes(ymin = trump_pct - 1.96 * trump_se, ymax = trump_pct + 1.96 * trump_se, fill = "Trump"), alpha = 0.2) +
#   
#   # Actual poll data points for Harris and Trump in each state
#   geom_point(data = state_poll_data, aes(x = end_date, y = pct, color = Candidate), alpha = 0.5) +
#   
#   # Facet by state
#   facet_wrap(~ state, ncol = 3, scales = "free_y") +
#   
#   # Axis labels and title
#   labs(
#     title = "Polling Trends for Harris and Trump by Swing State",
#     x = "Date",
#     y = "Polling Percentage (%)",
#     color = "Candidate",
#     fill = "Candidate"
#   ) +
#   
#   # Customize x-axis to show every 1 month, starting from August 2024 to November 2024
#   scale_x_date(
#     limits = c(as.Date("2024-08-01"), as.Date("2024-11-05")),
#     date_breaks = "1 month",
#     date_labels = "%b"
#   ) +
#   
#   # Add a dashed line for November 5, 2024
#   geom_vline(xintercept = as.numeric(as.Date("2024-11-05")), linetype = "dashed", color = "black") +
#   
#   # Customize colors for Harris and Trump
#   scale_color_manual(values = c("Harris" = "blue", "Trump" = "red")) +
#   scale_fill_manual(values = c("Harris" = "blue", "Trump" = "red")) +
#   
#   # Theme for cleaner look
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     strip.text = element_text(size = 10, face = "bold"),  # Style facet labels (state names)
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
#     panel.spacing = unit(1.5, "lines")  # Increase spacing between facets
#   )

# Save models individually
saveRDS(harris_model, file = "models/models_harris.rds")
saveRDS(trump_model, file = "models/models_trump.rds")


                               